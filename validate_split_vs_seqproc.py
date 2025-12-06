#!/usr/bin/env python3
import argparse
import sys
import gzip

def open_fastq(path):
    if path.endswith('.gz'):
        return gzip.open(path, 'rt')
    else:
        return open(path, 'rt')

def read_fastq_entry(f):
    # Read 4 lines
    try:
        head = f.readline()
        if not head: return None
        seq = f.readline()
        plus = f.readline()
        qual = f.readline()
        return (head.strip(), seq.strip(), plus.strip(), qual.strip())
    except ValueError:
        return None

def parse_read_id(header):
    # header: @SRR... or @ReadName ...
    # splitcode/seqproc might modify headers or append comments, and splitcode
    # appends barcode annotations after a '::' suffix. We normalize by:
    #   1) taking the first whitespace-delimited token
    #   2) stripping the leading '@'
    #   3) dropping everything after the first '::' (if present)
    first = header.split()[0][1:]
    return first.split("::", 1)[0]

LINKER1 = "GTGGCCGCTGTTTCGCATCGGCGTACGACT"
LINKER2 = "ATCCACGTGCTTGAGAGGCCAGAGCATTCG"


def find_linker_hamming(seq: str, pattern: str, max_dist: int = 2):
    """Return the best match position of 'pattern' in 'seq' with Hamming<=max_dist.

    This mirrors seqproc's behavior of allowing up to max_dist mismatches over
    a fixed-length linker sequence. If no position within the threshold is
    found, returns None.
    """
    m = len(pattern)
    n = len(seq)
    if n < m:
        return None

    best_pos = None
    best_dist = max_dist + 1
    for i in range(0, n - m + 1):
        window = seq[i : i + m]
        # Simple Hamming distance on A/C/G/T/N; treat 'N' as mismatch.
        dist = sum(1 for a, b in zip(window, pattern) if a != b)
        if dist < best_dist:
            best_dist = dist
            best_pos = i
            if dist == 0:
                break

    if best_dist <= max_dist:
        return best_pos
    return None


def main():
    parser = argparse.ArgumentParser(description="Validate seqproc output against splitcode output (subset)")
    parser.add_argument("--sp-r1", required=True, help="Seqproc R1")
    parser.add_argument("--sp-r2", required=True, help="Seqproc R2")
    parser.add_argument("--sc-r1", required=True, help="Splitcode R1")
    parser.add_argument("--sc-r2", required=True, help="Splitcode R2")
    args = parser.parse_args()

    print(f"Loading Seqproc reads from {args.sp_r1} and {args.sp_r2}...")
    
    sp_reads = {} # ID -> (seq1, seq2)
    
    with open_fastq(args.sp_r1) as f1, open_fastq(args.sp_r2) as f2:
        while True:
            r1 = read_fastq_entry(f1)
            r2 = read_fastq_entry(f2)
            if not r1 or not r2:
                break
                
            id1 = parse_read_id(r1[0])
            id2 = parse_read_id(r2[0])
            
            if id1 != id2:
                print(f"Error: Seqproc R1/R2 desync: {id1} != {id2}")
                sys.exit(1)
                
            sp_reads[id1] = (r1[1], r2[1])

    print(f"Loaded {len(sp_reads)} reads from Seqproc.")

    print(f"Streaming Splitcode reads from {args.sc_r1} and {args.sc_r2}...")
    
    sc_total = 0
    matches = 0
    mismatches = 0
    not_in_sp = 0
    
    with open_fastq(args.sc_r1) as f1, open_fastq(args.sc_r2) as f2:
        while True:
            r1 = read_fastq_entry(f1)
            r2 = read_fastq_entry(f2)
            if not r1 or not r2:
                break
            
            sc_total += 1
            if sc_total % 100000 == 0:
                print(f"Processed {sc_total} splitcode reads...", end='\r')

            id1 = parse_read_id(r1[0])
            id2 = parse_read_id(r2[0])
            
            if id1 != id2:
                print(f"Error: Splitcode R1/R2 desync: {id1} != {id2}")
                sys.exit(1)
            
            if id1 in sp_reads:
                sp_seq1, sp_seq2 = sp_reads[id1]
                sc_seq1, sc_seq2 = r1[1], r2[1]
                
                # Check R1: seqproc generally passes R1 through unchanged, while
                # splitcode may prepend a final-barcode sequence to R1. We
                # treat R1 as matching if either the full sequences are equal,
                # or if the seqproc R1 matches the *suffix* of splitcode R1.
                if sp_seq1 == sc_seq1:
                    r1_ok = True
                elif len(sc_seq1) > len(sp_seq1) and sc_seq1.endswith(sp_seq1):
                    r1_ok = True
                else:
                    r1_ok = False
                
                # Check R2: seqproc outputs UMI+BC3+BC2+BC1, while splitcode's R2
                # here includes additional sequence (e.g. poly-A and full read).
                # To compare apples-to-apples under Hamming-tolerant linkers, we
                # locate approximate linker1/linker2 positions in the splitcode R2
                # (<=2 mismatches) and extract:
                #   (i1-18:i1)       -> UMI(10)+BC3(8)
                #   (i1+L1:i1+L1+8) -> BC2(8)
                #   (i2+L2:i2+L2+8) -> BC1(8)
                i1 = find_linker_hamming(sc_seq2, LINKER1, 2)
                i2 = find_linker_hamming(sc_seq2, LINKER2, 2)
                if i1 is not None and i2 is not None and i1 >= 18:
                    L1 = len(LINKER1)
                    L2 = len(LINKER2)
                    umi_bc3 = sc_seq2[i1-18:i1]
                    bc2 = sc_seq2[i1+L1:i1+L1+8]
                    bc1 = sc_seq2[i2+L2:i2+L2+8]
                    sc_transformed = umi_bc3 + bc2 + bc1
                    r2_ok = (sp_seq2 == sc_transformed)
                else:
                    r2_ok = False
                
                if r1_ok and r2_ok:
                    matches += 1
                else:
                    mismatches += 1
                    if mismatches <= 10:
                        print(f"\nMismatch at {id1}:")
                        if not r1_ok:
                            print(f"  R1 Mismatch:")
                            print(f"    SP: {sp_seq1}")
                            print(f"    SC: {sc_seq1}")
                        if not r2_ok:
                            print(f"  R2 Mismatch:")
                            print(f"    SP: {sp_seq2}")
                            print(f"    SC (raw): {sc_seq2}")
                            if len(sc_seq2) >= 94:
                                print(f"    SC (tr):  {sc_transformed}")
                            else:
                                print(f"    SC too short ({len(sc_seq2)})")
            else:
                not_in_sp += 1

    print(f"\nValidation Complete.")
    print(f"Total Splitcode Reads: {sc_total}")
    print(f"Total Seqproc Reads:   {len(sp_reads)}")
    print(f"Intersection Count:    {matches + mismatches}")
    print(f"Matches:               {matches}")
    print(f"Mismatches:            {mismatches}")
    
    if mismatches == 0:
        print("SUCCESS: All intersecting reads match exactly.")
    else:
        print(f"FAILURE: {mismatches} reads differed.")

if __name__ == "__main__":
    main()
