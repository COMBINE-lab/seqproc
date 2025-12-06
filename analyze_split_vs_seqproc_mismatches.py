#!/usr/bin/env python3
import argparse
import gzip

LINKER1 = "GTGGCCGCTGTTTCGCATCGGCGTACGACT"
LINKER2 = "ATCCACGTGCTTGAGAGGCCAGAGCATTCG"

PAPER_BC_PATH = "matchbox_2018_eval/round1_barcodes_paper.txt"


def open_fastq(path):
    if path.endswith(".gz"):
        return gzip.open(path, "rt")
    return open(path, "rt")


def read_fastq_entry(f):
    try:
        head = f.readline()
        if not head:
            return None
        seq = f.readline()
        plus = f.readline()
        qual = f.readline()
        return head.strip(), seq.strip(), plus.strip(), qual.strip()
    except ValueError:
        return None


def parse_read_id(header: str) -> str:
    # Normalize headers from both tools: strip leading '@', drop '::[...]' suffix
    first = header.split()[0][1:]
    return first.split("::", 1)[0]


def find_linker_hamming(seq: str, pattern: str, max_dist: int = 2):
    """Return best match position of 'pattern' in 'seq' with Hamming<=max_dist.

    Mirrors seqproc's behavior of allowing up to max_dist mismatches over a
    fixed-length linker sequence. If no such position exists, returns None.
    """
    m = len(pattern)
    n = len(seq)
    if n < m:
        return None

    best_pos = None
    best_dist = max_dist + 1
    for i in range(0, n - m + 1):
        window = seq[i : i + m]
        dist = sum(1 for a, b in zip(window, pattern) if a != b)
        if dist < best_dist:
            best_dist = dist
            best_pos = i
            if dist == 0:
                break

    if best_dist <= max_dist:
        return best_pos
    return None


def load_paper_barcodes(path: str = PAPER_BC_PATH) -> set[str]:
    """Load the 96-element SPLiT-seq 2018 barcode library from the paper.

    This is used only for *reporting* how often BC1/2/3 fall outside the
    designed barcode set; it does not change match/mismatch semantics.
    """
    barcodes: set[str] = set()
    with open(path) as f:
        for line in f:
            s = line.strip()
            if s and not s.startswith("#"):
                barcodes.add(s)
    return barcodes


def transform_splitcode_r2(seq2: str):
    """Extract UMI+BC3+BC2+BC1 from splitcode R2 by locating linker1/linker2.

    Uses a Hamming-distance search (<=2 mismatches) for each linker to match
    seqproc's Hamming-tolerant linker behavior.
    """
    i1 = find_linker_hamming(seq2, LINKER1, 2)
    i2 = find_linker_hamming(seq2, LINKER2, 2)
    if i1 is None or i2 is None or i1 < 18:
        return None
    L1 = len(LINKER1)
    L2 = len(LINKER2)
    umi_bc3 = seq2[i1 - 18 : i1]
    bc2 = seq2[i1 + L1 : i1 + L1 + 8]
    bc1 = seq2[i2 + L2 : i2 + L2 + 8]
    return umi_bc3 + bc2 + bc1


def main():
    ap = argparse.ArgumentParser(
        description="Analyze where seqproc and splitcode disagree on UMI/BC3/BC2/BC1 (distance-2 run)"
    )
    ap.add_argument("--sp-r1", required=True, help="Seqproc R1 (distance-2)")
    ap.add_argument("--sp-r2", required=True, help="Seqproc R2 (distance-2)")
    ap.add_argument("--sc-r1", required=True, help="Splitcode R1 (distance-2)")
    ap.add_argument("--sc-r2", required=True, help="Splitcode R2 (distance-2)")
    args = ap.parse_args()

    # Load all seqproc reads into memory (19k reads; fine)
    print(f"Loading Seqproc reads from {args.sp_r1} and {args.sp_r2}...")
    sp_reads = {}
    with open_fastq(args.sp_r1) as f1, open_fastq(args.sp_r2) as f2:
        while True:
            r1 = read_fastq_entry(f1)
            r2 = read_fastq_entry(f2)
            if not r1 or not r2:
                break
            id1 = parse_read_id(r1[0])
            id2 = parse_read_id(r2[0])
            if id1 != id2:
                raise RuntimeError(f"Seqproc R1/R2 desync: {id1} != {id2}")
            sp_reads[id1] = (r1[1], r2[1])
    print(f"Loaded {len(sp_reads)} seqproc reads.")

    # Load paper-derived 96-barcode library to assess whitelist adherence.
    paper_barcodes = load_paper_barcodes()
    print(f"Loaded {len(paper_barcodes)} paper barcodes from {PAPER_BC_PATH}.")

    # Stream splitcode and compare
    total_sc = 0
    in_intersection = 0
    matches = 0
    mismatches = 0

    # Per-component mismatch counters among intersecting reads
    umi_diff = bc3_diff = bc2_diff = bc1_diff = 0
    only_bc1_diff = 0

    # Counts of reads whose BC1/2/3 fall outside the 96-barcode paper library
    sp_off_whitelist = 0
    sc_off_whitelist = 0

    with open_fastq(args.sc_r1) as f1, open_fastq(args.sc_r2) as f2:
        while True:
            r1 = read_fastq_entry(f1)
            r2 = read_fastq_entry(f2)
            if not r1 or not r2:
                break
            total_sc += 1
            if total_sc % 100000 == 0:
                print(f"Processed {total_sc} splitcode reads...", end="\r")

            id1 = parse_read_id(r1[0])
            id2 = parse_read_id(r2[0])
            if id1 != id2:
                raise RuntimeError(f"Splitcode R1/R2 desync: {id1} != {id2}")

            if id1 not in sp_reads:
                continue

            in_intersection += 1
            sp_seq1, sp_seq2 = sp_reads[id1]
            sc_seq1, sc_seq2 = r1[1], r2[1]

            # R1 comparison with suffix rule (same as validator)
            if sp_seq1 == sc_seq1 or (
                len(sc_seq1) > len(sp_seq1) and sc_seq1.endswith(sp_seq1)
            ):
                r1_ok = True
            else:
                r1_ok = False

            sc_trans = transform_splitcode_r2(sc_seq2)
            if sc_trans is None:
                r2_ok = False
            else:
                r2_ok = (sp_seq2 == sc_trans)

            # Decompose into UMI/BC3/BC2/BC1 (10/8/8/8) whenever possible, so we
            # can both compute per-component diffs and assess whitelist usage.
            if sc_trans is None or len(sp_seq2) < 34 or len(sc_trans) < 34:
                if r1_ok and r2_ok:
                    matches += 1
                else:
                    mismatches += 1
                continue

            sp_umi, sp_bc3, sp_bc2, sp_bc1 = (
                sp_seq2[0:10],
                sp_seq2[10:18],
                sp_seq2[18:26],
                sp_seq2[26:34],
            )
            sc_umi, sc_bc3, sc_bc2, sc_bc1 = (
                sc_trans[0:10],
                sc_trans[10:18],
                sc_trans[18:26],
                sc_trans[26:34],
            )

            # Track how often each tool produces BCs outside the 96-barcode
            # library from the paper (based on BC1/2/3 only).
            if not (sp_bc3 in paper_barcodes and sp_bc2 in paper_barcodes and sp_bc1 in paper_barcodes):
                sp_off_whitelist += 1
            if not (sc_bc3 in paper_barcodes and sc_bc2 in paper_barcodes and sc_bc1 in paper_barcodes):
                sc_off_whitelist += 1

            if r1_ok and r2_ok:
                matches += 1
                continue

            mismatches += 1

            u_eq = sp_umi == sc_umi
            b3_eq = sp_bc3 == sc_bc3
            b2_eq = sp_bc2 == sc_bc2
            b1_eq = sp_bc1 == sc_bc1

            if not u_eq:
                umi_diff += 1
            if not b3_eq:
                bc3_diff += 1
            if not b2_eq:
                bc2_diff += 1
            if not b1_eq:
                bc1_diff += 1

            if u_eq and b3_eq and b2_eq and not b1_eq:
                only_bc1_diff += 1

    print("\nAnalysis complete.")
    print(f"Total Splitcode reads:      {total_sc}")
    print(f"Seqproc reads:             {len(sp_reads)}")
    print(f"Intersection (IDs):        {in_intersection}")
    print(f"Exact matches (R1+R2):     {matches}")
    print(f"Mismatches (R1 and/or R2): {mismatches}")
    if mismatches:
        print("Per-component mismatches among intersecting reads:")
        print(f"  UMI differs:    {umi_diff}")
        print(f"  BC3 differs:    {bc3_diff}")
        print(f"  BC2 differs:    {bc2_diff}")
        print(f"  BC1 differs:    {bc1_diff}")
        print(f"  Only BC1 differs (UMI+BC3+BC2 agree): {only_bc1_diff}")

    print("\nWhitelist adherence (paper 96-barcode library):")
    print(f"  Seqproc off-paper BC1/2/3 reads:   {sp_off_whitelist}")
    print(f"  Splitcode off-paper BC1/2/3 reads: {sc_off_whitelist}")


if __name__ == "__main__":
    main()
