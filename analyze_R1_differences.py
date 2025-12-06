#!/usr/bin/env python3
import argparse
import gzip
from collections import Counter

LINKER1 = "GTGGCCGCTGTTTCGCATCGGCGTACGACT"
LINKER2 = "ATCCACGTGCTTGAGAGGCCAGAGCATTCG"


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
    first = header.split()[0][1:]
    return first.split("::", 1)[0]


def hamming(a: str, b: str) -> int:
    return sum(x != y for x, y in zip(a, b))


def find_linker_hamming(seq: str, motif: str, max_dist: int) -> int | None:
    m = len(motif)
    best_i = None
    best_d = max_dist + 1
    if len(seq) < m:
        return None
    for i in range(0, len(seq) - m + 1):
        d = hamming(seq[i : i + m], motif)
        if d <= max_dist and d < best_d:
            best_i = i
            best_d = d
            if d == 0:
                break
    if best_i is None or best_d > max_dist:
        return None
    return best_i


def transform_splitcode_r2(seq2: str, max_dist: int = 2) -> str | None:
    i1 = find_linker_hamming(seq2, LINKER1, max_dist)
    i2 = find_linker_hamming(seq2, LINKER2, max_dist)
    if i1 is None or i2 is None or i1 < 18:
        return None
    L1 = len(LINKER1)
    L2 = len(LINKER2)
    umi_bc3 = seq2[i1 - 18 : i1]
    bc2 = seq2[i1 + L1 : i1 + L1 + 8]
    bc1 = seq2[i2 + L2 : i2 + L2 + 8]
    return umi_bc3 + bc2 + bc1


def classify_r1(sp: str, sc: str) -> str:
    if sp == sc:
        return "equal"
    if len(sc) > len(sp) and sc.endswith(sp):
        return "sc_superset_suffix"
    if len(sc) > len(sp) and sc.startswith(sp):
        return "sc_superset_prefix"
    if len(sp) > len(sc) and sp.endswith(sc):
        return "sp_superset_suffix"
    if len(sp) > len(sc) and sp.startswith(sc):
        return "sp_superset_prefix"
    return "other"


def main():
    ap = argparse.ArgumentParser(
        description="Analyze R1 differences between seqproc and splitcode on intersecting reads",
    )
    ap.add_argument("--sp-r1", required=True)
    ap.add_argument("--sp-r2", required=True)
    ap.add_argument("--sc-r1", required=True)
    ap.add_argument("--sc-r2", required=True)
    args = ap.parse_args()

    # Load all seqproc reads into memory
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

    total_sc = 0
    in_intersection = 0
    matches = 0
    mismatches = 0

    # R1 classification across intersection
    r1_class_all = Counter()
    r1_class_mismatches = Counter()

    # length deltas for mismatches
    len_deltas = []  # (len_sc - len_sp)

    with open_fastq(args.sc_r1) as f1, open_fastq(args.sc_r2) as f2:
        while True:
            r1 = read_fastq_entry(f1)
            r2 = read_fastq_entry(f2)
            if not r1 or not r2:
                break
            total_sc += 1

            id1 = parse_read_id(r1[0])
            id2 = parse_read_id(r2[0])
            if id1 != id2:
                raise RuntimeError(f"Splitcode R1/R2 desync: {id1} != {id2}")

            if id1 not in sp_reads:
                continue

            in_intersection += 1
            sp_seq1, sp_seq2 = sp_reads[id1]
            sc_seq1, sc_seq2 = r1[1], r2[1]

            # R1: same logic as validator (seqproc R1 must equal or be suffix of splitcode R1)
            if sp_seq1 == sc_seq1 or (
                len(sc_seq1) > len(sp_seq1) and sc_seq1.endswith(sp_seq1)
            ):
                r1_ok = True
            else:
                r1_ok = False

            # R2: compare seqproc's 34-bp UMI+BC3+BC2+BC1 to transformed splitcode R2
            sc_trans = transform_splitcode_r2(sc_seq2)
            if sc_trans is None:
                r2_ok = False
            else:
                r2_ok = (sp_seq2 == sc_trans)

            r1_cls = classify_r1(sp_seq1, sc_seq1)
            r1_class_all[r1_cls] += 1

            if r1_ok and r2_ok:
                matches += 1
                continue

            mismatches += 1
            r1_class_mismatches[r1_cls] += 1

            # track length deltas for mismatches
            len_deltas.append(len(sc_seq1) - len(sp_seq1))

    print("\nAnalysis complete.")
    print(f"Total Splitcode reads:      {total_sc}")
    print(f"Seqproc reads:             {len(sp_reads)}")
    print(f"Intersection (IDs):        {in_intersection}")
    print(f"Exact matches (R1+R2):     {matches}")
    print(f"Mismatches (R1 and/or R2): {mismatches}")

    print("\nR1 classification across all intersecting reads:")
    for k, v in r1_class_all.most_common():
        print(f"  {k:22s}: {v}")

    print("\nR1 classification among mismatches only:")
    for k, v in r1_class_mismatches.most_common():
        print(f"  {k:22s}: {v}")

    if len_deltas:
        n = len(len_deltas)
        mean_delta = sum(len_deltas) / n
        min_delta = min(len_deltas)
        max_delta = max(len_deltas)
        print("\nLength delta (len(sc) - len(sp)) among mismatches:")
        print(f"  count: {n}")
        print(f"  mean:  {mean_delta:.2f}")
        print(f"  min:   {min_delta}")
        print(f"  max:   {max_delta}")


if __name__ == "__main__":
    main()
