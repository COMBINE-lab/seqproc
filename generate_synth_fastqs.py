#!/usr/bin/env python3
import argparse
import os
import random
from pathlib import Path


NUCS = "ACGT"


def nuc(i: int) -> str:
    # Same pattern as antisequence_benches.rs: N[i & 3]
    return NUCS[i & 3]


def mutate_anchor_hamming1(anchor: str) -> str:
    """Return a variant of `anchor` with Hamming distance exactly 1."""
    idx = random.randrange(len(anchor))
    orig = anchor[idx]
    choices = [b for b in NUCS if b != orig]
    new_base = random.choice(choices)
    return anchor[:idx] + new_base + anchor[idx + 1 :]


def mutate_anchor_far(anchor: str, min_distance: int = 2) -> str:
    """Return a variant of `anchor` with Hamming distance >= min_distance."""
    # For the 6-bp sci3 anchor, just flip two distinct positions.
    if len(anchor) < min_distance:
        raise ValueError("anchor too short for requested minimum distance")
    indices = random.sample(range(len(anchor)), k=min_distance)
    anchor_list = list(anchor)
    for idx in indices:
        orig = anchor_list[idx]
        choices = [b for b in NUCS if b != orig]
        anchor_list[idx] = random.choice(choices)
    return "".join(anchor_list)


def make_fastq_pair_10x(num_reads: int) -> tuple[str, str]:
    """Replicate make_fastq_pair_10x from antisequence_benches.rs.

    R1: 16bp barcode + 10bp UMI (26 nt total)
    R2: 60bp cDNA
    """
    lines_r1 = []
    lines_r2 = []
    for i in range(num_reads):
        # Headers
        lines_r1.append(f"@r{i}\n")
        lines_r2.append(f"@r{i}\n")

        # R1 sequence: 16bp barcode + 10bp UMI
        seq_r1 = []
        for j in range(16):
            seq_r1.append(nuc(i + j))
        for j in range(10):
            seq_r1.append(nuc(i + 16 + j + 1))
        seq_r1_str = "".join(seq_r1)
        lines_r1.append(seq_r1_str + "\n")
        lines_r1.append("+\n")
        lines_r1.append("I" * (16 + 10) + "\n")

        # R2 sequence: 60bp cDNA
        seq_r2 = []
        for j in range(60):
            seq_r2.append(nuc(i + j * 7 + 3))
        seq_r2_str = "".join(seq_r2)
        lines_r2.append(seq_r2_str + "\n")
        lines_r2.append("+\n")
        lines_r2.append("I" * 60 + "\n")

    return ("".join(lines_r1), "".join(lines_r2))


def make_fastq_pair_sci3(
    num_reads: int,
    anchor_hamming1_prob: float = 0.0,
    anchor_bad_prob: float = 0.0,
) -> tuple[str, str]:
    """Replicate make_fastq_pair_sci3 from antisequence_benches.rs.

    R1: brc1 (9 or 10 nt) + anchor CAGAGC + UMI[8] + b[10]
    R2: 80bp cDNA
    """
    anchor = "CAGAGC"
    lines_r1 = []
    lines_r2 = []
    for i in range(num_reads):
        lines_r1.append(f"@r{i}\n")
        lines_r2.append(f"@r{i}\n")

        # brc1: length 9 or 10 alternating
        bc_len = 9 if i % 2 == 0 else 10
        seq_r1 = []
        for j in range(bc_len):
            seq_r1.append(nuc(i + j * 5))
        # anchor (optionally mutated depending on probabilities)
        r = random.random()
        if r < anchor_hamming1_prob:
            this_anchor = mutate_anchor_hamming1(anchor)
        elif r < anchor_hamming1_prob + anchor_bad_prob:
            this_anchor = mutate_anchor_far(anchor)
        else:
            this_anchor = anchor
        seq_r1.append(this_anchor)
        # UMI 8
        for j in range(8):
            seq_r1.append(nuc(i + j * 11 + 2))
        # trailing b[10]
        for j in range(10):
            seq_r1.append(nuc(i + j * 13 + 1))
        seq_r1_str = "".join(seq_r1)
        lines_r1.append(seq_r1_str + "\n")
        lines_r1.append("+\n")
        lines_r1.append("I" * len(seq_r1_str) + "\n")

        # R2 cDNA 80bp
        seq_r2 = []
        for j in range(80):
            seq_r2.append(nuc(i + j * 9 + 7))
        seq_r2_str = "".join(seq_r2)
        lines_r2.append(seq_r2_str + "\n")
        lines_r2.append("+\n")
        lines_r2.append("I" * 80 + "\n")

    return ("".join(lines_r1), "".join(lines_r2))


def make_fastq_pair_splitseq(num_reads: int) -> tuple[str, str]:
    """
    R1: 60bp cDNA (Bio)
    R2: 86bp Tech (UMI 10 + R3 8 + L2 30 + R2 8 + L1 22 + R1 8)
    Structure based on SPLiT-seq tutorial positions.
    """
    # Linker 2 (between R3 and R2)
    linker2 = "GTGGCCGATGTTTCGCATCGGCGTACGACT"
    # Linker 1 (between R2 and R1)
    linker1 = "ATCCACGTGCTTGAGACTGTGG"

    lines_r1 = []
    lines_r2 = []

    for i in range(num_reads):
        lines_r1.append(f"@r{i}\n")
        lines_r2.append(f"@r{i}\n")

        # R1: cDNA 60bp
        seq_r1 = []
        for j in range(60):
            seq_r1.append(nuc(i + j * 7 + 3))
        seq_r1_str = "".join(seq_r1)
        lines_r1.append(seq_r1_str + "\n")
        lines_r1.append("+\n")
        lines_r1.append("I" * 60 + "\n")

        # R2: UMI(10) + R3(8) + L2(30) + R2(8) + L1(22) + R1(8)
        seq_r2 = []
        # UMI 10
        for j in range(10):
            seq_r2.append(nuc(i + j * 11 + 1))
        # R3 8
        for j in range(8):
            seq_r2.append(nuc(i + j * 13 + 2))
        # Linker 2 (30)
        seq_r2.append(linker2)
        # R2 8
        for j in range(8):
            seq_r2.append(nuc(i + j * 17 + 3))
        # Linker 1 (22)
        seq_r2.append(linker1)
        # R1 8
        for j in range(8):
            seq_r2.append(nuc(i + j * 19 + 4))

        seq_r2_str = "".join(seq_r2)
        lines_r2.append(seq_r2_str + "\n")
        lines_r2.append("+\n")
        lines_r2.append("I" * len(seq_r2_str) + "\n")

    return ("".join(lines_r1), "".join(lines_r2))


def write_pair(r1: str, r2: str, out_dir: Path, prefix: str) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    r1_path = out_dir / f"{prefix}_R1.fastq"
    r2_path = out_dir / f"{prefix}_R2.fastq"
    r1_path.write_text(r1, encoding="utf-8")
    r2_path.write_text(r2, encoding="utf-8")
    print(f"Wrote {r1_path} and {r2_path}")


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description=(
            "Generate synthetic 10x and sci3 FASTQ pairs matching the "
            "antisequence_benches.rs generators (uncompressed)."
        )
    )
    p.add_argument(
        "--protocol",
        choices=["10x", "sci3", "split_seq", "both", "all"],
        default="all",
        help="Which protocol(s) to generate (default: all)",
    )
    p.add_argument(
        "--run-sizes",
        type=int,
        nargs="+",
        default=[1000, 10000, 1_000_000, 10_000_000],
        help="List of N (reads) to generate for each protocol",
    )
    p.add_argument(
        "--out-dir",
        type=str,
        default="data/bench",
        help="Output directory (relative or absolute)",
    )

    p.add_argument(
        "--anchor-hamming1-prob",
        type=float,
        default=0.0,
        help=(
            "Per-read probability of inserting a 1-mismatch variant of the sci3 "
            "anchor in R1 (default 0.0, i.e. always use the exact anchor)."
        ),
    )
    p.add_argument(
        "--anchor-bad-prob",
        type=float,
        default=0.0,
        help=(
            "Per-read probability of inserting an anchor with Hamming distance "
            ">=2 from the true sci3 anchor in R1 (default 0.0)."
        ),
    )
    return p.parse_args()


def main() -> None:
    args = parse_args()
    out_dir = Path(args.out_dir)

    if args.anchor_hamming1_prob < 0.0 or args.anchor_bad_prob < 0.0:
        raise SystemExit("Anchor mismatch probabilities must be non-negative")
    if args.anchor_hamming1_prob + args.anchor_bad_prob > 1.0 + 1e-9:
        raise SystemExit("anchor_hamming1_prob + anchor_bad_prob must be <= 1.0")

    do_all = args.protocol == "all"
    do_10x = args.protocol in {"10x", "both"} or do_all
    do_sci3 = args.protocol in {"sci3", "both"} or do_all
    do_split = args.protocol == "split_seq" or do_all

    for n in args.run_sizes:
        if do_10x:
            r1, r2 = make_fastq_pair_10x(n)
            write_pair(r1, r2, out_dir, prefix=f"10x_N{n}")
        if do_sci3:
            r1, r2 = make_fastq_pair_sci3(
                n,
                args.anchor_hamming1_prob,
                args.anchor_bad_prob,
            )
            write_pair(r1, r2, out_dir, prefix=f"sci3_N{n}")
        if do_split:
            r1, r2 = make_fastq_pair_splitseq(n)
            write_pair(r1, r2, out_dir, prefix=f"split_seq_N{n}")


if __name__ == "__main__":
    main()
