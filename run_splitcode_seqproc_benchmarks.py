#!/usr/bin/env python3
import argparse
import os
import shlex
import subprocess
import sys
from pathlib import Path


HERE = Path(__file__).resolve().parent

PROTOCOL_INPUT_NAME = {
    "10x": "10x",
    "sci3": "sci3",
    "sci3_tolerant": "sci3",
}

PROTOCOL_GEOM_NAME = {
    "10x": "10x.geom",
    "sci3": "sci3.geom",
    "sci3_tolerant": "sci3_tolerant.geom",
}

PROTOCOL_SPLITCODE_CONFIG_NAME = {
    "10x": "splitcode_10x.txt",
    "sci3": "splitcode_sci3.txt",
    "sci3_tolerant": "splitcode_sci3_tolerant.txt",
}


def run(cmd: str) -> None:
    print(f"[run] {cmd}")
    subprocess.run(cmd, shell=True, check=True)


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description=(
            "Run seqproc and splitcode benchmarks via bench_command_to_csv.py "
            "for 10x and sci3 at multiple N and threads."
        )
    )
    p.add_argument(
        "--run-sizes",
        type=int,
        nargs="+",
        default=[1_000_000, 10_000_000],
        help="List of N (reads) to benchmark",
    )
    p.add_argument(
        "--threads",
        type=int,
        nargs="+",
        default=[4],
        help="Thread counts to benchmark",
    )
    p.add_argument(
        "--runs",
        type=int,
        default=5,
        help="Repetitions per (tool,protocol,N,threads) for median time",
    )
    p.add_argument(
        "--out-csv",
        type=str,
        default="split_vs_seqproc_times.csv",
        help="Output CSV file for all benchmarks",
    )
    p.add_argument(
        "--splitcode-bin",
        type=str,
        default="../splitcode/build/src/splitcode",
        help="Path to splitcode binary",
    )
    p.add_argument(
        "--seqproc-cargo",
        type=str,
        default="cargo",
        help="cargo executable to use for running seqproc binary",
    )
    p.add_argument(
        "--data-dir",
        type=str,
        default="data/bench",
        help="Directory where synthetic FASTQs were generated",
    )
    p.add_argument(
        "--protocols",
        type=str,
        nargs="+",
        choices=["10x", "sci3", "sci3_tolerant"],
        default=["10x", "sci3"],
        help="Which protocols to benchmark (10x, sci3, sci3_tolerant)",
    )
    return p.parse_args()


def main() -> None:
    args = parse_args()

    bench_helper = HERE / "bench_command_to_csv.py"
    python_exe = shlex.quote(sys.executable)
    data_dir = (HERE / args.data_dir).resolve()

    splitcode_bin = Path(args.splitcode_bin).resolve()
    if not splitcode_bin.exists():
        raise SystemExit(f"splitcode binary not found at {splitcode_bin}")

    # Protocols to benchmark (10x, sci3, sci3_tolerant)
    protocols = args.protocols

    for protocol in protocols:
        input_name = PROTOCOL_INPUT_NAME[protocol]
        for n in args.run_sizes:
            r1 = data_dir / f"{input_name}_N{n}_R1.fastq"
            r2 = data_dir / f"{input_name}_N{n}_R2.fastq"
            if not (r1.exists() and r2.exists()):
                print(f"[skip] Missing {r1} or {r2}; run generate_synth_fastqs.py first")
                continue

            for t in args.threads:
                # splitcode benchmark
                split_cmd = " ".join(
                    [
                        shlex.quote(str(splitcode_bin)),
                        "-c",
                        shlex.quote(str(HERE / "splitcode_configs" / PROTOCOL_SPLITCODE_CONFIG_NAME[protocol])),
                        "--nFastqs=2",
                        shlex.quote(str(r1)),
                        shlex.quote(str(r2)),
                        "-o",
                        "/dev/null,/dev/null",
                        "-t",
                        str(t),
                    ]
                )

                run(
                    " ".join(
                        [
                            python_exe,
                            shlex.quote(str(bench_helper)),
                            "--tool",
                            "splitcode",
                            "--protocol",
                            protocol,
                            "--run-size",
                            str(n),
                            "--threads",
                            str(t),
                            "--runs",
                            str(args.runs),
                            "--out-csv",
                            shlex.quote(args.out_csv),
                            "--",
                            split_cmd,
                        ]
                    )
                )

                # seqproc benchmark via cargo run --bin seqproc
                # Assumes there is a geom file for each protocol in seqproc/, e.g. 10x.geom, sci3.geom, sci3_tolerant.geom
                geom_path = HERE / PROTOCOL_GEOM_NAME[protocol]
                if not geom_path.exists():
                    print(f"[skip seqproc] Missing geom file {geom_path}; create it to benchmark seqproc for {protocol}")
                    continue

                seqproc_cmd = " ".join(
                    [
                        shlex.quote(args.seqproc_cargo),
                        "run",
                        "--release",
                        "--bin",
                        "seqproc",
                        "--",
                        "--geom",
                        shlex.quote(str(geom_path)),
                        "--file1",
                        shlex.quote(str(r1)),
                        "--file2",
                        shlex.quote(str(r2)),
                        "--threads",
                        str(t),
                        "--out1",
                        "/dev/null",
                        "--out2",
                        "/dev/null",
                    ]
                )

                run(
                    " ".join(
                        [
                            python_exe,
                            shlex.quote(str(bench_helper)),
                            "--tool",
                            "seqproc",
                            "--protocol",
                            protocol,
                            "--run-size",
                            str(n),
                            "--threads",
                            str(t),
                            "--runs",
                            str(args.runs),
                            "--out-csv",
                            shlex.quote(args.out_csv),
                            "--",
                            seqproc_cmd,
                        ]
                    )
                )


if __name__ == "__main__":
    main()
