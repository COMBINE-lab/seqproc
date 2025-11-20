#!/usr/bin/env python3
import argparse
import csv
import os
import shlex
import subprocess
import sys
import time
from datetime import datetime
from typing import List


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Run an external command multiple times, measure wall-clock time, "
            "and append a median time row to a CSV (similar to bench_times_to_csv.py)."
        )
    )

    parser.add_argument("--tool", required=True, help="Name of the tool (e.g. seqproc, splitcode)")
    parser.add_argument("--protocol", required=True, help="Protocol name (e.g. 10x, sci3)")
    parser.add_argument("--run-size", type=int, required=True, help="Number of reads (N) processed")
    parser.add_argument("--threads", type=int, required=True, help="Number of threads used by the tool")
    parser.add_argument(
        "--runs",
        type=int,
        default=5,
        help="Number of repetitions to time (median of these will be recorded)",
    )
    parser.add_argument(
        "-o",
        "--out-csv",
        default="benchmark_command_times.csv",
        help="Output CSV file (will be created if missing, otherwise appended)",
    )
    parser.add_argument(
        "--benchmark-name",
        default=None,
        help=(
            "Optional benchmark name. If omitted, a name like "
            "'<tool>_<protocol>_N=<run_size>_T=<threads>' is used."
        ),
    )

    # Everything after "--" is treated as the command to run.
    if "--" in sys.argv:
        idx = sys.argv.index("--")
        known_args = sys.argv[1:idx]
        cmd_args = sys.argv[idx + 1 :]
    else:
        # Fallback: require explicit -- to avoid accidental parsing mistakes
        parser.error("You must separate script args from the command with --, e.g. ... -- seqproc ...")
        raise SystemExit(2)

    args = parser.parse_args(known_args)
    args.command = cmd_args
    return args


def ensure_csv_with_header(path: str) -> None:
    """Create CSV file with header if it does not exist or is empty."""
    need_header = not os.path.exists(path) or os.path.getsize(path) == 0
    if need_header:
        with open(path, "w", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            writer.writerow(
                [
                    "timestamp",
                    "benchmark",
                    "time_mid_seconds",
                    "tool",
                    "protocol",
                    "run_size",
                    "threads",
                    "runs",
                    "command",
                ]
            )


def run_command_timed(cmd: List[str]) -> float:
    """Run a command and return wall-clock duration in seconds.

    Raises subprocess.CalledProcessError if the command fails.
    """
    start = time.perf_counter()
    result = subprocess.run(cmd, check=True)
    end = time.perf_counter()
    # If the command wrote a lot to stdout/stderr, user is responsible for redirecting.
    return end - start


def median(values: List[float]) -> float:
    if not values:
        raise ValueError("No values for median")
    vs = sorted(values)
    n = len(vs)
    mid = n // 2
    if n % 2 == 1:
        return vs[mid]
    return 0.5 * (vs[mid - 1] + vs[mid])


def main() -> None:
    args = parse_args()

    if not args.command:
        print("Error: No command provided after --", file=sys.stderr)
        sys.exit(2)

    runs = max(1, int(args.runs))

    # Human-readable benchmark name
    benchmark = args.benchmark_name
    if benchmark is None:
        benchmark = f"{args.tool}_{args.protocol}_N={args.run_size}_T={args.threads}"

    # Time the command multiple times
    durations: List[float] = []
    cmd_display = " ".join(shlex.quote(c) for c in args.command)

    for i in range(runs):
        try:
            dur = run_command_timed(args.command)
        except subprocess.CalledProcessError as e:
            print(
                f"Run {i+1}/{runs} for benchmark '{benchmark}' failed "
                f"with exit code {e.returncode}",
                file=sys.stderr,
            )
            sys.exit(e.returncode if e.returncode is not None else 1)
        durations.append(dur)

    t_mid = median(durations)

    ensure_csv_with_header(args.out_csv)

    ts = datetime.now().astimezone().isoformat(timespec="seconds")
    with open(args.out_csv, "a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(
            [
                ts,
                benchmark,
                t_mid,
                args.tool,
                args.protocol,
                int(args.run_size),
                int(args.threads),
                runs,
                cmd_display,
            ]
        )

    print(
        f"Recorded benchmark '{benchmark}' to {args.out_csv}: "
        f"median time {t_mid:.6f} s over {runs} runs",
        file=sys.stderr,
    )


if __name__ == "__main__":
    main()
