#!/usr/bin/env python3
import sys, re, csv, os
from datetime import datetime

# Usage:
#   cat benchmark_output.txt | python bench_times_to_csv.py
#   python bench_times_to_csv.py benchmark_output.txt
#   python bench_times_to_csv.py -o my_times.csv benchmark_output.txt
#
# CSV columns: timestamp,benchmark,time_mid_seconds


def to_seconds(value: float, unit: str) -> float:
    u = unit.strip().lower()
    if u in {"s", "sec", "secs", "second", "seconds"}:
        return value
    if u in {"ms", "millisecond", "milliseconds"}:
        return value / 1000.0
    # microseconds: allow both "us" and "µs"
    if u in {"us", "µs", "microsecond", "microseconds"}:
        return value / 1_000_000.0
    if u in {"ns", "nanosecond", "nanoseconds"}:
        return value / 1_000_000_000.0
    # Fallback: assume seconds
    return value


def looks_like_bench_name(s: str) -> bool:
    s = s.rstrip()
    if not s:
        return False
    if s.startswith(" "):
        return False
    if s.startswith("Benchmarking "):
        return False
    if s.startswith("Gnuplot "):
        return False
    if s.startswith("Warning:"):
        return False
    if s.startswith("Found "):
        return False
    if s.startswith("Performance "):
        return False
    # line like "antisequence_10x_trivial/10x_N=1000"
    return True


def main() -> None:
    args = sys.argv[1:]
    out_csv = "benchmark_times.csv"
    in_path = None

    if "-o" in args:
        i = args.index("-o")
        if i + 1 >= len(args):
            print("Error: -o requires a filename", file=sys.stderr)
            sys.exit(2)
        out_csv = args[i + 1]
        args = args[:i] + args[i + 2 :]
    if args:
        in_path = args[0]

    # Read lines
    if in_path:
        with open(in_path, "r", encoding="utf-8", errors="replace") as f:
            lines = f.readlines()
    else:
        lines = sys.stdin.read().splitlines()

    # Prepare CSV (overwrite for this run; per-config files are separate)
    fcsv = open(out_csv, "w", newline="", encoding="utf-8")
    writer = csv.writer(fcsv)
    writer.writerow(["timestamp", "benchmark", "time_mid_seconds"])

    # Regex to capture the three time values and units inside brackets
    # Example: time:   [748.20 µs 759.71 µs 774.92 µs]
    time_re = re.compile(
        r"time:\s*"  # prefix
        r"\[\s*"    # opening bracket
        r"([0-9.]+)\s*(\S+)\s+"  # first value + unit
        r"([0-9.]+)\s*(\S+)\s+"  # second (median) value + unit
        r"([0-9.]+)\s*(\S+)\s*"  # third value + unit
        r"\]",
        re.IGNORECASE,
    )

    current_bench = None

    for line in lines:
        if looks_like_bench_name(line):
            current_bench = line.strip()
            continue

        m = time_re.search(line)
        if m and current_bench:
            try:
                mid_val = float(m.group(3))
                mid_unit = m.group(4)
            except ValueError:
                continue
            tsec = to_seconds(mid_val, mid_unit)
            ts = datetime.now().astimezone().isoformat(timespec="seconds")
            writer.writerow([ts, current_bench, tsec])

    fcsv.close()


if __name__ == "__main__":
    main()
