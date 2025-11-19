#!/usr/bin/env python3
import sys, re, csv, os
from datetime import datetime, timezone

# Usage:
#   cat benchmark_output.txt | python bench_change_to_csv.py
#   python bench_change_to_csv.py benchmark_output.txt
#   python bench_change_to_csv.py -o my_changes.csv benchmark_output.txt
#
# CSV columns: timestamp,benchmark,change_mid_percent

def main():
    args = sys.argv[1:]
    out_csv = "benchmark_changes.csv"
    in_path = None
    if "-o" in args:
        i = args.index("-o")
        if i + 1 >= len(args):
            print("Error: -o requires a filename", file=sys.stderr)
            sys.exit(2)
        out_csv = args[i+1]
        args = args[:i] + args[i+2:]
    if args:
        in_path = args[0]

    # Read lines
    if in_path:
        with open(in_path, "r", encoding="utf-8", errors="replace") as f:
            lines = f.readlines()
    else:
        lines = sys.stdin.read().splitlines()

    # Prepare CSV (add header if new)
    new_file = not os.path.exists(out_csv)
    fcsv = open(out_csv, "a", newline="", encoding="utf-8")
    writer = csv.writer(fcsv)
    if new_file:
        writer.writerow(["timestamp", "benchmark", "change_mid_percent"])

    # Regex to capture the 3 values inside brackets
    change_re = re.compile(r"change:\s*\[\s*([+-]?\d+(?:\.\d+)?)%\s+([+-]?\d+(?:\.\d+)?)%\s+([+-]?\d+(?:\.\d+)?)%\s*\]", re.IGNORECASE)

    current_bench = None
    # A simple filter for lines that look like a benchmark id (Criterion prints a bare line with the name)
    def looks_like_bench_name(s: str) -> bool:
        s = s.rstrip()
        if not s: return False
        if s.startswith(" "): return False
        if s.startswith("Benchmarking "): return False
        if s.startswith("Gnuplot "): return False
        if s.startswith("Warning:"): return False
        if s.startswith("Found "): return False
        if s.startswith("Performance "): return False
        # line like "antisequence_10x_trivial/10x_N=1000"
        return True

    for line in lines:
        if looks_like_bench_name(line):
            current_bench = line.strip()
            continue

        m = change_re.search(line)
        if m and current_bench:
            # middle percent
            mid = m.group(2)
            # timestamp in local time (ISO 8601)
            ts = datetime.now().astimezone().isoformat(timespec="seconds")
            writer.writerow([ts, current_bench, mid])
            # keep current_bench to allow multiple matches under same section if present

    fcsv.close()

if __name__ == "__main__":
    main()
