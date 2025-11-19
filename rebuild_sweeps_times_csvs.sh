#!/usr/bin/env bash
set -euo pipefail

# Rebuild sweeps/*.csv from existing Criterion text outputs
# using bench_times_to_csv.py to extract median times.
# This does NOT rerun any benchmarks.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

for txt in sweeps/bench_c*_t*.txt; do
  csv="${txt%.txt}.csv"
  echo "Rebuilding $csv from $txt"
  python3 bench_times_to_csv.py -o "$csv" "$txt"
done
