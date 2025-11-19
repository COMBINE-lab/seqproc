#!/usr/bin/env bash
set -euo pipefail

# Run a sweep over (ANTISEQ_CHUNK_SIZE, ANTISEQ_THREADS) and store
# both raw Criterion output and CSV summaries (via bench_change_to_csv.py).

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

BASELINE_NAME="base_c1024_t2"
OUT_DIR="sweeps"
mkdir -p "$OUT_DIR"

# Baseline: chunk size 1024, threads 2
export ANTISEQ_CHUNK_SIZE=1024
export ANTISEQ_THREADS=2
export ANTISEQ_LARGE=1

echo "=== Running baseline: CHUNK_SIZE=$ANTISEQ_CHUNK_SIZE THREADS=$ANTISEQ_THREADS ==="
cargo bench --bench antisequence_benches -- --save-baseline "$BASELINE_NAME"

# Sweep matrix
CHUNKS=(256 512 1024 2048)
THREADS=(1 2 4 8)

for cs in "${CHUNKS[@]}"; do
  for th in "${THREADS[@]}"; do
    export ANTISEQ_CHUNK_SIZE="$cs"
    export ANTISEQ_THREADS="$th"
    export ANTISEQ_LARGE=1

    label="c${cs}_t${th}"
    txt="$OUT_DIR/bench_${label}.txt"
    csv="$OUT_DIR/bench_${label}.csv"

    echo "=== Running config: CHUNK_SIZE=$cs THREADS=$th ==="
    cargo bench --bench antisequence_benches -- --baseline "$BASELINE_NAME" | tee "$txt"
    python3 bench_change_to_csv.py -o "$csv" "$txt"
  done
done
