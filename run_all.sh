#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./run_all.sh                # run cargo test, then all benches
#   ./run_all.sh preopt         # run tests, then compare benches vs 'preopt' baseline
#   ./run_all.sh preopt postopt # run tests, compare vs 'preopt', save new 'postopt' baseline (if script supports)
#
# Notes:
# - Honors existing repo scripts if present (run_benchmark.sh / set_benchmark.sh).
# - By default uses batching with ANTISEQ_CHUNK_SIZE=1024 (override via env).
# - By default this runs 1M-read benches (ANTISEQ_LARGE=1). To disable, set ANTISEQ_LARGE=0.

BASELINE=${1:-}
NEW_BASELINE=${2:-}

export ANTISEQ_CHUNK_SIZE=${ANTISEQ_CHUNK_SIZE:-1024}
export ANTISEQ_LARGE=${ANTISEQ_LARGE:-1}
# You can also tune input prefetching:
# export ANTISEQ_PREFETCH=${ANTISEQ_PREFETCH:-256}

printf "\n==> Running correctness tests (cargo test)\n\n"
cargo test

printf "\n==> Running Criterion benches\n\n"
if [[ -n "${BASELINE}" ]]; then
  if [[ -x "./run_benchmark.sh" ]]; then
    if [[ -n "${NEW_BASELINE}" ]]; then
      ./run_benchmark.sh "${BASELINE}" "${NEW_BASELINE}"
    else
      ./run_benchmark.sh "${BASELINE}"
    fi
  else
    if [[ -n "${NEW_BASELINE}" ]]; then
      cargo bench --bench antisequence_benches -- --baseline "${BASELINE}" --save-baseline "${NEW_BASELINE}"
    else
      cargo bench --bench antisequence_benches -- --baseline "${BASELINE}"
    fi
  fi
else
  if [[ -x "./run_benchmark.sh" ]]; then
    ./run_benchmark.sh
  else
    cargo bench --bench antisequence_benches
  fi
fi
