#!/usr/bin/env bash
set -euo pipefail

# Simple wrapper to profile benches with `samply`.
#
# Requirements:
#   - Install samply: https://github.com/mstange/samply
#       macOS:   brew install samply       # or cargo install samply
#       Linux:   cargo install samply
#
# Usage examples:
#   ./profile_bench.sh 10x_                       # profile 10x synthetic benches
#   ./profile_bench.sh sci3_                      # profile sci3 benches (all)
#   ./profile_bench.sh 10x_trivial_1M             # profile 1M-read 10x bench
#
# Notes:
#   - You can open the captured profile in Firefox Profiler automatically if you have Firefox.
#   - By default this runs the full Criterion benchmark binary filtered by your arg.
#
#   The output will print a link to open in the Firefox profiler UI.

FILTER=${1:-}
if ! command -v samply >/dev/null 2>&1; then
  echo "samply not found. Please install it (e.g., brew install samply or cargo install samply)." >&2
  exit 1
fi

# Default to batching and 1M benches unless overridden
export ANTISEQ_BATCH_SIZE=${ANTISEQ_BATCH_SIZE:-1024}
export ANTISEQ_LARGE=${ANTISEQ_LARGE:-1}

if [[ -n "${FILTER}" ]]; then
  exec samply record -- cargo bench --bench antisequence_benches -- "${FILTER}"
else
  exec samply record -- cargo bench --bench antisequence_benches
fi
