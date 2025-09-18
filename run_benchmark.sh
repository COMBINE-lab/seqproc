#!/usr/bin/env bash
set -euo pipefail

# run_benchmark.sh
# Run ALL seqproc benches and compare against a saved Criterion baseline.
# Usage:
#   ./run_benchmark.sh [BASELINE_NAME] [NEW_BASELINE_NAME]
# Examples:
#   ./run_benchmark.sh preopt
#   ./run_benchmark.sh preopt postopt
# If NEW_BASELINE_NAME is provided, we will also save a new baseline after running.

BASELINE="${1:-preopt}"
SAVE_BASELINE="${2:-}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${SCRIPT_DIR}"

# Ensure the sci3 on-disk 10k defaults exist so the disk benches run.
R1_DEFAULT="${SCRIPT_DIR}/data/sci3/SRR7827206_1_10k.fastq.gz"
R2_DEFAULT="${SCRIPT_DIR}/data/sci3/SRR7827206_2_10k.fastq.gz"
if [[ ! -f "${R1_DEFAULT}" || ! -f "${R2_DEFAULT}" ]]; then
  echo "[run_benchmark] sci3 10k defaults not found; attempting to fetch subset via streaming..."
  STREAM_ONLY=1 ./scripts/fetch_sci3.sh SRR7827206 10000 || {
    echo "[run_benchmark] WARNING: Unable to fetch sci3 10k subset automatically. Continuing; on-disk benches may be skipped." >&2
  }
fi

# Run and compare against a baseline for ALL benches in benches/antisequence_benches.rs
if [[ -n "${SAVE_BASELINE}" ]]; then
  echo "[run_benchmark] Comparing against baseline '${BASELINE}' and saving new baseline '${SAVE_BASELINE}'"
  cargo bench --bench antisequence_benches -- --baseline "${BASELINE}" --save-baseline "${SAVE_BASELINE}"
else
  echo "[run_benchmark] Comparing against baseline '${BASELINE}'"
  cargo bench --bench antisequence_benches -- --baseline "${BASELINE}"
fi

cat <<EOF

[run_benchmark] Complete.
- Open the comparison report:
    open target/criterion/report/index.html
- Disk group report:
    open target/criterion/antisequence_sci_rna_seq3_disk/report/index.html
- Raw JSON estimates live under:
    target/criterion/<group>/<function>/new/estimates.json
    target/criterion/<group>/<function>/base/estimates.json
EOF
