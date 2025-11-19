#!/usr/bin/env bash
set -euo pipefail

# set_benchmark.sh
# Save a Criterion baseline for ALL seqproc benches.
# Usage:
#   ./set_benchmark.sh [BASELINE_NAME]
# Example:
#   ./set_benchmark.sh preopt

BASELINE="${1:-preopt}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${SCRIPT_DIR}"

# Ensure data exists for on-disk sci3 bench (10k subset).
R1_DEFAULT="${SCRIPT_DIR}/data/sci3/SRR7827206_1_10k.fastq.gz"
R2_DEFAULT="${SCRIPT_DIR}/data/sci3/SRR7827206_2_10k.fastq.gz"

if [[ ! -f "${R1_DEFAULT}" || ! -f "${R2_DEFAULT}" ]]; then
  echo "[set_benchmark] sci3 10k defaults not found; attempting to fetch subset via streaming..."
  # Fetch minimal subset without downloading full FASTQs
  STREAM_ONLY=1 ./scripts/fetch_sci3.sh SRR7827206 10000 || {
    echo "[set_benchmark] WARNING: Unable to fetch sci3 10k subset automatically. Continuing without on-disk bench." >&2
  }
fi

# Save the baseline for ALL benches in benches/antisequence_benches.rs
# This includes: 10x (in-memory), sci3 (in-memory), sci3 tolerant (in-memory),
# sci3 disk (ENA subset), and multi-run disk aggregate (if data present).
# Additionally, enable 1M-read synthetic benches by default via ANTISEQ_LARGE=1.
# You can override by exporting ANTISEQ_LARGE=0 before calling this script.
 echo "[set_benchmark] Saving Criterion baseline (ANTISEQ_CHUNK_SIZE=${ANTISEQ_CHUNK_SIZE:-1024}, ANTISEQ_LARGE=${ANTISEQ_LARGE:-1}): ${BASELINE}"
ANTISEQ_CHUNK_SIZE=${ANTISEQ_CHUNK_SIZE:-1024} ANTISEQ_LARGE=${ANTISEQ_LARGE:-1} cargo bench --bench antisequence_benches -- --save-baseline "${BASELINE}"

cat <<EOF

[set_benchmark] Baseline saved: ${BASELINE}
Open the HTML report:
  open target/criterion/report/index.html
Run comparisons later with:
  ./run_benchmark.sh ${BASELINE}
Optionally save a post-optimization baseline:
  ./run_benchmark.sh ${BASELINE} postopt
Notes:
- To include the on-disk 1M ENA bench, set SCI3_R1_1M and SCI3_R2_1M to gzipped FASTQ paths before running.
- To disable 1M synthetic benches, run: ANTISEQ_LARGE=0 ./set_benchmark.sh ${BASELINE}
- To change batch size, set ANTISEQ_CHUNK_SIZE (default 1024)
EOF
