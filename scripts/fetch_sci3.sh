#!/usr/bin/env bash
set -euo pipefail

# Fetch paired-end FASTQs for a sci-RNA-seq3 run from ENA and create a small subset.
# Defaults:
#   accession = SRR7827206 (used in scg-lib-structs doc as example sci3 dataset)
#   N = 10000 reads per mate
# Usage:
#   ./scripts/fetch_sci3.sh [accession] [N]
#
# Output files:
#   seqproc/data/sci3/<ACC>_1.fastq.gz
#   seqproc/data/sci3/<ACC>_2.fastq.gz
#   seqproc/data/sci3/<ACC>_1_<N>k.fastq.gz
#   seqproc/data/sci3/<ACC>_2_<N>k.fastq.gz
#
# To use with the on-disk Criterion bench, export:
#   export SCI3_R1=seqproc/data/sci3/<ACC>_1_<N>k.fastq.gz
#   export SCI3_R2=seqproc/data/sci3/<ACC>_2_<N>k.fastq.gz

ACC="${1:-SRR7827206}"
N_READS="${2:-10000}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
OUT_DIR="$ROOT_DIR/data/sci3"
mkdir -p "$OUT_DIR"

INFO_TSV=$(mktemp)
trap 'rm -f "$INFO_TSV"' EXIT

echo "[fetch_sci3] Querying ENA for $ACC ..."
curl -s "https://www.ebi.ac.uk/ena/portal/api/filereport?accession=${ACC}&result=read_run&fields=fastq_ftp,fastq_md5&format=tsv&download=false" \
  -o "$INFO_TSV"

FTP_FIELD=$(tail -n +2 "$INFO_TSV" | awk -F"\t" '{print $3}' | tr -d '\r')
if [[ -z "${FTP_FIELD}" ]]; then
  echo "[fetch_sci3] ERROR: Could not resolve fastq_ftp from ENA for $ACC" >&2
  exit 1
fi

# ENA may separate entries with ';' or ','
IFS=' ' read -r -a FILES <<< "$(echo "$FTP_FIELD" | tr ';,' '  ')"
if [[ ${#FILES[@]} -lt 2 ]]; then
  echo "[fetch_sci3] ERROR: Expected paired-end FASTQs for $ACC, got: ${#FILES[@]}" >&2
  exit 1
fi

R1_URL="https://${FILES[0]}"
R2_URL="https://${FILES[1]}"
R1_GZ="$OUT_DIR/${ACC}_1.fastq.gz"
R2_GZ="$OUT_DIR/${ACC}_2.fastq.gz"

if [[ ! -f "$R1_GZ" ]]; then
  echo "[fetch_sci3] Downloading R1 -> $R1_GZ"
  curl -L --fail --progress-bar "$R1_URL" -o "$R1_GZ"
else
  echo "[fetch_sci3] R1 already exists: $R1_GZ (skipping download)"
fi
if [[ ! -f "$R2_GZ" ]]; then
  echo "[fetch_sci3] Downloading R2 -> $R2_GZ"
  curl -L --fail --progress-bar "$R2_URL" -o "$R2_GZ"
else
  echo "[fetch_sci3] R2 already exists: $R2_GZ (skipping download)"
fi

# Choose a decompressor available on macOS/Linux
DECOMP="gunzip -c"
if command -v gzcat >/dev/null 2>&1; then
  DECOMP="gzcat"
elif command -v zcat >/dev/null 2>&1; then
  DECOMP="zcat"
fi

LINES=$((4 * N_READS))
R1_OUT_SMALL="$OUT_DIR/${ACC}_1_${N_READS}k.fastq.gz"
R2_OUT_SMALL="$OUT_DIR/${ACC}_2_${N_READS}k.fastq.gz"

if [[ ! -f "$R1_OUT_SMALL" ]]; then
  echo "[fetch_sci3] Creating subset R1 (${N_READS} reads) -> $R1_OUT_SMALL"
  set +o pipefail
  eval "$DECOMP \"$R1_GZ\"" | head -n "$LINES" | gzip -c > "$R1_OUT_SMALL" || true
  set -o pipefail
else
  echo "[fetch_sci3] Subset R1 already exists: $R1_OUT_SMALL (skipping)"
fi
if [[ ! -f "$R2_OUT_SMALL" ]]; then
  echo "[fetch_sci3] Creating subset R2 (${N_READS} reads) -> $R2_OUT_SMALL"
  set +o pipefail
  eval "$DECOMP \"$R2_GZ\"" | head -n "$LINES" | gzip -c > "$R2_OUT_SMALL" || true
  set -o pipefail
else
  echo "[fetch_sci3] Subset R2 already exists: $R2_OUT_SMALL (skipping)"
fi

if [[ "$N_READS" == "10000" ]]; then
  R1_ALIAS="$OUT_DIR/${ACC}_1_10k.fastq.gz"
  R2_ALIAS="$OUT_DIR/${ACC}_2_10k.fastq.gz"
  if [[ ! -e "$R1_ALIAS" ]]; then
    (cd "$OUT_DIR" && ln -s "$(basename "$R1_OUT_SMALL")" "$(basename "$R1_ALIAS")") || true
  fi
  if [[ ! -e "$R2_ALIAS" ]]; then
    (cd "$OUT_DIR" && ln -s "$(basename "$R2_OUT_SMALL")" "$(basename "$R2_ALIAS")") || true
  fi
fi

cat <<EOS

[fetch_sci3] Done.
Use the following to point the on-disk Criterion benchmark at the subset:
  export SCI3_R1="$R1_OUT_SMALL"
  export SCI3_R2="$R2_OUT_SMALL"
Then run:
  cargo bench --bench antisequence_benches -- sci3_disk
EOS
