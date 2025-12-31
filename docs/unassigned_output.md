# Unassigned Reads Output

## Overview

The `--unassigned1` and `--unassigned2` flags allow outputting reads that fail processing (filtering, linker matching, or whitelist validation) to separate files for downstream analysis.

## Usage

```bash
seqproc --geom geometry.geom \
  --file1 input_R1.fastq --file2 input_R2.fastq \
  --out1 assigned_R1.fastq --out2 assigned_R2.fastq \
  --unassigned1 unassigned_R1.fastq --unassigned2 unassigned_R2.fastq \
  --threads 4
```

## Parameters

- `--unassigned1 <FILE>`: Output file for R1 reads that failed processing
- `--unassigned2 <FILE>`: Output file for R2 reads that failed processing

## Behavior

Reads are considered "unassigned" if they fail any of:
- Linker/anchor matching (sequence not found within allowed distance)
- Whitelist filtering (barcode not in whitelist)
- Length requirements (read too short for geometry)

## Current Limitations

The current implementation uses antisequence's `TryOp` which only catches reads that fail due to missing required labels/attributes. Reads filtered by `RetainOp` (e.g., whitelist failures) are dropped rather than routed to unassigned output.

**Workaround:** Use the provided `extract_unassigned.py` script to compute unassigned reads as (input - assigned):

```bash
python extract_unassigned.py \
  input_R1.fastq input_R2.fastq \
  assigned_R2.fastq \
  unassigned_R1.fastq unassigned_R2.fastq \
  count.txt
```

## Use Cases

- **QC analysis**: Understand why reads failed processing
- **Debugging**: Investigate barcode extraction failures
- **Recovery**: Attempt alternative processing on failed reads

## Example Workflow

```bash
# Run seqproc with whitelist filtering
seqproc --geom splitseq_dual_anchor_whitelist.geom \
  --file1 reads_R1.fastq --file2 reads_R2.fastq \
  --out1 assigned_R1.fastq --out2 assigned_R2.fastq

# Extract unassigned reads using workaround script
python extract_unassigned.py \
  reads_R1.fastq reads_R2.fastq \
  assigned_R2.fastq \
  unassigned_R1.fastq unassigned_R2.fastq \
  unassigned_count.txt

# Analyze why reads failed
python analyze_unassigned.py unassigned_R2.fastq
```

## Future Improvements

A native implementation that properly routes filtered reads to unassigned output would require modifications to antisequence's `RetainOp` to support a "reject" output stream.
