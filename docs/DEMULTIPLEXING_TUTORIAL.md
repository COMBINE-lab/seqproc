# Demultiplexing with seqproc

A beginner-friendly guide to demultiplexing sequencing reads using seqproc.

## Table of Contents

1. [What is Demultiplexing?](#what-is-demultiplexing)
2. [Prerequisites](#prerequisites)
3. [Quick Start](#quick-start)
4. [Step-by-Step Guide](#step-by-step-guide)
5. [Understanding the Output](#understanding-the-output)
6. [Advanced Options](#advanced-options)
7. [Troubleshooting](#troubleshooting)

---

## What is Demultiplexing?

**Demultiplexing** (or "demuxing") is the process of separating pooled sequencing reads into sample-specific files based on barcode sequences. When multiple samples are sequenced together in a single run, each sample is tagged with a unique barcode. Demultiplexing identifies these barcodes and routes reads to the correct sample.

```
Pooled FASTQ ─────────────────────────────────────────────────────────────────►
  │
  ├── Barcode AACGTGAT ──► sample_A_R1.fastq, sample_A_R2.fastq
  ├── Barcode TGGTGGTA ──► sample_B_R1.fastq, sample_B_R2.fastq
  ├── Barcode CGCTGATC ──► sample_C_R1.fastq, sample_C_R2.fastq
  └── Unknown barcode  ──► unassigned_R1.fastq, unassigned_R2.fastq
```

---

## Prerequisites

### 1. Install seqproc

```bash
# Clone the repository
git clone https://github.com/COMBINE-lab/seqproc.git
cd seqproc

# Build the release binary
cargo build --release

# The binary will be at: target/release/seqproc
```

### 2. Prepare Your Files

You need three things:
1. **Input FASTQ files** (R1 and R2)
2. **A geometry file** (`.geom`) describing the read structure
3. **A sample map file** (TSV) mapping barcodes to sample names

---

## Quick Start

If you just want to get started quickly:

```bash
seqproc \
  --geom your_protocol.geom \
  --file1 reads_R1.fastq.gz \
  --file2 reads_R2.fastq.gz \
  --demux-map barcode_to_sample.tsv \
  --demux-label seq2.bc1 \
  --demux-out-dir ./demux_output \
  --threads 4
```

This will create per-sample FASTQ files in `./demux_output/`.

---

## Step-by-Step Guide

### Step 1: Create Your Sample Map File

The sample map is a tab-separated file (TSV) that maps barcode sequences to sample names:

```
# barcode_to_sample.tsv
# Lines starting with # are comments
AACGTGAT	sample_A
TGGTGGTA	sample_B
CGCTGATC	sample_C
AAACATCG	sample_D
```

**Format:**
- Column 1: Barcode sequence (must match exactly what appears in reads)
- Column 2: Sample name (used for output file naming)
- Lines starting with `#` are ignored (comments)
- Empty lines are ignored

### Step 2: Understand Your Geometry File

A geometry file tells seqproc how to parse your reads. Here's an example for SPLiT-seq:

```
# splitseq.geom
read1 = r:
linker1 = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]
linker2 = f[ATCCACGTGCTTGAGAGGCCAGAGCATTCG]

bc3 = b[8]
bc2 = b[8]
bc1 = b[8]
umi = u[10]

1{<read1>}
2{
    r:
    <umi>
    <bc3>
    anchor_relative(hamming(<linker1>, 3))
    <bc2>
    anchor_relative(hamming(<linker2>, 3))
    <bc1>
}

-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
```

**Key components:**
- `b[8]` - Barcode of 8 base pairs
- `u[10]` - UMI of 10 base pairs
- `r:` - Read sequence (variable length)
- `f[...]` - Fixed sequence (linker)
- `hamming(<linker>, 3)` - Allow up to 3 mismatches

### Step 3: Choose Your Demux Label

The `--demux-label` tells seqproc which barcode to use for demultiplexing:

| Label | Meaning |
|-------|---------|
| `seq2.bc1` | Barcode `bc1` from read 2 |
| `seq2.bc2` | Barcode `bc2` from read 2 |
| `seq1.cb` | Cell barcode from read 1 |

The format is `seq{read_number}.{barcode_label}`.

### Step 4: Run Demultiplexing

```bash
seqproc \
  --geom splitseq.geom \
  --file1 SRR_1.fastq.gz \
  --file2 SRR_2.fastq.gz \
  --demux-map barcode_to_sample.tsv \
  --demux-label seq2.bc1 \
  --demux-out-dir ./my_demux_output \
  --threads 4
```

### Step 5: Check Your Output

After running, you'll see output files like:

```
my_demux_output/
├── sample_A_R1.fastq
├── sample_A_R2.fastq
├── sample_B_R1.fastq
├── sample_B_R2.fastq
├── sample_C_R1.fastq
├── sample_C_R2.fastq
├── sample_D_R1.fastq
├── sample_D_R2.fastq
├── unassigned_R1.fastq   # Reads with unknown barcodes
└── unassigned_R2.fastq
```

---

## Understanding the Output

### Output File Naming

Output files follow the pattern: `{sample_name}_R{1|2}.fastq`

- **sample_name**: From your sample map file
- **R1/R2**: Read 1 or Read 2
- **unassigned**: Reads where the barcode wasn't found in your map

### Counting Reads Per Sample

```bash
# Count reads per sample
for f in my_demux_output/*_R1.fastq; do
  sample=$(basename "$f" _R1.fastq)
  count=$(grep -c "^@" "$f")
  echo "$sample: $count reads"
done
```

---

## Advanced Options

### Custom Output Directory

```bash
--demux-out-dir /path/to/output
```

### Custom Unassigned Name

By default, reads without matching barcodes go to "unassigned". This is configured in the code but uses `unassigned` as the default.

### Multi-threading

Use `--threads` to speed up processing:

```bash
--threads 8  # Use 8 CPU threads
```

### Combining with Summary Statistics

```bash
seqproc \
  --geom your.geom \
  --file1 R1.fastq.gz \
  --file2 R2.fastq.gz \
  --demux-map samples.tsv \
  --demux-label seq2.bc1 \
  --demux-out-dir output \
  --summary stats.json \
  --threads 4
```

---

## Troubleshooting

### "Failed to load sample map"

- Check that your TSV file path is correct
- Ensure the file is readable
- Verify it's tab-separated (not spaces)

### All Reads Go to "unassigned"

- Verify your barcode sequences match exactly (case-sensitive)
- Check you're using the correct `--demux-label`
- Ensure your geometry file correctly extracts the barcode

### No Output Files Created

- Check that the output directory is writable
- Look for error messages in stderr
- Verify your geometry file parses correctly

### Wrong Number of Reads

- Ensure your geometry file matches your sequencing protocol
- Check for linker/anchor mismatches (try increasing Hamming distance)

---

## Example: Complete SPLiT-seq Demux Workflow

```bash
#!/bin/bash

# 1. Set up paths
SEQPROC=/path/to/seqproc/target/release/seqproc
GEOM=splitseq.geom
R1=SRR6750041_1.fastq.gz
R2=SRR6750041_2.fastq.gz
SAMPLE_MAP=bc1_samples.tsv
OUTPUT_DIR=demux_results

# 2. Create sample map (example with 96-well plate)
cat > $SAMPLE_MAP << 'EOF'
AACGTGAT	well_A01
AAACATCG	well_A02
ATGCCTAA	well_A03
# ... add more barcodes
EOF

# 3. Run demultiplexing
$SEQPROC \
  --geom $GEOM \
  --file1 $R1 \
  --file2 $R2 \
  --demux-map $SAMPLE_MAP \
  --demux-label seq2.bc1 \
  --demux-out-dir $OUTPUT_DIR \
  --threads 4

# 4. Summarize results
echo "=== Demux Summary ==="
for f in $OUTPUT_DIR/*_R1.fastq; do
  name=$(basename "$f" _R1.fastq)
  count=$(grep -c "^@" "$f" 2>/dev/null || echo 0)
  printf "%-20s %d reads\n" "$name" "$count"
done | sort -t$'\t' -k2 -nr
```

---

## Need Help?

- **Documentation**: Check the seqproc repository README
- **Issues**: Report bugs on GitHub
- **Geometry Files**: See `matchbox_2018_eval/` for example geometries

---

*This tutorial covers seqproc's demultiplexing feature. For general seqproc usage, see the main documentation.*
