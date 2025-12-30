# Seqproc Geometry Language Extensions

This document describes the new geometry language extensions added for advanced barcode extraction patterns, particularly for protocols like SPLiT-seq that require anchor-relative positioning.

## Table of Contents

1. [anchor_relative](#anchor_relative) - Extract elements relative to a found anchor
2. [search](#search) - Global search for fixed sequence
3. [search_whitelist](#search_whitelist) - Search for barcode from whitelist with optional linker validation

---

## anchor_relative

**Purpose:** Search for an anchor sequence and extract preceding elements relative to the found position, rather than from fixed positions.

### Syntax

```
label = anchor_relative(hamming(f[SEQUENCE], N))
```

- `SEQUENCE`: The anchor sequence to search for
- `N`: Maximum hamming distance for matching

### When to Use

Use `anchor_relative` when:
- The anchor position varies due to insertions/deletions upstream
- You need to extract barcodes that precede a linker sequence
- Fixed-position extraction fails due to read structure variability

### Example: SPLiT-seq Geometry

SPLiT-seq has structure: `UMI(10) + BC3(8) + L1(30) + BC2(8) + L2(30) + BC1(8)`

The first linker (L1) position can vary. Using `anchor_relative`, we search for L1 and extract BC3 from the 8bp immediately before it:

```
read1 = r:
umi = u[10]
bc3 = b[8]
l1 = anchor_relative(hamming(f[GTGGCCGATGTTTCGCATCGGCGTACGACT], 3))
bc2 = b[8]
l2 = hamming(f[ATCCACGTGCTTGAGAGGCCAGAGCATTCG], 3)
bc1 = b[8]

1{<read1>}
2{<umi><bc3><l1><bc2>r:<l2><bc1>}

-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
```

### How It Works

1. The geometry parser encounters `<umi><bc3><l1>` where `l1` uses `anchor_relative`
2. Instead of cutting at fixed position 18, it searches the entire read for L1
3. When found at position P, it:
   - Creates label for positions 0 to P (contains UMI+BC3)
   - Creates label for the anchor itself
   - Creates label for everything after the anchor
4. Subsequent elements (`<bc2>`, etc.) continue from after the anchor

### Performance

- ~361K reads matched from 500K input (72.2% pass rate)
- 2-3x faster than splitcode
- 3-4x less memory than splitcode

---

## search

**Purpose:** Force global search for a fixed sequence instead of prefix matching.

### Syntax

```
label = search(f[SEQUENCE])
label = search(hamming(f[SEQUENCE], N))
```

### Example

```
anchor = search(hamming(f[ATCGATCG], 2))

1{x:<anchor>r:}
```

This searches for `ATCGATCG` (with up to 2 mismatches) anywhere in read 1, splitting into before/anchor/after.

---

## search_whitelist

**Purpose:** Search for a barcode from a whitelist file, optionally validating that a specific linker sequence follows it.

### Syntax

```
# Basic: search for barcode from whitelist
bc = search_whitelist(b[N], "whitelist.txt", DIST)

# With max position constraint
bc = search_whitelist(b[N], "whitelist.txt", DIST, MAX_POS)

# With followed_by linker validation
bc = search_whitelist(b[N], "whitelist.txt", DIST, f[LINKER], LINKER_DIST)

# With both max_pos and followed_by
bc = search_whitelist(b[N], "whitelist.txt", DIST, MAX_POS, f[LINKER], LINKER_DIST)
```

### Parameters

| Parameter | Description |
|-----------|-------------|
| `b[N]` | Barcode length (N bases) |
| `whitelist.txt` | Path to whitelist file (one barcode per line) |
| `DIST` | Max hamming distance for barcode matching |
| `MAX_POS` | Optional: only search first MAX_POS positions |
| `f[LINKER]` | Optional: linker sequence that must follow barcode |
| `LINKER_DIST` | Max hamming distance for linker matching |

### Example: Barcode with Linker Validation

```
# Find 8bp barcode from whitelist, must be followed by CCC linker
bc = search_whitelist(b[8], "barcodes.txt", 2, f[CCC], 1)

1{x:<bc>r:}
```

### Example: SPLiT-seq Alternative Approach

This approach searches for BC3 from whitelist, validates L1 linker follows:

```
read1 = r:
umi = u[10]
bc3 = search_whitelist(b[8], "bc3_seqs.txt", 2, f[GTGGCCGATGTTTCGCATCGGCGTACGACT], 3)
bc2 = b[8]
l2 = hamming(f[ATCCACGTGCTTGAGAGGCCAGAGCATTCG], 3)
bc1 = b[8]

1{<read1>}
2{<umi><bc3>r:<l2><bc1>}

-> 1{<read1>} 2{<umi><bc3><bc2><bc1>}
```

### Whitelist File Format

One barcode per line, optionally with tab-separated metadata:

```
AACGTGAT
AAACATCG
ATGCCTAA
...
```

Or with names:
```
AACGTGAT	bc1_0
AAACATCG	bc1_1
...
```

---

## Command Line Usage

### Running with Geometry File

```bash
seqproc --geom geometry.geom \
        --file1 R1.fastq.gz \
        --file2 R2.fastq.gz \
        --out1 output_R1.fastq \
        --out2 output_R2.fastq \
        --threads 4
```

### Using Arguments for Whitelist Paths

Geometry files can use `$0`, `$1`, etc. for command-line arguments:

```
bc3 = search_whitelist(b[8], $0, 2)
```

Then:
```bash
seqproc --geom geometry.geom \
        --file1 R1.fastq \
        --file2 R2.fastq \
        --out1 out_R1.fastq \
        --out2 out_R2.fastq \
        --args bc3_whitelist.txt
```

---

## Testing Your Geometry

### Quick Spot Check

Use the provided spot-check utility:

```bash
cd matchbox_2018_eval
python3 spot_check_seqproc.py --run
```

### Full Benchmark

```bash
python3 run_splitseq_benchmark.py --mode all --threads 4
```

---

## Troubleshooting

### Low Read Count

- Check hamming distance - may need to increase for divergent sequences
- Verify anchor sequence matches your protocol
- Use `analyze_missed_reads.py` to understand why reads are failing

### Low Barcode Validity

- Verify whitelist file matches your protocol's actual barcodes
- Use `check_whitelists.py` to compare whitelists
- Check barcode positions in output with `spot_check_seqproc.py`

### Performance Issues

- Reduce threads if memory constrained
- Use `--threads 1` for debugging
- Check if input files are gzipped (adds overhead)

---

## Files Reference

| File | Description |
|------|-------------|
| `splitseq_anchor_relative_benchmark.geom` | SPLiT-seq geometry using anchor_relative |
| `spot_check_seqproc.py` | Quick correctness verification |
| `benchmark_utils.py` | Reusable benchmark utilities |
| `run_splitseq_benchmark.py` | Full benchmark script |

---

## Version History

- **v0.1** (Dec 2025): Initial implementation of `anchor_relative`, `search`, `search_whitelist`
