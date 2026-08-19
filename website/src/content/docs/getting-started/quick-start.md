---
title: Quick start
description: Validate a geometry and process a paired FASTQ dataset.
---

This example extracts the standard 10x Chromium v2 barcode and UMI layout.

## 1. Write the geometry

Create `10x-v2.geom`:

```text
bc = b[16]
umi = u[10]
bio = r:

1{<bc><umi>}
2{<bio>}
-> 1{<bc><umi>} 2{<bio>}
```

The interval prefix identifies its role: `b` is a barcode, `u` a UMI, and `r`
a biological read. `[16]` and `[10]` are fixed lengths; `:` consumes the
remaining sequence.

## 2. Validate and inspect

```console
seqproc validate 10x-v2.geom
seqproc explain 10x-v2.geom
```

`validate` exits nonzero if parsing, compilation, or semantic validation fails.
`explain` prints both normalized EFGDL and the compiled representation, which
is useful when reviewing a protocol configuration.

## 3. Process paired reads

```console
seqproc run \
  --geom 10x-v2.geom \
  --file1 reads_R1.fastq.gz \
  --file2 reads_R2.fastq.gz \
  --out1 processed_R1.fastq.gz \
  --out2 processed_R2.fastq.gz \
  --threads 8
```

The `.gz` suffix selects gzip output. Primary output paths are not stdout
defaults: if an output is omitted, that stream is discarded. Always name every
output you intend to retain.

## 4. Add an auditable summary

```console
seqproc run \
  --geom 10x-v2.geom \
  --file1 reads_R1.fastq.gz --file2 reads_R2.fastq.gz \
  --out1 processed_R1.fastq.gz --out2 processed_R2.fastq.gz \
  --threads 8 \
  --summary run-summary.json \
  --statistics-level basic
```

`basic` records input, accepted, rejected, and provenance totals with light
instrumentation. Omit `--statistics-level` to request the detailed summary,
which also records read-length and match-stage distributions.

## 5. Check the result

At minimum, confirm that:

- `seqproc` exited with status 0;
- paired output files contain the same number of records;
- the summary's accepted and rejected totals agree with expectations;
- the geometry and exact whitelist/map files are archived with the run.

For more complex examples, see [protocol recipes](../../guides/protocol-recipes/)
and the exact configurations in the
[paper analysis repository](https://github.com/COMBINE-lab/seqproc-paper-analysis).
