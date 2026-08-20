---
title: Command line
description: The seqproc subcommands and important run options.
---

## Commands

```console
seqproc validate <geometry>
seqproc explain <geometry>
seqproc run [OPTIONS]
```

| Command | Purpose |
| --- | --- |
| `validate` | Parse, compile, and semantically validate a geometry without reading FASTQ data. |
| `explain` | Print normalized EFGDL and the compiled geometry representation. |
| `run` | Process one single-end or paired-end FASTQ input. |

The old flag-only form is accepted for one compatibility cycle, but new
workflows should use `seqproc run`.

## Required run inputs

`--geom` and `--file1` are required. Supply `--file2` for paired-end input.
Output arguments are optional syntactically, but an omitted primary output is
discarded. Use `--out1` and, for two-output geometries, `--out2` explicitly.

```console
seqproc run --geom protocol.geom --file1 R1.fastq --out1 clean_R1.fastq
```

## Parallel execution and ordering

`--threads N` selects the transform worker count. Unordered execution is the
default. `--preserve-order` keeps records in input order with a bounded reorder
buffer while transformations remain parallel.

The staged pipeline is enabled automatically for ordered output. It can be
requested for unordered processing with `--staged-pipeline`; this can help
expensive geometries, while the normal worker path is often better for cheap
ones. Advanced controls are:

- `--batch-size N`
- `--queue-capacity N`
- `--max-in-flight-batches N`

When the staged planner proves that a terminal projection is safe, it can
render FASTQ directly without materializing the projected records. This is
enabled by default for the measured one-worker case. Use
`--no-direct-output-rendering` only to validate or benchmark the fallback.

Treat these as workload-specific tuning controls and benchmark before changing
their defaults.

## Additional geometry arguments

One or more values supplied with `--additional` fill positional placeholders
such as `$0` in a geometry. This lets a shared geometry select a whitelist or
mapping file at invocation time.

```text
bc = filter_within_dist(b[8], $0, 1)
```

```console
seqproc run --geom protocol.geom --additional barcodes.txt \
  --file1 reads.fastq.gz --out1 clean.fastq.gz
```

## Output and reporting options

- `--unassigned1` and `--unassigned2` retain records rejected by the main graph.
- `--demux-map`, `--demux-label`, and `--demux-out-dir` route accepted reads by
  sample barcode.
- `--summary FILE` writes a versioned JSON run report.
- `--statistics-level basic|detailed` controls summary detail.

## Compression options

- `--gzip-level 0..9` controls gzip compression for `.gz` outputs (default 3).
- `--parallel-gzip` emits concatenated gzip members compressed by transform
  workers.
- `--parallel-gzip-stream` emits one logical gzip member using a bounded
  compression pool.
- `--accelerated-gzip-input` selects the speculative gzip decoder.

See [compressed I/O](../../guides/compressed-io/) before enabling the parallel
backends, because their thread accounting and interoperability differ.

The executable is the authoritative reference for the installed revision:

```console
seqproc run --help
```
