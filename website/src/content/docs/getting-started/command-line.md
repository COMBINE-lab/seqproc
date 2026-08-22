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
| `run` | Process one, two, or three synchronized FASTQ segments. |

The old flag-only form is accepted for one compatibility cycle, but new
workflows should use `seqproc run`.

## Required run inputs

`--geom` and `--read1` are required. Supply `--read2` and optionally `--read3`
for synchronized multi-segment input; lane indices must be contiguous.
Each read lane accepts repeated options and comma-separated ordered shards.
Corresponding shards are processed together without temporary concatenation:

```console
seqproc run --geom protocol.geom \
  --read1 lane1_R1.fastq.gz,lane2_R1.fastq.gz \
  --read2 lane1_R2.fastq.gz,lane2_R2.fastq.gz \
  --out1 clean_R1.fastq.gz --out2 clean_R2.fastq.gz
```

All lanes must have the same shard count, and every corresponding shard must
contain the same number of records. Plain and gzip shards may be mixed. Empty
shards are valid when they occur in every lane. `--file1` and `--file2` remain
single-path compatibility aliases.

If complete fragments are stored consecutively in one physical FASTQ stream,
use repeatable/comma-separated `--interleaved-input` instead of `--read1` and
`--read2`. The compiled geometry is the sole authority for the number of
records per fragment:

```console
seqproc run --geom paired.geom \
  --interleaved-input part1.fastq.gz,part2.fastq.gz \
  --out1 clean_R1.fastq.gz --out2 clean_R2.fastq.gz
```

Each shard must end on a complete fragment. Interleaved stdin is supported as
`--interleaved-input -`; output remains one separate target per logical lane.
Ordinary gzip and `--accelerated-gzip-input` work for interleaved file shards.

Three-segment protocols such as scATAC use the bounded third lane directly:

```console
seqproc run --geom scatac.geom \
  --read1 genomic_R1.fastq.gz \
  --read2 cell_barcode.fastq.gz \
  --read3 genomic_R2.fastq.gz \
  --out1 clean_R1.fastq.gz \
  --out2 clean_barcode.fastq.gz \
  --out3 clean_R2.fastq.gz
```

The current public bound is three input and output segments. Geometries with a
larger arity fail during validation rather than after workers start.

Output arguments are optional syntactically, but an omitted primary output is
discarded. Use `--out1` and, for two-output geometries, `--out2` explicitly.

```console
seqproc run --geom protocol.geom --read1 R1.fastq --out1 clean_R1.fastq
```

Use `-` for one stdin source or one stdout target. Gzip input is detected from
its magic bytes; use `--stdout-gzip` to compress stdout because it has no file
suffix. Diagnostics always go to stderr. In a pipeline, `--summary -` also
writes JSON to stderr so the FASTQ stream remains clean:

```console
gzip -cd reads.fastq.gz | seqproc run --geom protocol.geom \
  --read1 - --out1 - --stdout-gzip > clean.fastq.gz
```

At most one source may consume stdin and at most one FASTQ lane may target
stdout. Stdin must be the only shard in its separate or interleaved input
stream. Parallel gzip
output modes are path-only; stdout uses the ordinary bounded writer path.
If a downstream process closes the pipe, seqproc cancels the remaining graph
and exits 0 without a diagnostic, following normal Unix early-consumer
semantics. Only stdout `EPIPE` receives this treatment: ENOSPC, quota
exhaustion, named-pipe/file failures, and every other output error remain
nonzero.

## Parallel execution and ordering

`--threads N` selects the transform worker count. Unordered execution is the
default. `--preserve-order` keeps records in input order with a bounded reorder
buffer while transformations remain parallel.

`--execution-mode auto|whole-graph|pipeline` controls the execution planner.
Automatic mode preserves the measured low-overhead whole-graph path unless
ordered output requires the pipeline. `--staged-pipeline` remains a legacy
alias for requesting pipeline execution in automatic mode. Advanced pipeline
controls are:

- `--batch-size N`
- `--queue-capacity N`
- `--max-in-flight-batches N`
- `--batch-memory-budget-mib N`
- `--pipeline-input-mode worker-local|dedicated-reader`

Pipeline bounds are planned deterministically from the compiled graph, input
and output arity, static geometry lengths, compression choices, and a 256 MiB
memory budget. The planner does not sample or consume reads. Any exact bound
listed above overrides only that dimension. Use
`--no-dynamic-batch-planning` to retain all fixed compatibility defaults.

When the staged planner proves that a terminal projection is safe, it can
render FASTQ directly without materializing the projected records. This is
enabled by default for the measured one-worker case. Use
`--no-direct-output-rendering` only to validate or benchmark the fallback.

Compilation applies only conservative, proof-backed graph rewrites. Use
`--no-graph-optimization` to build a structural-optimization oracle for
byte-equivalence and performance comparisons. It does not disable independent
runtime choices such as direct terminal rendering; disable both when testing
the fully materialized terminal path.

For focused optimizer ablations,
`--no-dead-label-elimination` retains metadata whose production is proven
dead, and `--no-early-filter-placement` preserves the original position of
selective filters. Both passes disable themselves when statistics or tracing
make operation order observable and are reported separately in summary JSON.

Treat these as workload-specific tuning controls and benchmark before changing
their defaults.

## Additional geometry arguments

For EFGDL 2, prefer declared named resources and explicit bindings:

```text
header { efgdl = 2 }
resources { whitelist }
bc = filter_within_dist(b[8], $whitelist, 1)
```

```console
seqproc run --geom protocol.geom --bind whitelist=barcodes.txt \
  --file1 reads.fastq.gz --out1 clean.fastq.gz
```

Named resources are resolved once during graph construction. Missing,
duplicate, unknown, and unreadable bindings fail before FASTQ processing.

### Positional compatibility

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

- `--unassigned1`, `--unassigned2`, and `--unassigned3` retain records rejected
  by the main graph. When used, supply exactly one target per input lane (use
  `/dev/null` for a lane you intentionally discard).
- `--demux-map`, `--demux-label`, and `--demux-out-dir` route accepted reads by
  sample barcode; fixed `--outN` targets cannot be combined with demultiplexing.
- `--summary FILE` writes a versioned JSON run report; `--summary -` writes it
  to stderr.
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
