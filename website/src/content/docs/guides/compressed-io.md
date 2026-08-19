---
title: Compressed I/O
description: Choose gzip input and output backends with explicit thread accounting.
---

`seqproc` recognizes gzip output from a `.gz` output suffix. Compression level
3 is the default speed/size tradeoff; levels 0 through 9 are accepted.

```console
seqproc run ... --out1 processed.fastq.gz --gzip-level 3
```

## Input backends

The default FASTQ parser uses `needletail`, including its transparent gzip
decompression. This is the conservative default.

`--accelerated-gzip-input` instead feeds regular `.gz` inputs through
`rapidgzip-core`'s speculative decoder while retaining the same transformation
graph:

```console
seqproc run ... --accelerated-gzip-input \
  --gzip-input-threads 1 \
  --gzip-input-chunk-size 262144
```

The worker count is a ceiling per gzip input and the decoder creates workers
adaptively. More decoder threads are not automatically faster when they share a
fixed CPU allocation with transform workers. Benchmark the full workload and
report decoder and transform counts separately.

## Serial gzip output

Without a parallel-output flag, each `.gz` output is a conventional serial gzip
stream. This maximizes interoperability and has the simplest thread accounting.

## Parallel multi-member output

```console
seqproc run ... --parallel-gzip
```

Transform workers compress batches concurrently and emit concatenated gzip
members. This is often the highest-throughput output path. The resulting file
is valid multi-member gzip, but downstream consumers must actually read all
members; test this before adopting it in a pipeline.

## Parallel single-stream output

```console
seqproc run ... --parallel-gzip-stream \
  --gzip-threads 4 \
  --gzip-block-size 131072
```

This backend emits one logical gzip member with dictionary continuity across
blocks. Its compression pool defaults to `min(--threads, 4)` and is additional
to transform workers, so record it as a separate resource. The block size must
be at least 32 KiB.

Parallel single-stream output currently supports fixed primary outputs only;
it is rejected when demultiplexed or unassigned outputs are requested.

## Benchmarking compressed runs

For defensible comparisons, state:

- whether input and output were compressed;
- compression level and backend;
- transform, decompression, and compression worker counts;
- filesystem and cache state;
- whether time includes parsing, compression, and output writing.

Use uncompressed or `/dev/null` output to isolate transformation cost, then
report a separate end-to-end result for the actual data product.
