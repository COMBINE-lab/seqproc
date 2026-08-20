---
title: Performance
description: Select execution modes and benchmark seqproc without hidden work.
---

Start with the default execution path and change one dimension at a time.

```console
seqproc run --geom protocol.geom --file1 reads.fastq.gz \
  --out1 /dev/null --threads 8
```

`/dev/null` isolates parsing and geometry execution from output writing. It is
appropriate for compute-only comparisons, but it does not replace an
end-to-end test that emits the real FASTQ product.

## Thread count

`--threads N` controls transform workers and accepts `1` for single-threaded
geometry execution. Optional gzip decompression and single-stream compression
can create separately reported workers; do not count them as though they were
included inside `--threads`.

Measure the thread counts that matter for real deployments. Scaling is
geometry-dependent: fixed slicing, anchor search, whitelist lookup, and gzip
can have different bottlenecks.

## Ordered versus unordered output

Unordered output avoids reassembly and is the throughput-oriented default.
`--preserve-order` assigns batch sequence numbers and restores input order with
a bounded buffer. It no longer forces transformations to execute serially, but
it may add synchronization and buffering overhead.

Use ordered output when downstream consumers require it, and benchmark the
same semantic mode across revisions.

## Staged execution

Ordered output enables the bounded reader → worker → writer pipeline
automatically. `--staged-pipeline` opts unordered runs into the same structure.
It can help costly graphs by separating stages, but cheap geometries may be
faster on the normal worker path.

`--batch-size`, `--queue-capacity`, and `--max-in-flight-batches` expose tuning
controls and memory bounds. Retain defaults unless a representative benchmark
shows a stable improvement.

For a safe terminal sequence of projections followed by FASTQ output, a
one-worker staged pipeline writes the projected sequence, quality, and header
directly into recycled output buffers. This avoids materializing intermediate
records and preserves byte-identical FASTQ output. Multiworker runs currently
retain the materializing path because measurements did not establish a robust
gain. `--no-direct-output-rendering` disables the optimization for validation
or A/B measurement; it is not recommended for normal use.

The normal whole-graph worker remains the one-thread default: it is faster than
the staged pipeline on very cheap graphs even after direct rendering. Direct
rendering therefore does not silently change backend selection.

## Statistics

No `--summary` means statistics collection is off. For manuscript-quality
performance measurements:

1. use statistics-disabled runs for headline timing;
2. collect a separate basic or detailed report for interpretation;
3. run a paired on/off experiment to quantify instrumentation overhead.

## Reproducible measurement checklist

Record at least:

- seqproc commit, build profile, Cargo lockfile, Rust version, and CPU target;
- geometry and auxiliary-file checksums;
- FASTQ and reference checksums;
- transform, decompression, and compression worker counts;
- ordered/unordered, summary level, batch settings, and allocator;
- wall time, CPU time, peak RSS, output count, and output checksum;
- warm/cold cache state, filesystem, CPU affinity, and competing load.

Randomize tool order and report replicate-level values rather than only a
minimum or mean.
