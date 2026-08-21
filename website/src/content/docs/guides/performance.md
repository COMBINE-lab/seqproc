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

## Optimization and execution planning

seqproc freezes each operation graph after validation, applies conservative
compile-time rewrites, and then creates a deterministic execution plan.
Summaries report the original and optimized operation counts, pass-level
changes, cost classes, requested mode, selected backend, effective pipeline
bounds, and stable reason codes.

`--execution-mode auto|whole-graph|pipeline` controls backend selection. The
current automatic policy is deliberately conservative: it preserves the
measured low-overhead whole-graph default unless input ordering requires the
bounded pipeline. Forced modes exist for reproducible crossover measurements,
not as a promise that one backend wins for every geometry.
The calibration environment, workloads, and results are archived in the
[ANTISEQUENCE Milestone 2 crossover report](https://github.com/COMBINE-lab/ANTISEQUENCE/blob/dev/docs/benchmarks/milestone-2-execution-crossover-2026-08-20.md).

For a forced or ordered pipeline,
`--pipeline-input-mode worker-local|dedicated-reader` controls whether transform
workers parse their own batches or receive them from a separate reader. The
worker-local mode is the measured default; the dedicated reader is an explicit
workload-specific experiment and adds one background thread.

`--no-graph-optimization` disables structural graph rewrites. Compare its
output byte-for-byte with the default when adding a pass. Direct terminal
rendering is an independent pipeline optimization, so also use
`--no-direct-output-rendering` when the oracle must materialize the terminal
projection.

Use `--no-dead-label-elimination` or `--no-early-filter-placement` to isolate
one proof-backed pass while retaining all other rewrites. The summary's
optimization report records stable pass-level change counts. Neither pass
introduces a per-record feature check: compilation either
rewrites the graph or leaves the runtime node sequence unchanged.

## Staged execution

Ordered output enables the bounded reader → worker → writer pipeline
automatically. `--execution-mode pipeline` opts unordered runs into the same
structure; `--staged-pipeline` is retained as a compatibility alias.
It can help costly graphs by separating stages, but cheap geometries may be
faster on the normal worker path.

`--batch-size`, `--queue-capacity`, and `--max-in-flight-batches` expose exact
tuning controls. When a control is absent, the deterministic batch planner
uses graph cost, static geometry length, lane count, compression, worker count,
and `--batch-memory-budget-mib` (256 MiB by default) to select it. Planning
does not sample the input, so it cannot consume stdin or make runs
data-dependent. Summaries report the selected bounds, estimated live bytes,
and stable reason codes. `--no-dynamic-batch-planning` restores fixed pipeline
defaults for compatibility and A/B measurements.

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

## Long-read branching

Either-orientation matching and conditional retry must preserve the original
record while evaluating another graph path. The ANTISEQUENCE backend uses a
size-dispatched copy-on-write representation for these branch points: long
records initially share immutable FASTQ storage, while short records retain
the faster ordinary-copy path. The first mutation remains branch-local, and
records that never branch continue to use recycled owned buffers.

This is automatic and does not change EFGDL syntax or output. Synthetic
branch-isolation measurements are maintained in the
[ANTISEQUENCE graph API guide](https://github.com/COMBINE-lab/ANTISEQUENCE/blob/dev/docs/graph-api.md#copy-on-write-graph-branches);
protocol throughput should still be measured end to end because matching,
decompression, and output can dominate the saved copy.

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
