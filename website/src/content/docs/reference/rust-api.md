---
title: Rust API
description: Compile a geometry and execute it through RunConfig and RunReport.
---

The CLI is a thin client over the public execution API. The typed entry point
is `RunConfig` plus `run`, which returns a `RunReport` on success.

Internally, `run` finishes constructing the ANTISEQUENCE operation graph,
validates input/transform/output stage order, and freezes it as a structurally
immutable `CompiledGraph` before starting any reader, worker, or writer thread.
The current EFGDL compiler explicitly selects ANTISEQUENCE's compatibility
`MissingInputPolicy::Skip`; moving individual language operations to strict
`Error` or per-read `Reject` behavior will be a versioned semantic change.

```rust
use std::fs;
use std::path::PathBuf;

use antisequence::graph::ExecutionMode;
use seqproc::execute::{compile_geom, run, RunConfig};
use seqproc::io_config::{InputLane, InputSource, OutputTarget};

let source = fs::read_to_string("protocol.geom")?;
let compiled = compile_geom(source).expect("geometry must compile");

let mut config = RunConfig::new("reads_R1.part1.fastq.gz").with_input_lanes([
    InputLane::new(["reads_R1.part1.fastq.gz", "reads_R1.part2.fastq.gz"]),
    InputLane::new(["reads_R2.part1.fastq.gz", "reads_R2.part2.fastq.gz"]),
]);
config.output1 = Some(PathBuf::from("processed_R1.fastq.gz"));
config.output2 = Some(PathBuf::from("processed_R2.fastq.gz"));
config.threads = 8;
config.execution_mode = ExecutionMode::Auto;
config.graph_optimization = true;

let report = run(config, compiled)?;
println!("effective transform threads: {}", report.effective_threads);
# Ok::<(), anyhow::Error>(())
```

For streams, construct typed targets rather than using a sentinel path:

```rust
# use seqproc::execute::RunConfig;
# use seqproc::io_config::{InputLane, InputSource, OutputTarget};
let config = RunConfig::new("unused")
    .with_input_lanes([InputLane::single(InputSource::Stdin)])
    .with_outputs([OutputTarget::Stdout]);
```

Only configurations that contain a stream use the reader/writer-backed graph
nodes. Path-only configurations retain the existing optimized file operators.

`RunConfig::with_interleaved_input` supplies ordered physical shards whose
records alternate by logical lane. The compiled geometry determines arity;
callers do not repeat it in the run configuration.

## `RunConfig`

`RunConfig::new(input1)` selects conservative defaults:

- one transform thread;
- unordered output;
- automatic execution planning (currently the normal whole-graph path unless
  ordering requires a pipeline);
- conservative graph optimization;
- gzip level 3;
- serial output compression;
- default `needletail` input decoding;
- statistics off.

`with_input_lanes` supplies ordered shards for each biological read lane.
`input1` and `input2` remain the one-path compatibility representation when
`input_lanes` is `None`. Fields are public for explicit configuration of output paths,
ordering, execution mode, graph optimization, pipeline input mode and bounds, gzip
backends, additional geometry arguments, demultiplexing, direct terminal
rendering, and statistics.

If a compiled geometry transforms into two reads, both primary output paths are
required unless demultiplexing handles output routing.

## `RunReport`

The report describes effective execution choices even when statistics are off:

- effective threads and ordering;
- the compile-time optimization report, including pass-level node changes and
  opaque semantic barriers;
- the selected execution backend, graph-cost summary, effective bounds, and
  stable planner reason codes;
- selected pipeline and bounded-stage information, including whether direct
  terminal rendering was selected, when applicable;
- compression and decompression backends;
- optional `SeqprocStats` when collection was enabled.

Use these effective fields in logs rather than reconstructing runtime behavior
from requested options.

## Error handling

`run` is fallible and returns an error for invalid thread counts, unsupported
option combinations, FASTQ opening/parsing failures, graph execution failures,
and output failures. Applications should propagate or report the error and exit
nonzero; do not discard it.

The Rust API is still pre-1.0. Pin the exact seqproc and ANTISEQUENCE revisions
for applications that need API stability.
