# seqproc development roadmap

## Purpose and scope

This is the canonical post-release engineering roadmap for seqproc and its
ANTISEQUENCE backend. It contains only product capabilities, correctness,
performance, testing, and user-facing documentation work.

Development proceeds on the `dev` branches of seqproc and ANTISEQUENCE. A
milestone becomes complete only when its implementation, compatibility path,
tests, performance evidence, user documentation, and machine-readable
diagnostics have landed together.

The milestones below are ordered. Later work may be prototyped, but it should
not become the new public contract until the earlier abstractions it depends on
are stable.

## Status legend

- **Planned**: no supported seqproc interface exists yet.
- **Foundation exists**: lower-level code is useful, but the end-to-end
  seqproc capability and its contract are incomplete.
- **In progress**: implementation has begun on `dev` but has not cleared every
  acceptance gate.
- **Complete**: every listed acceptance gate has passed and the documentation
  describes the released behavior.

## Cross-cutting requirements

Every milestone must satisfy these requirements:

1. Existing EFGDL 1 and EFGDL 2 protocols that do not use the new capability
   retain byte-identical output and remain performance-neutral within a
   predeclared measurement-noise threshold.
2. New execution paths are fallible. Malformed configuration, missing
   resources, unequal FASTQ inputs, parse failures, and output failures must not
   panic.
3. Inputs are streamed with bounded memory. File lists, stdin, compression,
   interleaving, and additional lanes must not require whole-file buffering or
   seeking.
4. `seqproc validate`, `seqproc explain`, summaries, and dry-run output expose
   the same normalized configuration used by `seqproc run`.
5. Optimized and reference behavior must be differentially testable.
6. Feature-specific Criterion or end-to-end measurements accompany changes
   that affect a per-record path. Disabled features must add no allocation,
   synchronization, or formatting work.
7. New CLI forms receive integration tests. Compatibility aliases are retained
   for one documented transition cycle unless retaining them would be unsafe.
8. Documentation examples are executed or compiled in tests where practical.

## Ordered milestones

| Order | Capability | Current status | Principal dependency |
| ---: | --- | --- | --- |
| 1 | Named resource bindings | Complete | EFGDL 2 document model |
| 2 | Lists of FASTQ files per lane | Complete | Grouped input-source model |
| 3 | stdin/stdout | Complete | Input/output target model |
| 4 | Interleaved input | Complete | Stream targets and geometry arity |
| 5 | Three or more input segments | Foundation exists in compiler/backend | Generalized bounded lane model |
| 6 | Fully typed `SeqprocError` | Partial foundations only | Public I/O and resource contracts |
| 7 | Remaining optimizer passes | Foundation exists | Effects, scoped metadata, recursive liveness |
| 8 | Dynamic batch-size planning | Planned | Stable execution and lane-cost model |
| 9 | Continuous fuzzing and full-language reference interpreter | Partial matcher oracle only | Stable language and I/O contracts |
| 10 | Dry-run support | Planned | Resources, inputs, errors, and planning |
| 11 | Protocol registry | Planned | Dry-run and named resources |
| 12 | seqspec import | Planned | Protocol registry and bounded lane model |

---

## 1. Named resource bindings

### Goal

Replace error-prone positional-only resource references such as `$0` and `$1`
with declared, named resources. Whitelists, replacement maps, anchor sets, and
other auxiliary files should be understandable from the geometry itself.

### Proposed contract

EFGDL 2 gains a resource declaration block:

```text
header { efgdl = 2 }

resources {
    barcode_whitelist
    replacements = "default-replacements.tsv"
}

bc = filter_within_dist(b[8], $barcode_whitelist, 1)
corrected = map(<bc>, $replacements, self)
```

Runtime bindings use explicit names:

```console
seqproc run \
  --bind barcode_whitelist=barcodes.txt \
  --bind replacements=experiment-replacements.tsv \
  ...
```

The exact block spelling must be checked against the EFGDL grammar before it is
frozen. The semantic model is not optional: named declarations, optional
default paths, and `$name` references must be distinct AST variants rather than
encoded as ordinary strings.

### Implementation

- Introduce a typed `ResourceRef` with `Literal`, `Positional`, and `Named`
  variants.
- Introduce `ResourceBindings`, keyed by validated identifiers, in `RunConfig`.
- Resolve resources once while building the graph; never perform a name lookup
  per read.
- Report required, defaulted, supplied, unused, and unresolved resources from
  `validate`, `explain`, dry-run, and run summaries.
- Resolve relative defaults against a documented base. Prefer the geometry
  file's directory for CLI execution and require an explicit base for
  in-memory library geometries.
- Retain `$0`, `$1`, and `--additional` for one compatibility cycle.
- Replace the current missing-positional-resource panic with a fallible error
  immediately, even before the broader error milestone is complete.

### Acceptance criteria

- Missing, duplicate, malformed, and unknown bindings produce source-located or
  CLI-located errors and a nonzero exit code.
- Named and equivalent positional geometries produce byte-identical output.
- Default and overridden paths behave deterministically.
- Geometry and resource digests identify both the declaration and resolved
  resource content without reading resources per record.
- Named bindings add no measurable steady-state processing overhead.

### Completion record

- **Implementation:** seqproc commit `6a43b18` (`Add named EFGDL resource
  bindings`). No ANTISEQUENCE change was required.
- **Compatibility:** quoted literals and `$0`/`--additional` remain supported.
  `CompiledData::interpret` retains its unit-returning compatibility behavior;
  `try_interpret` is the fallible positional API. EFGDL 2 relative literals
  and named defaults are geometry-relative, while explicit bindings retain
  invocation-relative semantics.
- **Tests:** `cargo test --lib` (276 passed), `cargo test --test
  cli_workflow_tests` (15 passed), and `cargo test --test bench_regression` (19
  passed). Named, positional, defaulted, missing, duplicate, unknown, and
  provenance paths are covered; named and positional output is asserted byte
  identical.
- **Performance gate:** resources are resolved, validated, and BLAKE3-digested
  once before graph construction. The compiled per-record graph is identical
  for named and positional references, so the feature adds no per-record
  allocation, lookup, synchronization, or branch. The existing SE/PE
  regression benchmark passed.
- **Documentation:** EFGDL overview, command-line guide, summaries guide, and
  summary schema 1.7.0 document declarations, bindings, defaults, resolution,
  and content digests.

---

## 2. Lists of FASTQ files per lane

### Goal

Allow a logical input lane to be split across ordered file shards:

```console
seqproc run \
  --read1 fa1.fq,fb1.fq \
  --read2 fa2.fq,fb2.fq \
  ...
```

This means process `(fa1, fa2)` and then `(fb1, fb2)`. It must not flatten the
four paths into four biological read lanes.

### Proposed contract

- `--read1`, `--read2`, and later `--read3` accept comma-separated paths and
  repeated occurrences. Repetition and comma separation have the same order.
- Existing `--file1` and `--file2` remain aliases for one compatibility cycle.
- Every lane has the same number of shards for ordinary grouped input.
- Corresponding shards advance together and must contain the same number of
  FASTQ records. A mismatch reports the lane, shard, and record index.
- Mixed plain and gzip shards are allowed when each source is independently
  detectable or explicitly configured.
- Output is equivalent to logical lane-wise concatenation, without creating
  temporary concatenated files.

### Implementation

- Replace `RunConfig::input1`/`input2` as the primary representation with a
  bounded collection of `InputLane` values, each containing ordered
  `InputSource` segments.
- Add a grouped input constructor to ANTISEQUENCE. Do not reuse
  `InputFastqOp::from_files` by flattening paths: that constructor currently
  interprets each path as a separate lane.
- Advance all active lane readers in lockstep and open the next shard lazily.
- Preserve origin metadata with both lane and shard identity.
- Track per-lane and per-shard counts in detailed reports.
- Provide deprecated builders/wrappers for the current one-path API.

### Acceptance criteria

- Single-end, paired-end, plain, gzip, and mixed-compression shard sets match
  equivalent explicitly concatenated streams byte for byte.
- Empty shards work; unequal shard counts and unequal records within a shard
  fail reliably without deadlock.
- Ordered and unordered execution preserve fragment membership across lanes.
- Peak memory is independent of the number and aggregate size of shards.
- Opening the next shard does not leak descriptors or retain completed
  decompressor pools.

### Completion record

- **Implementation:** seqproc commit `271e117` (`Add grouped FASTQ shard
  inputs`) and ANTISEQUENCE commit `7f5a588` (`Add synchronized grouped FASTQ
  shard input`).
- **Compatibility:** `--file1` and `--file2` remain single-file aliases. A run
  in which every lane has one shard is dispatched to the pre-existing
  `InputFastqOp`; only grouped runs instantiate `GroupedInputFastqOp`.
- **Tests:** ANTISEQUENCE `cargo test` (357 passed); seqproc `cargo test --lib`
  (276 passed), `cargo test --test cli_workflow_tests` (16 passed), and `cargo
  test --test bench_regression` (19 passed). Coverage includes repeated and
  comma-separated arguments, mixed plain/gzip shards, empty shards, unequal
  lane/shard records, per-shard statistics, and byte identity with explicitly
  concatenated logical lanes.
- **Performance gate:** readers are opened one shard per lane at a time and
  completed readers are dropped at each boundary. The one-file compatibility
  path retains the existing operator and adds no per-record branch,
  allocation, or synchronization; the existing SE/PE regression benchmark
  passed.
- **Documentation:** the README, command-line and Rust API guides, summary
  guide, and summary schema 1.8.0 describe shard ordering, equality rules,
  compression mixing, compatibility aliases, and per-shard counts.

---

## 3. stdin/stdout

### Goal

Make seqproc composable in Unix pipelines without temporary FASTQ files or
seekable input.

### Proposed contract

- `-` denotes stdin for an input source and stdout for an output target.
- At most one independent input source may consume stdin.
- At most one output lane may target stdout in the first implementation. Other
  lanes may still target files. Multiple lanes on one stdout require a future
  explicit interleaved-output contract; they must never be silently mixed.
- Logs, diagnostics, summaries without an explicit `-` target, and progress
  information go to stderr so stdout remains a clean FASTQ stream.
- Stream compression uses explicit options where suffix inference is
  unavailable. Input gzip magic-byte detection may be supported without
  seeking; output compression must be explicit for stdout.
- Broken-pipe behavior is documented and tested.

### Implementation

- Add typed `InputSource::{Path, Stdin}` and
  `OutputTarget::{Path, Stdout, Discard}` variants.
- Unify file-backed and writer-backed graph construction instead of maintaining
  a feature-incomplete alternate code path.
- Ensure worker, compression, and output-stage cancellation propagates after a
  broken pipe or upstream parse failure.
- Do not close process-owned stdin/stdout handles prematurely.
- Ensure the staged pipeline and accelerated decompressor do not require seek.

### Acceptance criteria

- File-to-stdout, stdin-to-file, and stdin-to-stdout results match the
  corresponding file-only run.
- Interleaved stdin can carry paired or three-segment input once Milestone 4 is
  enabled.
- Plain and explicitly gzip-compressed streams round-trip correctly.
- A downstream reader closing early terminates all worker stages without a
  panic, deadlock, or corrupted diagnostic stream.
- Summary JSON can be directed to a file or stderr without contaminating FASTQ
  stdout.

### Completion record

- **Implementation:** seqproc commit `dddea38` (`Add typed stdin and stdout
  FASTQ streams`); ANTISEQUENCE commits `5f50af1` (`Harden stream-backed FASTQ
  operators`) and `7b5be94` (`Test broken-pipe cancellation`).
- **Compatibility:** `InputSource::{Path, Stdin}` and
  `OutputTarget::{Path, Stdout, Discard}` are the primary stream model, while
  legacy path fields remain supported. Path-only runs still select the
  pre-existing `InputFastqOp`/`GroupedInputFastqOp` and
  `OutputFastqFileOp`; writer-backed construction is selected only when stdout
  is present.
- **Tests:** seqproc all-target check; `cargo test --lib` (276 passed), `cargo
  test --test cli_workflow_tests` (17 passed), and `cargo test --test
  bench_regression` (19 passed). ANTISEQUENCE `cargo test --lib` (359 passed)
  plus an explicit broken-pipe cancellation test. File-to-stdout,
  stdin-to-file, stdin-to-stdout, gzip stdin, gzip stdout, multiple-stream
  rejection, clean stderr summary routing, and accepted-fragment counts are
  covered.
- **Performance gate:** the typed-target checks occur once during graph
  construction. A path-only configuration follows the unchanged optimized
  operators and incurs no per-record stream branch; the existing SE/PE
  regression benchmark passed. Stream output uses bounded per-thread buffers.
- **Reporting and documentation:** summary schema 1.9.0 records path/stdin and
  path/stdout/discard topology without exposing path names. The README and CLI,
  summary, and Rust API guides document stream ownership, explicit stdout gzip,
  stderr reporting, limits, and nonzero broken-pipe behavior.

---

## 4. Interleaved input

### Goal

Expose ANTISEQUENCE's existing interleaved-reader foundation through seqproc,
with arity derived from the compiled input geometry rather than separately
specified in two places.

### Proposed contract

```console
seqproc run --interleaved-input reads.fastq --geom paired.geom ...
```

- The compiled geometry determines whether records are consumed in groups of
  one, two, or three in the first implementation.
- Interleaved file lists and interleaved stdin are valid.
- Separate-lane options and `--interleaved-input` are mutually exclusive.
- The number of records must be divisible by the geometry's input arity.
- Output remains separate by lane unless a later explicit interleaved-output
  feature is added.

### Implementation

- Route the CLI and `RunConfig` through `InputFastqOp`'s interleaved reader
  semantics after adapting them to grouped shards and typed sources.
- Define fragment ordering and batching in units of complete interleaved
  groups, not individual FASTQ records.
- Retain lockstep read names, origins, statistics, unassigned routing, and
  ordered-output sequence numbers.
- Test accelerated gzip and ordinary gzip on interleaved sources.

### Acceptance criteria

- Deinterleaved output is byte-identical to equivalent separate-lane input.
- Truncation within an interleaved group reports the expected and observed
  arity and fails without emitting a partial fragment.
- One-, two-, and three-lane inputs work with one and multiple workers.
- Ordering, detailed statistics, and rejected-read routing count fragments and
  lane records consistently.
- No extra per-record allocation is introduced relative to separate inputs.

### Completion record

- **Implementation:** seqproc commit `f9ce343` (`Add geometry-driven
  interleaved FASTQ input`) and ANTISEQUENCE commit `cf29232` (`Add grouped
  interleaved FASTQ input`).
- **Contract:** repeatable/comma-separated `--interleaved-input` and
  `RunConfig::with_interleaved_input` accept ordered file shards or singleton
  stdin. Geometry arity is authoritative; separate-lane options conflict;
  output remains separate by logical lane.
- **Tests:** ANTISEQUENCE `cargo test --lib` (363 passed); seqproc all-target
  check, `cargo test --lib` (276 passed), `cargo test --test
  cli_workflow_tests` (18 passed), and `cargo test --test bench_regression` (19
  passed). Coverage includes one-, two-, and three-record backend groups,
  one/two workers, lazy empty shards, interleaved stdin, mixed plain/gzip
  accelerated input, byte identity with separate lanes, rejection/unassigned
  routing, ordered output, detailed per-shard statistics, and explicit
  expected/observed truncation diagnostics.
- **Performance gate:** Criterion over 20,000 paired fragments measured
  separate input at 2.4866 ms and interleaved input at 2.5266 ms (1.6%
  difference). Both paths reuse bounded read storage; the feature adds no
  branch or allocation to separate-input record processing.
- **Reporting and documentation:** summary schema 1.10.0 adds the authoritative
  `input_layout`; the README and CLI, summary, and Rust API guides document
  arity, shards, stdin, compression, output separation, and truncation.

---

## 5. Three or more input segments

### Goal

Support protocols such as scATAC-seq that supply three synchronized FASTQ
segments, while retaining efficient fixed-arity hot paths and avoiding an
unbounded dynamic-lane abstraction.

### Arity policy

- First supported public arities: **1, 2, and 3**.
- Reserve the design for a small fixed maximum, provisionally **8**, without
  promising all arities in the first release.
- Prefer fixed representations for active hot paths. Candidate designs include
  `InputFastqOp<const N: usize>` behind a type-erased graph node, or an enum of
  `One`, `Two`, and `Three` variants containing fixed arrays.
- Do not force const generics through the entire `Graph`, `Read`, or expression
  API merely for aesthetic type-level arity. Measure code size, compile time,
  dispatch cost, and optimizer complexity before choosing.
- Geometry validation remains the authority for required input and output
  arity. Gaps and unsupported indices receive diagnostics.

### Proposed first-pass CLI

```console
seqproc run \
  --read1 genomic_R1.fastq.gz \
  --read2 cell_barcode.fastq.gz \
  --read3 genomic_R2.fastq.gz \
  --geom scatac.geom \
  --out1 transformed_R1.fastq.gz \
  --out2 transformed_barcode.fastq.gz \
  --out3 transformed_R2.fastq.gz
```

The generalized Rust API uses bounded lane collections rather than adding
`input3`, `input4`, and so on as public struct fields. CLI aliases may remain
explicit for the common first three lanes.

### Implementation

- Generalize primary, unassigned, and demultiplexed output routing from two
  hard-coded lanes to the compiled output arity.
- Generalize `RunConfig`, `RunReport`, input statistics, validation, and output
  requirements to the bounded lane model.
- Preserve efficient specialized loops for the measured one- and two-lane
  cases; add a fixed three-lane path.
- Upgrade the existing three-read graph-construction test to full FASTQ
  execution, rejection, transformation, ordering, and reporting tests.
- Add a representative scATAC geometry and small public or synthetic fixture.

### Acceptance criteria

- One- and two-lane controls remain byte-identical and performance-neutral.
- Three separate files and a three-way interleaved file produce identical
  outputs.
- File lists, stdin, gzip, unassigned output, summaries, and ordering all work
  for three lanes.
- Unsupported arity fails during validation rather than in a worker.
- Benchmarks report any binary-size or throughput effect of fixed-arity
  specialization.

---

## 6. Fully typed `SeqprocError`

### Goal

Make every public execution path return structured, matchable errors. Remove
`anyhow` from the public contract and remove production panics caused by user
input.

### Proposed contract

- Define `SeqprocError` with `thiserror` rather than hand-writing repetitive
  display/source implementations.
- Keep focused internal error enums where they add useful context, and convert
  them at subsystem boundaries without erasing their sources.
- `run(RunConfig, CompiledData) -> Result<RunReport, SeqprocError>` becomes the
  primary API.
- Geometry diagnostics retain source spans and may be represented as a
  dedicated aggregate variant.
- The CLI maps errors to stable broad classes such as usage/configuration,
  malformed input, and execution/output failure. Exact numeric codes need not
  become a public fine-grained ABI unless users require them.

### Required variants

At minimum, errors distinguish:

- geometry lexing, parsing, and semantic compilation;
- missing, duplicate, unused-as-error, and unreadable resource bindings;
- invalid input arity, shard mismatch, record-count mismatch, and interleaved
  truncation;
- FASTQ parse/decompression failure with lane, shard, and record context;
- invalid output topology and output/compression failure;
- invalid thread, batch, queue, or execution-planner configuration;
- graph compilation and execution failure;
- unsupported feature combinations; and
- broken pipes where policy requires reporting them.

### Implementation

- Replace `parse_additional_args` panics and `from_readers` unwraps first.
- Audit public and CLI-reachable `panic!`, `unwrap`, `expect`, log-and-return,
  and stringly typed `anyhow!` paths.
- Preserve legacy wrappers only when they can delegate without suppressing an
  error; otherwise deprecate them explicitly.
- Add error context at the layer that knows lane, shard, resource, geometry
  span, or output target.

### Acceptance criteria

- No malformed user input in the supported matrix unwinds the process.
- Library tests can match error variants without parsing display text.
- Every CLI error produces a nonzero status and a concise actionable message.
- Error sources remain available for debugging and reports omit sensitive path
  contents unless explicitly requested.
- A source audit documents every remaining intentional panic as an internal
  invariant.

---

## 7. Remaining optimizer passes

### Goal

Use the implemented operation descriptors, invalidation effects, scoped
metadata, and recursive liveness analysis for additional proof-backed graph
optimization.

### Pass 1: dead-label elimination

- Remove creation or retention of interval labels proven dead at every
  continuation.
- Do not remove operations that can reject, change sequence, affect routing,
  emit trace/statistical observations, or change missing-input behavior.
- Preserve wildcard and scoped record/lane metadata semantics.
- Apply recursively to privately owned nested graphs; shared or opaque nodes
  remain barriers.

### Pass 2: early selective-filter placement

- Move a selective rejecting operation earlier only across operations that are
  proven independent, non-rejecting, order-insensitive, and effect-preserving.
- Preserve which error/rejection reason is externally reported.
- Disable or constrain movement when detailed stage statistics or tracing make
  operation order observable.
- Use static selectivity hints first. Runtime-adaptive graph reordering is out
  of scope because it complicates reproducibility and semantics.

### Additional safe candidates

- Adjacent projection/slice fusion with proven coordinate equivalence.
- Redundant existence-check elimination after unconditional production.
- Constant predicate and unreachable branch removal.
- Common-prefix hoisting from ordered-choice arms when rejection and mutation
  behavior are identical.

### Acceptance criteria

- Every pass is separately reported, configurable, and ablatable.
- Generated-graph differential tests compare optimized and unoptimized labels,
  outputs, rejection reasons, statistics, and errors.
- Nested branches, loops, orientation routing, and scoped metadata have
  dedicated liveness tests.
- Feature-off and no-op graphs do not regress by more than 3%; accepted passes
  demonstrate a meaningful win on a declared workload.

---

## 8. Dynamic batch-size planning

### Goal

Choose bounded batch and queue settings from the compiled graph, lane arity,
read sizes, compression, output mode, and worker count while preserving manual
controls and reproducibility.

### Proposed contract

- `--batch-size` remains an exact override.
- Automatic planning reports its selected batch size, queue capacity,
  in-flight bound, inputs, and stable reason codes.
- The heuristic is deterministic for the same compiled graph, configuration,
  and declared/sampled read statistics.
- If a small prefix is sampled, it is consumed exactly once and then processed
  as ordinary input; stdin and non-seekable sources remain supported.
- Hard minimum, maximum, and memory-budget bounds prevent pathological plans.

### Implementation

- Extend graph cost summaries with estimated per-fragment work and live bytes.
- Account for fixed-arity lane count, output buffers, reorder slots, and
  compression buffers in the memory estimate.
- Compare static-only planning with a one-batch length sample. Prefer the
  simpler method unless sampling produces a stable material gain.
- Retain fixed-size benchmark controls and a planner-disable switch.
- Avoid continuous adaptation until deterministic phase-boundary behavior and
  memory accounting are proven.

### Acceptance criteria

- A checked-in matrix covers short/long, one/two/three-lane, simple/expensive,
  plain/gzip, ordered/unordered, and stdin/file workloads.
- Automatic planning improves the aggregate matrix without more than 5%
  regression in any representative condition.
- Peak live memory stays within the reported bound under delayed workers and
  output backpressure.
- Plans and output are deterministic across repeated runs.

---

## 9. Continuous fuzzing and full-language reference interpreter

### Goal

Provide an independent semantic oracle for the complete supported EFGDL
language and continuously exercise malformed and adversarial inputs.

### Reference interpreter

- Implement a deliberately simple interpreter that favors clarity over speed.
- Cover EFGDL 1 compatibility and every supported EFGDL 2 construct: layout
  algebra, fixed output sequence, header templates, indexed captures, named
  resources, matching modes, ambiguity policies, transformations, orientation,
  filtering, and bounded multi-lane input.
- Use the existing exhaustive matcher as a component, not the optimized graph
  compiler as the implementation of the oracle.
- Return normalized labels, output reads, rejection reason, and structured
  errors for differential comparison.

### Continuous fuzzing

- Add `cargo-fuzz` targets for lexer/parser, semantic compiler, FASTQ parsing,
  matcher backends, transformation/projection, and graph execution.
- Maintain minimized regression corpora in version control when licensing and
  size permit.
- Add structure-aware generators for valid nested layouts and near-valid
  malformed documents.
- Run short deterministic fuzz smoke tests in ordinary CI and longer campaigns
  on a schedule or dedicated runner.
- Exercise scalar and supported SIMD paths, multiple thread counts, optimized
  and unoptimized graphs, and statistics off/on.

### Acceptance criteria

- Generated supported programs agree with the reference interpreter on output,
  labels, rejection, ambiguity, and errors.
- Crash, timeout, and excessive-allocation findings become permanent regression
  tests.
- Corpus execution is reproducible and bounded in ordinary CI.
- Unsafe code and externally sourced optimized algorithms have documented
  invariants and sanitizer/Miri coverage where feasible.

---

## 10. Dry-run support

### Goal

Allow users to validate a complete invocation—including resources, inputs,
outputs, compression, graph optimization, and execution planning—without
emitting transformed reads.

### Proposed contract

```console
seqproc run --dry-run --geom protocol.geom --read1 reads.fq --bind whitelist=wl.txt
```

Dry-run is stronger than `validate` and more operational than `explain`:

- `validate` checks the geometry language;
- `explain` displays normalized geometry and graph structure; and
- dry-run resolves a complete invocation and reports what would execute.

Dry-run must not create or truncate primary, unassigned, demultiplexed, or
summary output files. By default it may inspect file metadata and compression
headers but does not scan the full data. A separately named bounded preview
mode may process a small number of fragments to the null sink.

### Report contents

- normalized EFGDL and BLAKE3 geometry digest;
- required and resolved resources plus content digests when requested;
- input arity, lane/shard topology, interleaving, compression, and seekability;
- output topology and compression decisions;
- selected matcher backends and ambiguity policies;
- graph optimization report and execution plan;
- estimated batch memory and effective worker/compression counts; and
- warnings for deprecated aliases or ignored/unused bindings.

Human-readable and versioned JSON output are both required.

### Acceptance criteria

- Dry-run catches every configuration error that can be detected without
  consuming full FASTQ input.
- It performs no output mutation, including demultiplexing-directory creation.
- Dry-run and the subsequent real run report identical normalized resources,
  graph, and plan for the same inputs.
- stdin limitations are explicit: validation must not consume stdin merely to
  inspect it.

---

## 11. Protocol registry

### Goal

Make curated, versioned protocol geometries discoverable and runnable without
copying opaque command fragments, while retaining offline reproducibility and
transparent source inspection.

### Proposed contract

```console
seqproc protocol list
seqproc protocol show scatac-example@1
seqproc protocol run scatac-example@1 --read1 ... --read2 ... --read3 ...
```

- Registry entries are immutable by `(name, version)` and identified by a
  BLAKE3 content digest.
- Every entry contains EFGDL source, declared resources, supported input/output
  arity, provenance, citations, license, and a small validation fixture or
  checksum.
- Built-in and explicitly configured local registries work offline.
- Network registry fetching, if added, is opt-in and content-addressed. A name
  must never silently resolve to changed content.
- Users can print and export the exact geometry before executing it.

### Implementation

- Define a versioned registry manifest/schema independently of the summary
  schema.
- Resolve registry protocols into the same compiled geometry,
  `ResourceBindings`, and `RunConfig` used by ordinary files.
- Integrate dry-run as the default inspection path.
- Define precedence and diagnostics for built-in, user, and explicitly supplied
  registry locations.
- Keep protocol knowledge out of matcher and execution backends.

### Acceptance criteria

- Registry and directly supplied equivalent geometries produce byte-identical
  output.
- Offline resolution is deterministic and checksum-verified.
- Missing resources, incompatible arity, schema versions, and digest mismatch
  fail before processing.
- At least one one-lane, one paired-lane, and one three-lane protocol fixture
  are tested end to end.

---

## 12. seqspec import

### Goal

Import the explicitly supported subset of seqspec into EFGDL 2 and the protocol
registry without silently approximating unsupported semantics.

### Proposed contract

```console
seqproc import seqspec protocol.yaml --output protocol.geom
seqproc import seqspec protocol.yaml --registry-entry protocol-entry.yaml
```

- Import is a compile-time conversion, not a runtime dependency.
- The original seqspec document and its digest remain in provenance.
- Unsupported, ambiguous, or lossy constructs produce explicit diagnostics.
- Import never fabricates whitelist correction, ambiguity, orientation, or
  output-transformation policies that are absent from the source.

### Implementation

- Pin and document the supported seqspec schema versions.
- Build a typed intermediate protocol representation rather than translating
  YAML fields directly into EFGDL text.
- Map read segments, fixed anchors, barcode/UMI regions, read orientation, and
  declared resources into the bounded lane and named-resource models.
- Render stable, formatted EFGDL 2 suitable for review and version control.
- Reuse registry validation, dry-run, and source diagnostics.
- Preserve unknown extension fields in provenance when possible, but do not
  claim they affect execution.

### Acceptance criteria

- Fixture imports cover representative single-cell RNA, scATAC/multi-segment,
  and another supported protocol shape.
- Re-importing identical source produces byte-identical normalized EFGDL and
  registry digests.
- Supported imports execute identically to manually authored reference
  geometries.
- Unsupported constructs identify the exact source location and required
  manual decision.
- Import requires no network access when schemas and referenced resources are
  local.

---

## Deferred beyond this roadmap

These are not commitments of the ordered milestones above:

- unbounded or dynamically sized capture collections;
- unlimited runtime FASTQ-lane arity;
- implicit mixing of multiple output lanes on stdout;
- runtime-adaptive graph reordering based on observed biological outcomes;
- a stable third-party plugin ABI; and
- automatic remote protocol updates without content pinning.

They may be reconsidered only after the bounded, explicit versions have shipped
and been measured.

## Completion record

When a milestone is completed, update its status in the ordered table and add:

- implementation commit(s) in seqproc and ANTISEQUENCE;
- compatibility and migration notes;
- exact test commands and results;
- benchmark artifact paths and non-regression thresholds; and
- documentation pages added or changed.

Do not erase rejected designs or failed performance experiments. Record why
they were rejected so later work does not repeat the same experiment without
new evidence.
