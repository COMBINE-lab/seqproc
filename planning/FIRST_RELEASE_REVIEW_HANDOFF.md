# seqproc and ANTISEQUENCE first-release review handoff

**Prepared:** 2026-08-21

**Purpose:** independent technical and release review before publishing the first
crates.io releases of `antisequence` and `seqproc`, producing a tagged seqproc
binary release, and submitting a Bioconda recipe.

**Release status:** proposed feature freeze; not yet approved, published, or
packaged for Bioconda.

## Executive summary

The current `dev` branches are substantially beyond the software described by
the July 2026 preprint. The work is no longer a narrow performance patch. It
includes a versioned EFGDL 2 language, richer protocol composition and
ambiguity semantics, named resources, streaming and multi-segment input,
fallible public APIs, a typed error model, a unified matcher, an immutable graph
API, proof-gated graph optimization, deterministic execution and batch
planning, low-overhead reporting, release automation, and substantially broader
tests and documentation.

The recommendation represented by this handoff is:

1. Freeze the current feature boundary as the first public release.
2. Conduct an independent correctness, API, portability, packaging, and
   documentation review of both repositories together.
3. Fix release-blocking findings without adding another major feature family.
4. Merge reviewed `dev` into `main`, publish ANTISEQUENCE first, then seqproc,
   then create and test the Bioconda recipe from an immutable release archive.
5. Defer the full-language reference interpreter and continuous-fuzzing program
   in Milestone 9, dry-run, the protocol registry, and seqspec import to later
   releases.

Milestone 9 is intentionally not a prerequisite for this first release. A
low-level reference matcher exists and the new language paths have directed,
differential, property-style, and end-to-end tests, but the planned independent
interpreter and continuous fuzzing infrastructure are a separate, substantial
project. The deferral must be stated accurately; the current code should not be
described as having a full reference implementation or continuous fuzzing.

## Review outcome requested

The reviewer should return:

- a release recommendation: **approve**, **approve after listed fixes**, or
  **do not release**;
- release-blocking findings separated from post-release improvements;
- an assessment of public API and EFGDL 2 stability;
- confirmation that optimized and fallback paths preserve documented
  semantics;
- confirmation of the supported CPU and operating-system envelope;
- confirmation that the release and Bioconda build procedures are reproducible;
- a final list of exact commits suitable for tagging.

No crate should be published and no release tag should be pushed until this
review and the release gates near the end of this document are complete.

## Repository and version boundaries

The review spans two repositories because seqproc compiles EFGDL into, and
executes it through, ANTISEQUENCE graphs.

| Boundary | seqproc | ANTISEQUENCE | Meaning |
| --- | --- | --- | --- |
| Preprint-era baseline | `03cb76335887ccc1f5191f458a4837debe424a31` | `cd20194c9151b8679bba56411b231b88ac51526f` | Repository heads at the preprint date, 2026-07-28 |
| Journal benchmark freeze | `cbc156bea6f5d713ea73bbba1e6c55b7a85a58d8` | `f3b0f1ea0b286b680d963382f2bf7316058d83e4` | Exact implementations used for the revised paper benchmarks; do not silently replace these provenance records with the release heads |
| Current `main` | `d07355d315d2a85fab8f4d340d555777168386aa` | `83af892b81149224c6231ebb70a6672175e105c0` | Journal-hardening work already merged |
| Feature-inventory head (`dev`, before this handoff document) | `cdc2084f789e8302da57783d6aa3e7c0e64eef53` | `baa4bc26cc1041197de6dc57d08d3d0a6425bb5a` | Feature-frozen implementation heads inventoried here; the documentation commit and any review fixes will advance the final release SHAs |

The preprint-to-review-head diff comprises 54 seqproc commits affecting 96
files (`23,964` insertions, `1,063` deletions) and 47 ANTISEQUENCE commits
affecting 60 files (`14,946` insertions, `836` deletions).

Both `Cargo.toml` files still declare version `0.1.0` and Rust `1.88`. The
release version is therefore a decision still to confirm, not a completed
release action. The seqproc review head pins the ANTISEQUENCE review head
exactly, by Git revision, while also declaring registry version `0.1.0` for
packaging.

The crates.io sparse-index endpoints for `seqproc` and `antisequence` returned
404 on 2026-08-21. This suggests that neither name is currently published, but
the reviewer must reconfirm namespace availability immediately before the
irreversible publication step.

## What was present in the preprint version

The preprint version established the central design: a concise EFGDL geometry
description was compiled into an ANTISEQUENCE execution graph, separating the
user-facing protocol language from a reusable Rust backend. It could parse and
transform FASTQ inputs, express the paper's demonstrated protocols, and execute
with multiple workers. That architecture remains intact.

The post-preprint work did not replace this design. It made the boundaries
explicit and safer, corrected a fast-path semantic defect, generalized the
language and I/O contracts, and optimized the compiled graph and matcher while
preserving compatibility with headerless EFGDL 1 geometries.

## Change inventory and design rationale

### 1. Journal hardening and public execution interfaces

#### Added

- A subcommand CLI with `seqproc run`, `seqproc validate`, and `seqproc
  explain`, while retaining the legacy flag-only invocation for compatibility.
- A typed `RunConfig`/`RunReport` execution interface.
- Versioned JSON summary schemas, now covering accepted and rejected reads,
  failure reasons, effective execution settings, provenance, matcher details,
  and per-input statistics.
- A unified normal and summary pipeline so enabling a report does not remove
  demultiplexing, unassigned-read output, compression, or transformations.
- Reliable nonzero CLI failures for malformed FASTQ, invalid resources,
  graph failures, output errors, and invalid configuration.
- `off`, `basic`, and `detailed` statistics levels.

#### Design decisions

- Validation and explanation use the same parser, resource resolution, and
  graph compilation path as execution. A configuration should not validate
  through a different interpretation than the one that runs.
- Statistics are worker-local during processing and aggregated at the end.
  Normal runs avoid the former shared atomics and histogram locks.
- Summaries are schema-versioned rather than treated as an informal diagnostic
  blob because they are part of reproducible workflow automation.
- Legacy entry points remain wrappers for a compatibility period, but the
  fallible interface is the primary contract.

#### Relevant changes

- seqproc commits: `44c9e5c`, `3088bac`, `c9d7147`.
- ANTISEQUENCE commits: `a5de904`, `2f93076`, `9268150`, `96c9c9f`,
  `19eef2c`.
- seqproc sources: `src/bin/bin.rs`, `src/execute.rs`, `src/lib.rs`,
  `src/error.rs`.
- Schemas: `schemas/seqproc-summary-*.schema.json` and `schemas/README.md`.
- Tests: `tests/cli_workflow_tests.rs`, `tests/error_handling_tests.rs`, and
  `tests/bench_regression.rs`.

### 2. Correct and configurable ambiguity handling

#### Added or corrected

- The short-pattern/low-Hamming optimized path now honors `FILTER` semantics
  instead of accepting a read merely because a near barcode existed.
- Duplicate whitelist entries are normalized as duplicate inputs rather than
  treated as biological ambiguity.
- Genuine ambiguity between distinct candidates is explicit and configurable.
- EFGDL annotations accept both assignment syntax such as
  `#[ambig_policy = accept]` and call syntax where arguments are appropriate.
- Pattern ambiguity and position ambiguity are separate axes.
- Position policies include first, last, best-distance, rejection, and
  quality-aware tie resolution where the semantics are well-defined.
- Anchor-set ambiguity policies are tested end to end.

#### Design decisions

- Duplicate lines in an input list do not constitute multiple biological
  answers; resource normalization handles them once at load time.
- Equidistant matches to different sequences are genuine ambiguity and must be
  governed by a documented policy rather than hash iteration order.
- Pattern choice and occurrence position are different questions. Conflating
  them makes policies surprising and prevents deterministic explanation.
- Quality-aware position selection is supported for Hamming/exact cases.
  Quality selection for edit-distance alignments is deliberately rejected
  until insertion/deletion and gap-quality semantics are defined.
- `best` minimizes the primary distance first and applies the configured tie
  behavior second; it is not an undocumented alias for first match.

#### Relevant changes

- seqproc commits: `3088bac`, `8b8d54d`, `29b6bc5`, `dd4a3cc`.
- ANTISEQUENCE commits: `2f93076`, `759b239`, `7b5341d`, `1c6a147`,
  `bbbb065`.
- seqproc sources: `src/geometry/parser.rs`,
  `src/geometry/compile/functions.rs`, `src/geometry/compile/layout.rs`.
- ANTISEQUENCE sources: `src/matcher.rs`,
  `src/graph/ops/match_any_op.rs`, `src/patterns.rs`.
- Tests: `tests/annotation_tests.rs`, `tests/anchor_set_tests.rs`,
  `tests/layout_algebra_tests.rs`, and `tests/paper_chemistry_tests.rs`.

### 3. EFGDL 2 document model and provenance

#### Added

- An EFGDL document header, for example `header { efgdl = 2 }`.
- Headerless geometries retain EFGDL 1 behavior.
- BLAKE3 digests for normalized geometry and bound resource content.
- Fixed literal sequence insertion into transformed output reads.
- FASTQ header templates that can prepend, append, or replace a header using
  literal text and captured values.
- Version-aware validation, normalized explanation, and summary provenance.

#### Design decisions

- The language version lives in a general document header rather than a
  one-off marker. This gives future document-level declarations an intentional
  home.
- BLAKE3 was selected over SHA-256 for fast content provenance without placing
  a cryptographic hash on the per-read processing path.
- Inserted fixed bases receive a deterministic synthetic quality (`I`) because
  FASTQ sequence and quality lengths must remain equal. This behavior is
  documented and reviewable rather than implicit.
- Header modification has a specialized path and incurs no formatting,
  materialization, or allocation work when no header transformation is
  configured.
- The digest identifies normalized configuration and resolved resource
  content; it is not used as a security signature.

#### Relevant changes

- seqproc commits: `e1bed15`, `576fd54`, `8b1fbf0`.
- ANTISEQUENCE commit: `aac6fad`.
- seqproc sources: `src/geometry/lexer.rs`, `src/geometry/parser.rs`,
  `src/geometry/interpret.rs`, `src/geometry/compile/transformation.rs`, and
  `src/resources.rs`.
- ANTISEQUENCE sources: `src/graph/ops/project_op.rs`, `src/read.rs`.
- User documentation: `README.md` and `website/src/content/docs/`.

### 4. Complex-protocol layout algebra

#### Added

- Bounded choice/either expressions.
- Optional layout terms.
- Statically bounded repeat terms.
- Statically lowered indexed captures.
- Conditional terminal projections and orientation-aware alternatives.
- Whitelist-backed anchor sets.
- Validation and normalization bounds to prevent uncontrolled graph growth.

#### Design decisions

- Layout constructs are compile-time algebra, not a dynamically interpreted
  per-read mini-language. Bounded alternatives are lowered to explicit graph
  structures that can be validated and optimized.
- Repeat counts and indexed captures are statically bounded. This provides the
  useful protocol expressiveness without introducing runtime-dynamic label
  arity or unbounded metadata.
- Indexed captures were implemented holistically for the statically bounded
  case because that representation remains appropriate under a future broader
  record/lane metadata redesign.
- Choice is ordered and deterministic. Fast successful-first-choice behavior,
  late fallback, and rejected alternatives have dedicated performance tests.
- Native dual-orientation execution uses branching and value-returning
  fallbacks; it does not require preprocessing a reverse-complemented copy.
- Terminal projection inside nested graphs became safe only after scoped
  metadata, recursive liveness, and graph freezing were added.

#### Relevant changes

- seqproc commits: `29b6bc5`, `dd4a3cc`, `fae0dfe`, `b6c4995`.
- ANTISEQUENCE commits: `15ca1b6`, `325722b`, `e26ae51`, `54d1e75`,
  `2c99a67`, `1a949cf`, `07a6b22`.
- seqproc sources: `src/geometry/compile/layout.rs`,
  `src/geometry/compile/reads.rs`, `src/geometry/compile/definitions.rs`.
- ANTISEQUENCE sources: `src/graph/ops/switch_op.rs`,
  `src/graph/ops/try_op.rs`, `src/graph/ops/try_orientation_op.rs`,
  `src/graph/ops/project_op.rs`.
- Tests: `tests/layout_algebra_tests.rs`, `tests/anchor_set_tests.rs`.
- Design record: ANTISEQUENCE `docs/metadata-liveness-redesign.md`.

### 5. Named resource bindings

#### Added

- EFGDL 2 resource declarations with optional geometry-relative defaults.
- Named references such as `$barcode_whitelist`.
- Repeated `--bind name=path` CLI bindings.
- Required, supplied, defaulted, unused, unresolved, and digested resource
  reporting.
- Positional `$0`, `$1`, and `--additional` compatibility.

#### Design decisions

- Resource references are typed as literal, positional, or named variants;
  names are not encoded as ordinary strings.
- Resolution, validation, loading, and BLAKE3 hashing occur once before graph
  construction. There is no per-record name lookup.
- Geometry-declared relative defaults resolve relative to the geometry file.
  Explicit CLI bindings retain invocation-relative path semantics.
- In-memory geometry users must provide an explicit base for relative defaults.
- Unknown and unused explicit bindings are diagnosed to catch spelling errors.

#### Relevant changes

- seqproc commit: `6a43b18`.
- Sources: `src/resources.rs`, `src/io_config.rs`,
  `src/geometry/compile/definitions.rs`, `src/geometry/interpret.rs`.
- Completion record: `planning/ROADMAP.md`, Milestone 1.

### 6. Streaming, sharded, interleaved, and multi-segment FASTQ I/O

#### Added

- Ordered lists of FASTQ shards per logical lane via comma-separated and
  repeated `--read1`, `--read2`, and `--read3` arguments.
- Lazy, synchronized shard advancement without temporary concatenation.
- stdin input and stdout output with typed stream targets.
- Graceful broken-pipe cancellation.
- Interleaved single-stream input for multi-segment geometries.
- Bounded support for one, two, and three input segments.
- Plain and gzip inputs, including independently detected mixed shard sets.

#### Design decisions

- A list of files is a list of ordered shards within one biological lane, not
  additional read segments. Corresponding lane shards advance in lockstep.
- Unequal shard counts and unequal records within a shard are errors containing
  lane, shard, and record context.
- Inputs remain streaming and memory-bounded; files and decompressors are
  opened lazily and released after their shard.
- stdin is a unique stream and therefore cannot be bound to multiple lanes or
  mixed ambiguously with independent sources.
- stdout is supported only where one serialized output stream is unambiguous;
  invalid multi-output combinations fail before processing.
- Interleaved arity is derived from the compiled geometry rather than inferred
  from filenames or records.
- The first generalized implementation supports one to three segments. This
  avoids heap-allocated dynamic arity and invasive public API churn while
  covering common read 1/read 2/index-read and scATAC-style inputs. Expansion
  beyond three should follow demonstrated protocol demand.
- `needletail` remains the default parser. `paraseq` was evaluated but did not
  improve the representative paired-end path enough to justify replacing the
  stable parser. `rapidgzip-core` is an opt-in parallel-gzip backend rather than
  a mandatory dependency/path.

#### Relevant changes

- seqproc commits: `271e117`, `dddea38`, `f9ce343`, `8f9cd35`.
- ANTISEQUENCE commits: `7f5a588`, `5f50af1`, `7b5be94`, `cf29232`,
  `33b7df4`.
- seqproc sources: `src/io_config.rs`, `src/execute.rs`, `src/bin/bin.rs`.
- ANTISEQUENCE sources: `src/graph/ops/input_fastq_op.rs`,
  `src/graph/ops/grouped_input_fastq_op.rs`,
  `src/graph/ops/output_fastq_op.rs`.
- Completion records: `planning/ROADMAP.md`, Milestones 2 through 5.

### 7. Fully typed errors

#### Added

- A public `SeqprocError` hierarchy based on `thiserror`.
- Typed variants for CLI/configuration, geometry parsing and compilation,
  resources, FASTQ parsing, synchronized input, graph execution, compression,
  output, and report serialization.
- Thread-safe preservation of concrete graph errors across worker boundaries.
- A panic audit and regression tests covering user-controlled failure paths.

#### Design decisions

- The primary API no longer erases expected failures into `anyhow::Error`.
  `anyhow` can still be used at an application boundary for presentation, but
  callers can program against stable error categories.
- User input must not trigger `panic!`, `unwrap`, or log-and-return success.
- Graph errors are `Send + Sync` and retain their source/context across scoped
  workers rather than being reduced to strings.
- Compatibility wrappers may preserve old signatures temporarily, but all new
  execution paths are fallible.

#### Relevant changes

- seqproc commit: `76fa2a8`.
- ANTISEQUENCE commits: `231f958`, `1389ccf`.
- seqproc source: `src/error.rs`.
- ANTISEQUENCE source: `src/errors.rs`.
- Tests: `tests/error_contract_tests.rs`,
  `tests/error_handling_tests.rs`.
- Audit: `planning/PANIC_AUDIT.md`.

### 8. Immutable compiled graphs, effects, metadata, and liveness

#### Added

- A `GraphBuilder`/`CompiledGraph` split.
- Validated, immutable execution graphs after compilation.
- Explicit operation effects and missing-input policies.
- Scoped control metadata for nested graphs.
- Recursive liveness analysis and safe metadata recycling.
- `ProjectOp` and `SwitchOp` primitives supporting efficient terminal layouts
  and conditional alternatives.
- Copy-on-write handling for long-read transformations.

#### Design decisions

- Graph mutation ends before execution. This permits one validation point,
  stable optimizer assumptions, and sharing across workers.
- Optimization is driven by declared effects and proof conditions, not a list
  of operation names assumed to be safe.
- Nested graphs receive scoped metadata identities. Flat/global temporary
  labels were not sufficient for choice, repeat, orientation fallback, and
  terminal projection.
- Liveness is recursive across graph boundaries; a nested consumer keeps a
  producer live, and temporary metadata is recycled only after the final
  reachable use.
- Metadata tracking has a low-overhead disabled path. Protocols that do not
  need scoped metadata do not pay for constructing it per record.
- Read storage is copy-on-write so alternatives do not eagerly clone long
  sequences that remain unchanged.

#### Relevant changes

- ANTISEQUENCE commits: `63b259e`, `c026632`, `aac6fad`, `15ca1b6`,
  `baa991b`, `54d1e75`, `2c99a67`, `1a949cf`, `07a6b22`.
- seqproc commits: `13a3e10`, `77f58b1`, `fae0dfe`, `b6c4995`.
- ANTISEQUENCE sources: `src/graph.rs`, `src/graph/ops.rs`, `src/read.rs`,
  `src/graph/ops/project_op.rs`, `src/graph/ops/switch_op.rs`.
- Design documentation: `docs/graph-api.md` and
  `docs/metadata-liveness-redesign.md`.

### 9. Unified matcher and optimized search paths

#### Added

- A shared `MatchSpec` and `MatcherPlan` defining match and ambiguity
  semantics independently from the selected search kernel.
- Static dispatch among direct exact matching, Hamming lookup, seeded search,
  exhaustive comparison, Myers bit-parallel edit distance, long-pattern
  fallback, pigeonhole filtering, and SIMD-assisted dynamic programming.
- Optimized short-edit-pattern searches on long reads.
- A low-level simple reference matcher used for differential checks.
- Reusable per-worker scratch state for seed hits and dynamic-programming
  buffers.
- A rolling-hash/filter/hash-table path using a lightweight rolling hash,
  `hashbrown` raw tables, and a fast non-cryptographic Fx-style hasher.

#### Design decisions

- Semantics live above kernels. Selecting a faster algorithm must not change
  filtering, ambiguity, orientation, or tie-breaking behavior.
- Dispatch is chosen from pattern length/count, distance type and threshold,
  and candidate structure. There is no universal hard-coded winner.
- The short Hamming path was repaired, not removed. It remains specialized and
  fast while applying the same `FILTER` and ambiguity contract as the general
  matcher.
- Hash tables do not use SipHash in the trusted, internal barcode hot path.
  The design uses a fast non-cryptographic hasher because the keys are loaded
  from explicit protocol resources, not exposed as an adversarial network map.
- The current rolling hash is purpose-built for this path; it is not `ntHash`.
  Replacing it or adopting another crate requires measured crossover and
  correctness evidence rather than a dependency substitution by reputation.
- Precomputed Hamming lookup remains preferable to adopting `seqhash` solely
  for API similarity because current matching must preserve resource
  normalization, ambiguity policies, and filtering semantics.

#### Relevant changes

- ANTISEQUENCE commits: `44de27c`, `45f0bdb`, `f3b0f1e`, `759b239`.
- seqproc commits: `f0b1b1a`, `46d66d6`, `edbf9ef`.
- ANTISEQUENCE sources: `src/matcher.rs`, `src/seed_search.rs`,
  `src/patterns.rs`, `src/graph/ops/match_any_op.rs`.
- Design documentation: `docs/unified-matcher.md`.

### 10. Graph optimization and execution planning

#### Added

- Proof-backed removal of no-ops.
- Safe adjacent idempotent-operation fusion.
- Terminal match/filter/projection fusion and direct terminal rendering.
- Proven dead-label elimination.
- Early selective-filter placement when dependencies and effects permit it.
- Barriers for statistics, tracing, opaque operations, and uncertain effects.
- An execution-plan report and calibrated automatic execution-mode selection.

#### Design decisions

- Reordering is allowed only when read/write sets, rejection behavior, external
  effects, ordering requirements, and metadata liveness prove equivalence.
- Statistics and tracing can make an otherwise equivalent rewrite observable;
  they therefore act as optimizer barriers where necessary.
- Cheap unordered whole-graph workloads can use direct worker-local execution.
  Ordered output retains the reader/worker/writer pipeline and bounded reorder
  behavior.
- Explicit user execution-mode choices override the automatic plan and are
  reported as requested and effective modes.
- Direct terminal rendering bypasses intermediate read materialization only
  when the terminal projection is proven final and safe.

#### Relevant changes

- seqproc commits: `edbf9ef`, `83a9844`, `77f58b1`, `92b1cda`.
- ANTISEQUENCE commits: `9acd373`, `96fd343`, `a42a86d`, `59bfbfe`.
- seqproc sources: `src/execute.rs`, `src/geometry/compile/mod.rs`,
  `src/geometry/compile/transformation.rs`.
- ANTISEQUENCE sources: `src/graph.rs`, `src/graph/ops.rs`.
- Documentation: ANTISEQUENCE `docs/graph-optimization-and-planning.md`,
  `docs/optimizer-passes.md`, and
  `docs/benchmarks/milestone-2-execution-crossover-2026-08-20.md`.
- Completion record: seqproc `planning/ROADMAP.md`, Milestone 7.

### 11. Deterministic dynamic batch-size planning

#### Added

- A deterministic planner that derives batch size from lane count, expected
  read length, graph cost, compression, worker count, and a memory budget.
- Plan visibility in explanation and runtime reports.
- An exact manual override for reproducible experiments and troubleshooting.

#### Design decisions

- Planning is static and deterministic. It does not sample or consume input
  records, so stdin, gzip streams, and reproducibility are unaffected.
- The default memory budget is 256 MiB and is divided across in-flight stages
  and workers.
- Long or expensive records receive smaller batches to avoid excessive admitted
  working sets; short inexpensive reads keep the established efficient batch.
- The planner is bounded and conservative. It is not an adaptive feedback loop
  whose decisions could depend on record order or machine timing.

#### Relevant changes

- seqproc commit: `0a5ff38`.
- ANTISEQUENCE commit: `baa4bc2`.
- seqproc sources: `src/execute.rs`, `src/io_config.rs`, `src/bin/bin.rs`.
- Documentation: ANTISEQUENCE `docs/batch-planning.md` and seqproc
  `planning/ROADMAP.md`, Milestone 8.
- Feature measurements: `benches/protocol-feature-results-2026-08-21.md`.

### 12. Release engineering, CI, dependency review, and documentation

#### Added

- Cached, tiered CI: a fast pull-request path and a comprehensive scheduled and
  release path.
- Cancellation of superseded CI runs and caching only after successful builds.
- `bump_and_publish.sh` workflows in both repositories.
- cargo-dist configuration and multi-platform archive/checksum generation for
  seqproc.
- An Astro/Starlight documentation site for seqproc, including light and dark
  logo assets.
- Dependency audit records and an explicit Rust 1.88 floor.

#### Design decisions

- Pull requests should receive useful feedback quickly, while comprehensive
  all-target, documentation, and release checks remain mandatory before a tag.
- ANTISEQUENCE is a library crate and therefore does not create executable
  artifacts. It is published before seqproc.
- seqproc uses cargo-dist for tagged executable releases. Release artifacts are
  separate from the crates.io source package.
- Build flags are checked into `.cargo/config.toml` to make release builds
  reproducible. The current aggressive target CPU choices require a portability
  decision before Bioconda publication; see the release blockers below.
- The public documentation deploys from `main`, so `dev` documentation becomes
  public only after the reviewed branch promotion.

#### Relevant changes

- seqproc commits: `631ba81`, `f6fafe2`, `e13f8dd`, `b2a6846`, `348b439`,
  `032a1ee`, `f634449`, `769be4a`.
- ANTISEQUENCE commits: `da29107`, `c799281`, `5efba5a`.
- Release scripts: both repositories' `scripts/bump_and_publish.sh`.
- seqproc release files: `dist-workspace.toml`, `.cargo/config.toml`,
  `.cargo/config-portable.toml`, `.github/build-setup.yml`, and
  `.github/workflows/release.yml`.
- CI: seqproc `.github/workflows/actions.yml` and `comprehensive.yml`;
  ANTISEQUENCE `.github/workflows/ci.yaml`.
- Dependency records: each repository's `DEPENDENCY_AUDIT.md`.
- Documentation: seqproc `README.md`, `website/`, and
  `.github/workflows/docs.yml`.

## Compatibility contract

The intended first-release compatibility story is:

- Headerless EFGDL continues to mean EFGDL 1.
- EFGDL 2 features require the document header.
- EFGDL 1 geometries not using new features should remain byte-identical.
- Legacy flag-only seqproc invocation remains accepted for one documented
  transition cycle.
- `--file1`, `--file2`, positional resources, and the older convenience API
  remain compatibility wrappers.
- Explicit execution and batch overrides reproduce the requested mode rather
  than silently substituting an automatic choice.
- Ordered output is deterministic; unordered execution may change record order
  but not the retained/transformed record multiset.
- Enabling a report does not alter transformations, demultiplexing, or output.
- Features absent from a protocol should add no per-record allocation,
  synchronization, or formatting work where a zero-cost disabled path was
  promised.

The reviewer should test these as contractual claims, not merely inspect that
the old functions still compile.

## Performance work and current evidence

The optimization program used profiles and representative protocol workloads,
then measured each feature or rewrite against its semantic equivalent. The
important conclusions are:

- Worker-local statistics removed shared atomic and histogram-lock contention.
- Short-pattern edit and Hamming paths retained their specialized algorithms
  after correctness fixes.
- Match/filter/terminal projection fusion removed unnecessary intermediate
  reads and graph passes.
- Direct terminal rendering and copy-on-write reads reduced long-read copying.
- Whole-graph worker-local execution improved cheap low-thread workloads, while
  the ordered pipeline remains available where required.
- Scoped metadata has a disabled fast path and is recursively live only where
  needed.
- Dynamic batching reduced admitted memory for long reads and improved the
  measured long-read feature case without perturbing the short-read control.

Recorded feature-specific evidence includes:

- early-filter optimization: approximately `4.843x` in its targeted benchmark;
- dead-metadata elimination: near-zero work versus approximately `80 ms` in the
  targeted control;
- no-op control overhead: approximately `0.2%`;
- dynamic batching on the long-read feature case: approximately `14.1%` faster
  with an estimated `16x` reduction in admitted batch working set;
- unchanged planned batch for the short-read control.

The detailed records live in:

- seqproc `benches/protocol-feature-results-2026-08-21.md`;
- ANTISEQUENCE `docs/benchmarks/metadata-liveness-overhead-2026-08-21.md`;
- ANTISEQUENCE
  `docs/benchmarks/milestone-2-execution-crossover-2026-08-20.md`;
- Criterion targets in both repositories' `benches/` directories.

These are feature and micro/end-to-end controls, not a substitute for the
final release-head regression campaign. The manuscript's full technology
benchmark tables remain tied to the journal benchmark freeze (`cbc156b` and
`f3b0f1e`), not the current `dev` heads. Before release, run a representative
preprint-baseline versus release-head comparison and byte-equivalence checks on
protocols that do not request new behavior.

## Current verification evidence

The most recent milestone records report:

- ANTISEQUENCE: 370 tests passing;
- seqproc library suite: 276 tests passing in the most recently recorded full
  library run;
- seqproc CLI workflow suite: 20 tests passing;
- seqproc typed-error suite: 6 tests passing;
- seqproc benchmark regression suite: 19 tests passing;
- Astro documentation build passing under Node 22.

Coverage includes EFGDL parsing and compilation, paper chemistries, layout
algebra, nested choice/repeat/optional constructs, normalization bounds,
anchor ambiguity policies, named and positional resource equivalence, sharded
input, stdin/stdout, broken pipes, interleaving, three-segment input, typed
errors, optimized/reference matcher comparisons, and execution/batch planning.

These counts are historical evidence from milestone completion, not a claim
that the exact proposed release heads have already cleared every release gate.
The reviewer must rerun the complete locked suites, clippy, documentation, and
packaging against the final commits after any review fixes.

## Important fixes relative to the preprint implementation

The reviewer should pay particular attention to these behavior changes because
they can change retained read sets while correcting the intended semantics:

1. The original short Hamming fast path could bypass a surrounding `FILTER` in
   the SPLiT-seq paired-end configuration. The optimized path now preserves the
   filter and ambiguity contract.
2. Duplicate whitelist lines are normalized and no longer manufacture an
   ambiguous match.
3. Equidistant distinct candidates obey an explicit policy rather than an
   incidental iteration order.
4. Summary-mode execution now uses the same graph/output behavior as normal
   execution.
5. Failures that previously panicked or logged and returned are propagated as
   typed errors with nonzero CLI status.
6. Requested one-thread execution is reported separately from effective
   execution structure. Input parsing or writer stages may exist in pipelined
   modes; the execution report must make the distinction observable.

The paper-analysis repository separately records benchmark configurations,
validation definitions, and the exact journal benchmark commits. It must not be
used to imply that release-head performance was measured where only the frozen
journal implementation was measured.

## Deferred work: explicitly outside the first release

The canonical roadmap is `planning/ROADMAP.md`. The following items remain
incomplete and must not be advertised as release features:

### Milestone 9: full reference interpreter and continuous fuzzing

Present now:

- a low-level reference matcher;
- differential tests for matcher kernels and manually expanded layout forms;
- property-style coverage of bounded layout constructs;
- a detailed implementation plan in
  `planning/MILESTONE_9_REFERENCE_FUZZING.md`.

Deferred:

- an independent full-EFGDL reference interpreter;
- canonical semantic test corpora spanning the full language;
- fuzz targets for parser, compiler, optimized/reference equivalence, FASTQ,
  resources, and graph execution;
- continuous OSS-Fuzz or equivalent long-running fuzz infrastructure;
- minimized-corpus lifecycle and fuzzing release gates.

### Later product milestones

- `seqproc dry-run` with complete resource/input/output and execution-plan
  reporting;
- a versioned protocol registry;
- seqspec import;
- more than three independent input segments;
- unbounded/dynamic indexed captures;
- quality-based ambiguity for edit-distance alignments until gap-quality
  semantics are specified;
- a broader record/lane metadata redesign beyond the scoped metadata needed by
  current bounded graphs.

## Source and design map for reviewers

### seqproc

| Concern | Primary locations |
| --- | --- |
| CLI, compatibility invocation, exit status | `src/bin/bin.rs` |
| Run configuration, execution, reports, planning | `src/execute.rs`, `src/io_config.rs`, `src/lib.rs` |
| Typed public errors | `src/error.rs`, `planning/PANIC_AUDIT.md` |
| EFGDL tokens and syntax | `src/geometry/lexer.rs`, `src/geometry/parser.rs` |
| EFGDL semantic interpretation | `src/geometry/interpret.rs` |
| Graph compilation | `src/geometry/compile/` |
| Named resources and digests | `src/resources.rs` |
| Summary contract | `schemas/` |
| New-language tests | `tests/layout_algebra_tests.rs`, `tests/anchor_set_tests.rs`, `tests/annotation_tests.rs` |
| Compatibility and chemistry tests | `tests/bench_regression.rs`, `tests/paper_chemistry_tests.rs`, `tests/diff_tests.rs` |
| CLI and failures | `tests/cli_workflow_tests.rs`, `tests/error_contract_tests.rs`, `tests/error_handling_tests.rs` |
| Performance tests and records | `benches/` |
| Roadmap and deferred fuzzing | `planning/ROADMAP.md`, `planning/MILESTONE_9_REFERENCE_FUZZING.md` |
| User documentation | `README.md`, `website/src/content/docs/` |
| Release/CI | `scripts/bump_and_publish.sh`, `dist-workspace.toml`, `.cargo/`, `.github/` |

### ANTISEQUENCE

| Concern | Primary locations |
| --- | --- |
| Graph builder, compiled graph, optimizer/planner | `src/graph.rs`, `src/graph/ops.rs` |
| Matcher contract and dispatch | `src/matcher.rs` |
| Search implementations | `src/seed_search.rs`, `src/patterns.rs`, `src/graph/ops/match_any_op.rs` |
| Read storage and metadata | `src/read.rs` |
| FASTQ inputs and shard/interleaved handling | `src/graph/ops/input_fastq_op.rs`, `src/graph/ops/grouped_input_fastq_op.rs` |
| FASTQ output | `src/graph/ops/output_fastq_op.rs` |
| Conditional/nested execution | `src/graph/ops/switch_op.rs`, `try_op.rs`, `try_orientation_op.rs`, `project_op.rs` |
| Typed backend errors | `src/errors.rs` |
| Design documents | `docs/graph-api.md`, `docs/unified-matcher.md`, `docs/graph-optimization-and-planning.md`, `docs/optimizer-passes.md`, `docs/batch-planning.md`, `docs/metadata-liveness-redesign.md` |
| Release/CI | `scripts/bump_and_publish.sh`, `.github/workflows/ci.yaml` |

## Commit landmarks

These are navigation aids, not a substitute for reviewing the full diffs.

### seqproc

| Work | Commit(s) |
| --- | --- |
| Journal CLI/report API | `44c9e5c` |
| Ambiguity policies | `3088bac` |
| Worker-local statistics | `c9d7147` |
| Matcher and projection optimization | `f0b1b1a`, `46d66d6`, `edbf9ef` |
| Corrected SPLiT-seq PE fixtures | `cbc156b` |
| Release/CI/docs groundwork | `631ba81` through `769be4a` |
| EFGDL 2 header and BLAKE3 provenance | `e1bed15` |
| Fixed output and header templates | `576fd54` |
| Complex-protocol semantics | `29b6bc5`, `dd4a3cc` |
| Named resources | `6a43b18` |
| Sharded FASTQ lanes | `271e117` |
| stdin/stdout | `dddea38` |
| Interleaved input | `f9ce343` |
| Three segments | `8f9cd35` |
| Typed errors | `76fa2a8` |
| Optimizer passes | `92b1cda` |
| Dynamic batching | `0a5ff38` |
| Milestone 9 plan only | `cdc2084` |

### ANTISEQUENCE

| Work | Commit(s) |
| --- | --- |
| Pipeline hardening | `a5de904` |
| Ambiguity | `2f93076` |
| Worker-local statistics | `9268150`, `96c9c9f` |
| Edit and terminal optimization | `44de27c`, `45f0bdb`, `f3b0f1e` |
| Graph semantics and immutability | `63b259e`, `c026632` |
| Fixed/projected output and branching | `aac6fad`, `15ca1b6`, `baa991b`, `54d1e75` |
| Unified matcher | `759b239` |
| Position and quality ambiguity | `7b5341d`, `1c6a147`, `bbbb065` |
| Scoped metadata and recursive liveness | `2c99a67`, `1a949cf`, `07a6b22` |
| Shards, streams, interleaving, bounded lanes | `7f5a588`, `5f50af1`, `cf29232`, `33b7df4` |
| Typed errors | `231f958`, `1389ccf` |
| Optimizer passes | `59bfbfe` |
| Dynamic batching | `baa4bc2` |

## Reviewer work plan

### Phase 1: semantic and API review

1. Compare each proposed public type and CLI form with the compatibility
   contract above.
2. Review EFGDL 2 grammar, normalization, bounds, and source-located errors.
3. Verify named-resource path resolution and provenance for file-backed and
   in-memory geometries.
4. Review fixed-sequence quality generation and header-template escaping,
   invalid UTF-8 handling, and injection of captured values.
5. Audit both ambiguity axes, including duplicates, ties, orientation, quality,
   and deterministic ordering.
6. Review graph freezing, effect declarations, optimizer proofs, recursive
   liveness, and nested terminal projection.
7. Confirm every expected user/configuration/I/O failure returns a typed error
   rather than panicking.
8. Decide whether the public Rust APIs are sufficiently coherent to stabilize
   as `0.1.0`.

### Phase 2: correctness and differential tests

1. Run all locked test suites on the exact candidate heads.
2. Compare preprint and release candidates on geometries that do not request
   changed semantics; require byte-identical ordered output or identical
   unordered record multisets.
3. Independently reproduce the corrected short-Hamming/FILTER case.
4. Differentially compare all matcher plans against the simple matcher across
   exact, Hamming, edit, tie, rejection, truncated, reverse-orientation, and
   malformed cases.
5. Compare layout algebra with manually expanded equivalent graphs.
6. Exercise successful first choice, late fallback, all alternatives rejected,
   nested repeat/optional, and large anchor sets.
7. Test single, paired, and three-segment input across plain, gzip, shard lists,
   stdin, stdout, interleaved input, empty shards, unequal shards, and broken
   pipes.
8. Test ordered and unordered execution at multiple worker counts and batch
   overrides for deterministic semantics.

### Phase 3: performance and memory review

1. Re-run the feature-specific benchmarks with CPU affinity and recorded tool,
   compiler, allocator, compression, and cache state.
2. Compare stats off/basic/detailed and verify the disabled path is effectively
   neutral.
3. Compare EFGDL 1 protocols before and after new-language support.
4. Profile low-thread and high-thread cases; check for parser, allocator,
   writer, and compression bottlenecks.
5. Verify dynamic-batch decisions and peak memory for short, paired, anchored,
   whitelist, long-read, gzip, and three-segment cases.
6. Require identical outputs before accepting an optimization result.
7. Record thresholds and retain raw Criterion/end-to-end artifacts with the
   release review.

### Phase 4: packaging and portability review

1. Confirm both crate names and the intended version.
2. Inspect the output of `cargo package --list` for secrets, generated data,
   missing docs, tests, and excess assets.
3. Unpack each `.crate` in a clean temporary directory and run its documented
   checks without relying on the repository checkout.
4. Run `cargo publish --dry-run --locked` for ANTISEQUENCE, then seqproc.
5. Verify that seqproc's packaged manifest resolves the registry ANTISEQUENCE
   release rather than the development Git dependency.
6. Generate API documentation with warnings denied where practical.
7. Run dependency licensing and vulnerability checks on final lock files.
8. Build and smoke-test every cargo-dist target artifact on a compatible host.
9. Resolve the CPU-baseline issue described below before calling artifacts or
   Bioconda packages portable.

## Known release issues and decisions still required

### P0: resolve before publishing either crate

- **CPU portability:** the checked-in seqproc `.cargo/config.toml` and
  `.cargo/config-portable.toml` currently request `x86-64-v3` plus AVX2 on
  x86-64, `neoverse-n1` on Linux ARM64, and `apple-a14` on macOS ARM64.
  ANTISEQUENCE also selects the AVX2 block-aligner feature on x86-64. This may
  be appropriate for optimized release assets but is too restrictive as an
  undocumented universal source-build/Bioconda baseline. Decide and test one
  of these designs:

  - make repository/source builds portable and inject aggressive flags only in
    explicitly labeled cargo-dist jobs;
  - publish separately labeled baseline and optimized artifacts; or
  - document a deliberately restricted CPU floor and ensure Bioconda supports
    it, which is unlikely to be the most user-friendly first release.

  The reviewer must verify not only compiler flags but runtime dispatch and
  dependency feature selection.

- **Exact release heads:** review fixes will change the SHAs in this document.
  Record final reviewed commits and merge `dev` into `main` in both repositories
  before tagging.
- **Version and namespaces:** explicitly approve `0.1.0` (or choose another
  version) and reconfirm both crates.io names immediately before publishing.
- **Publication ordering:** publish and verify ANTISEQUENCE first. Then update
  or confirm seqproc's version requirement and run its dry-run against the
  registry release.
- **Full final-head gates:** run formatting, clippy, all locked/all-target
  tests, docs, vulnerability audit, license review, packaging dry-runs, and
  smoke tests after the last fix.
- **Release metadata:** neither repository currently has a root
  `CHANGELOG.md` or `CITATION.cff`. Add them, and review Cargo package metadata
  such as authors, homepage, documentation URL, repository, keywords, and
  categories before publication.
- **User documentation:** seqproc README/site text still describes tagged
  releases as planned. Replace those instructions only once the release URLs
  and installation commands are real. Ensure ANTISEQUENCE public API docs are
  sufficient for a library release.
- **License contents:** verify package inclusion of primary license files and
  produce a complete third-party license bundle for binary/Bioconda artifacts.

### P1: strongly recommended release-quality checks

- Create a machine-readable release manifest recording repository SHAs,
  versions, Rust/Cargo/cargo-dist versions, lockfile digests, supported targets,
  CPU floors, artifact BLAKE3/SHA-256 checksums, and smoke-test results.
- Add a clean-room example that compiles against the published ANTISEQUENCE API
  and a seqproc smoke protocol that validates and transforms a tiny FASTQ.
- Review whether every serialized summary schema intended to remain public
  needs a migration/compatibility statement; there are currently multiple
  historical schemas in `schemas/`.
- Confirm the docs deployment after `dev` is promoted to `main` and validate
  internal links at `https://combine-lab.github.io/seqproc/`.
- Attach the first-release review report and benchmark artifacts to the GitHub
  release or an immutable archival record.

## Proposed crates.io release procedure

After review approval and all P0 items:

1. Merge the reviewed ANTISEQUENCE `dev` branch to `main` and record the merge
   SHA.
2. Merge the reviewed seqproc `dev` branch to `main` and update its exact
   ANTISEQUENCE release dependency as needed.
3. Add final changelogs and citation/package metadata before the release
   commits are approved.
4. Run the repositories' `scripts/bump_and_publish.sh` in dry-run mode.
5. Publish ANTISEQUENCE; wait until the registry/index and a clean test project
   can resolve it.
6. Re-run seqproc's package and publication dry-runs against that registry
   version.
7. Publish seqproc.
8. Push the reviewed seqproc release tag to trigger cargo-dist.
9. Verify the generated GitHub release, checksums, installers, and every target
   archive before announcing it.

The existing scripts are useful guardrails, but the reviewer should read them
rather than treating them as proof. In particular, confirm their version
editing, clean-tree/origin checks, tag ordering, rollback instructions, and
behavior after a partial publication failure.

## Proposed Bioconda release procedure

No Bioconda recipe currently exists in this ecosystem checkout. Create it only
after the immutable GitHub/source release exists.

The recipe should follow current Bioconda guidance:

1. Use the immutable seqproc release source archive and verify its SHA-256.
2. Set build number `0` for the first recipe of the chosen version.
3. Use `{{ compiler('rust') }}` in build requirements.
4. Bundle dependency licenses using `cargo-bundle-licenses` and include the
   generated license file in `about.license_file`.
5. Prefer the documented locked install form:
   `cargo install -v --locked --no-track --root $PREFIX --path .`.
6. Ensure the recipe neutralizes or replaces repository CPU-specific rustflags
   unless the supported host feature is guaranteed and declared.
7. Test at minimum `seqproc --version`, `seqproc validate` on a packaged tiny
   EFGDL geometry, and an end-to-end tiny FASTQ transformation with an output
   checksum.
8. Run `bioconda-utils lint`, local Docker build/test, and a mulled
   build-and-test before opening the recipe pull request.
9. Review platform coverage explicitly. Bioconda supports Linux x86-64/ARM64
   and macOS x86-64/ARM64, but a recipe or dependency may support a smaller
   subset; do not infer portability from cargo-dist target names.
10. Open the recipe PR, preserve CI artifacts, respond to automated lint/build
    review, and merge only after all supported-platform tests pass.

Current upstream references for this review are:

- [Bioconda recipe guidelines](https://bioconda.github.io/contributor/guidelines.html)
- [Bioconda contribution workflow](https://bioconda.github.io/contributor/workflow.html)
- [Testing Bioconda recipes locally](https://bioconda.github.io/contributor/building-locally.html)
- [Bioconda platform FAQ](https://bioconda.github.io/faqs.html)

The Bioconda package is a seqproc binary package. ANTISEQUENCE is consumed as a
Rust dependency during seqproc's source build and does not need a separate
Bioconda package unless a later use case independently requires one.

## Suggested release acceptance checklist

The reviewer can copy this list into the release issue.

### Scope and provenance

- [ ] The final ANTISEQUENCE and seqproc SHAs are recorded.
- [ ] The journal benchmark SHAs remain separately and accurately identified.
- [ ] Deferred Milestone 9 and later features are not advertised as complete.
- [ ] The release version and crates.io namespaces are confirmed.

### Correctness

- [ ] EFGDL 1 compatibility tests pass byte-for-byte.
- [ ] EFGDL 2 parser/compiler/error tests pass.
- [ ] Matcher optimized/reference differential tests pass.
- [ ] Layout algebra expanded-equivalent tests pass.
- [ ] Every anchor and ambiguity policy has an end-to-end test.
- [ ] Nested liveness, graph optimization, and terminal projection tests pass.
- [ ] Shards, stdin/stdout, interleaved, gzip, and one/two/three-segment tests
      pass.
- [ ] No user-controlled error path panics.
- [ ] Ordered and unordered multi-worker semantics are verified.

### Performance

- [ ] Disabled new features are performance-neutral within a declared noise
      threshold.
- [ ] Statistics-off overhead is negligible.
- [ ] Low-thread and scaling profiles have no unexplained regression.
- [ ] Dynamic batch memory and throughput results are reproduced.
- [ ] All performance comparisons first verify identical outputs.

### Rust packages and artifacts

- [ ] `cargo fmt --check` passes in both repositories.
- [ ] Locked clippy/tests/all-targets/docs pass in both repositories.
- [ ] Final lockfiles pass security and license review.
- [ ] `cargo package --list` and unpacked-crate clean-room tests pass.
- [ ] ANTISEQUENCE `cargo publish --dry-run` passes.
- [ ] Published ANTISEQUENCE resolves in a clean project.
- [ ] seqproc `cargo publish --dry-run` passes against that registry version.
- [ ] CPU baselines and runtime dispatch are approved and documented.
- [ ] All cargo-dist archives/installers/checksums pass smoke tests.
- [ ] `CHANGELOG.md`, `CITATION.cff`, and package metadata are complete.

### Bioconda

- [ ] The recipe uses the immutable release archive and verified checksum.
- [ ] Rust compiler and locked cargo build requirements are correct.
- [ ] Third-party licenses are bundled.
- [ ] Repository CPU-specific flags do not make the package incompatible with
      supported hosts.
- [ ] CLI, validation, and end-to-end FASTQ tests pass locally.
- [ ] `bioconda-utils lint`, Docker build, and mulled test pass.
- [ ] Supported platforms are explicit and tested.

## Final handoff note

The current software is a credible first-release boundary: it retains the
preprint's DSL/backend architecture while correcting an important optimized
filter path and adding a coherent language, I/O, error, optimizer, matcher,
reporting, and release foundation. The appropriate next action is independent
review and stabilization, not another large capability milestone.

Approval should nevertheless be conditional on resolving source/binary CPU
portability, rerunning the full final-head gates, completing release metadata,
and proving the actual crates.io and Bioconda packages in clean environments.
Those are release-engineering obligations, not reasons to fold Milestone 9 into
this release.
