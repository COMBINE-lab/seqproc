# Milestone 9 implementation plan: reference interpreter and continuous fuzzing

## Numbering and relationship to the roadmap

This document decomposes **roadmap Milestone 9**, continuous fuzzing and a
full-language reference interpreter. Roadmap Milestone 8 is the completed
dynamic batch-size planner (`seqproc` `0a5ff38`, ANTISEQUENCE `baa4bc2`).

The work here begins only after Milestone 8 and is a prerequisite for the
complete-invocation dry-run work in roadmap Milestone 10. Each submilestone is
intended to land as an independently reviewed commit on `dev`. The parent
milestone remains incomplete until every completion gate in this document has
passed.

## Objective

Build a deliberately simple, deterministic execution oracle for every
supported EFGDL 1 and EFGDL 2 runtime semantic, then continuously compare that
oracle with the optimized seqproc/ANTISEQUENCE execution path over generated,
adversarial, and permanent regression inputs.

The result must detect disagreements in:

- accepted versus rejected fragments;
- transformed output names, sequences, and quality strings;
- capture values and normalized labels;
- mapping and anchor selection;
- pattern and position ambiguity decisions;
- orientation and ordered-choice decisions;
- rejection reasons; and
- structured errors.

This is correctness infrastructure, not a second performance backend. The
reference path may be slow, but its memory use and work must remain explicitly
bounded for generated inputs.

## Independence boundary

The reference interpreter consumes seqproc's validated, normalized semantic IR
(`CompiledData`) and resolved resources. It may share:

- lexing, parsing, source diagnostics, and semantic validation;
- normalized layout alternatives and the indexed-capture registry;
- typed resource resolution and normalized whitelist/map loading; and
- public ambiguity-policy types.

It must not call or reuse:

- `CompiledData::interpret*`;
- `Graph`, graph operations, graph optimization, or execution planning;
- `execute_stack`, graph expression lowering, or FASTQ projection nodes;
- optimized `MatchAnyOp` dispatch, seeded search, SIMD DP, or rolling-hash
  candidate generation; or
- optimized worker, batching, ordering, compression, or output code.

This boundary tests runtime lowering and execution independently while avoiding
a second parser whose disagreements would be difficult to classify. Parser and
semantic-compiler correctness are covered separately by structural generators,
manual-equivalence tests, and malformed-input fuzz targets.

The simple matcher uses exhaustive candidate enumeration plus plainly written
Hamming and Levenshtein distance. It must not select an optimized backend.

## Stable oracle contract

The initial API lives behind the Cargo feature `reference-interpreter` so it
adds no code or dependencies to ordinary seqproc builds. The intended types
are:

```rust
pub struct ReferenceConfig {
    pub ambiguity_seed: u64,
    pub limits: ReferenceLimits,
}

pub struct ReferenceFastqRecord {
    pub name: Vec<u8>,
    pub sequence: Vec<u8>,
    pub quality: Vec<u8>,
}

pub enum ReferenceOutcome {
    Accepted {
        outputs: Vec<ReferenceFastqRecord>,
        captures: Vec<ReferenceCapture>,
        decisions: Vec<ReferenceDecision>,
    },
    Rejected {
        reason: ReferenceRejection,
        captures: Vec<ReferenceCapture>,
        decisions: Vec<ReferenceDecision>,
    },
}

pub fn interpret_fragment(
    compiled: &CompiledData,
    resources: &ReferenceResources,
    inputs: &[ReferenceFastqRecord],
    config: &ReferenceConfig,
) -> Result<ReferenceOutcome, ReferenceError>;
```

Names are illustrative until submilestone 9.1 freezes the API. Observable
collections must use stable source/read order rather than hash-table order.
Errors and rejection reasons must be typed; display text is not the
differential comparison contract.

## Resource and randomness rules

- Resource files are parsed once before interpreting records.
- Duplicate identical whitelist or mapping rows are normalized once, matching
  the supported runtime contract. Conflicting mappings are errors.
- Reference resources preserve source order for `first`/`accept` semantics.
- Random ambiguity uses the same documented seed and read-identity inputs as
  optimized execution, but the oracle implements the selection independently.
- Quality policies consume the original quality slice corresponding to each
  mismatch. Ties that do not exceed `min_delta` follow the documented no-match
  behavior.
- Every limit failure is a typed `ReferenceError::LimitExceeded`, never a
  timeout, panic, or unbounded allocation.

## Submilestones

### 9.0 Contract freeze and fixture inventory

**Deliverables**

- Add a machine-readable feature/semantics inventory covering every supported
  syntax and runtime behavior.
- Classify existing tests as parser, semantic compiler, optimized runtime,
  manual-equivalence, or low-level matcher differential coverage.
- Freeze normalized comparison rules for labels, captures, decisions,
  rejection, output records, and errors.
- Declare conservative generator limits for source bytes, AST nodes, layout
  alternatives, reads, read length, resource entries, and edit distance.
- Record the current regression and feature-benchmark baselines before the
  reference module is reachable from production builds.

**Gate**

- Every row in the coverage matrix below has an owner submilestone, fixture,
  and proposed oracle assertion.
- The reference feature is disabled by default and ordinary build dependency
  resolution is unchanged.

### 9.1 Reference data model and exhaustive matching kernel

**Deliverables**

- Add the feature-gated public reference types and stable comparison
  normalization.
- Implement allocation-bounded exact, Hamming, and Levenshtein distance.
- Enumerate full, prefix, suffix, search, and bounded-search candidates
  exhaustively.
- Implement distinct-pattern ambiguity policies: `accept`, `first`, `random`,
  `quality`, `no_match`, and `error`.
- Implement equal-position policies: `leftmost`, `rightmost`, `quality`,
  `no_match`, and `error`.
- Report all equal-best candidates and the final selection in reference
  decisions so a disagreement is diagnosable.

**Gate**

- Exhaustive tests enumerate all DNA strings through a declared small bound
  and compare distances with the existing low-level matcher oracle tests.
- Policy tests cover duplicate patterns, distinct equal-best patterns,
  repeated placements, absent quality, low-quality mismatches, `min_delta`
  boundaries, and deterministic random replay.
- No optimized matcher module is imported by the reference implementation.

### 9.2 Linear layouts, captures, transformations, and output construction

**Deliverables**

- Interpret fixed-length, fixed-sequence, ranged, and terminal-unbounded input
  intervals for one to three input lanes.
- Preserve capture occurrence/source-lane metadata and support indexed
  captures.
- Apply `reverse`, `reverse-complement`, truncate, pad, normalize, and remove
  transformations directly to owned reference values and qualities.
- Construct output reads from captured and fixed sequence segments.
- Implement append, prepend, and replace output-header templates.
- Define the reference quality assigned to constructed fixed bases and verify
  qualities remain length-aligned after every transformation.

**Gate**

- Every EFGDL 1 linear fixture and its EFGDL 2 spelling agree with optimized
  execution on accepted output bytes.
- Existing protocols not using newer features remain byte-identical across
  optimized/unoptimized graph execution and the reference path.
- Transformation invariants receive property tests: length agreement,
  reverse-complement involution, truncation/padding bounds, and header
  independence when no header template exists.

### 9.3 Resources, maps, filters, anchors, and ambiguity

**Deliverables**

- Normalize named, positional, literal, and defaulted resource bindings into
  reference whitelist, mapping, and anchor-set values.
- Implement exact, Hamming, and edit-distance mapping plus fallback stacks.
- Implement whitelist filtering without conflating duplicate input rows with
  equal-best distinct barcodes.
- Implement fixed anchors, anchor sets, ranged/unbounded anchor search, and
  `anchor_relative` extraction.
- Apply both pattern and position ambiguity policies to all supported anchor
  scopes.

**Gate**

- End-to-end tests cover every anchor-set pattern-policy × position-policy
  combination, including all error policies.
- Duplicate-resource normalization and ambiguous-query behavior have separate
  fixtures.
- Hamming-expanded and unexpanded resources are treated as different explicit
  inputs; the oracle does not silently expand user resources.
- Resource digests and reference results are deterministic across repeated
  loads.

### 9.4 Layout algebra, orientation, conditional output, and rejection

**Deliverables**

- Evaluate normalized ordered choice with successful-first semantics.
- Evaluate optional and fixed-repeat layouts, including nested combinations
  within the compiler's normalization bound.
- Implement either-orientation retry without mutating the saved forward input.
- Implement match-block/conditional output selection and transformed arm
  stacks.
- Return stable rejection categories for structural mismatch, filter failure,
  ambiguity drop, missing capture, and malformed runtime state.

**Gate**

- Differential tests compare layout algebra with manually expanded equivalent
  geometries.
- Property tests cover nested choice, repeat, optional terms, and normalization
  bounds.
- Tests cover successful first choice, late fallback, all alternatives
  rejected, forward orientation, reverse orientation, and both rejected.
- Multi-lane rejection never mixes fragments or capture state between lanes.

### 9.5 Full-record differential driver and permanent corpus

**Deliverables**

- Add a test driver that runs one normalized fragment through both reference
  and optimized paths and produces a minimal structured difference.
- Compare optimization on/off, statistics off/basic/detailed, supported SIMD
  dispatch, and one/multiple worker execution where applicable.
- Add curated fixtures for all paper protocols and every post-preprint feature.
- Add adversarial fixtures for truncation, malformed quality, repeated anchors,
  indels, ambiguity, reverse orientation, empty reads, maximum supported arity,
  and unequal input structure.
- Store only small, license-compatible regression inputs in the repository.

**Gate**

- The coverage inventory has no unsupported or untested semantic row.
- Every discovered discrepancy is either fixed or checked in as an explicitly
  documented expected semantic distinction.
- Corpus runs are deterministic, bounded, and emit replay commands on failure.

### 9.6 Structure-aware generators and stable fuzz-smoke layer

**Deliverables**

- Generate valid EFGDL 1 and EFGDL 2 semantic programs from typed components,
  not unrestricted string concatenation.
- Generate near-valid programs with one controlled defect and an expected
  diagnostic class.
- Generate matching FASTQ fragments and resources alongside valid programs.
- Add shrinkable property tests for reference-versus-optimized execution.
- Seed generators from a logged value and print a directly replayable failing
  fixture.

**Gate**

- Deterministic smoke suites run on stable Rust in fast CI.
- At least 1,000 generated valid cases and 1,000 near-valid cases run within a
  predeclared CI time budget.
- Shrunk failures retain the semantic feature that caused the disagreement.

### 9.7 `cargo-fuzz` workspace and targets

**Deliverables**

- Add an isolated `fuzz/` Cargo workspace excluded from release packaging.
- Add libFuzzer targets for:
  1. lexer/parser and source diagnostics;
  2. semantic compilation and normalization;
  3. malformed FASTQ ingestion;
  4. exhaustive-versus-optimized matcher behavior;
  5. transformation and terminal projection; and
  6. full reference-versus-optimized fragment execution.
- Provide compact seed corpora drawn from real language constructs and
  permanent regressions.
- Bound target input sizes before parsing or allocating derived structures.
- Write exact local replay instructions.

**Gate**

- Every target completes a fixed-run smoke campaign with no crash, timeout,
  excessive allocation, or mismatch.
- Corpus files are deterministic, reviewable, small, and license-compatible.
- A fuzzer finding cannot mutate ordinary fixtures or write outside its
  artifact directory.

### 9.8 Continuous CI, sanitizers, and triage workflow

**Deliverables**

- Extend fast CI with stable deterministic generator/corpus smoke tests.
- Add a scheduled/manual fuzz workflow using pinned nightly and pinned
  `cargo-fuzz`, with one bounded job per target.
- Upload minimized crash artifacts and the toolchain/commit/replay metadata.
- Add focused Miri coverage for safe reference primitives and sanitizer
  coverage for reachable unsafe/external optimized kernels where supported.
- Document severity, deduplication, minimization, regression-test promotion,
  and responsible-disclosure handling.

**Gate**

- Pull requests receive a bounded signal without making the already-heavy
  comprehensive suite materially slower.
- Scheduled jobs are independently restartable and one target timing out does
  not discard other artifacts.
- A synthetic failing target proves artifact upload and local replay.

### 9.9 Performance-neutral rollout and completion audit

**Deliverables**

- Confirm the default binary does not compile or call the reference module.
- Re-run representative simple, anchored, whitelist, layout-choice, long-read,
  compressed-I/O, ordered-output, and multi-lane performance controls.
- Verify byte identity for protocols that do not use new test-only features.
- Publish an operator/developer guide for running differential tests, stable
  smoke tests, libFuzzer campaigns, replay, and corpus promotion.
- Update the canonical roadmap completion record with exact commits, commands,
  counts, and measured overhead.

**Gate**

- Default-feature executable size and hot-path allocation counts are
  unchanged within measurement precision.
- Median throughput does not regress by more than 2% in any representative
  control; a larger difference blocks completion even if aggregate throughput
  improves.
- `cargo test --locked --workspace --all-targets`, the reference-feature
  suite, docs, the stable fuzz smoke layer, and fixed-run libFuzzer campaigns
  all pass from clean builds.

## Coverage matrix

| Semantic area | Oracle implementation | Differential owner | Fuzz owner |
| --- | --- | --- | --- |
| EFGDL 1 compatibility | 9.2 | 9.2/9.5 | 9.6/9.7 |
| EFGDL 2 headers/resources | 9.3 | 9.3/9.5 | 9.6/9.7 |
| Fixed/ranged/unbounded intervals | 9.2/9.3 | 9.2/9.5 | 9.6/9.7 |
| Layout choice/optional/repeat | 9.4 | 9.4/9.5 | 9.6/9.7 |
| Indexed captures | 9.2 | 9.2/9.5 | 9.6/9.7 |
| Fixed output sequence | 9.2 | 9.2/9.5 | 9.6/9.7 |
| Header templates | 9.2 | 9.2/9.5 | 9.6/9.7 |
| Scalar transformations | 9.2 | 9.2/9.5 | 9.6/9.7 |
| Mapping/filter fallback | 9.3 | 9.3/9.5 | 9.6/9.7 |
| Exact/Hamming/edit matching | 9.1/9.3 | 9.1/9.5 | 9.7 |
| Pattern ambiguity policies | 9.1/9.3 | 9.1/9.3 | 9.6/9.7 |
| Position ambiguity policies | 9.1/9.3 | 9.1/9.3 | 9.6/9.7 |
| Anchors/anchor sets | 9.3 | 9.3/9.5 | 9.6/9.7 |
| Orientation retry | 9.4 | 9.4/9.5 | 9.6/9.7 |
| Conditional output | 9.4 | 9.4/9.5 | 9.6/9.7 |
| One/two/three lanes | 9.2/9.4 | 9.2/9.5 | 9.6/9.7 |
| Rejection/error categories | 9.1–9.4 | 9.5 | 9.6/9.7 |
| FASTQ ingestion failures | N/A | existing typed errors | 9.7 |
| Optimizer/execution modes | independent oracle | 9.5 | 9.7/9.8 |

## Commit and review discipline

Each submilestone follows this sequence:

1. land implementation, tests, and developer documentation together;
2. run its focused gate and record exact commands/results;
3. commit and push the implementation;
4. update this document with the commit and completion evidence in a separate
   traceability commit; and
5. do not mark the next submilestone complete merely because its scaffolding
   exists.

No submilestone may weaken an oracle comparison to make an optimized result
pass. Semantic disagreements must be resolved by examining the language
contract and fixtures, then fixing either the oracle or production path with a
permanent regression test.

## Milestone completion definition

Roadmap Milestone 9 becomes **Complete** only when 9.0 through 9.9 are complete,
the coverage matrix has no unowned supported semantic, both repositories are
clean and pushed on `dev`, and the canonical roadmap contains the final commit,
test, fuzz, compatibility, and performance record.
