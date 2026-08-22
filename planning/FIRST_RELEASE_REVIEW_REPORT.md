# First-release review report: seqproc + ANTISEQUENCE

**Review date:** 2026-08-21
**Reviewed heads:** seqproc `e967c51` (dev), ANTISEQUENCE `baa4bc2` (dev)
**Scope:** independent technical review requested by
`planning/FIRST_RELEASE_REVIEW_HANDOFF.md`, covering semantics, correctness,
error handling, I/O, optimizer soundness, packaging, and portability.
**Method:** full-suite execution on the exact candidate heads; direct code
review of the matcher, EFGDL front-end, graph/optimizer, I/O, CLI, and
release-engineering surfaces; empirical reproduction of I/O defects against a
release build; spot-verification of every headline finding against source.

---

## Status after the 2026-08-21 fix pass

A first round of mechanical fixes was applied on top of the reviewed heads
and committed to both `dev` branches. ANTISEQUENCE: `5b30b5c` (input loss
and bounded-match panics), `053b924` (typed nested-graph errors),
`aa8cb0a` (release branch guard) — pushed; `dev` head is now
`aa8cb0adf7a4f33e817860c912b20d126c19ef03`. seqproc: `1fca512` (anchor_set
positional fix + compile-time distance/range validation), `6b6da77`
(--version and non-UTF-8 arguments), `c49a90e` (release branch guard),
plus a follow-up commit pinning the new ANTISEQUENCE revision.
Verified after the pass: ANTISEQUENCE 372/372 tests
green (370 + 2 new); every seqproc suite green **except `diff_tests`**
(51 compile tests incl. 7 new; 9/9 anchor-set); clippy unchanged (trivial
pre-existing warnings only); `--version`, `explain`, and non-UTF-8 smoke
tests pass.

**Fixed:**
- P0-1a: `#[anchor_set($N)]` positional regression (`parser.rs` keeps the
  `$`; `anchor_set_tests` updated to assert `ResourceRef::Positional(0)`).
- P0-3 (partial): `--version`/`-V` added; non-UTF-8 argv panic
  (`args_os()`); both `unreachable!()` sites now return spanned compile
  errors; `explain` fixed-seq panic now falls back to the normalized-away
  representation.
- P0-2e: compile-time validation added — hamming/edit distance ≤ sequence
  length, map/filter mismatch ≤ max interval length, `[a-b]` requires
  `a ≤ b` (7 new regression tests in `compile_tests.rs`).
- P0-2a: lane-0 EOF on the default paired path now verifies all other lanes
  are exhausted; unequal counts error in both directions (new test).
- P0-2b: empty-shard classification requires `is_file()`; FIFOs and process
  substitutions now stream through the reader path.
- P0-3 (matcher): `ExactBoundedMatch` verification and all three bounded
  seed-prefilter windows clamp instead of panicking on short reads, and the
  prefilter windows now include position `to` (matching inclusive
  verification); new test covers all three bounded types on short reads.
- Nested-op `panic!("Expected some reads!")` → typed `MissingNodeInput`
  (4 files).
- Both publish scripts now refuse to tag from a non-`main` branch
  (dry-run exempt).

**Still open (deliberately left for the maintainer/next agent — these need
a decision or deeper rework):**
- P0-1b: legacy output-arity policy (`diff_tests` still red — decide:
  restore implicit second output or amend contract + tests).
- P0-2c: `GraphNode::finish()` flush hook (output errors on sub-1 MiB
  writes still report exit 0).
- P0-2d: Hamming `k()` min-match/edit-budget conflation (needs matcher
  benchmarking alongside the fix).
- P0-2f: `HammingLookup` non-ACGT fall-through.
- P0-2g: AVX2 seed-searcher candidate loss (fix must land before shipping
  any AVX2 artifact).
- P0-2h: `match N.attr` validation for non-`ori` attributes.
- P0-4a–4e: all optimizer soundness items.
- P0-5: CPU portability (portable repo config + block-aligner SIMD as a
  cargo feature).
- P0-6 remainder: CHANGELOG/CITATION/metadata, package `exclude`,
  MSRV/macOS CI, publish-before-tag ordering decision.
- All P1 items.

### Maintainer resolution after the independent review

The list immediately above records the state at reviewer handoff. The
maintainer pass subsequently addressed the implementation findings rather than
deleting that historical record. The final ANTISEQUENCE review-fix head is
`a10d990ed5c66dd8a4edb61b36dc3cce74543238`; seqproc pins that exact revision.
The seqproc release-candidate SHA will be recorded here after its final gates
and commit.

**P0 resolution:**

| Finding | Resolution |
| --- | --- |
| 1b, legacy output arity | Restored the legacy flag-only prefix contract: an untransformed paired geometry plus only `-o` writes R1. Typed/transformed output lists retain exact-arity validation. A dedicated CLI regression covers the old invocation. |
| 2c, final output errors | Added a fallible recursive `GraphNode::finish()` contract and calls before successful return. Plain, gzip, nested, and parallel writers propagate final flush/footer errors. Constant output paths are materialized even for zero-record runs. `/dev/full` and empty-output regressions cover this boundary. |
| 2d, Hamming planning semantics | Separated Hamming minimum-match semantics from edit budgets, guarded mixed lengths, and verified the selected seed width. Backend/reference tests cover seeded and exhaustive Hamming plans. |
| 2f, `HammingLookup` with non-ACGT input | Lookup misses containing non-ACGT bases fall through to the general Hamming verifier; the fast path is retained for encodable reads. |
| 2g, AVX2 candidate loss | Enumerates every bit in each hit mask and uses unsigned-safe lane handling. Targeted AVX2 differential tests cover same-position multi-pattern hits and lane 7. |
| 2h, `match N.attr` | The compiler accepts only `ori`, requires the referenced read to carry `#[match_ori(either)]`, and validates the metadata index before its `u8` conversion. |
| 4a, mutable statistics after optimization | Removed `CompiledGraph`'s mutable-graph dereference. Statistics are selected on the builder before compilation and immutable on the compiled graph. |
| 4b, terminal-projection ablation | Execution now applies the compiled optimization decision to pipeline configuration. Disabling the pass disables direct terminal rendering; optimized/unoptimized byte-equivalence remains tested. |
| 4c, `SetOp` proof | Reorder/removal eligibility now requires a constant optimized expression and record/lane metadata target. Built-in nodes explicitly attest complete effects; third-party nodes are opaque by default. |
| 4d, nested early termination | `TryOp` and `TryOrientationOp` retain all previously accepted records when a nested graph signals completion. |
| 4e, orientation invalidation | `TryOrientationOp` declares sequence invalidation and participates in recursive liveness. Live forward-coordinate labels across the operation are rejected. |
| 5, CPU portability | Portable SIMD is the default in both crates. ANTISEQUENCE uses SSE2 on x86-64 and NEON on AArch64; AVX2 is a mutually exclusive opt-in feature. Repository and release configs no longer impose `x86-64-v3`, AVX2, or host-specific flags. |
| 6, release mechanics | Added CHANGELOGs, CITATION files, homepage/documentation metadata, package allowlists, locked publish/package commands, MSRV and macOS/ARM CI, and publish-before-tag ordering. Publish scripts require a clean `main` branch outside dry-run mode. |

**P1 resolution:**

| Finding | Resolution |
| --- | --- |
| Broken-pipe detection | Both byte-writer and file-writer error chains recognize `BrokenPipe`; ordinary output failures remain nonzero. Whether stdout EPIPE should be a Unix-success exit is the one remaining policy decision (see below). |
| Empty plain/gzip inputs and shards | Empty streams, zero-byte files, valid empty gzip members, and empty shards are valid zero-record boundaries. Requested constant output files are still created, with valid gzip footers where applicable. |
| Silently ignored `--out2` | Legacy target collection no longer truncates before validation. Supplying `--out2` to a one-output geometry is an arity error and creates no misleading file. |
| Edit-distance quality policy | Pattern-axis `quality` ambiguity is rejected for edit distance at construction, matching the already-rejected position-axis case. |
| Reference tie ordering / stats drift | The reference and optimized paths share deterministic ordering, including prefix/suffix ties. Detailed statistics cache the reference result rather than recomputing it per seed, and statistics-on/off coordinate equality is tested. |
| Backend differential coverage | Added a literal matcher matrix covering every executable reference-comparable backend: direct exact, exact search, Hamming lookup (including `N`), seeded exact/Hamming/edit, exhaustive Hamming, Myers64, and Myers-long. Plan-only families are asserted as such; SIMD-specific cases run under the AVX2 gate. |
| Optimizer hygiene | Semantic no-op elimination respects trace observability; missing-input policy is honored in `try_run_one`; shared nested graphs are reported as explicit barriers; budget reports include `memory_budget_satisfied` and truthful reason codes. |
| Annotation/index validation | Unknown or duplicate annotations are errors, read indices must be contiguous and ordered, and read-level annotations are validated where consumed. |
| EFGDL version/syntax contract | Documentation now states that the established annotations remain headerless-compatible while new EFGDL 2 files should carry the version header. Assignment syntax and the previously requested flat call syntax are both accepted for ambiguity/position policies. |
| Parser robustness | Added a 128-level preflight nesting bound, leading-header diagnostic context, fallible integer and `$N` parsing, and correct lexing of identifiers such as `Anchor1` without changing fixed-sequence tokenization. |
| Detailed-statistics performance cliff | Reference position results are cached per pattern/read candidate context instead of running the full oracle for every seed hit. |
| Fork/recycle allocations | Resetting shared reads now replaces storage without first deep-cloning data that is about to be overwritten; recycled capacity is retained where ownership permits. |
| Schema housekeeping | Only schema 1.12.0 and its history README enter the crate package. CLI tests compile that schema and deeply validate every emitted report; new optimizer/batch fields are required by the schema. |
| Dependency hygiene | Both crates use `thiserror` 2; `nix` is current and Unix-target-scoped; `rapidgzip-core` is optional in ANTISEQUENCE and explicitly enabled by seqproc; lockfiles and complete direct-dependency audits are retained. `regex` is current. The deferred `colored` 2 → 3 migration is UI-only, outside matcher/execution hot paths, and is recorded explicitly in ANTISEQUENCE's audit. |
| Docs/downstream example | ANTISEQUENCE now has a compiled README doc-test plus `examples/downstream_smoke.rs`. Its release gate packages and extracts the crate, creates a temporary downstream project, and runs that example against the extracted package. seqproc pins the same reviewed library revision. |
| Unsafe hygiene | The Hamming tail no longer performs an out-of-bounds over-read. Remaining unsafe `Send` wrappers carry explicit safety justifications. |

Two release-policy choices remain intentionally unresolved because either
behavior can be defensible and affects users rather than internal correctness:

1. **stdout broken pipe:** retain the current nonzero execution failure, or
   treat EPIPE on stdout as successful early-consumer termination. The
   maintainer recommendation is exit 0 only for stdout EPIPE; ENOSPC, quota,
   file-writer failures, and every other output error must remain nonzero.
2. **optimized binary artifacts:** publish portable artifacts only, or add
   separately and unmistakably labeled x86-64-v3/AVX2 artifacts. The
   maintainer recommendation for 0.1.0 is portable artifacts only; users can
   still build explicitly with `--no-default-features --features
   antisequence/simd-avx2`.
3. **first public version:** confirm `0.1.0` for both crates, or select a
   different coordinated version before publication. The maintainer
   recommendation is `0.1.0`: neither name has an earlier public release, the
   APIs are intentionally pre-1.0, and the changelogs are already organized
   around that boundary.

Note: seqproc pins ANTISEQUENCE by git `rev` in `Cargo.toml`; the pin and
lockfile now resolve to `a10d990e`, so seqproc builds include the complete
ANTISEQUENCE review-fix series.

### Post-fix verification evidence

The historical review and its original red-gate table remain below so that the
defects and the evidence that found them are not erased. They no longer
describe the review-fix heads. The maintainer pass completed these gates on
this host:

| Gate | Post-fix result |
| --- | --- |
| ANTISEQUENCE portable library tests | **385/385 pass**, locked, with accelerated gzip enabled |
| ANTISEQUENCE AVX2 library tests | **385/385 pass**, locked, using the mutually exclusive explicit AVX2 feature |
| ANTISEQUENCE clippy/docs/downstream package gate | Warning-denied clippy and docs pass; README doc-test passes; the packaged crate builds and runs from an extracted clean-room downstream project |
| seqproc complete all-target test run | **Pass**: 276 library, 9 anchor-set, 27 annotation, 19 benchmark-regression, 24 CLI, 58 compile, 1 differential, 6 error-contract, 2 error-handling, 12 layout, 12 lexer, 69 paper-chemistry, and 31 parser tests; both benchmark binaries also complete their smoke workloads |
| seqproc explicit AVX2 gate | **276/276 library tests pass** against ANTISEQUENCE `a10d990e` |
| seqproc clippy/docs/MSRV | Warning-denied all-target clippy and docs pass; all-target check passes on Rust 1.88 |
| Formatting and manifests | `cargo fmt --check`, `git diff --check`, locked metadata (including seqproc all-features), `cargo dist plan`, and `cargo dist generate --check` pass |
| Package boundaries | seqproc lists exactly 85 intended files; ANTISEQUENCE lists 67. Planning documents, the Astro site, generated dependencies, and build output do not enter either source package |
| Documentation site | Astro production build passes (20 generated pages plus search index) |

The only package gate that cannot be completed before publication ordering is
seqproc's registry-resolved `cargo publish --dry-run`: its manifest correctly
requires ANTISEQUENCE `0.1.0`, which does not yet exist on crates.io. The
release script therefore publishes and verifies ANTISEQUENCE first, waits for
registry resolution, and only then performs seqproc's full package/publish
dry-run. This is an operational release gate, not an unresolved source defect.

**Current disposition:** the implementation blockers and P1 code-quality
findings from this review are resolved. Do not publish yet: publication still
requires the three product decisions above, promotion of the reviewed `dev`
heads to `main`, registry-order verification, artifact smoke tests, and the
Bioconda release procedure. The original recommendation below is retained as
the reviewer's disposition of the pre-fix heads and is superseded for the
post-fix implementation by this section.

---

## Release recommendation

**Approve after listed fixes — do not tag or publish from the current heads.**

The architecture, the language design, the layout-algebra bounds, the typed
error model, the shard/stream I/O design, and the release tooling are all
genuinely strong, and most of the handoff's claims verify. But the candidate
heads fail their own locked test suite, and the review found multiple
independent silent-data-loss defects reachable from ordinary inputs, several
user-input panics, optimizer proof gaps that falsify the "proof-gated"
guarantee as stated, and an unresolved CPU-portability blocker that rustflag
changes alone cannot fix. None of these findings undermines the design; all of
them are fixable without a new feature family. After the P0 list below is
fixed, the full gates should be rerun and the fixes re-reviewed before
tagging.

---

## Independently verified gates (candidate heads, this host)

| Gate | Result |
| --- | --- |
| ANTISEQUENCE `cargo test --locked` | **370/370 pass** (matches handoff claim) |
| seqproc `cargo test --locked` | **FAILS**: `anchor_set_tests` 2 failures, `diff_tests` 1 failure; all other suites pass (276 lib, 27 annotation, 19 bench-regression, 20 CLI, 44 compile, 6+2 error, 12 layout, 10 lexer, 69 paper-chemistry, 31 parser) |
| `cargo fmt --check`, both repos | clean |
| `cargo clippy --locked --all-targets`, both repos | 2 trivial style warnings each, no errors |
| `cargo doc --no-deps --locked`, both repos | zero warnings (but zero doc-tests in ANTISEQUENCE) |
| ANTISEQUENCE `cargo publish --dry-run --locked` | passes (72 files, 182 KiB compressed) |
| crates.io sparse index, both names | 404 on 2026-08-21 — namespaces still available |
| CLI smoke (validate + paired-end run on test data) | works |
| `seqproc --version` / `-V` | **does not exist** (clap version flag not wired) |

The handoff's historical test counts were accurate when recorded, but the
release-candidate head is red. This alone blocks tagging.

---

## P0 — release blockers

### 1. The locked seqproc suite is red on the candidate head

**1a. `#[anchor_set($N)]` positional references are broken everywhere**
(`anchor_set_tests`: 2 failures). Root cause: the lexer tokenizes `$0` as
`Token::Arg(0)`, and the annotation-argument parser stringifies it as `"0"`
without the `$` (`src/geometry/parser.rs:537`), while named args keep their
`$` (`parser.rs:864`). The `strip_prefix('$')` check added by the
named-resources commit (`6a43b18`, `src/geometry/compile/definitions.rs:344`)
therefore never matches, and `$0` compiles to `ResourceRef::Literal("0")` — a
relative file path named "0". Under EFGDL 2 with in-memory geometry this errors
("requires an explicit geometry base directory"); with file-based geometry it
resolves to `<geometry_dir>/0` and fails as unreadable. The exact syntax the
diagnostic message recommends (`#[anchor_set($0)]`) is nonfunctional. Fix: map
`Token::Arg(n)` to `format!("${n}")` in the annotation-argument parser (or
treat pure-numeric args as positional in `definitions.rs`).

**1b. Legacy flag-only invocation broken for no-transform two-read geometries**
(`diff_tests`: `match.geom`, `bounded_match.geom` exit 2 with "1 primary
output targets were supplied, but the geometry emits 2 reads"). The strict
output-arity validation introduced with the stdin/stdout work (`dddea38`) and
typed errors (`76fa2a8`) rejects the legacy single `-o` invocation for
geometries without a `->` transform, which emit both reads. `diff_tests.rs`
has not changed since before the preprint, so this violates the release's own
compatibility contract ("legacy flag-only invocation remains accepted",
"EFGDL 1 geometries … byte-identical"). Decide: restore an implicit second
output target for the legacy path, or change the contract and the tests
deliberately — but do not ship with the suite red.

### 2. Silent data loss / silently wrong output (worst failure class for a scientific tool)

All of these produce exit 0 with wrong or missing output. Empirically
reproduced by the I/O review against a release build unless noted.

- **2a. Default paired-end path drops trailing R2 records when R1 is
  shorter.** `antisequence/src/graph/ops/input_fastq_op.rs:374-380` treats
  lane-0 EOF as clean end-of-input without checking lanes 1..N. `--read1
  short --read2 long` exits 0 having silently dropped the extra R2 records;
  the reverse direction correctly errors. The sharded
  (`GroupedInputFastqOp`) path gets this right; the *default* single-file
  path does not.
- **2b. FIFO / process-substitution shards silently skipped.**
  `grouped_input_fastq_op.rs:70-81` classifies any `metadata().len() == 0`
  path as an empty shard; FIFOs and `/dev/fd/*` always report size 0, so
  `--read1 <(zcat a.fq.gz),b.fq` drops the whole first shard with exit 0.
  Needs `is_file() && len() == 0` or a first-read probe.
- **2c. Output flush errors are discarded — sub-1 MiB outputs report success
  on write failure.** Final flush happens in `Drop`
  (`output_fastq_op.rs:571-578`, `:1029-1038`) where the error is thrown
  away; `GraphNode` has no fallible finalize hook. `--out1 /dev/full` exits 0
  for any output that fits the 1 MiB `BufWriter`. ENOSPC/quota/NFS errors are
  invisible. Fix requires a `finish() -> Result<()>` on `GraphNode` called
  before teardown — the largest single fix on this list.
- **2d. Hamming seeding conflates min-matching-bases with an edit budget.**
  `antisequence/src/graph.rs:3139-3151`: `MatchType::k()` passes the Hamming
  threshold (minimum matching bases, per `matcher.rs:360-362` and the kernel
  at `match_any_op.rs:2028`) into `k_from_edits` as if it were an edit
  budget. Consequences: lenient thresholds violate the pigeonhole guarantee
  (seeded kernel silently misses matches that the exhaustive kernel and
  reference matcher find — kernel choice changes results, refuting the
  semantics-above-kernels claim); typical whitelist thresholds degrade to
  comparing every read against the entire whitelist while `MatcherPlan`
  reports `SeededCandidates`. Additionally `k_from_edits` computes `len - e`
  unguarded — mixed-length pattern sets can underflow: panic in debug, wrap
  in release, and a wrapped `k` yields an empty seed index and **zero matches
  for every read, silently**.
- **2e. Distance-vs-length validation is absent in the EFGDL compiler, and
  release builds silently drop 100% of reads.** Five unguarded subtractions:
  `seqproc/src/geometry/interpret.rs:441, 985, 1003, 1143, 1158, 824, 865`.
  `1{hamming(f[ACG], 10)r:}` or `#[hamming(10)]` on a 3-mer wraps
  `seq.len() - n` to ~`usize::MAX` in release (no overflow-checks) and
  becomes an unsatisfiable min-match threshold: every read rejected, exit 0.
  Debug builds panic instead. Same class: inverted ranges `b[12-8]`
  (`interpret.rs:1064`, `compile/utils.rs:394`, with `ilog2()` panicking on
  0). Fix: compile-time validation that distance ≤ pattern/interval length
  and `a <= b` in ranges.
- **2f. `HammingLookup` false-negatives on non-ACGT bases.** The short-Hamming
  fast path enumerates only ACGT substitutions and does not fall through to
  the general path on miss (`match_any_op.rs:116-152`, `:1238`, `:1266`), so
  a read with one `N` in a barcode that the exhaustive kernel accepts at
  distance 1 is rejected. `N` bases are ubiquitous in real data; this is the
  most likely-in-practice backend-dependent divergence. Documented in a
  commit rather than fixed; either fall through on miss or enumerate `N`.
- **2g. AVX2 seed searcher drops candidates** (`seed_search.rs:118, 126-131`,
  active only in AVX2 builds — which the checked-in configs make the
  default): `trailing_zeros()` takes only the lowest set bit when two pattern
  k-mers hit the same position (other pattern never verified → lost matches
  and under-reported ambiguity), and `_mm256_cmpgt_epi8` is a signed compare
  so lane index 7 makes the byte negative and the candidate is dropped.
  Build flags change match results.
- **2h. `match N.attr` on a non-`ori` attribute:** compile-time guard covers
  only `ori` (`compile/mod.rs:525-544`); other attributes either panic on
  the read-index conversion (`interpret.rs:332-335`) or, for valid indices,
  silently route no records so output passes through untransformed.

### 3. Panics reachable from ordinary user input

The "user input must never panic" contract has counterexamples in both crates:

- `seqproc` CLI: non-UTF-8 argument (a legal Linux filename) panics at
  `src/bin/bin.rs:543` via `std::env::args()`; exit 101. One-line fix
  (`args_os()` + lossy join).
- EFGDL compiler: `1{self}` hits `unreachable!()`
  (`compile/reads.rs:452-454`); `foo = <bar[2]>` hits `unreachable!()`
  (`compile/definitions.rs:248-250`); `seqproc explain` panics on geometries
  with fixed-sequence labels in the transform
  (`compile/mod.rs:318-322` — `run` and `validate` succeed, `explain`
  panics: a real validate/explain-vs-execute divergence); oversized integer
  literals panic in the lexer via `unwrapped()` (`lexer.rs:207, 250-252`).
- ANTISEQUENCE matcher, on short/truncated reads (routine FASTQ input):
  `ExactBoundedMatch` inclusive-slice panic when the read is shorter than the
  bounded window (`match_any_op.rs:1443-1445` — the Hamming/Edit bounded arms
  use the correct exclusive form); the bounded-scope seed prefilter panics
  when `from > to` after clamping (`:1286-1289`) and uses an
  exclusive window while verification is inclusive (a seeded-vs-exhaustive
  divergence at the boundary); the reference-matcher path panics via
  `.expect("compiled matcher bounds are valid")` (`:676-677`) whenever
  detailed statistics or a non-default position policy are on and a read is
  truncated.
- Nested graph ops `panic!("Expected some reads!")` instead of returning the
  typed `MissingNodeInput` error (`while_op.rs:27`, `select_op.rs:27`,
  `fork_op.rs:21`, `try_orientation_op.rs:77`).

`planning/PANIC_AUDIT.md` has drifted from the tree — its own grep command
surfaces the sites above. Regenerate after fixes.

### 4. Optimizer soundness gaps (falsify "proof-gated" as stated)

- **4a. Statistics level is mutable post-compile through `Deref`**
  (`graph.rs:1662-1667` takes `&self` on atomics, reachable on
  `CompiledGraph` via `graph.rs:1228-1234`), while `dead_label_elimination`
  and `early_selective_filter_placement` gate their proofs on
  `statistics_level() == Off` **at compile time** (`graph.rs:1470, 1506`).
  Setting a level after compilation re-enables observation of an
  already-rewritten stream. Snapshot the level at compile or reject
  post-compile mutation when those passes fired.
- **4b. Terminal projection "fusion" is a counter, not a pass, and cannot be
  ablated.** `graph.rs:1547-1559` only counts candidates; the transformation
  happens at execution time gated on `PipelineConfig::direct_output_rendering`
  (default `true`, `graph.rs:2101-2107`) and never consults the compiled
  optimization config. `compile_with(disabled())` therefore does **not** give
  the byte-level differential oracle the docs claim, and `try_run_planned`
  computes a `direct_output_rendering` decision it never applies
  (`graph.rs:1324-1328` vs `:1353-1362`).
- **4c. `SetOp::reorder_safe` is a behavioral probe, not a declared-effects
  proof** (`set_op.rs:38-42`): it evaluates the expression on an empty read
  and discards the `expr.optimize()` read-independence result — exactly the
  insufficient condition `docs/graph-api.md` warns against. Third-party
  `ExprNode` impls can be wrongly reordered/removed.
- **4d. `TryOp`/`TryOrientationOp` drop already-accepted reads on early
  termination** (`try_op.rs:59-71`, `try_orientation_op.rs:102-106`): any
  `done` signal mid-batch returns only the last read's output. Silent loss.
- **4e. `TryOrientationOp` declares `PreserveAll` while reverse-complementing
  a lane in place** (`try_orientation_op.rs:132-139`, default effect from
  `graph.rs:794-801`): labels established before it silently refer to
  forward-orientation coordinates on rc'd bytes, and liveness/filter-placement
  treat them as intact. Should declare a lane invalidation effect. The general
  footgun: overriding `produced_names()` silently opts a node out of the
  opaque barrier.

### 5. CPU portability — unresolved, and deeper than rustflags

- `.cargo/config.toml` and `.cargo/config-portable.toml` are **byte-identical**
  and both request `x86-64-v3` + AVX2 (plus `neoverse-n1` / `apple-a14` on
  ARM). The "portable" file is not portable; repository/source builds — and a
  Bioconda build that does not neutralize the config — inherit a post-2013
  CPU floor. (Note `cargo install --path .` honors the package's
  `.cargo/config.toml`, and it ships in the crate.)
- **Neutralizing rustflags is not sufficient.** ANTISEQUENCE hard-wires
  block-aligner's `simd_avx2` feature for all x86-64
  (`antisequence/Cargo.toml:50-51`), and block-aligner 0.5 has **no runtime
  CPU detection** — its AVX2 intrinsics run unconditionally when the
  edit-distance DP path executes. A plain x86-64 build still SIGILLs on
  pre-AVX2 hosts on that path. `seed_search.rs` uses compile-time
  `cfg(target_feature = "avx2")` gates (fine — scalar fallback), but the
  block-aligner feature choice must become a cargo feature (e.g.
  `simd_sse2` default on x86-64, `simd_avx2` opt-in for optimized artifacts)
  before Bioconda or generic source builds are safe.
- Recommended design (matches the handoff's first option): portable defaults
  in the repo (no target-cpu pinning, SSE2 block-aligner), aggressive flags +
  `simd_avx2` injected only in the labeled cargo-dist jobs via
  `.github/build-setup.yml`, and the CPU floor of each artifact documented in
  the release notes. Note also `-C target-feature=+avx2` is redundant with
  `x86-64-v3`.
- Fixing 2g (the AVX2 seed-searcher bugs) is prerequisite to shipping any
  AVX2-optimized artifact.

### 6. Release mechanics

- **`seqproc --version` does not exist** — the handoff's own Bioconda test
  plan requires it. Add `#[command(version)]`.
- No `CHANGELOG.md` or `CITATION.cff` in either repo (handoff acknowledges);
  Cargo metadata lacks `homepage`/`documentation`.
- `bump_and_publish.sh` is well built (dry-run, rollback, clean-tree and
  origin checks, sensible ANTISEQUENCE-first ordering) but does **not check
  the current branch** — it will happily tag and push from `dev` — and it
  pushes the tag (triggering cargo-dist) *before* `cargo publish`, so a
  publish failure leaves a public tag with binaries but no crate. Add a
  `main`-branch guard and document the ordering trade-off. `cargo publish`
  is also invoked without `--locked`.
- The seqproc crate package includes internal `planning/` documents (this
  handoff, the panic audit, roadmap, Milestone 9 plan) and benchmark-result
  markdown. Not secrets, but add an `exclude` before publishing.
- CI runs only `ubuntu-latest` on `stable`: no MSRV (1.88) job, no
  macOS/ARM test job, despite shipping those release targets and declaring
  that floor. Add at least an MSRV check and one macOS runner before
  claiming the support envelope.

---

## P1 — strongly recommended before or immediately after tagging

- **Broken-pipe exit contract:** `--out1 - | head` exits 1 (no panic, no
  hang — verified at 8 threads), but it is indistinguishable from ENOSPC
  under `set -o pipefail`. Decide (0 or a dedicated code), document, test.
  Detection is also fragile: EPIPE is recognized only via the `BytesIo`
  variant and only inside `WorkerFailures` (`execute.rs:62-97`); the
  file-writer path misclassifies it.
- **Empty-gzip shard** (valid 20-byte gzip of an empty file) aborts the run
  with an opaque `GraphExecution` error while 0-byte plain shards are
  tolerated (`grouped_input_fastq_op.rs:70-81`); reclassify as FastqInput and
  handle consistently. A single empty FASTQ input also cannot be processed
  at all.
- **`--out2` silently ignored for 1-read geometries**
  (`execute.rs:116-128` truncates before the arity check) — a user typo
  produces no second file with exit 0.
- **Pattern-axis quality ambiguity is not rejected for edit distance**
  (`match_any_op.rs:1699-1712`) — asymmetric with the position axis, which
  correctly rejects it; per-read it either errors or silently applies
  Hamming-style scoring. Reject at construction.
- **Two reference oracles disagree on tie-breaking** (`matcher.rs:323-325`
  leftmost-start vs `match_any_op.rs:2819-2821` earliest-end), and the
  matcher-oracle path is swapped in when detailed statistics are enabled —
  so enabling statistics can change emitted coordinates. Pick one ordering,
  make `reference_match` normative, add a stats-on/stats-off equality test.
  Similarly `edit_prefix`/`edit_suffix` keep the longest equal-best window
  while the reference keeps the shortest (`:2408`, `:2473`).
- **The differential-testing claim must be restated.** ANTISEQUENCE has
  excellent brute-force oracles for the *edit kernels*
  (`match_any_op.rs:2854-3009`, 5k–100k randomized cases), but there is no
  test comparing matcher *plans/backends* against `reference_match`, and six
  backends are never asserted in `MatcherPlan` tests. `MatcherPlan` is
  currently descriptive, not prescriptive (reported backend can differ from
  the executed kernel). Either build the plan-level differential matrix (it
  would have caught 2d, 2f, and the tie-break divergences) or scope the
  handoff/docs language to kernel-level oracles.
- **Optimizer hygiene:** `semantic_noop_elimination` is not gated on trace
  observability (`graph.rs:1443`, unlike the other three passes);
  `try_run_one` ignores `MissingInputPolicy` (`graph.rs:2898-2905`);
  optimizer output differs depending on whether the caller retained the
  `Arc` from `Graph::add` (`Arc::get_mut` at `graph.rs:1424`) — surprising
  and worth a compile-time warning; the batch planner's 256 MiB budget is
  advisory when fixed buffers exceed it but still reports
  `"bounded_peak_memory"`.
- **Annotation-name allowlist:** unknown annotations are silently discarded
  (`definitions.rs:364`), so `#[match_orientation(either)]` (typo for
  `match_ori`) silently disables orientation matching. ~15 lines to error on
  unknown names; prevents silently-wrong scientific output. Related:
  read-level annotations are keyed by declared index but consumed by
  position (`compile/mod.rs:393` vs `interpret.rs:200-210`) with no
  uniqueness/contiguity validation.
- **EFGDL 2 gating accuracy:** `anchor_set`, `ambig_policy`, and
  `position_policy` work in headerless (EFGDL 1) documents — the repo's own
  tests rely on it. Either gate them behind the header or amend the
  version-contract language. Also `#[ambig_policy(...)]` call syntax is
  deliberately rejected (assignment-only), which contradicts the handoff's
  "both syntaxes" phrasing — the one-canonical-form design is better; fix
  the document.
- **Parser robustness:** no recursion-depth limit in the two recursive
  parsers or `layout.rs::expand` (deeply nested input stack-overflows with
  no diagnostic — the width bounds do not cap depth); malformed `header {`
  blocks fall back to "definition named header" with a misleading
  diagnostic; identifiers starting with an uppercase nucleotide letter
  mis-lex (`Anchor1` → `A`, `nchor1` — pre-existing).
- **Performance cliff:** `reference_position_candidate` runs the full
  exhaustive oracle once per seed hit per pattern whenever detailed stats or
  a non-default position policy is active (`match_any_op.rs:1394`). Hoist or
  cache before recommending `detailed` stats for long reads.
- **Fork/recycle allocations:** `reset_fastq_entry` deep-clones shared
  storage it is about to overwrite (`read.rs:179, 203-206`), and
  `share_in_place` permanently forfeits recycled capacity — the
  copy-on-write design is correct (no aliasing bug found) but leaves easy
  wins.
- **Schema housekeeping:** 13 summary schemas (1.0.0–1.12.0) ship in a
  first release that has never had a public consumer; collapse to 1.12.0
  (+ README history) or move historical schemas out of the package. The CLI
  workflow test validates only top-level key presence; the review's deep
  validation against 1.12.0 passed across 6 configurations, so wire a real
  JSON-Schema validator into the tests to keep it that way.
- Unify `thiserror` major versions (seqproc 2.x, ANTISEQUENCE 1.x); update
  `nix` 0.26 (and note seqproc is Unix-only at compile time due to `mkfifo` —
  document or cfg-gate); reconsider `rapidgzip-core` as a mandatory
  dependency (handoff calls it opt-in — the *path* is opt-in, the dependency
  is not); `colored`/`regex` in a hot-path library are dependency-hygiene
  questions for later.
- Zero doc-tests in ANTISEQUENCE and no clean-room downstream example —
  the handoff's own P1 items; both worthwhile before a library release.
- Unsafe hygiene: the page-guarded 8-byte over-read in `hamming`
  (`match_any_op.rs:2035-2053`) is practically safe but formally UB and will
  trip Miri/ASan; two `unsafe impl Send` wrappers lack safety comments.

---

## Compatibility-contract assessment (tested as contractual claims)

| Contract claim | Verdict |
| --- | --- |
| Headerless EFGDL = EFGDL 1; v2 features gated by header | **Partial** — layout/output/resources gated; annotations not (P1) |
| EFGDL 1 geometries byte-identical | **Fails today** via 1b (legacy arity break); otherwise held on the 69 paper-chemistry tests |
| Legacy flag-only invocation accepted | **Fails** for no-transform 2-read geometries (1b); otherwise works with deprecation warning |
| `--file1/--file2`, positional resources, convenience API wrappers | Hold — except `#[anchor_set($N)]` (1a) and `--out2` truncation (P1) |
| Explicit execution/batch overrides reproduced | Holds (verified in planner review; overrides are exact) |
| Ordered deterministic / unordered multiset-equal | Holds under stress (queue-capacity 1, 8 threads, ring wraparound) |
| Reports don't alter behavior | Holds — byte-identical output with `--summary`; deep schema validation clean |
| Zero-cost disabled paths | Holds structurally (worker-local stats, disabled metadata path); re-measure at final heads per handoff plan |

## Handoff-document accuracy corrections

The handoff is unusually honest, but four claims need restating before it is
used as release documentation: (1) "differential tests compare all matcher
plans against the simple matcher" — only kernel-level oracles exist, no
plan/backend-level differential matrix; (2) "duplicate whitelist entries are
normalized" — true in seqproc's loader (`processors/mod.rs:234-254`, tested),
not in ANTISEQUENCE, whose docs delegate to the caller — fine, but say so;
(3) "`rapidgzip-core` is an opt-in backend rather than a mandatory
dependency" — the code path is opt-in, the dependency is unconditional;
(4) "annotations accept both assignment and call syntax" — each annotation
accepts exactly one canonical form by design.

## What is solid (verified, no action needed)

- The layout-algebra cardinality bounds are the best-engineered part of the
  new language: `checked_mul` before allocation, per-alternative segment
  checks, no exponential intermediates; the review could not defeat them.
- The SPLiT-seq short-Hamming/FILTER fix is real, correctly implemented on
  both hit and miss paths, and regression-tested across the 8/9 bp backend
  boundary (`lib.rs:1788`).
- Pattern-axis tie-breaking is deterministic — every resolution path sorts
  before selecting; no hash-iteration-order dependence was found anywhere.
- Recursive metadata liveness is correct across all seven nested-graph node
  types, including the `while` fixed-point; no wrongly-recycled label could
  be constructed. Copy-on-write read storage has no aliasing bug.
- The typed error model is genuine: `Send + Sync` verified by compile probe,
  worker errors preserved across scoped threads, exit codes consistent
  (2 config / 3 FASTQ / 1 execution), and truncated-FASTQ diagnostics are
  excellent.
- Shard lockstep, lazy open (one fd per lane, 5 MB RSS on 60 shards),
  interleaved arity 1–3, mixed plain/gzip shards, invalid stream-combination
  rejection, and named-resource resolution/digesting all verified exactly as
  documented.
- Validation/explain/run share one compile path (`compile_geom_typed`
  funnel) — the one divergence found is the `explain` normalizer panic (3).
- Batch planning is genuinely deterministic (pure function, no sampling, no
  clock); release scripts and CI are thoughtfully constructed; the summary
  emitted by the code validates cleanly against schema 1.12.0.

## Suggested fix order

1. One-liners and small validations first: `--version`, `args_os()`,
   `$N` annotation fix (1a), `unreachable!()` → errors, distance/range
   validation (2e), branch guard in the publish script.
2. The legacy-arity decision (1b) — a policy call, then either code or
   test/contract updates.
3. The I/O trio (2a, 2b, 2c) — 2c needs the `GraphNode::finish` hook.
4. Matcher: `k()` semantics + underflow (2d), bounded-scope panics,
   `HammingLookup` N-handling (2f), AVX2 seed-searcher fixes (2g).
5. Optimizer: stats-level snapshot (4a), direct-rendering config plumbing
   (4b), `SetOp` proof (4c), `TryOp` early-termination (4d), orientation
   invalidation effect (4e).
6. Portability: make repo config portable, feature-gate block-aligner SIMD,
   inject aggressive flags only in cargo-dist jobs; then re-verify 2g-fixed
   AVX2 artifacts.
7. Rerun every gate on the final heads; regenerate `PANIC_AUDIT.md`; then
   proceed with the handoff's publication procedure (which is otherwise
   sound: ANTISEQUENCE first, registry-resolution check, tag last).

The deferral of Milestone 9 remains the right call — but items 2d/2f/2g are
precisely the class of defect the deferred plan-level differential harness
would catch, so a minimal backend-vs-reference differential matrix (even a
few hundred randomized cases per backend) is the highest-value test to add
with the fixes rather than waiting for the full Milestone 9 program.
