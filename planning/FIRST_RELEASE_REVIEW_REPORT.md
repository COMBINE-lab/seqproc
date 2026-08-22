# First-release review report: seqproc + ANTISEQUENCE

**Original review date:** 2026-08-21
**Originally reviewed heads:** seqproc `e967c51` (dev), ANTISEQUENCE
`baa4bc2` (dev)
**Final re-review packet prepared:** 2026-08-22
**Final implementation boundary:** seqproc
`ef4dee75b79efd836e8d63e11cac02ac9193da69`, including an exact dependency
pin to ANTISEQUENCE `773e1ced7bae6170b1358a2d2198f1c152109624`.
This boundary includes the final demultiplexed-output provenance correction;
subsequent review-report updates do not change executable behavior, manifests,
or lockfiles.
**Scope:** independent technical review requested by
`planning/FIRST_RELEASE_REVIEW_HANDOFF.md`, covering semantics, correctness,
error handling, I/O, optimizer soundness, packaging, and portability.
**Method:** full-suite execution on the exact candidate heads; direct code
review of the matcher, EFGDL front-end, graph/optimizer, I/O, CLI, and
release-engineering surfaces; empirical reproduction of I/O defects against a
release build; spot-verification of every headline finding against source.

---

## Instructions for the final reviewer

Review the implementation boundary above rather than rerunning the audit on
the original candidate heads. The original findings are intentionally retained
below as an audit trail; their wording and line references describe the
pre-fix tree. The final reviewer should verify the resolution tables and focus
on regression risk introduced by the fixes, especially matcher backend
equivalence, graph-finalization error propagation, optimizer effect proofs,
and the architecture-specific release contract.

The implementation work is concentrated in these commits:

| Repository | Concern-fix commits | Release/architecture commits |
| --- | --- | --- |
| seqproc | `1fca512`, `6b6da77`, `c49a90e`, `1925425`, final blocker repair `2c26678`, second-pass closure `9ed2d87`, demux-provenance correction `ef4dee7` | `1141162`, final ANTISEQUENCE pin/pre-tag cleanup `770499d`, cargo-dist-compatible baseline selection `388777b`, exact final pin `4566953` |
| ANTISEQUENCE | `5b30b5c`, `053b924`, `5672682`, `46065aa`, `5468b3f`, `0c16ed2`, sticky finalization `3617472`, hot-path refinement `773e1ce` | `477462b`, `1d1c10d`, final review cleanup `272ba77` |

The previously hosted fast paths were green before the final local blocker
closure; the final `dev` pushes will be checked again by the next review pass:

- ANTISEQUENCE Fast CI run
  [32552480133](https://github.com/COMBINE-lab/ANTISEQUENCE/actions/runs/32552480133)
  on `b5fecee`.
- seqproc Fast CI run
  [32587272717](https://github.com/COMBINE-lab/seqproc/actions/runs/32587272717)
  on pre-correction head `e3eafa0`; the final `ef4dee7` correction is covered
  by the local exact-boundary gates below and awaits the final pushed-head CI.

### Decision ownership

The following were product or language decisions, not conclusions that the
fixing maintainer made silently:

| Decision | Owner and outcome |
| --- | --- |
| Legacy one-output flag behavior | The reviewer correctly identified a policy fork. The user had already required one release cycle of backward compatibility, so the implementation preserves the historical legacy-prefix behavior: an untransformed paired geometry with only `-o` writes R1. Typed output lists and transformed geometries remain exact-arity. |
| EFGDL version marker | Deferred to the user during feature design. The user selected a structured header containing `efgdl = 2`, rather than a standalone magic marker. Headerless files retain EFGDL 1 compatibility. |
| Ambiguity-policy syntax | Deferred to the user. Both assignment syntax (`#[ambig_policy = accept]`) and flat call syntax (`#[ambig_policy(accept)]`) are accepted and normalized to the same representation. Argument-bearing policies remain call-like. |
| Geometry/resource digest | Deferred to the user. BLAKE3 was selected over SHA-256 for the versioned geometry/resource digest because it provides the required stable content identity with lower hashing overhead. |
| Library versus executable SIMD contract | Explicitly deferred after the reviewer showed that unconditional block-aligner AVX2 was unsafe for a general library. The user approved a library-safe ANTISEQUENCE default (SSE2 on x86_64, NEON on aarch64) and an explicitly tuned seqproc executable contract. |
| Official x86 binary floor | Explicitly deferred to the user. The user selected x86-64-v3/AVX2 as the floor for official seqproc x86 artifacts. These artifacts are labeled and guarded at startup; they are not described as portable. |
| Lower-floor and multiversion strategy | The user approved retaining a separately buildable/tested baseline artifact rather than adding runtime dispatch now. `cargo-multivers` is deferred until measured v3-versus-v4 gains justify added release complexity. |
| Fixed release CPU targets | Presented as part of the architecture plan and approved by the user: x86-64-v3 for x86 Linux/macOS, Neoverse N1 for Linux aarch64, and Apple A14 for macOS aarch64. Local repository builds use `target-cpu=native`. |

Correctness repairs—EOF lockstep, fallible graph finalization, matcher
semantics, AVX2 candidate enumeration, optimizer effects, panic removal, and
typed validation—were not treated as policy choices. They were resolved in
favor of preserving documented semantics and making failures explicit.

Two publication policies were **not** inferred from the architecture approval
and remain for the user to confirm before publishing:

1. whether stdout `EPIPE` should be Unix-success while every file/output error
   remains nonzero; and
2. whether the coordinated first public version should be `0.1.0`.

These do not leave a silent-correctness path open, but they should remain on
the release checklist rather than being decided by a reviewer or release
script.

---

## Independent re-review that produced the final blocker list (2026-08-22, boundary seqproc `4e0de31` / ANTISEQUENCE `b5fecee`)

### Verdict

**The maintainer pass is substantially verified and of high quality — approve
after the short fix list below.** Every gate in the "Post-fix verification
evidence" table reproduced on this host exactly as claimed: ANTISEQUENCE
386/386 baseline and 388/388 release-SIMD (warning-denied), doc-test, 67
package files; seqproc full all-target suite green including `diff_tests`,
87 package files, `verify_simd_equivalence.sh` 9/9 byte-identical,
`cargo dist generate --check`/`plan` clean, `/dev/full` now exits 1,
publish-before-tag ordering fixed, MSRV/macOS CI present, CHANGELOG/CITATION
in place, crates.io namespaces still free. The library/executable SIMD split
is provably correct (block-aligner `simd_avx2` is absent from the default
dependency graph; `compile_error!` enforces backend exclusivity), the
CPUID predicate itself is complete including OSXSAVE/XGETBV, and an A/B
against the pre-fix build showed the matcher fixes are a large net
performance win (a 400k-barcode × 20k-read workload: pre-fix >600 s,
post-fix 51 s, single debug thread). One resolution-table claim per
subsystem did not fully survive scrutiny; none undermines the architecture.

### Remaining release blockers (all small relative to the completed work)

> **Update 2026-08-22:** items 6 and 7 below were subsequently fixed by the
> reviewer in ANTISEQUENCE `cd4a9e4` — see "Resolution of blockers 6 and 7"
> after this list. Items 1–5 and 8–9 remain open.

1. **Legacy output permissiveness leaks into `seqproc run`.**
   `execute.rs:682-695` gates on `config.outputs.is_none()`, which both the
   legacy flag-only path and the modern `run` subcommand satisfy
   (`bin.rs:349-389` converge). Empirically: `seqproc run --geom paired
   --out1 X` exits 0 writing only R1, and a paired geometry with **no**
   output flags exits 0 writing nothing (pre-fix: arity error). The report's
   claim (line: "restored the legacy **flag-only** prefix contract") does
   not match the implementation. Fix: thread an invoked-via-legacy flag into
   `RunConfig` and require at least one non-`Discard` primary target;
   `diff_tests` (flag-only) still passes.
2. **`--unassigned2` is still silently ignored** —
   `unassigned_targets_from_legacy` (`execute.rs:148-160`) truncates with
   `.take(input_arity)` before validation, the exact pattern fixed for
   `--out2`. Users believing rejected mates are retained lose them silently.
3. **`finish()` runs on failed/invalid runs and materializes outputs.**
   Empirically confirmed: a run failing on malformed input truncates a
   pre-existing output file (pre-fix it was untouched); at the library
   level, re-running a graph after a config-validation failure appends a
   second deflate stream after a finalized gzip footer (silent corruption).
   Fix: validate pipeline config before `finish_after`, materialize constant
   outputs only on success, and make `finish()` terminal-state-guarded.
4. **The CPUID guard cannot report its own failure in v3 dist artifacts**
   (verified by disassembly of `target/dist/seqproc`): the error-path code
   is compiled with VEX instructions, so a pre-AVX host SIGILLs before the
   message prints, and the XGETBV check is dead code exactly where it
   matters. Cheapest complete fix on Linux: add
   `-C link-arg=-Wl,-z,x86-64-v3` to the x86 entries of
   `.cargo/config-release.toml` (glibc ≥ 2.33 refuses to load with a clean
   "CPU ISA level" message); otherwise make the failure path allocation-free.
   The guard *is* effective for plain `cargo install` builds.
5. **`target-cpu=native` default vs. hardcoded v3 guard.** A binary built
   with the checked-in `native` config on a newer node (e.g., AVX-512
   build/login node) passes the v3 guard on an older compute node and can
   SIGILL in real work — the classic HPC build-node/compute-node split.
   Fix: drive the guard from the `SEQPROC_TARGET_FEATURES` string `build.rs`
   already exports (~60 lines; also fixes `cpu_floor` under-reporting), or
   drop `native` from the checked-in config.
6. **Per-read `vec![None; n_patterns]` in the general match loop**
   (`match_any_op.rs:1455-1459`). Not a regression — the pre-fix exhaustive
   behavior was far worse — but measured at ~2.4 ms/read at 400k patterns
   (linear in whitelist size; a 10x v3 6.8M whitelist extrapolates to
   ~40 ms/read/thread). The oracle cache should be allocated lazily, only
   when detailed statistics or a non-default position policy makes the
   reference oracle active. Trivial fix; whitelists are seqproc's headline
   use case.
7. **Quadratic tie-break on long reads:** `edit_search_myers`/`edit_search_dp`
   now run a full reverse DP per equal-best end (`match_any_op.rs:2366-2374`,
   `:2461-2469`); unbounded `best_ends` on repetitive 10 kb reads is
   O(n²m). Compute the leftmost start for the smallest end first, or bound
   the candidate set. Matters for the long-read SPLiT-seq protocol.
8. **Mixed-length Hamming seeding still unsound:** `get_searcher` derives
   `k` from the *minimum* literal length (`match_any_op.rs:654-659`), but
   for `Hamming(Count(c))` longer patterns have larger mismatch budgets, so
   the chosen k can exceed their safe bound → silent false negatives. Claimed
   fixed; not fixed. Guard: fall back to exhaustive when literal lengths
   differ under Hamming, or take the per-pattern minimum safe k.
9. **Provenance smoke on a real dist artifact before tagging:** CI asserts
   the v3 contract via `RUSTFLAGS` env, not via `config-release.toml` +
   `--target` as cargo-dist builds do. Run one `dist build --artifacts=local`
   and check `--version --verbose` reports `compiler CPU target: x86-64-v3`;
   add the assertion to the release workflow.

### Resolution of blockers 6 and 7 (2026-08-22 reviewer fix, ANTISEQUENCE `cd4a9e4`)

Blockers 6 and 7 were fixed by the reviewer in ANTISEQUENCE commit
`cd4a9e4` ("Keep positional-oracle caching and tie resolution off the hot
path"); seqproc's pin advances accordingly. For the next agent, the precise
mechanics of both defects and their fixes:

**Blocker 6 — what the O(pattern-count) work was.** The matching itself was
never a list scan: pattern sets are indexed once at graph construction (a
k-mer seed index, or a precomputed neighbor table for short Hamming
patterns), and per-read work is proportional to seed hits. The regression
was bookkeeping: the detailed-statistics fix memoizes the *reference
position oracle* (the exhaustive positional matcher consulted only when
detailed statistics or a non-default position policy is active) once per
(read, pattern). That cache was a dense `vec![None; patterns.len()]` —
one 40-byte slot per whitelist entry — allocated and zero-filled for
**every read**, even on the default path where the oracle immediately
returns `None`. At 400k patterns that is ~16 MB of memset per read
(~2.4 ms measured); a 6.8M-barcode 10x v3 whitelist would be ~270 MB per
read. Fix: the cache is now a sparse `FxHashMap` keyed by the pattern
indices the seed index actually surfaces for that read. An empty map
allocates nothing, so the default path pays zero; the active path pays
O(#seed-hit patterns) instead of O(#whitelist). Measured on the
400k × 20k-read workload: 51 s → 7.7 s single-threaded debug (the
remainder is fixed startup cost — the 2k-read case dropped from 7.6 s to
3.3 s), outputs byte-identical.

**Blocker 7 — what the quadratic behavior was.** The deterministic
tie-ordering fix made the edit kernels collect **every** end position
achieving the best score, then run a *reverse DP over the entire prefix*
`text[..end]` for each one to recover that end's leftmost start, then take
the lexicographic (start, end) minimum. On a repetitive read, a pattern
can have O(n) equal-best ends and each reverse DP is O(end·m) →
O(n²·m) per pattern per read. Two exact bounds remove this:
(1) *window bound* — an alignment within k edits of an m-length pattern
spans at most m+k text characters (each edit changes length by ≤1), so a
placement ending at `end` cannot start before `end − (m+k)`; the reverse
DP now scans only that suffix, O((m+k)·m) per end, exact because any
excluded longer window necessarily exceeds the edit budget; (2) *early
termination* — ends arrive in ascending order, and once the current best
start `s*` satisfies `end − (m+k) ≥ s*` no later end can produce a smaller
start (and a start tie loses on the larger end), so the scan stops after
at most ~2(m+k) ends past the first winner. Worst case drops from
O(n²·m) to O((m+k)²·m), independent of read length. Applied identically
to `edit_search_myers` (≤64 bp), `edit_search_dp` (>64 bp fallback), and
`edit_search_long_myers` (>64 bp Myers, which also no longer materializes
every sub-threshold hit). Exposure note: the CLI anchor pipeline routes
through windowed/precomputed kernels first, so the reachable worst case
from seqproc geometries is primarily >64 bp patterns on long reads
(`edit_search_long_myers`) and runtime expression-derived patterns; the
library API reaches all three directly.

Verification: ANTISEQUENCE 386/386 baseline and 388/388 release-SIMD
suites pass (these include the randomized `edit_search` vs
`reference_edit_search` oracle matrices that pin exact
(score, start, end) triples), clippy clean, and both benchmark workloads
above produce byte-identical outputs against the pre-fix binary.

### Maintainer resolution of the final re-review blockers (2026-08-22)

All nine blockers above are now resolved. The ANTISEQUENCE corrections span
`5468b3f`, `0c16ed2`, and the subsequent lifecycle closure through `773e1ce`;
seqproc pins the exact full revision
`773e1ced7bae6170b1358a2d2198f1c152109624`. The final seqproc
implementation commit and packaged-artifact evidence are recorded in the
verification table below.

| Blocker | Resolution and design decision | Regression/evidence |
| --- | --- | --- |
| 1, modern output permissiveness | Added an explicit `OutputCompatibility` policy. `RunConfig` and `seqproc run` default to exact arity; only the deprecated flag-only invocation selects `LegacyPrefix`. Non-demultiplexed runs must have at least one non-`Discard` primary target. | CLI tests prove modern paired `--out1` fails, an all-discard topology fails, and the legacy flag-only paired prefix remains byte-correct. |
| 2, ignored `--unassigned2` | Removed pre-validation truncation from legacy unassigned-target construction. A hole is represented as `Discard`, so `--unassigned2` on a one-lane geometry becomes a two-target topology and is rejected. | CLI regression checks the typed arity error, that the primary output is never created, and that a pre-existing `unassigned2` file is untouched. |
| 3, unsafe graph finalization | Added an atomic graph lifecycle (`ready` → `running` → `finishing` → `finished`/`finish failed`). Numeric pipeline configuration is validated before execution is claimed. Success recursively finalizes all nodes; failure uses `finish_existing` to flush only writers already opened, aggregates execution and finalization errors, and never materializes an untouched output. Finalization failure is sticky and execution after completion remains typed. | ANTISEQUENCE tests cover retry after invalid pre-execution configuration, rejection of a second execution, zero-record constant output, final-flush failure, repeated finish after failure, and simultaneous parse-plus-flush failure. seqproc protects untouched sentinels and returns nonzero for final output failures. |
| 4, pre-main v3 failure | Linux x86_64 release binaries now emit the standard `GNU_PROPERTY_X86_ISA_1_NEEDED` note directly, with the cumulative baseline/v2/v3 mask `0x7`. This is loader-visible before globally optimized Rust code can execute and works with the older GNU linker on the release host, which silently ignored `-z x86-64-v3`. macOS retains the documented best-effort raw guard because Mach-O has no equivalent contract in this release. | The reusable release gate extracts the actual cargo-dist archive and parses the ELF64 `.note.gnu.property` structure with `scripts/verify_x86_64_v3_elf.py`; the final local artifact reports mask `0x7`. |
| 5, native under-reporting | `cpu_floor` and the raw CPUID/XGETBV guard are now driven by `SEQPROC_TARGET_FEATURES`, not a hardcoded v3 list. The checker covers every feature enabled on the supported native build host, including AVX-512 register-state requirements. `target-cpu=native` provenance is explicitly labeled non-portable. | Native `--version --verbose` reports the build-host contract and exact feature list; host compatibility is tested. Fixed artifacts still report the reproducible v3 contract. |
| 6, oracle-cache hot path | See the correction to the reviewer fix below. The cache is now both sparse **and optional**: an operation-level predicate is evaluated once per batch/read path, and the default leftmost/statistics-off path performs no oracle calls, inserts, hashing, or allocation. | The complete ANTISEQUENCE suite passes; the active cache remains keyed only by seed-hit pattern indices. |
| 7, edit tie resolution | The reviewer's window and monotone-stop proof is retained with overflow-safe `saturating_add`. Explicit 10 kb repetitive-read regressions cover both the ≤64 bp Myers path and the >64 bp DP/long-Myers paths and require the exact leftmost `(score,start,end)` result. | At final ANTISEQUENCE head `773e1ce`, 397/397 baseline and 401/401 release-SIMD/accelerated all-target tests pass, including the existing 100,000-case short oracle and 2,000-case long-Myers differential matrices. |
| 8, mixed-length Hamming seeding | The shared seed length is now the minimum of each literal's independently safe `MatchType::k(pattern.len())`. If any valid pattern has no guaranteed exact seed, dispatch falls back to exhaustive verification. This retains seeding whenever the common bound is sound rather than disabling all mixed-length acceleration. | End-to-end regression uses three 8 bp literals plus a 16 bp literal under `HammingSearch(Count(8))`; the accepted query has no old 8 bp seed and reproduces the former false negative. |
| 9, real cargo-dist provenance | Added a cargo-dist `global-artifacts-jobs` gate (generated, not hand-edited CI). It downloads the actual x86_64 Linux archive before hosting, runs the packaged executable, checks compiler target/floor/SIMD provenance, and parses the ELF note. | `.github/workflows/verify-release-provenance.yml`, generated `.github/workflows/release.yml`, and the exact final-head local cargo-dist artifact smoke described below. |

#### Evaluation of the reviewer's two fixes

**Sparse positional-oracle cache (blocker 6): sound idea, incomplete hot-path
implementation.** Replacing the dense O(pattern-count) vector with an
`FxHashMap` was the correct data structure for the active oracle: its size is
now proportional to seed-hit patterns. However, the submitted implementation
still called `reference_position_candidate` and inserted `pattern_idx -> None`
for every surfaced pattern on the default path. The empty map itself allocated
nothing, but the first insertion allocated and every candidate paid hashing;
the review text's “default path pays zero” claim therefore did not hold. The
final implementation constructs `Option<FxHashMap<...>>` only when detailed
statistics or a non-leftmost position policy can activate the exhaustive
oracle. This is both sound and the lowest-overhead design within the existing
matcher: the common path is a predictable `None` branch with no per-candidate
oracle/cache work, while the uncommon path retains sparse memoization.

**Bounded reverse-DP tie resolution (blocker 7): sound and appropriate.** The
proof is exact: an alignment of an m-base pattern at edit distance d spans at
most m+d text bases, and candidate ends are monotone. Once a later end's
earliest possible start is not smaller than the current best start, it cannot
win the lexicographic `(start,end)` order. The inclusive-end long-Myers path
correctly uses `end + 1`; all window additions are now saturating. The current
long path can still retain O(n) equal-best end indices before traceback, so a
streaming/end-compressed representation is a possible future memory
optimization, but the pathological O(n²m) time behavior is gone and the
remaining linear memory is bounded by read length. No semantic shortcut or
approximate tie behavior was introduced.

### Final comprehensive re-review, second pass (2026-08-22, reviewer; boundary seqproc `2c26678`+working tree / ANTISEQUENCE `272ba77`)

**Verdict: the closure work is genuine and mostly excellent — but four new
findings must be fixed before tagging, one of which will fail the release
CI itself.**

Gates reproduced on this host: ANTISEQUENCE `272ba77` fmt/clippy clean,
**397/397** baseline and **399/399** release-SIMD library tests
(warning-denied, accelerated-gzip enabled); full seqproc suite green
against the final pin (28 CLI tests, all other suites unchanged);
`--version --verbose` on a native build now reports the truthful
non-portable floor and exact feature list; a `config-release`-equivalent
build carries the `GNU_PROPERTY_X86_ISA_1_NEEDED` mask `0x7` note and
passes `scripts/verify_x86_64_v3_elf.py`; a failed run on malformed input
no longer truncates a pre-existing output file (re-verified with the
original sentinel reproduction).

**Confirmed genuinely closed** (verified in code and/or by execution):
modern `run` exact-arity + all-discard rejection with the legacy prefix
preserved for flag-only invocation (the `--file1` alias hole is closed
structurally by `args_conflicts_with_subcommands` — load-bearing, deserves
a comment); `--unassigned2` over-arity rejection with pre-existing files
untouched; the feature-string-driven CPUID/XGETBV guard (all 44 features
of this native host mapped, correct AVX-512 XCR0 state bits, leaf
fallbacks fail closed); the ELF ISA note — engineered *better* than the
report describes (a hand-written `global_asm!` note rather than a link
flag, correct type/mask, OR-merged by GNU ld); the release-provenance
workflow (really downloads the artifact, runs it, greps the contract,
parses the note, and gates `host` on success); CI restoration of
paper-chemistry/diff/clippy to PR triggers; the graph lifecycle
compare-exchange (two concurrent runs cannot both win; config validated
before execution claimed; `GraphAlreadyFinished` typed); mixed-length
Hamming seeding (per-literal `k`, min over literals — the pigeonhole proof
holds, uniform-length sets keep their width, and the regression test
reproduces the old false negative); position-quality construction
rejection with the predicate deduplicated structurally; the
`TryOrientationOp` rework (namespaced `__as_to_{id}` key, typed errors,
orientation semantics byte-identical, plus a latent `done`-branch bug
fixed beyond what was asked); `k()` totality for alignment thresholds
including NaN/∞; `MatcherPlan` re-documented as advisory; the vacuous
non-ACGT test replaced with a real coordinate-asserting one; no weakened
or deleted tests in either repo.

**New release blockers found in this pass (4):**

1. **The release tag build will fail.** `scripts/verify_simd_equivalence.sh`
   drives every fixture through `seqproc run … --out1` — now Strict — and
   three fixtures (`match`, `edit_match`, `bounded_match`) have no `->`
   transform and emit two reads. Reproduced: exit 2, and the script is
   `set -euo pipefail`. This is the byte-identity gate in the
   comprehensive `baseline` job that runs on tag push. Fix: give those
   three invocations an `--out2`, or drive them through the legacy
   flag-only form as `diff_tests` does — then **run the script**, not just
   `cargo test`.
2. **Demultiplexed runs silently discard `--out1/--out2/--out3`** (exit 0,
   file never created): both new topology gates short-circuit on
   `config.demux.is_none()`, and the demux branch never consumes
   `primary_targets`. Reproduced. This is the same silent-output-loss
   class the commit closes for non-demux runs. Fix: error (or loudly warn)
   when primary outputs are combined with `--demux-map`.
3. **`finish()` errors are not sticky, and the failure path discards
   finalization errors** (ANTISEQUENCE `graph.rs`): the state swaps to
   `FINISHED` before the work, so a second `finish()` returns `Ok(())`
   even when the first failed — a false "data is on disk" signal — and a
   failed run drops footer/flush errors entirely (Drop swallows them),
   so an ENOSPC root cause behind a broken-pipe cancellation is
   undiagnosable and a truncated-but-valid `.gz` can pass silently.
   Fix: make the finish error sticky and aggregate finalization errors on
   the failure path as the pre-lifecycle code did. Related documentation
   gap: "failed execution does not finalize outputs" holds only for
   failures before the first output write; once streaming has begun,
   `File::create` has already truncated the destination — inherent to
   streaming, but the docs/CHANGELOG should say so.
4. **Unconditional `compare_exchange` on the `run_one`/`try_run_one` hot
   path**: nested `TryOp`/`WhileOp` graphs execute it once per read; a
   failing CAS still takes the cacheline exclusively, so N pipeline
   workers sharing one nested graph (exactly seqproc's unassigned-read
   routing) ping-pong a previously read-only line. Fix is mechanical — a
   `Relaxed` load fast path that only CASes when `READY` — then re-run the
   benchmark suite.

**Should fix before tagging (cheap):** under-arity unassigned targets
still silently drop mates (`--unassigned1` alone on a paired geometry;
mirror of the fixed case); both CHANGELOGs omit the user-visible breaking
changes (strict `run` arity, zero-output rejection, unassigned arity, the
graph lifecycle/`GraphAlreadyFinished`/retry-impossible trade-off, the
`MatchAnyOp::new` panic-surface widening, the mixed-length seeding
correctness fix) and ANTISEQUENCE's rewrote its already-folded 0.1.0
section while leaving `[Unreleased]` empty; the resolution table's test
counts (390/392) are stale — actual 395/397 (397/399 with
accelerated-gzip); qualify the README `cargo install --locked seqproc`
line (until ANTISEQUENCE 0.1.0 is published from `272ba77`, a registry
build resolves a different backend than this review validated); add a
seqproc-level `compile_error!` for `baseline-simd`+`release-simd` (the
inherited message tells the portable-build user to pick `release-simd`);
`build.rs` drops `-C target-feature=+X` flags from provenance and the
guard (fidelity regression vs. the old `CARGO_CFG_TARGET_FEATURE` path);
comment that `linker = "gcc"` is load-bearing (rust-lld silently discards
the ISA note — verified: the lld-linked default build lacks the note, the
gcc-linked build carries it); a `const` assertion for the 24-byte
`InlineString` boundary of `__as_to_{id}`.

**Acceptable to defer with issues:** finalization-error reporting nuances
beyond blocker 3 (NEW-1), no-retry-after-transient-failure (documented
trade-off, seqproc unaffected), `run_one`-then-`run` ordering strictness,
position-quality silently inert for Exact/Hamming at non-Search scopes
(asymmetric with the new Edit rejection), the CPUID guard's fail-open on
unknown future features (add a test that every `SEQPROC_TARGET_FEATURES`
entry is in the covered set), `read_rest_avx2` page-trick over-read
(masked-correct, blocks Miri/ASAN), provenance verification for
aarch64/macOS artifacts, `output_compatibility` in a future schema 1.14,
and the unpoliced `pub interpret_with_unassigned` entry point.

**Bottom line:** after blockers 1–4 and the cheap items above, this
codebase is ready for the release procedure (merge to `main`, final-head
gates, ANTISEQUENCE publish, registry-resolved seqproc dry-run, publish,
tag — the provenance gate will then verify the artifact automatically) and
the two open user decisions (stdout `EPIPE` policy; confirm `0.1.0`)
remain the only policy items.

### Maintainer resolution of the second-pass review (2026-08-22)

All four new blockers and every cheap pre-tag item in the second-pass review
have been addressed. The reviewer implemented blockers 3 and 4 in
ANTISEQUENCE `3617472`; the maintainer audited those changes and refined the
hot-path load in `773e1ce`. seqproc pins the latter exact revision. The
historical findings above remain verbatim so the next reviewer can reproduce
the audit trail rather than seeing only the resolved state.

| Second-pass blocker | Resolution | Direct evidence |
| --- | --- | --- |
| 1, strict `run` broke the SIMD release gate | `verify_simd_equivalence.sh` now supplies `--out2` for the three paired, untransformed fixtures (`match`, `edit_match`, and `bounded_match`). It compares both lanes between the generic and tuned binaries while retaining the historical lane-1 expected-output comparison. This uses the modern strict interface rather than hiding the incompatibility behind the deprecated CLI. | The complete script was executed on the final source: **9/9 fixtures byte-identical** between SSE2 and x86-64-v3/AVX2; both lanes are checked for all three paired fixtures. |
| 2, demultiplexing silently ignored fixed outputs | Output topology validation now rejects any non-`Discard` primary target combined with demultiplexing using typed `PrimaryOutputsWithDemultiplexing`, before any primary file or demultiplexing directory is created. Explicit all-`Discard` primary topology remains valid because it requests no lost destination. | CLI regression checks exit status 2, the typed diagnostic, and absence of both the requested primary file and demultiplexing directory. |
| 3, non-sticky and discarded finalization errors | See the detailed reviewer-fix evaluation below. ANTISEQUENCE now has explicit `FINISHING`/`FINISH_FAILED` states and a recursive `finish_existing` hook. Failed execution finalizes only already-open writers, aggregates execution and finalization errors, and never creates an untouched output or materializes a constant output. A repeated `finish()` after failure returns typed `GraphFinalizationFailed`. | Tests force a sticky flush failure and a simultaneous parse-plus-flush failure; all nested control-flow operations forward `finish_existing`. Final ANTISEQUENCE counts are **397/397** baseline and **401/401** release-SIMD/accelerated all-target tests. |
| 4, lifecycle CAS on every nested-read call | `begin_or_continue_execution` first polls the state with a read-only `Relaxed` load and returns immediately for `RUNNING`; only a possible `READY` transition attempts the `AcqRel` CAS. The CAS remains the synchronization/ownership operation, while ordinary per-read polling no longer requests cacheline exclusivity. | Source audit confirms there is no CAS on the ordinary running path. Both complete ANTISEQUENCE feature matrices pass, and unchanged seqproc protocols remain covered by the full byte-equivalence and regression suites. |

#### Evaluation of the reviewer's blocker-3 and blocker-4 implementation

**Sticky finalization and failure aggregation: sound and appropriately
conservative.** A single generic sticky error is preferable to retaining an
arbitrary I/O error object indefinitely inside every graph: the first caller
receives the complete underlying failure(s), while subsequent callers receive
the typed fact that finalization failed and can never mistake the graph for
durable output. Separating `finish_existing` from `finish` is necessary and
well designed. A default no-op prevents failure handling from creating new
side effects; file/demultiplexing, borrowed-writer, JSON, and every nested
control-flow node explicitly flush or forward only state that already exists.
The failed-run path preserves both root causes through aggregation. The
documentation also now states the unavoidable streaming boundary: an output
that was opened before a later failure may already have been truncated or
partially written, whereas an untouched output is not created merely by
failure cleanup. No simpler implementation provides all three properties
(sticky status, no new failure-path outputs, and complete I/O diagnostics).

**Read-before-CAS lifecycle polling: sound, with one maintainer refinement.**
The reviewer's structure correctly removes exclusive cacheline traffic from
the common `GRAPH_RUNNING` path. The initial implementation used an `Acquire`
load; the maintainer changed this to `Relaxed`. Polling does not publish or
consume graph data, and atomic coherence plus the subsequent `AcqRel` CAS
provides the required state-transition synchronization. This is the smallest
hot-path operation consistent with the lifecycle. A stale `READY` observation
only causes the CAS to fail and observe the current state; same-thread calls
after `finish` remain ordered by atomic coherence. Concurrent execution and
finalization are not a supported public protocol and are independently
protected by the finishing state.

#### Resolution of the second-pass cheap items

| Finding | Disposition |
| --- | --- |
| Under-arity unassigned output silently dropped a mate | Nonempty unassigned topology must now equal input arity exactly. A typed `UnassignedArityMismatch` is raised before any output is created; an intentionally discarded lane is expressed explicitly as `Discard` (`/dev/null` in the CLI). Paired-input regression covers the former `--unassigned1`-only loss. |
| User-visible changes missing from changelogs | Both 0.1.0 changelogs now record strict modern output topology, exact unassigned topology, demux/fixed-output conflict, graph single-use lifecycle, sticky/aggregated finalization, `MatchAnyOp::new`'s fallible-construction boundary, and mixed-length Hamming correctness. These entries stay in the coordinated first-release section because no earlier crate version was published. |
| README advertised an unavailable registry build | README and website now qualify `cargo install --locked seqproc`: it applies after the coordinated exact seqproc/ANTISEQUENCE 0.1.0 publication; until then, users are directed to the reviewed checkout. |
| Requested seqproc-local baseline/release feature conflict | A local `baseline-simd` passthrough was implemented and tested, then deliberately removed. cargo-dist performs all-feature workspace discovery before its configured build; the passthrough activates ANTISEQUENCE's mutually exclusive same-version SSE2/AVX2 dependency aliases and makes `cargo metadata` fail before release compilation. The supported selector remains `--no-default-features --features antisequence/baseline-simd`; ANTISEQUENCE's own `compile_error!` is still the authoritative exclusivity guard. This is an empirical cargo-dist compatibility decision, not an omitted check. |
| Explicit `-C target-feature=+X,-Y` missing from provenance/guard | `build.rs` now parses both split (`-C`, `target-feature=...`) and joined (`-Ctarget-feature=...`) encoded rustflags, applies ordered additions/removals to the CPU preset, and sorts/deduplicates the exact resulting feature contract used by verbose provenance and the runtime guard. |
| GNU linker requirement unexplained | `.cargo/config-release.toml` documents that GNU ld is load-bearing because it preserves/merges the hand-written GNU ISA-needed note; the reviewed rust-lld silently drops it. |
| Internal orientation key near `InlineString` limit | ANTISEQUENCE now has a compile-time size assertion for the 24-byte `__as_to_{id}` boundary. |
| `args_conflicts_with_subcommands` looked incidental | The parser declaration now documents this as a load-bearing compatibility boundary preventing legacy aliases such as `--file1` from bypassing strict subcommand semantics. |
| Stale counts and docs | Counts, exact pins, command examples, output-topology docs, failure/streaming caveats, and installation instructions are refreshed in this report, both changelogs, README, graph API documentation, and the Astro site. |

The explicitly deferrable follow-ups in the second-pass review remain
deferrals, not hidden release claims: richer retained finalization diagnostics,
retry semantics after transient failure, `run_one`/`run` ordering policy,
position-quality behavior outside search scope, fail-closed handling for
future unknown CPU features, sanitizer-friendly AVX2 tail loading, non-Linux
artifact provenance, a future report-schema compatibility field, and tighter
visibility around the low-level interpreter entry point. None recreates a
silent data-loss or incorrect-match path in the supported seqproc workflow.

### Final release-readiness assessment (2026-08-22, reviewer)

**Verdict: READY FOR RELEASE**, pending only the two user-owned policy
decisions and the mechanical release procedure. No open finding in either
repository blocks tagging.

**Release-candidate boundary.** ANTISEQUENCE `dev` at
`773e1ced7bae6170b1358a2d2198f1c152109624` (the reviewer's sticky-finalization
and read-only-polling implementation `3617472`, plus the maintainer's
`Relaxed`-load refinement). seqproc `dev` at
`ef4dee75b79efd836e8d63e11cac02ac9193da69`, including `9ed2d87`
(output-topology and provenance closures), `4566953` (the exact dependency
pin), and the final effective demultiplexing-topology correction.

**How the second-pass blockers closed.** The work was completed
cooperatively: the maintainer fixed blockers 1 (SIMD-gate arity) and 2
(demux/primary-output conflict) plus every cheap item; the reviewer fixed
blockers 3 (sticky, aggregated finalization with the `finish_existing`
failure-path hook) and 4 (read-only lifecycle polling) in ANTISEQUENCE, which
the maintainer then audited and refined. Each side's changes were
independently reviewed by the other — no fix in this pass shipped unreviewed.

**Verification on the final boundary (this host).**

| Gate | Result |
| --- | --- |
| ANTISEQUENCE lib tests, baseline + accelerated-gzip, `-D warnings` | 399/399 (includes new sticky-finish and failed-run-aggregation regressions) |
| ANTISEQUENCE lib tests, release-SIMD + accelerated-gzip, `-D warnings` | 401/401 |
| ANTISEQUENCE fmt / clippy all-targets | clean / zero errors |
| seqproc full `--no-fail-fast` suite at the final pin | all suites green, including 31/31 CLI-workflow tests (demux-conflict, exact-unassigned-arity, and effective demux-topology regressions included) |
| `scripts/verify_simd_equivalence.sh`, executed end to end | 9/9 fixtures byte-identical between SSE2 and x86-64-v3/AVX2, both lanes compared for the paired untransformed fixtures |
| `cargo fmt --check`, `cargo dist plan`, `cargo dist generate --check` | clean / exit 0 / exit 0 |
| Warning-denied seqproc all-target clippy | clean after all second-pass source and dependency-pin changes |
| Explicit target-feature provenance probe | split `-C target-cpu=x86-64-v3` plus joined `-Ctarget-feature=+aes,-sse4.2` records `aes`, removes `sse4.2`, and retains the CPU preset's remaining features |
| Final cargo-dist x86_64 Linux archive | rebuilt from `ef4dee7` under the fixed release config; SHA-256 `d4def76f7069aed934665322d169ab7e654e8c7a2fc3c50895781f64691af023`; packaged binary reports x86-64-v3/AVX2 provenance |
| ELF ISA note in that packaged binary | present, mask `0x7`, `verify_x86_64_v3_elf.py` passes; absent (as expected and documented) under rust-lld local builds |
| Failed-run output preservation | re-verified: pre-existing outputs untouched by input-stage failures; finalization errors aggregated, sticky on repeat |
| crates.io namespaces | both still 404/unclaimed |

**What remains before the tag — all procedural, none code:**

1. **Two user decisions** (unchanged): stdout `EPIPE` exit-code policy
   (recommendation stands: exit 0 for stdout EPIPE only), and confirmation of
   `0.1.0` as the coordinated first version (recommendation: yes).
2. **The release procedure itself**, in the order the scripts already
   encode: merge both reviewed `dev` branches to `main`; rerun the locked
   gates on the merge commits; publish ANTISEQUENCE and wait for registry
   resolution; run seqproc's registry-resolved `cargo publish --dry-run`
   (the one gate that structurally cannot run earlier); publish seqproc; push
   the tag; let the generated release workflow build artifacts and the
   provenance job verify the x86_64 Linux archive; smoke-test the remaining
   artifacts; then the Bioconda recipe per the handoff (portable build:
   `--no-default-features --features antisequence/baseline-simd` with
   neutralized rustflags).
3. **Known accepted limitations to carry into the release notes**, already
   documented in changelogs/docs: a graph executes at most once (no
   retry-after-failure); an output the run began writing is truncated by that
   streaming even if the run later fails; the ELF loader enforcement of the
   v3 floor requires glibc ≥ 2.33 (older loaders fall back to the best-effort
   CPUID guard); artifact provenance is machine-verified for x86_64 Linux
   only; and the deferred-follow-up list above.

**Residual-risk judgement.** The remaining risk surface is the one any first
release carries: paths only real-world data exercises (exotic FASTQ corner
cases, unusual filesystems, pre-AVX hosts on old glibc) and the deferred
items, none of which can silently lose reads or misreport success in the
supported workflows — every such path found across three review passes now
either works, errors loudly with a typed diagnostic, or is documented as a
contract boundary. The review's recurring failure mode (fixes validated only
as far as the tests reached) was closed in this pass by executing the actual
release gates — the equivalence script, the dist checks, the ELF verification
— rather than the test suite alone; the release procedure should preserve
that habit end to end.

### Maintainer correction after the final assessment: effective demultiplexing topology

The final audit found one non-blocking but misleading provenance case. A
demultiplexed run creates sample-dependent path writers dynamically, while
the fixed-output validation path represents an omitted `--outN` list with
placeholder `Discard` targets. `RunReport.output_topology` and the versioned
summary had reused those placeholders. Consequently, a paired demultiplexed
run could correctly emit two FASTQ lanes while reporting `output_arity = 2`
and `output_topology = ["discard"]`.

Commit `ef4dee7` separates configured fixed-output targets from the effective
reported topology. Fixed-output runs continue to report their exact configured
target kinds. Demultiplexed runs now report one `path` target for every emitted
read lane, so the same paired run reports `["path", "path"]` in both the Rust
`RunReport` and the machine-readable `SeqprocStats` path. This deliberately
does not enumerate sample names or generated filenames: their cardinality and
values are data-dependent, while `output_topology` is defined as a list of
sink kinds rather than concrete destinations.

The correction retains summary schema 1.13.0. It neither changes the JSON
shape nor adds a vocabulary value—`path` already denotes a filesystem-backed
sink—so a schema bump or a new `dynamic-path` enum would add compatibility
cost without improving the field's documented meaning. The writer graph,
demultiplexing lookup, filenames, FASTQ bytes, and performance-sensitive read
path are unchanged; only the post-run provenance vector is corrected.

The new paired-end CLI regression executes a real barcode-to-sample routing,
asserts that `sample_R1.fastq` and `sample_R2.fastq` are created, validates the
summary against schema 1.13.0, and requires `output_arity = 2` with
`output_topology = ["path", "path"]`. On the exact `ef4dee7` boundary,
formatting and warning-denied all-target clippy pass, all 31 CLI workflow tests
pass, and the exhaustive all-target source/benchmark gate completes without a
failure. The release-profile artifact was rebuilt from this boundary; its
SHA-256 and ELF/provenance checks are recorded above and below.

### Critical analysis of the blocker-6 cache design (2026-08-22, reviewer)

Questions raised: is the sparse-map fix optimal; is a fresh map created per
read; would a worker-level cache or an epoch/versioned map be better?

**Answers.** The `cd4a9e4` fix did create a fresh `FxHashMap` per read, and
— the real inefficiency — inserted a cached `None` per distinct seed-hit
pattern even when the oracle was inactive, so the default path paid one
malloc/free per read plus hashing per seed hit for nothing.
`FxHashMap::default()` itself is allocation-free; the first insert is not.
The optimal shape is not a better map but **no map**: every input to the
oracle-activity decision (position policy, match scope, metric, the
detailed-statistics flag) is read-invariant, so activity can be resolved
once per read and the cache and oracle calls skipped entirely when off.
The maintainer's follow-up (`5468b3f`, `uses_reference_position_oracle` +
`Option<FxHashMap>` via `.then(..)`) implements exactly this and is the
right design.

**On worker-level caching and epoch maps:** a thread-local map reused with
`clear()` is *worse* here, not better — hashbrown's `clear()` costs
O(high-water capacity), so a single degenerate read (low-complexity
sequence seed-hitting a large fraction of the whitelist) permanently
inflates the map and every subsequent read pays that memset, quietly
re-creating the per-read O(big-constant) pathology. An epoch/versioned map
(stamp entries with a per-read counter; stale entries read as absent)
would eliminate that risk, but it is complexity spent on the *diagnostic*
path only: with the gate in place, the common path does zero map work, and
on the active path a fresh small map is O(distinct-hit patterns) and
robust by construction. Empirically the map overhead was already noise —
the 400k × 20k workload spends ~385 µs/read total, and a malloc plus a few
FxHash ops is ~100 ns (<0.05%) — so the gate's value is architectural
(zero cost regardless of seed-hit counts), not a measurable speedup.
Recommendation: keep `5468b3f`'s design; do not add epoch machinery unless
active-path profiling ever shows map churn.

**One behavioral divergence introduced by the gate (should fix):**
`uses_reference_position_oracle` mirrors the `Ok(None)` early-outs of
`reference_position_candidate` but not its `Err` backstop. For
`PositionAmbiguityPolicy::Quality` combined with an **alignment** metric,
the candidate previously returned a hard error on the first seed hit
("position quality policy requires exact or Hamming search"); the gate
evaluates false for alignment metrics, so that configuration now silently
proceeds with the optimized path and the quality policy is ignored.
seqproc's compiler blocks this combination, and `MatchAnyOp::try_new`
rejects the *pattern*-axis quality/edit pairing — but nothing in
ANTISEQUENCE validates the *position*-axis policy against the metric at
construction, so direct library users lose the only guard. Recommended
fix: reject position-quality with non-Exact/Hamming metrics in `try_new`
(making the runtime backstop dead code and the gate exactly correct), and
add a unit test asserting `uses_reference_position_oracle` agrees with
`reference_position_candidate`'s early-out prefix across the spec matrix
so the duplicated predicate cannot drift.

**Maintainer resolution:** fixed in ANTISEQUENCE `0c16ed2`. Construction now
rejects position-quality for every non-exact/non-Hamming metric before backend
planning, while the runtime check remains defensive. The policy/metric matrix
is covered directly, so the optional-cache gate cannot silently turn an
invalid direct-library configuration into an accepted one.

### Cheap pre-tag improvements (should-fix, not blocking)

All of this list was completed before the final handoff:

| Review suggestion | Final disposition |
| --- | --- |
| Restore chemistry, differential, and clippy checks on pull requests | Restored in seqproc fast PR CI. The heavier comprehensive/release matrix remains separately scheduled, preserving the requested fast-versus-release CI split. |
| Namespace orientation bookkeeping and remove `.expect()` panics | `TryOrientationOp` uses an operation-unique internal batch key, removes it on every accepted path, reports missing/type-invalid control state as typed graph errors, and has nested/collision regressions (`0c16ed2`). |
| Add `/dev/full`, byte-level optimizer ablation, and nested-`done` coverage | Plain and gzip real-filesystem `/dev/full` failures, nested completion in both try operators, and optimized/unoptimized FASTQ byte equality are now explicit regressions. |
| Validate alignment fractions, clarify matcher plans, strengthen non-ACGT test | `MatchAnyOp::try_new` rejects non-finite or out-of-range identity/overlap; `MatchType::k` remains total and underflow-free for direct callers. `MatcherPlan.backend` is documented as advisory execution planning with correctness fallbacks. The non-ACGT test now asserts a real fallback hit, a controlled miss, the selected plan, and the detailed match-distance histogram (`272ba77`). |
| Correct graph docs, changelogs, baseline feature, and install docs | Graph statistics/finalization ownership is corrected; post-preprint release notes are folded into 0.1.0; README/site installation uses `cargo install --locked`. A first-class seqproc `baseline-simd` passthrough was implemented and tested, then deliberately removed: cargo-dist discovers the workspace with all package features, which activates ANTISEQUENCE's mutually exclusive same-version SSE2/AVX2 dependency aliases and makes `cargo metadata` fail before the configured release-only build can start. The supported baseline selector therefore remains the explicit `antisequence/baseline-simd`; this preserves both portable source builds and cargo-dist release generation without weakening ANTISEQUENCE's library default. |
| Make the future Bioconda build portable | The handoff now specifies `RUSTFLAGS="-C target-cpu=x86-64" cargo install -v --locked --no-default-features --features antisequence/baseline-simd ...`, plus an in-package provenance check. This deliberately avoids both seqproc's default AVX2 backend and the checkout's host-native rustflags. |

### User decisions still open (unchanged from the maintainer's list)

1. stdout `EPIPE` exit-code policy (maintainer recommendation — exit 0 for
   stdout EPIPE only — is sound).
2. Confirm `0.1.0` as the coordinated first version (recommendation: yes).

### What this re-review confirmed as genuinely solid

The `GraphNode::finish()` recursion (all writers, all nested ops, correct
error aggregation, double-finish safe), the statistics immutability fix, the
optimizer ablation plumbing, the `SetOp` constant-folding proof, the
`TryOrientationOp` lane-invalidation + compile-time liveness rejection, the
Hamming k() pigeonhole math, the non-ACGT bypass design, the AVX2 bit
enumeration fix (tests run under `release-simd` and pass), the unified
`(start, end)` tie order with a stats-on/off equality test, annotation
allowlists with no legacy name wrongly rejected, the preflight nesting
bound, `Anchor1` lexing, schema 1.13.0 strict validation, the package
allowlists, and the publish-script ordering. No test assertions were
weakened anywhere in the fix range; the one deleted test was the documented,
user-directed ambiguity-syntax reversal, replaced by a stronger equivalence
test.

---

## Historical intermediate status after the first 2026-08-21 fix pass

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

**Still open at that intermediate commit (deliberately left for the
maintainer/next agent — these need
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
deleting that historical record. The original review-fix heads were
ANTISEQUENCE `a10d990ed5c66dd8a4edb61b36dc3cce74543238` and seqproc
`1925425b0ace241be09e8d70880104979c3228a5`. The later, explicitly approved
release-architecture decision advanced ANTISEQUENCE to
`b5fecee6feb19f53da3dfd0dcd7fa56ab8023612` and the pinned seqproc
implementation to `4e0de317bdf82a0c9b52dd97213a6e8ba7a5ed03`. ANTISEQUENCE's final
two commits remove two redundant imports and split incompatible Cargo test
selectors in CI; the latter is workflow-only. The seqproc commit advances the
immutable dependency pin and lockfile to that reviewed head. This report-only
follow-up does not alter compiled source, manifests, or tests.

**P0 resolution:**

| Finding | Resolution | Primary implementation and verification evidence |
| --- | --- | --- |
| 1b, legacy output arity | Restored the legacy flag-only prefix contract: an untransformed paired geometry plus only `-o` writes R1. Typed/transformed output lists retain exact-arity validation. This follows the user-approved one-release compatibility policy. | seqproc `src/execute.rs`; `tests/cli_workflow_tests.rs::legacy_single_output_preserves_paired_no_transform_behavior`; commit `1925425`. |
| 2c, final output errors | Added a fallible recursive `GraphNode::finish()` contract and calls before successful return. Plain, gzip, nested, and parallel writers propagate final flush/footer errors. Constant output paths are materialized even for zero-record runs. | ANTISEQUENCE `src/graph.rs`, `src/graph/ops/output_fastq_op.rs`, and `src/graph/ops/output_json_op.rs`; seqproc `src/execute.rs`; `/dev/full`, broken-writer, gzip-footer, and empty-output regressions; commits `5672682`, `4461e1d`, and `1925425`. |
| 2d, Hamming planning semantics | Separated Hamming minimum-match semantics from edit budgets, guarded mixed lengths, and verified the selected seed width. | ANTISEQUENCE `src/matcher.rs`, `src/graph.rs`, and `src/graph/ops/match_any_op.rs`; backend/reference matcher matrix in `src/lib.rs`; commit `5672682`. |
| 2f, `HammingLookup` with non-ACGT input | Lookup misses containing non-ACGT bases fall through to the general Hamming verifier; the fast path is retained for encodable reads. | ANTISEQUENCE `src/graph/ops/match_any_op.rs`; literal backend matrix and `N` cases in `src/lib.rs`; commit `5672682`. |
| 2g, AVX2 candidate loss | Enumerates every bit in each hit mask and derives lane nonzero state without the signed-byte comparison error. | ANTISEQUENCE `src/seed_search.rs`; AVX2 tests for same-position multi-pattern hits and lane 7; commits `5672682` and `477462b`; release-SIMD suite **388/388**. |
| 2h, `match N.attr` | The compiler accepts only `ori`, requires the referenced read to carry `#[match_ori(either)]`, and validates the metadata index before its `u8` conversion. It does not pretend arbitrary attributes are implemented. | seqproc `src/geometry/compile/mod.rs`; annotation and paper-chemistry compile regressions; commit `1925425`. |
| 4a, mutable statistics after optimization | Removed `CompiledGraph`'s mutable-graph dereference. Statistics are selected on the builder before compilation and immutable on the compiled graph. | ANTISEQUENCE `src/graph.rs`; optimizer/statistics regressions; commit `5672682`. |
| 4b, terminal-projection ablation | Execution now applies the compiled optimization decision to pipeline configuration. Disabling the pass disables direct terminal rendering. | ANTISEQUENCE `src/graph.rs`; seqproc `src/execute.rs`; optimized/unoptimized byte-equivalence and report assertions in seqproc `tests/cli_workflow_tests.rs`; commits `5672682` and `1925425`. |
| 4c, `SetOp` proof | Reorder/removal eligibility requires a constant optimized expression and record/lane metadata target. Built-in nodes explicitly attest complete effects; third-party nodes are opaque by default. | ANTISEQUENCE `src/graph.rs`, `src/graph/ops/set_op.rs`, and `docs/graph-api.md`; commit `5672682`. |
| 4d, nested early termination | `TryOp` and `TryOrientationOp` retain every previously accepted record when a nested graph signals completion. | ANTISEQUENCE `src/graph/ops/try_op.rs` and `try_orientation_op.rs`; nested completion regressions; commit `5672682`. |
| 4e, orientation invalidation | `TryOrientationOp` declares sequence invalidation and participates in recursive liveness. Live forward-coordinate labels across the operation are rejected. | ANTISEQUENCE `src/graph.rs`, `src/graph/ops/try_orientation_op.rs`, `docs/metadata-liveness-redesign.md`, and liveness tests; commit `5672682`. |
| 5, CPU portability | ANTISEQUENCE has a library-safe SSE2/NEON default. At the user's direction, seqproc has an explicit x86-64-v3 floor, selects ANTISEQUENCE `release-simd`, uses fixed cargo-dist platform targets, and keeps a separately tested baseline build. Raw-CPUID startup diagnostics and report provenance expose the contract. | ANTISEQUENCE `Cargo.toml`, `src/lib.rs`, and `src/graph/ops/match_any_op.rs`; seqproc `.cargo/config*.toml`, `.github/build-setup.yml`, `src/build_info.rs`, `build.rs`, schema 1.13.0, and `scripts/verify_simd_equivalence.sh`; commits `477462b` and `1141162`. |
| 6, release mechanics | Added CHANGELOGs, CITATION files, homepage/documentation metadata, package allowlists, locked publish/package commands, MSRV and macOS/ARM CI, and publish-before-tag ordering. Publish scripts require a clean `main` branch outside dry-run mode. | Both repositories' `Cargo.toml`, `CHANGELOG.md`, `CITATION.cff`, CI workflows, dependency audits, and `scripts/bump_and_publish.sh`; seqproc cargo-dist files; commits `46065aa`, `1925425`, and `1141162`. |

**P1 resolution:**

| Finding | Resolution | Primary implementation and verification evidence |
| --- | --- | --- |
| Broken-pipe detection | Both byte-writer and file-writer error chains recognize `BrokenPipe`; ordinary output failures remain nonzero. Whether stdout EPIPE should be Unix-success remains a user-owned publication policy. | ANTISEQUENCE writer/finalization paths and broken-writer tests in `src/lib.rs`; seqproc `src/error.rs` and `src/execute.rs`; commits `5672682` and `1925425`. |
| Empty plain/gzip inputs and shards | Empty streams, zero-byte files, valid empty gzip members, and empty shards are valid zero-record boundaries. Requested constant output files are created, with valid gzip footers where applicable. | ANTISEQUENCE input ops and `grouped_empty_gzip_shard_is_a_valid_boundary`; output materialization in commit `4461e1d`; seqproc CLI empty-input/output regressions. |
| Silently ignored `--out2` | Legacy target collection no longer truncates before validation. Supplying `--out2` to a one-output geometry is an arity error and creates no misleading file. | seqproc `src/execute.rs`, typed arity error in `src/error.rs`, and CLI regression near `tests/cli_workflow_tests.rs::legacy_single_output_preserves_paired_no_transform_behavior`; commit `1925425`. |
| Edit-distance quality policy | Pattern-axis `quality` ambiguity is rejected for edit distance at construction, matching the position-axis contract until gap-quality semantics exist. | ANTISEQUENCE `src/graph/ops/match_any_op.rs`, `src/lib.rs::edit_match_rejects_pattern_quality_ambiguity_at_construction`, and `docs/unified-matcher.md`; commit `5672682`. |
| Reference tie ordering / stats drift | Reference and optimized paths share deterministic ordering, including prefix/suffix ties. Detailed statistics cache the normative reference result, and enabling statistics does not change emitted coordinates. | ANTISEQUENCE `src/matcher.rs`, `src/graph/ops/match_any_op.rs`, and stats-on/off/tie regressions in `src/lib.rs`; commit `5672682`. |
| Backend differential coverage | Added a literal matcher matrix for every executable reference-comparable backend: direct exact, exact search, Hamming lookup (including `N`), seeded exact/Hamming/edit, exhaustive Hamming, Myers64, and Myers-long. Plan-only families are asserted separately. | ANTISEQUENCE matcher matrix in `src/lib.rs`, AVX2-gated seed tests in `src/seed_search.rs`, and baseline/release test jobs; commits `5672682` and `477462b`. |
| Optimizer hygiene | Semantic no-op elimination respects trace observability; `try_run_one` honors missing-input policy; shared nested graphs are explicit barriers; budget reports include `memory_budget_satisfied` and truthful reason codes. | ANTISEQUENCE `src/graph.rs`, optimizer docs, and planner/optimizer tests; seqproc schema 1.13.0 and deep report tests; commits `5672682` and `1925425`. |
| Annotation/index validation | Unknown or duplicate annotations are errors, read indices must be contiguous and ordered, and read-level annotations are validated where consumed. | seqproc `src/geometry/compile/definitions.rs`, `compile/mod.rs`, annotation/compile tests; commit `1925425`. |
| EFGDL version/syntax contract | Established annotations remain headerless-compatible; new EFGDL 2 files carry the user-selected structured header. At the user's direction, assignment and flat call forms of ambiguity/position policies compile equivalently. | seqproc parser/compiler, `tests/compile_tests.rs::legacy_and_explicit_ambiguity_policy_syntaxes_compile_equivalently`, README, and website EFGDL documentation; commit `1925425`. |
| Parser robustness | Added a 128-level preflight nesting bound, leading-header diagnostic context, fallible integer and `$N` parsing, and correct `Anchor1` lexing without changing fixed-sequence tokenization. | seqproc `src/execute.rs`, lexer/parser/compiler modules, `tests/compile_tests.rs::excessive_nesting_returns_a_bounded_diagnostic`, and `tests/lexer_tests.rs`; commits `1fca512` and `1925425`. |
| Detailed-statistics performance cliff | Reference position results are cached per pattern/read candidate context instead of running the full oracle for every seed hit. | ANTISEQUENCE `src/graph/ops/match_any_op.rs` plus detailed-statistics equality tests; commit `5672682`. |
| Fork/recycle allocations | Resetting shared reads replaces storage without deep-cloning bytes that are about to be overwritten; recycled capacity is retained where ownership permits. | ANTISEQUENCE `src/read.rs` and copy-on-write/recycling tests; commit `5672682`. |
| Schema housekeeping | Only current schema 1.13.0 and the history README enter the crate package. CLI tests compile the schema and deeply validate every report; optimizer, batch-planning, and build-provenance fields are required. | seqproc `Cargo.toml` package allowlist, `schemas/seqproc-summary-1.13.0.schema.json`, `schemas/README.md`, and `tests/cli_workflow_tests.rs`; commits `1925425` and `1141162`. |
| Dependency hygiene | Both crates use `thiserror` 2; `nix` is current and Unix-target-scoped; `rapidgzip-core` is optional in ANTISEQUENCE and explicitly enabled by seqproc; lockfiles and direct-dependency audits are retained. `regex` is current. `colored` 3 is deferred as a UI-only migration outside hot paths. | Both `Cargo.toml`/`Cargo.lock` files and `DEPENDENCY_AUDIT.md` records; commits `46065aa`, `4930f55`, and `1925425`. |
| Docs/downstream example | ANTISEQUENCE has a compiled README doc-test and a clean-room downstream example. The release gate packages/extracts the crate and runs the example from a temporary consumer project. | ANTISEQUENCE `README.md`, `examples/downstream_smoke.rs`, release script, and CI; commits `0e4edd8`, `20bbeff`, and `b5fecee`. |
| Unsafe hygiene | The Hamming tail no longer performs an out-of-bounds over-read. Remaining unsafe `Send` wrappers have explicit safety justifications. | ANTISEQUENCE `src/graph/ops/match_any_op.rs` and the affected input/output graph ops; commit `5672682`; warning-denied baseline/release gates. |

Two release-policy choices remain intentionally unresolved because either
behavior can be defensible and affects users rather than internal correctness:

1. **stdout broken pipe:** retain the current nonzero execution failure, or
   treat EPIPE on stdout as successful early-consumer termination. The
   maintainer recommendation is exit 0 only for stdout EPIPE; ENOSPC, quota,
   file-writer failures, and every other output error must remain nonzero.
2. **first public version:** confirm `0.1.0` for both crates, or select a
   different coordinated version before publication. The maintainer
   recommendation is `0.1.0`: neither name has an earlier public release, the
   APIs are intentionally pre-1.0, and the changelogs are already organized
   around that boundary.

The optimized-binary decision is now resolved: official x86_64 seqproc
artifacts have an explicit x86-64-v3/AVX2 floor rather than being labeled
portable. Official aarch64 artifacts use fixed Neoverse N1 (Linux) and Apple
A14 (macOS) compiler targets. `--version --verbose`, schema 1.13.0 build
provenance, and a baseline-versus-release byte-equivalence CI gate make this
contract auditable. Cargo multiversioning remains deferred until measurements
show enough v3-versus-v4 benefit to justify multiple implementations.

Note: seqproc pins ANTISEQUENCE by git `rev` in `Cargo.toml`; the pin and
lockfile resolve to `773e1ced7bae6170b1358a2d2198f1c152109624`. That exact head
contains the library-baseline/application-release SIMD split, all nine blocker
repairs, the reviewed cache/DP corrections, the follow-up construction and
nested-orientation protections, sticky failure-path finalization, read-only
hot-path lifecycle polling, and the remaining pre-tag cleanup.

### Post-fix verification evidence

The historical review and its original red-gate table remain below so that the
defects and the evidence that found them are not erased. They no longer
describe the review-fix heads. The maintainer pass completed these gates on
this host:

| Gate | Post-fix result |
| --- | --- |
| ANTISEQUENCE baseline all-target tests | **397/397 pass**, locked, at exact final head `773e1ce` |
| ANTISEQUENCE release-SIMD/accelerated all-target tests | **401/401 pass**, locked, using the mutually exclusive AVX2 backend at exact final head `773e1ce` |
| ANTISEQUENCE clippy/docs/downstream package gate | Warning-denied clippy and docs pass; README doc-test passes; the packaged crate builds and runs from an extracted clean-room downstream project |
| seqproc complete source regression run | **Pass** on the final source/dependency graph: 278 library, 31 CLI workflow, 58 compile, 1 differential, 6 error-contract, 2 error-handling, 12 layout-algebra, 12 lexer, 69 paper-chemistry, and 31 parser tests, plus anchor-set, annotation, benchmark-regression, and error suites and both Criterion benchmark binaries. The chemistry consistency case completed in 187.37 s rather than being skipped. Formatting and warning-denied all-target clippy pass at implementation boundary `ef4dee7`. |
| seqproc SIMD source gate | The tuned default suite passes against exact ANTISEQUENCE `773e1ce`; the explicit `antisequence/baseline-simd` compatibility build is warning-denied and mutually exclusive with the default. The exhaustive gate was rerun after the strict-output repair: **9/9** checked-in FASTQ fixtures are byte-identical, and both lanes are compared for the three paired pass-through fixtures. A local passthrough feature was rejected after an empirical cargo-dist metadata failure, as documented in the pre-tag disposition table. |
| seqproc cargo-dist profile smoke | The optimized `dist` profile rebuilt successfully from implementation boundary `ef4dee7` and exact ANTISEQUENCE pin `773e1ce`. Archive SHA-256 is `d4def76f7069aed934665322d169ab7e654e8c7a2fc3c50895781f64691af023`. The packaged executable reports target `x86_64-unknown-linux-gnu`, compiler CPU target/floor `x86-64-v3`, SIMD backend `x86-avx2`, and the expected v3 target features; the independent ELF parser reads ISA-needed mask `0x7`. |
| seqproc clippy/docs/MSRV | Warning-denied all-target clippy and docs pass; all-target check passes on Rust 1.88 |
| Formatting and manifests | `cargo fmt --check`, `git diff --check`, locked metadata (including seqproc all-features), `cargo dist plan`, and `cargo dist generate --check` pass |
| Package boundaries | seqproc lists exactly 87 intended files (including the new build script/provenance module); ANTISEQUENCE lists 67. Planning documents, the Astro site, generated dependencies, and build output do not enter either source package |
| Documentation site | Astro production build passes under Node 22.22.3 (20 generated pages plus search index) |
| Hosted fast CI before final demux-provenance correction | ANTISEQUENCE run `32585043975` passed on final head `773e1ce`; seqproc run `32587272717` passed on pre-correction head `e3eafa0`. The final reviewer should confirm the pushed `ef4dee7` seqproc boundary; the exact correction is already covered by the local gates in this table. |

The only package gate that cannot be completed before publication ordering is
seqproc's registry-resolved `cargo publish --dry-run`: its manifest correctly
requires ANTISEQUENCE `0.1.0`, which does not yet exist on crates.io. The
release script therefore publishes and verifies ANTISEQUENCE first, waits for
registry resolution, and only then performs seqproc's full package/publish
dry-run. This is an operational release gate, not an unresolved source defect.

**Current disposition:** the implementation blockers and P1 code-quality
findings from this review are resolved and are ready for an independent final
pass. The requested reviewer should either confirm each resolution row or add
a new finding tied to the final implementation boundary; findings copied from
the historical sections without checking the new source are not current.

Do not publish merely because this maintainer pass is complete. Publication
still requires the two explicit user decisions above, promotion of the
reviewed `dev` heads to `main`, registry-order verification, generated-artifact
smoke tests, and the Bioconda release procedure. The original recommendation
below is retained as the reviewer's disposition of the pre-fix heads and is
superseded for the post-fix implementation by this section.

### Suggested commands for the independent final pass

From ANTISEQUENCE `773e1ced7bae6170b1358a2d2198f1c152109624`:

```bash
cargo fmt --all --check
RUSTFLAGS='-D warnings' cargo test --locked --all-targets
RUSTFLAGS='-D warnings' cargo test --locked --all-targets \
  --no-default-features --features release-simd,accelerated-gzip
RUSTFLAGS='-D warnings' RUSTDOCFLAGS='-D warnings' \
  cargo test --locked --doc --features accelerated-gzip
cargo package --locked --allow-dirty --list
```

From seqproc implementation boundary `ef4dee75b79efd836e8d63e11cac02ac9193da69`
(or the later documentation-only `dev` head):

```bash
cargo fmt --all --check
RUSTFLAGS='-C target-cpu=x86-64-v3 -D warnings' cargo test --locked --lib
RUSTFLAGS='-C target-cpu=x86-64-v3 -D warnings' \
  cargo test --locked --test cli_workflow_tests
RUSTFLAGS='-D warnings' cargo check --locked --no-default-features \
  --features antisequence/baseline-simd --lib --bin seqproc
./scripts/verify_simd_equivalence.sh
cargo dist generate --check
cargo dist plan
cargo package --locked --allow-dirty --list
```

The reviewer should additionally inspect the raw-CPUID x86-64-v3 predicate in
`seqproc/src/build_info.rs`, confirm that release archives copy
`.cargo/config-release.toml`, and verify that ANTISEQUENCE rejects simultaneous
`baseline-simd` and `release-simd` selection. Cross-compiling the macOS ARM
target on this Linux host is not a valid substitute for the hosted macOS gate
because native C dependencies require an Apple toolchain.

---

## Original release recommendation (pre-fix heads)

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

## Original independently verified gates (pre-fix candidate heads, this host)

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

## Original P0 — release blockers on the pre-fix candidate heads

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

## Original P1 — recommendations for the pre-fix candidate heads

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

## Original compatibility-contract assessment

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

## Original handoff-document accuracy corrections

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

## Original suggested fix order

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
