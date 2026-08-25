# seqspec import design and coverage roadmap

## Contract

`seqproc import seqspec` converts the exactly supported portion of seqspec 0.3
and 0.4 input layout metadata into deterministic EFGDL 2. It does not infer an
output transformation, mismatch correction, or an ambiguity policy absent from
the source. The typed helper crate `crates/seqproc-seqspec-import` owns YAML
normalization, the intermediate representation, diagnostics, and rendering;
`src/seqspec_import.rs` owns filesystem, network, integrity, and atomic bundle
publication.

The source document and BLAKE3 digest are retained. Check-only never writes or
uses the network. Normal output is a new directory containing the source,
generated geometries, resolved resources, and a versioned report. Local and
HTTP(S) resources can be resolved; declared stored sizes and uncompressed
content MD5 values are checked, and separate stored/content BLAKE3 values are
always recorded. Import never downloads FASTQ data.

Exact onlists are the default. Capture-only is available, but fuzzy correction
is deliberately left to the user. Variable-length, clipped, gzip-compressed,
and reverse-oriented onlists are native capabilities. Pattern projection and
orientation occur once at graph construction. `pattern_boundary = matched` is
an explicit opt-in so older ranged-filter geometries remain byte-identical and
performance-neutral.

The runtime lane bound was increased from three to eight after the pinned
corpus showed that ten otherwise-supported modality projections require four
to six lanes. CLI input, output, unassigned output, parser, compilation, and
interleaved topology share the same bound.

## Reproducible corpus assessment

The corpus manifest is `planning/seqspec-corpora.toml`. It pins immutable
commits and sparse YAML-only paths for the pachterlab 0.4 examples and IGVF 0.3
examples. Run:

```console
cargo run -p xtask -- seqspec-compat --fetch
```

The generated JSON, modality CSV, specification CSV, Markdown, and converted
EFGDL live under `planning/generated/seqspec-compat/`. The specification table
has exactly one row per source YAML; the modality table remains the diagnostic
drill-down. Converted scripts preserve corpus and source paths beneath
`geometries/`, and every Markdown modality row links to its emitted script.
This generated tree is intentionally ignored by Git and regenerated on demand.
Duplicated protocol versions across corpora remain separate provenance-bearing
rows. `SupportedRequiresBinding` is generated support: the assessment layer
does not claim a locator exists until the CLI resolves it.

## Capability-driven order

The generated report is authoritative for counts. In the 2026-08-25 snapshot,
25 of 139 modality projections generate compiling EFGDL without an additional
selection decision; 71 require an explicit read/modality policy, 23 expose a
seqproc capability gap, and 20 are structurally invalid under the documented
seqspec contract. Capability counts deliberately exclude source-invalid and
policy-gated rows. The current priority order is:

1. `variable_read_window_layout`: bounded alternatives for reads that may end
   in more than one named region, without silently dropping filters or capture
   semantics.
2. `general_variable_boundary_matching`: observable delimiters or an explicit
   coalesced-capture policy for consecutive variable regions whose boundary is
   not identifiable from sequence alone.
3. `partial_fixed_prefix_matching`: bounded prefix matching when the observed
   window can terminate within a fixed region, with an explicit positional
   ambiguity contract.
4. `partial_onlist_window_matching`: efficiently match all observable prefix
   or suffix lengths without multiplying a large onlist or asking the user to
   preprocess it.

All-`N` fixed regions already normalize exactly to an unconstrained interval;
no exponential IUPAC expansion is performed. Other valid degenerate fixed
anchors will require a class-aware matcher. The current pinned documents that
use `Y` also declare a one-base fixed sequence as 1--15 bases long, so they are
reported as source-invalid rather than being used to justify a misleading
IUPAC-only feature count.

Policy rows (`NeedsUserPolicy`) are distinct. Many multi-modal documents do not
associate sequencing reads unambiguously with a biological modality; users can
already resolve these with repeated `--modality`/`--read`. Future automatic
association must be justified by the seqspec schema rather than filename
heuristics.

## Why remaining gaps are not approximated

A variable/clipped onlist has an observable answer: match a member of the
projected set and cut at the selected pattern length. In contrast, the boundary
between two consecutive random regions is not recoverable from the FASTQ bases
alone. Likewise, accepting a read that may contain a later onlist without
checking it would widen the source language. Those cases remain explicit until
EFGDL can preserve optional capture/resource semantics or the user chooses a
documented coalescing policy. Partial onlist windows remain a priority rather
than a permanent limitation: the implementation must avoid both external
whitelist preprocessing and an in-memory copy of every prefix of every
barcode.

## Gates

- 0.3 tagged YAML and null-valued legacy fields normalize without changing the
  archived source.
- Every generated geometry compiles before publication.
- Variable and clipped onlists execute end to end without external whitelist
  preprocessing.
- Existing ranged filters change behavior only with the new explicit boundary
  annotation.
- One through eight lanes compile and use bounded streaming input.
- Local required resources pass atomic-bundle and digest tests; unresolved
  required resources publish nothing.
- Compatibility reports never panic on a corpus document and rank unique
  modality/spec impacts rather than duplicated diagnostics.
- `benches/protocol_feature_benches.rs` includes fixed and native
  variable-pattern controls plus the expanded lane-arity matrix.

### 2026-08-25 validation snapshot

- `cargo +1.88.0 check --workspace --all-targets`: passed.
- `cargo clippy --workspace --all-targets -- -D warnings`: passed.
- `cargo +1.88.0 test --workspace`: 572 tests passed with no failures.
- Import integration tests: 15 passed, covering the typed assessment and CLI
  publication paths.
- Legacy/compiler regression gates: 279 library, 32 CLI workflow, 58 compile,
  and 12 layout-algebra tests passed. The arity assertion now verifies that
  eight lanes are accepted and nine are rejected.
- The pinned compatibility harness completed across every manifest-scoped
  source and regenerated `report.json`, `report.csv`, `specifications.csv`,
  `report.md`, and all compiling EFGDL scripts. Exact counts and the manifest
  digest live in the intentionally untracked generated report.
- With `SEQPROC_FEATURE_BENCH_READS=5000`, Criterion measured the fixed-length
  control at 1.6900 ms and native variable-boundary matching at 2.2196 ms per
  5,000-read run (medians of the reported intervals): approximately 2.96 and
  2.25 million reads/s, respectively. This 31% feature-path cost is the baseline
  for optimizing partial-window and unified matching; it is not paid by
  geometries that omit `pattern_boundary = matched`.
