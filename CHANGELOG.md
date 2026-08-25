# Changelog

All notable changes to seqproc are documented here. This project follows
[Semantic Versioning](https://semver.org/).

## [Unreleased]

- Add conservative `seqproc import seqspec` support for seqspec 0.3/0.4 input
  layouts, with network-free assessment, atomic provenance bundles, exact
  resource verification, a pinned official-corpus compatibility report with
  one row per source specification, and emitted compiling EFGDL conversions.
- Support variable-length, clipped, gzip-compressed, and reverse-oriented
  onlists natively through explicit EFGDL 2 pattern-boundary, projection, and
  orientation annotations; legacy geometries retain their prior semantics and
  pay no feature-path overhead.
- Increase the bounded FASTQ lane model from three to eight across the parser,
  CLI, streaming topology, outputs, and unassigned-read routing.

## [0.1.1] - 2026-08-22

- Restore the declared Rust 1.88 compatibility of the x86-64 CPU-floor
  diagnostic by isolating CPUID calls behind audited wrappers that compile
  with both the older unsafe and newer safe intrinsic signatures.

## [0.1.0] - 2026-08-22

Initial public release. It includes the preprint functionality plus the
post-preprint correctness, usability, and performance work reviewed for this
release.

- Make `seqproc run` require an exact, nonempty primary-output topology while
  preserving the deprecated flag-only prefix behavior for one compatibility
  cycle. Unassigned outputs likewise require exactly one target per input lane,
  and primary outputs combined with demultiplexing are rejected rather than
  silently ignored.
- Treat only a closed stdout pipe as normal Unix early-consumer termination
  (silent exit 0). ENOSPC, quota exhaustion, named-pipe/file failures, and all
  other output errors remain nonzero. Output writers retain their target
  identity through the type-erased execution graph so a file `EPIPE` cannot be
  mistaken for stdout closure.
- Report demultiplexed output topology from the effective writer graph: one
  `path` sink per emitted read lane, without exposing data-dependent sample
  filenames.
- Propagate ANTISEQUENCE's single-use graph lifecycle and fallible finalization:
  malformed input, output flush/footer failures, and repeated execution now
  produce typed nonzero errors instead of panic, silent truncation, or false
  success.
- Fix mixed-length Hamming seed planning so a fast-path seed is used only when
  it is guaranteed for every literal; this removes a silent false-negative
  case while retaining safe indexed matching.
- Make seqproc's executable default architecture-tuned while preserving
  ANTISEQUENCE's library-safe baseline: distributed x86_64 binaries target
  x86-64-v3/AVX2, aarch64 artifacts use fixed platform targets, and local
  repository builds use `target-cpu=native`.
- Add exact target-feature CPUID/XGETBV compatibility checks, Linux ELF loader
  ISA notes, `--version --verbose` build provenance, and build/SIMD provenance
  in summary schema 1.13.0. Release CI inspects the packaged cargo-dist binary
  before hosting it.
- Add comprehensive byte-equivalence gates between generic SSE2 and tuned
  AVX2 builds.

- EFGDL 2 document headers, named resources, fixed output sequences, output
  header templates, indexed captures, layout choice/optional/repeat algebra,
  anchor sets, and explicit ambiguity/position policies.
- One to three separate input lanes, ordered shard lists, stdin/stdout,
  interleaved FASTQ, gzip auto-detection, and optional accelerated gzip input.
- Typed `RunConfig`, `RunReport`, and `SeqprocError` APIs plus `run`, `validate`,
  and `explain` CLI subcommands; legacy flag-only invocation remains available
  for one compatibility cycle.
- Corrected SPLiT-seq FILTER/Hamming behavior, normalized duplicate whitelist
  entries, deterministic ambiguity handling, and exhaustive input validation.
- Worker-local statistics, versioned run summaries, proof-gated graph
  optimization, dynamic batch planning, and optimized low-thread execution.
- Reproducible multi-platform binary release automation with explicit CPU
  floors.

[Unreleased]: https://github.com/COMBINE-lab/seqproc/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/COMBINE-lab/seqproc/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/COMBINE-lab/seqproc/releases/tag/v0.1.0
