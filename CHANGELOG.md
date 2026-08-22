# Changelog

All notable changes to seqproc are documented here. This project follows
[Semantic Versioning](https://semver.org/).

## [Unreleased]

- Make seqproc's executable default architecture-tuned while preserving
  ANTISEQUENCE's library-safe baseline: distributed x86_64 binaries target
  x86-64-v3/AVX2, aarch64 artifacts use fixed platform targets, and local
  repository builds use `target-cpu=native`.
- Add an early raw-CPUID compatibility check, `--version --verbose` build
  provenance, and build/SIMD provenance in summary schema 1.13.0.
- Add comprehensive byte-equivalence gates between generic SSE2 and tuned
  AVX2 builds.

## [0.1.0] - 2026-08-21

Initial public release. It includes the preprint functionality plus the
post-preprint correctness, usability, and performance work reviewed for this
release.

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

[Unreleased]: https://github.com/COMBINE-lab/seqproc/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/COMBINE-lab/seqproc/releases/tag/v0.1.0
