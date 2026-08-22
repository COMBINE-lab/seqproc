# Dependency audit (2026-08-21)

This audit covers all direct dependencies and all 322 packages in the seqproc
release-candidate lockfile. Version information came from
the crates.io API and `cargo update --dry-run --verbose`; advisories came from
RustSec database commit `2f08fbb85332687b721f2f22706d07448369451b`
via `cargo-audit 0.22.2`.

The final paper campaign remains reproducibly identified by seqproc commit
`cbc156bea6f5d713ea73bbba1e6c55b7a85a58d8`, ANTISEQUENCE commit
`f3b0f1ea0b286b680d963382f2bf7316058d83e4`, and the committed lockfile at that
seqproc revision. Later maintenance updates do not rewrite that provenance.

## Immediate updates

The audit found four actionable issues in the old development/release closure:

- `crossbeam-epoch` 0.9.18: RUSTSEC-2026-0204, patched in 0.9.20;
- `tracing-subscriber` 0.3.19: RUSTSEC-2025-0055, patched in 0.3.20;
- `anyhow` 1.0.96: RUSTSEC-2026-0190, patched in 1.0.103;
- deprecated `tempdir` 0.3.7 pulled vulnerable `remove_dir_all` 0.5.3
  (RUSTSEC-2023-0018) into the dev graph.

The lockfile is refreshed to current compatible releases, the direct lower
bounds for `anyhow` and `tracing-subscriber` exclude the vulnerable releases,
and the one `tempdir` test is migrated to the already-used `tempfile` crate.
ANTISEQUENCE 0.1.0 was published first, and seqproc now resolves that immutable
crates.io release directly. The reviewed git implementation is preserved by
ANTISEQUENCE tag `v0.1.0` and release commit
`8d167f0188051849a5656ed313a40938ed21c098`.
The refreshed runtime closure had zero known RustSec vulnerabilities at the
recorded RustSec revision. The larger package count includes the test-only
JSON-Schema validator added after review.
The declared Rust floor is 1.88, matching the highest minimum in that graph
(`psm`/`ar_archive_writer`).

The release review also made CPU and platform dependencies explicit:
ANTISEQUENCE's portable matcher feature is the default, accelerated gzip is
enabled intentionally, and the AVX2 matcher is a mutually exclusive opt-in.
`nix` is current and target-scoped to Unix; only the legacy FIFO helper is
absent on non-Unix targets.

## Complete direct-dependency disposition

“Resolved” is the version in the refreshed lockfile; “latest” is the crates.io
`max_stable_version` on the audit date. This includes runtime and development
dependencies; the ANTISEQUENCE audit covers its backend dependencies.

| Dependency | Resolved | Latest | Decision |
|---|---:|---:|---|
| `antisequence` | 0.1.0 | 0.1.0 | Published and resolved from crates.io; baseline SIMD plus explicit accelerated-gzip support, with seqproc selecting the release-SIMD backend by default. |
| `rustc-hash` | 1.1.0 | 2.1.3 | Defer: hot-path maps/sets need performance and determinism A/B tests. |
| `tracing-subscriber` | 0.3.23 | 0.3.23 | Updated; current and no longer vulnerable. |
| `tracing` | 0.1.44 | 0.1.44 | Keep; current. |
| `ariadne` | 0.5.1 | 0.6.0 | Defer and coordinate with `chumsky` diagnostics. |
| `chumsky` | 0.12.0 | 0.13.0 | Defer: parser API/behavior migration needs property tests. |
| `clap` | 4.6.6 | 4.6.6 | Keep; current. |
| `anyhow` | 1.0.104 | 1.0.104 | Updated; current and no longer vulnerable. |
| `tempfile` | 3.27.0 | 3.27.0 | Keep; current; now replaces deprecated `tempdir`. |
| `nix` | 0.31.3 | 0.31.3 | Updated and target-scoped to Unix; only the legacy FIFO helper requires it. |
| `csv` | 1.4.0 | 1.4.0 | Keep; current. |
| `serde` | 1.0.229 | 1.0.229 | Keep; current. |
| `serde_json` | 1.0.151 | 1.0.151 | Keep; current. |
| `blake3` | 1.8.7 | 1.8.7 | Keep; current, algorithm-tagged geometry provenance digest. |
| `thiserror` | 2.0.20 | 2.0.20 | Keep; current and aligned with ANTISEQUENCE's direct dependency. |
| `assert_cmd` (dev) | 2.2.2 | 2.2.2 | Keep; current. |
| `similar-asserts` (dev) | 1.7.0 | 2.0.0 | Defer: test-output-only major migration. |
| `criterion` (dev) | 0.5.1 | 0.8.2 | Defer: benchmark harness migration is not release-critical. |
| `proptest` (dev) | 1.11.0 | 1.11.0 | Keep; current. |
| `flate2` | 1.1.9 | 1.1.9 | Keep; current zlib-rs backend for gzip streams and tests. |
| `jsonschema` (dev) | 0.50.0 | 0.50.0 | Added without resolver/network features; validates every emitted summary deeply against schema 1.13.0. |

ANTISEQUENCE separately records performance-sensitive backend candidates:
`needletail` 0.7, `rapidgzip-core` 0.3, rand/rand_xoshiro, and rustc-hash 2.
Those should be evaluated with byte-identical output gates and the established
technology benchmark blocks rather than folded into a release-only change.

## Transitive findings

`bio` 4.0.1 is current but brings RustSec unmaintained warnings for
`custom_derive`, `fxhash`, and `paste`, as well as old proc-macro families. The
only ANTISEQUENCE use of `bio` is its long-pattern Myers implementation.
Replacing that one use is the best route to a smaller, warning-free graph, but
it is an algorithmic change and therefore not part of this maintenance update.

Most other duplicate versions are dev-only (`proptest`, `criterion`) or are
forced by `bio`; they affect compilation and package size, not the per-read hot
path. The committed application lockfile remains necessary even though most
manifest requirements already accept current compatible patch/minor releases.

## Reproduction commands

```bash
cargo update --dry-run --verbose
cargo tree --duplicates
cargo audit
cargo test --locked --all-targets
cargo test --locked --lib --no-default-features --features antisequence/simd-avx2
cargo package --locked --allow-dirty --list
cargo dist plan
```
