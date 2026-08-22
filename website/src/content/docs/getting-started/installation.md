---
title: Installation
description: Build the current seqproc release candidate from source.
---

## Requirements

The current source tree requires:

- a Rust toolchain at version 1.88 or newer;
- Git;
- a C toolchain and the usual system build tools used by Rust dependencies.

## Build the pinned dependency set

Until tagged binary releases are published, clone the repository and build
with its committed lockfile:

```console
git clone https://github.com/COMBINE-lab/seqproc.git
cd seqproc
cargo build --release --locked
./target/release/seqproc --version --verbose
```

Using `--locked` prevents Cargo from silently selecting dependency versions
other than those recorded in `Cargo.lock`.

After the crate is published, the corresponding Cargo installation is:

```console
cargo install --locked seqproc
```

The repository configuration uses `target-cpu=native`, so a local source build
is optimized for—and should be run on—the host that compiled it. Do not
redistribute that executable as a generic binary.

Tagged artifacts instead use fixed, reviewable CPU targets:

- x86_64 Linux and macOS: x86-64-v3, including AVX2;
- aarch64 Linux: Neoverse N1;
- aarch64 macOS: Apple A14.

The x86_64 executable makes a best-effort check of the complete v3 feature set
through raw CPUID before processing and reports an actionable incompatibility.
`seqproc --version --verbose` reports the compiler, target, target features,
CPU floor, and compiled ANTISEQUENCE SIMD backend.

ANTISEQUENCE itself keeps an SSE2/NEON library default. For a seqproc
compatibility control or a pre-AVX2 x86_64 host, build its baseline backend and
override the repository's native code-generation setting:

```console
RUSTFLAGS="" cargo build --release --locked --no-default-features \
  --features antisequence/baseline-simd
```

To put the local build on your path:

```console
install -Dm755 target/release/seqproc "$HOME/.local/bin/seqproc"
```

Make sure `$HOME/.local/bin` is present in `PATH`, or invoke the binary by its
full path.

## Optional allocator builds

For controlled performance experiments, allocator features are forwarded to
the ANTISEQUENCE backend:

```console
cargo build --release --locked --features mimalloc
# or
cargo build --release --locked --features jemalloc
```

Use the default build unless you are benchmarking allocator behavior. Record
the selected feature, compiler, target, and commit whenever results will be
compared.

## Confirm the installation

```console
seqproc --help
seqproc --version --verbose
seqproc run --help
```

You should see the `run`, `validate`, and `explain` commands. Continue with the
[quick start](../quick-start/).
