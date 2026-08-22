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
./target/release/seqproc --version
```

Using `--locked` prevents Cargo from silently selecting dependency versions
other than those recorded in `Cargo.lock`.

The repository and tagged binary configuration is portable: SSE2 is the
x86_64 matcher floor and NEON is used on aarch64. It does not silently inherit
`target-cpu=native`, x86-64-v3, or a machine-specific ARM target.

For a local x86_64 build that will run only on AVX2-capable hosts:

```console
cp .cargo/config.local.example.toml .cargo/config.local.toml
cargo build --release --locked --no-default-features \
  --features antisequence/simd-avx2 \
  --config .cargo/config.local.toml
```

Do not distribute that binary as a generic x86_64 artifact; its CPU floor is
intentional. The portable and AVX2 matcher features are mutually exclusive.

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
seqproc run --help
```

You should see the `run`, `validate`, and `explain` commands. Continue with the
[quick start](../quick-start/).
