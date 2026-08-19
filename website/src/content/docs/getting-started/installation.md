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
