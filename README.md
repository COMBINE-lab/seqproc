<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="website/src/assets/seqproc_logo_oblique_wordmark_dark.svg">
    <source media="(prefers-color-scheme: light)" srcset="website/src/assets/seqproc_logo_oblique_wordmark.svg">
    <img alt="seqproc: geometry-driven FASTQ preprocessing" src="website/src/assets/seqproc_logo_oblique_wordmark.svg" width="680">
  </picture>
</p>

<p align="center">
  <a href="https://github.com/COMBINE-lab/seqproc/actions/workflows/actions.yml"><img alt="Fast CI" src="https://github.com/COMBINE-lab/seqproc/actions/workflows/actions.yml/badge.svg"></a>
  <a href="https://github.com/COMBINE-lab/seqproc/actions/workflows/comprehensive.yml"><img alt="Comprehensive CI" src="https://github.com/COMBINE-lab/seqproc/actions/workflows/comprehensive.yml/badge.svg"></a>
  <a href="https://combine-lab.github.io/seqproc/"><img alt="Documentation" src="https://github.com/COMBINE-lab/seqproc/actions/workflows/docs.yml/badge.svg"></a>
  <a href="LICENSE"><img alt="License: BSD-3-Clause" src="https://img.shields.io/badge/license-BSD--3--Clause-blue.svg"></a>
</p>

`seqproc` is a performance-oriented FASTQ preprocessing engine for single-cell
and other structured sequencing data. A compact geometry describes where
barcodes, UMIs, biological reads, anchors, and discarded sequence occur;
`seqproc` compiles that geometry into a multithreaded transformation pipeline.

This keeps protocol logic out of ad hoc scripts while supporting fixed and
variable intervals, approximate matching, barcode correction, filtering,
orientation-aware and conditional processing, constructed output sequence,
FASTQ-name templates, demultiplexing, ordered output, compressed I/O, and
versioned run summaries.

- **Documentation:** <https://combine-lab.github.io/seqproc/>
- **EFGDL language specification:** <https://efgdl-spec.readthedocs.io/>
- **Preprint:** <https://www.biorxiv.org/content/10.64898/2026.07.28.741211v1>
- **Reproducible paper analysis:** <https://github.com/COMBINE-lab/seqproc-paper-analysis>

## A first geometry

The following geometry describes the common 10x Chromium v2 layout: the first
FASTQ contains a 16-base cell barcode followed by a 10-base UMI, and the second
contains the biological read.

```efgdl
header {
  efgdl = 2,
  name = "10x Chromium v2",
}

bc = b[16]
umi = u[10]
bio = r:

1{<bc><umi>}
2{<bio>}
-> 1{<bc><umi>} 2{<bio>}
```

Save it as `10x-v2.geom`, validate it, inspect the compiled representation, and
run it:

```console
seqproc validate 10x-v2.geom
seqproc explain 10x-v2.geom
seqproc run --geom 10x-v2.geom \
  --read1 reads_R1.fastq.gz --read2 reads_R2.fastq.gz \
  --out1 processed_R1.fastq.gz --out2 processed_R2.fastq.gz \
  --threads 8
```

Output paths should be supplied explicitly; an omitted primary output is
discarded rather than written to standard output. See the
[quick start](https://combine-lab.github.io/seqproc/getting-started/quick-start/)
and [command-line reference](https://combine-lab.github.io/seqproc/getting-started/command-line/)
for paired-end, compressed-I/O, demultiplexing, and reporting examples.

Logical read lanes may be split across files without pre-concatenation. Repeat
`--read1`/`--read2` or use comma-separated paths; seqproc opens corresponding
shards lazily and verifies their record counts at every shard boundary.

`-` denotes stdin for one input lane and stdout for one output lane. For
example, `seqproc run --geom protocol.geom --read1 - --out1 -` is a clean FASTQ
filter in a Unix pipeline; diagnostics remain on stderr. Add `--stdout-gzip`
when stdout itself should be gzip-compressed.

For FASTQ files that alternate complete fragment segments in one stream, use
`--interleaved-input`. Its arity is derived from the geometry, and ordered file
shards are opened lazily just like separate read lanes.

The bounded public lane model supports one, two, or three segments, including
`--read3`, `--out3`, and `--unassigned3` for protocols such as scATAC-seq.

Library callers should use `compile_geom_typed` and `run`; both return the
matchable `SeqprocError` hierarchy rather than stringly typed `anyhow` errors.

New geometry files should declare EFGDL 2 in the general document header.
Optional metadata fields accept integers, quoted strings, or bare identifiers
and are retained for provenance tooling. Headerless files continue to use
legacy EFGDL 1 semantics.

EFGDL 2 input reads also support bounded layout algebra: ordered choice (`|`),
optional structure (`?`), fixed repetition (`*N`), and grouping. Alternatives
are normalized and validated at compile time, then retried through copy-on-write
graphs. See the [layout algebra guide](https://combine-lab.github.io/seqproc/efgdl/layout-algebra/)
for expansion limits, capture compatibility, and zero-runtime-overhead indexed
references such as `<round[2]>` for repeated named captures.

EFGDL 2 output layouts can construct fixed sequence with `f[...]`; for example,
`-> 1{f[ACGT]<bc><umi>}` prefixes those bases and assigns them `I` quality
scores while retaining qualities from captured intervals. They can also add
captured data to FASTQ names without an auxiliary tool:

```efgdl
-> #[header = append(" CB:Z:", <bc>, " UB:Z:", <umi>)]
   1{f[ACGT]<bc><umi>}
```

`append`, `prepend`, and `replace` templates are supported independently on
each output read. Header work is absent from the execution graph when no such
template is used. The [EFGDL 2 guide](https://combine-lab.github.io/seqproc/efgdl/version-2/)
documents the complete syntax, quality behavior, migration boundary, and a
runnable paired-end example.

## Install from source

Tagged binary releases are planned. During the pre-release phase, build the
pinned dependency set from source with Rust 1.88 or newer:

```console
git clone https://github.com/COMBINE-lab/seqproc.git
cd seqproc
cargo build --release --locked
./target/release/seqproc --help
```

## Ambiguous barcode matches

Equal-best matches against distinct whitelist or mapping entries use an
operation-specific default: filters accept set membership, while mapping
operations follow their no-match fallback. A geometry can select an explicit
policy:

```efgdl
#[ambig_policy = accept]
bc3 = filter_within_dist(b[8], "barcodes.txt", 1)

#[ambig_policy = quality(min_delta = 2)]
bc = map_with_mismatch(b[8], "barcode-map.tsv", self, 1)
```

Supported policies are `accept`, `no_match`, `first`, `random`, `quality`, and
`error`. The [ambiguity guide](https://combine-lab.github.io/seqproc/efgdl/annotations-and-ambiguity/)
documents their semantics and reproducibility guarantees.

Search anchors independently support `#[position_policy = leftmost]`,
`rightmost`, `no_match`, or `error`. A one-pattern-per-line anchor whitelist can
be attached with `#[anchor_set($0)]`, avoiding externally expanded geometry or
input preprocessing. Pattern ties and repeated-position ties remain separate
events and receive separate detailed-statistics counters.

## Development and reproducibility

Fast pull-request CI runs formatting, linting, core tests, and generated test
code using cached compiler outputs. Scheduled and release CI runs the complete
test, feature, benchmark-compilation, and sanitizer matrix.

```console
cargo fmt --all --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test --all-targets --all-features
```

The documentation site requires Node.js 22.12 or newer and has its own locked
build:

```console
cd website
npm ci
npm run build
```

The JSON emitted by `--summary` follows the versioned schemas in
[`schemas/`](schemas/). Runtime statistics are disabled unless requested, so
headline performance measurements do not silently include instrumentation.
Geometry provenance uses an algorithm-tagged BLAKE3 digest of the complete
geometry text, including its EFGDL header.

Compilation now emits an inspectable optimization report, and the execution
planner records why it selected the whole-graph or bounded-pipeline backend.
`--execution-mode` forces either backend for controlled comparisons, while
`--no-graph-optimization` provides a structural-optimization oracle. The
bounded pipeline can render a proven-safe terminal FASTQ projection directly
into recycled output buffers, avoiding intermediate record materialization
while preserving byte-identical output. The automatic planner keeps the
measured low-overhead whole-graph default unless ordered output requires the
pipeline. See the
[performance guide](https://combine-lab.github.io/seqproc/guides/performance/)
for selection rules and the validation escape hatch.

Please report bugs and feature requests through
[GitHub Issues](https://github.com/COMBINE-lab/seqproc/issues).

## Citation and license

Until a version of record is available, please cite the
[seqproc preprint](https://www.biorxiv.org/content/10.64898/2026.07.28.741211v1).
`seqproc` is distributed under the [BSD 3-Clause license](LICENSE).
