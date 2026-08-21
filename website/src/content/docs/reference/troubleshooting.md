---
title: Troubleshooting
description: Diagnose geometry, FASTQ, output, and performance problems.
---

## A geometry does not compile

Run both diagnostic commands:

```console
seqproc validate protocol.geom
seqproc explain protocol.geom
```

Check that definitions precede their references, paired read numbers match the
provided files, every transformation output label exists, and annotations are
attached to the relevant definition or read.

For ambiguity policies, use property syntax such as
`#[ambig_policy = quality(min_delta = 2)]`. Call syntax is reserved for
operation annotations such as `#[edit(2)]`.

The CLI uses broad stable exit classes: status 2 for geometry/configuration,
status 3 for malformed runtime FASTQ input, and status 1 for graph or output
execution failures. Library callers receive the corresponding structured
`SeqprocError` variant and source chain.

## A whitelist or map cannot be loaded

Relative paths are resolved from the process's working directory. Either run
from the expected directory, provide a stable path, or use an `$0` placeholder
with `--additional`.

Whitelist files are one raw sequence per line. Mapping files have no header and
use `replacement<TAB>sequence-to-match` order. Demultiplexing maps are a
different format: `barcode<TAB>sample`.

## Fewer reads are emitted than expected

Add an unassigned output and detailed summary:

```console
seqproc run ... \
  --unassigned1 rejected.fastq.gz \
  --summary report.json
```

Inspect rejection reasons and match-stage attrition. Common causes are short or
truncated reads, an anchor threshold that is too strict, a whitelist mismatch,
or `no_match` behavior for equal-best candidates.

For paired input, provide both unassigned paths if both rejected mates must be
retained.

## Output is missing

An omitted primary output is discarded. Supply `--out1` and, when the geometry
emits two reads, `--out2`. A two-read transformation without both paths is an
error.

Demultiplexing uses `--demux-out-dir` instead of fixed primary outputs and
currently emits per-sample `.fastq` files.

## A gzip consumer sees only part of a file

`--parallel-gzip` creates a concatenated multi-member gzip stream. Some tools
incorrectly stop after the first member. Use serial gzip or
`--parallel-gzip-stream`, or update the downstream reader after verifying this
is the cause.

## More threads do not improve runtime

Profile end to end. The bottleneck may be decompression, compression, output
locking, storage bandwidth, anchor matching, or a very cheap graph whose work
does not amortize scheduling costs. Do not increase accelerated input or gzip
pool threads without including those workers in the CPU budget.

## Reporting a problem

Open a [GitHub issue](https://github.com/COMBINE-lab/seqproc/issues) with:

- the seqproc commit or version and `rustc --version`;
- the smallest geometry and FASTQ that reproduce the behavior;
- exact command and complete stderr;
- expected versus observed output;
- checksums for any shareable auxiliary files.

Remove sensitive biological or participant data before attaching inputs.
