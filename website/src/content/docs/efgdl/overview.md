---
title: Language overview
description: The structure of an EFGDL geometry as implemented by seqproc.
---

An EFGDL file has three conceptual parts:

1. reusable **definitions** for named intervals or transformed intervals;
2. one or more numbered **input read layouts**;
3. an optional **output transformation** after `->`.

```text
bc = b[16]
umi = u[10]
bio = r:

1{<bc><umi>}
2{<bio>}
-> 1{<bc><umi>} 2{<bio>}
```

Definitions bind names such as `bc`. Angle brackets insert a reference to a
definition in a read layout or output. Read numbers correspond to FASTQ input
and output order.

## Input versus output

The left side of `->` must account for the input structure to be recognized.
The right side states what should be emitted. It may reorder, omit, combine, or
transform extracted intervals.

If no arrow is present, the recognized reads pass through according to the
compiled geometry. When reproducibility matters, prefer an explicit output
layout so the intended product is visible during review.

## Inline labels

Intervals can be named in a definition or inline. These forms express the same
kind of binding:

```text
bc = b[16]
1{<bc>r:}
```

```text
1{b<bc>[16]r:}
```

Named definitions are usually easier to annotate, reuse, and inspect.

## Composable functions

Functions wrap intervals and can be nested:

```text
short_bc = trunc_to(revcomp(b[16]), 10)
```

Matching and lookup functions can use a quoted path or a positional command
line argument:

```text
bc = filter_within_dist(b[8], $0, 1)
```

```console
seqproc run --geom protocol.geom --additional whitelist.txt \
  --file1 reads.fastq.gz --out1 clean.fastq.gz
```

Quoted relative paths and `--additional` paths are resolved in the execution
environment, so archive the invoked working directory or use stable paths in
reproduction packages.

## Compile before processing

```console
seqproc validate protocol.geom
seqproc explain protocol.geom
```

`validate` checks more than grammar: it also rejects unsupported combinations,
misplaced annotations, bad ambiguity-policy arguments, and incompatible output
layouts. `explain` is the best way to confirm what a new or generated geometry
will execute.

For the normative language definition, consult the
[EFGDL specification](https://efgdl-spec.readthedocs.io/).
