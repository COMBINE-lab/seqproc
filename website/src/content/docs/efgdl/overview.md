---
title: Language overview
description: The structure of an EFGDL geometry as implemented by seqproc.
---

An EFGDL file has four conceptual parts:

1. an optional versioned document **header**;
2. reusable **definitions** for named intervals or transformed intervals;
3. one or more numbered **input read layouts**;
4. an optional **output transformation** after `->`.

```text
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

The header is a version-neutral metadata block. EFGDL 2 documents must declare
`efgdl = 2`; additional scalar fields are retained for tooling and provenance.
Headerless files remain valid and use legacy EFGDL 1 semantics. Duplicate
fields, missing versions, and unsupported versions are rejected.

See [EFGDL 2](../version-2/) for the complete metadata grammar, migration
guidance, constructed sequence, FASTQ-name templates, and a runnable example.

Definitions bind names such as `bc`. Angle brackets insert a reference to a
definition in a read layout or output. Read numbers correspond to FASTQ input
and output order.

## Input versus output

The left side of `->` must account for the input structure to be recognized.
The right side states what should be emitted. It may reorder, omit, combine, or
transform extracted intervals.

EFGDL 2 output layouts may also construct fixed bases directly with `f[...]`:

```text
header { efgdl = 2 }
1{b<bc>[8]r<read>:}
-> 1{f[ACGT]<bc>f[T]<read>}
```

Inserted bases receive `I` quality scores. Existing captured intervals retain
their input qualities. Fixed construction is deliberately restricted to
explicit EFGDL 2 documents so legacy files do not silently change meaning.

Output reads can also modify their FASTQ record names with a typed header
template:

```text
header { efgdl = 2 }
1{b<bc>[8]r<read>:}
-> #[header = append(" CB:Z:", <bc>)] 1{<read>}
```

The supported modes are `append`, `prepend`, and `replace`. Quoted parts are
fixed text and `<label>` parts insert captured sequence. Delimiters are
explicit: include the desired space or punctuation in a quoted part. When the
annotation is absent, seqproc adds no header operation and the existing FASTQ
name is passed directly to the writer without constructing a replacement.

These are the output-construction additions gated by EFGDL 2. Existing
matching, filtering, mapping, ambiguity, orientation, and conditional-output
features retain their established semantics in a versioned document.

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
