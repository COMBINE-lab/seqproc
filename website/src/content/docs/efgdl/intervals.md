---
title: Intervals and layouts
description: Interval roles, shapes, labels, and read layouts.
---

Each interval combines a role prefix with a shape.

## Interval roles

| Prefix | Role | Typical use |
| --- | --- | --- |
| `b` | barcode | Cell, round, or other identifying barcode. |
| `s` | sample barcode | Barcode used to distinguish samples. |
| `u` | UMI | Unique molecular identifier. |
| `r` | read sequence | Biological sequence retained for downstream analysis. |
| `x` | discard | Sequence that is consumed but not needed in output. |
| `f` | fixed sequence | Literal adapter, linker, or anchor sequence. |

The role records intent and lets the compiler build named sequence components;
it does not by itself correct or filter an interval.

## Interval shapes

| Shape | Meaning | Example |
| --- | --- | --- |
| `[n]` | Exactly `n` bases. | `u[10]` |
| `[a-b]` | Between `a` and `b` bases, inclusive. | `b[9-10]` |
| `:` | The remaining unbounded sequence. | `r:` |
| `[ACGT]` | A literal nucleotide sequence; valid for `f`. | `f[CAGAGC]` |

Variable and unbounded intervals need a surrounding layout that makes their
boundaries determinable. An anchor is a common way to delimit a variable
prefix.

## Read layouts

Braces group intervals belonging to an input read:

```text
1{b[16]u[10]}
2{r:}
```

Paired files are supplied as `--file1` and `--file2`. The output transformation
uses the same numbered layout notation:

```text
1{b<bc>[16]u<umi>[10]}
2{r<bio>:}
-> 1{<bc><umi>} 2{<bio>}
```

An output need not reproduce consumed anchors or discarded sequence. In EFGDL
2 it may combine extracted labels with constructed `f[...]` literals:

```text
header { efgdl = 2 }
1{b<bc>[16]u<umi>[10]r<bio>:}
-> 1{f[ACGT]<bc><umi>} 2{<bio>}
```

Constructed bases receive `I` quality scores. See [EFGDL 2](../version-2/)
for output FASTQ-name templates and the complete versioned-output behavior.

## Definitions and references

Prefer definitions when an interval has a protocol-level name or operation:

```text
bc = filter_within_dist(b[8], "barcodes.txt", 1)
umi = u[10]
1{<bc><umi>r:}
-> 1{<bc><umi>}
```

Definitions also provide the attachment point for operation annotations such
as `#[edit(1)]` and the property annotation `#[ambig_policy = no_match]`.
