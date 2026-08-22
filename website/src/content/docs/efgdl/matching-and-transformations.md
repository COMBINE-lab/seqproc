---
title: Matching and transformations
description: Implemented sequence operations, searches, filters, and maps.
---

Operations wrap an interval and can be composed. Validate the complete geometry
because not every operation is meaningful for every shape.

## Sequence transformations

| Function | Effect |
| --- | --- |
| `rev(I)` | Reverse interval `I`. |
| `revcomp(I)` | Reverse-complement `I`. |
| `trunc(I, n)` / `trunc_left(I, n)` | Remove `n` bases from the right/left. |
| `trunc_to(I, n)` / `trunc_to_left(I, n)` | Retain a target length from the left/right side. |
| `pad(I, n, A)` / `pad_left(I, n, A)` | Add `n` copies of a nucleotide on the right/left. |
| `pad_to(I, n, A)` / `pad_to_left(I, n, A)` | Pad from the right/left to a target length. |
| `remove(I)` | Consume the interval but remove it from the retained representation. |
| `norm(I)` | Normalize a variable-length interval to its declared range. |

Use `seqproc explain` to check the direction of left/right transformations in
the compiled geometry before applying them to data.

## Whitelist filters

```text
bc = filter(b[8], "barcodes.txt")
```

`filter` requires exact membership. The distance-bounded form permits up to
`n` Hamming mismatches while retaining the observed sequence:

```text
bc = filter_within_dist(b[8], "barcodes.txt", 1)
```

Whitelist files contain one sequence per line; do not add a header, comments,
or blank rows. Identical duplicate entries are normalized internally.
Approximate filters require equal-length comparisons; use mapping or anchor
edit distance when the desired semantics include replacement or indels.

## Barcode maps

Mapping files place the replacement sequence in the first column and the
sequence to match in the second. Columns are tab-separated and there is no
header.

```text
AACGTGAT\tAACGTGAA
TGGTGGTA\tTGGTGGTT
```

```text
bc = map(b[8], "barcode-map.tsv", self)
bc_hamming = map_with_mismatch(b[8], "barcode-map.tsv", self, 1)
bc_edit = map_with_edit(b[8], "barcode-map.tsv", self, 1)
```

The third argument is the fallback transformation when no mapping is selected;
`self` leaves the observed interval unchanged. Conflicting duplicate mapping
keys are rejected because their replacement is not well-defined.

## Fixed anchors

An exact fixed sequence can be named directly:

```text
linker = f[CAGAGC]
1{b[8]<linker>u[10]r:}
```

Annotations make it approximate or request a relative search:

```text
#[search(relative)]
#[edit(1)]
linker = f[CAGAGC]

1{b[8]<linker>u[10]r:}
```

`#[hamming(n)]` permits substitutions only. `#[edit(n)]` uses Levenshtein
distance and can accommodate insertions and deletions. `#[search(relative)]`
locates the anchor relative to the current parsing position and is the
preferred spelling of the legacy `anchor_relative(...)` function.

Choose thresholds from protocol knowledge and controlled validation data;
larger search tolerances can improve sensitivity while increasing ambiguous or
spurious matches.

## Whitelist-backed anchor sets

When a protocol permits several known linker sequences, attach an anchor-set
file rather than preprocessing reads or expanding the geometry by hand:

```text
#[search(relative)]
#[anchor_set($0)]
#[edit(1)]
#[ambig_policy = quality(min_delta = 1)]
#[position_policy = best]
linker = f[CAGAGC]

1{b[8]<linker>u[10]r:}
```

The file contains one anchor per line; empty lines, comments beginning with
`#`, and identical duplicates are normalized. `$0` refers to the first
`--additional` value and a quoted path may be used instead. In the current
release every entry must have the same length as the placeholder `f[...]` so
Hamming thresholds and bounded positions remain identical across backends.

`ambig_policy` resolves equal-best **anchor-pattern** ties.
`position_policy` independently resolves repeated equal-best placements of the
selected anchor. This separation prevents a repeated linker in one read from
being confused with two distinct whitelist entries. Both axes support
quality-based resolution for exact/Hamming search. The pattern policy scores
each anchor at its own candidate window; the position policy compares the
equal-best windows of one anchor. Edit-distance quality policies are rejected
until a gap-quality model is explicitly defined.
