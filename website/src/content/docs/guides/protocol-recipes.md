---
title: Protocol recipes
description: Small, adaptable geometries for common protocol patterns.
---

These recipes illustrate language patterns. A production geometry should be
validated against the protocol's exact chemistry, read lengths, whitelist
release, and orientation. The
[paper analysis repository](https://github.com/COMBINE-lab/seqproc-paper-analysis/tree/main/configs/seqproc)
contains the versioned configurations used for manuscript comparisons.

## Fixed paired-end layout: 10x Chromium v2

```text
bc = b[16]
umi = u[10]
bio = r:

1{<bc><umi>}
2{<bio>}
-> 1{<bc><umi>} 2{<bio>}
```

This is a purely positional transformation: it does not correct the cell
barcode or filter it against a whitelist.

## Variable barcode followed by an approximate anchor

```text
#[edit(1)]
anchor = f[CAGAGC]
bc1 = b[9-10]
bc2 = b[10]
umi = u[8]

1{<bc1><anchor><umi><bc2>}
2{r<bio>:}
-> 1{<bc1><bc2><umi>} 2{<bio>}
```

The fixed anchor disambiguates the 9–10-base first barcode, and edit distance
one allows an insertion, deletion, or substitution in that anchor. The output
projects only the barcode and UMI fields into read 1.

## Whitelist-backed filtering without replacement

```text
#[ambig_policy = accept]
bc = filter_within_dist(b[8], "canonical-barcodes.txt", 1)
umi = u[10]

1{<bc><umi>r:}
-> 1{<bc><umi>}
```

This keeps the observed barcode if it is within Hamming distance one of any
canonical entry. `accept` is explicit because a sequence may be equally close
to multiple entries and filtering asks only whether set membership holds.

Use `map_with_mismatch` instead if the protocol requires substituting a
canonical barcode. A map file uses `replacement<TAB>sequence-to-match` order.

## Relative linker search

```text
prefix = r:
umi = u[10]
bc3 = b[8]

#[search(relative)]
#[edit(3)]
linker = f[GTGGCCGATGTTTCGCATCGGCGTACGACT]

1{<prefix><umi><bc3><linker>r:}
-> 1{<umi><bc3>}
```

The unbounded prefix absorbs sequence before an embedded cassette. The fixed
UMI and barcode are sliced relative to the located linker. Because permissive
anchor searches can create false positives, choose the anchor, adjacency rules,
and edit threshold using protocol-specific validation.

## Either-orientation long reads

Add a read-level annotation when the entire cassette may occur in either
orientation:

```text
#[match_ori(either)]
1{<prefix><umi><bc3><linker>r:}
-> 1{<umi><bc3>}
```

The current LR-SPLiT-seq paper geometry combines native dual-orientation
matching, adjacent fixed-length components, edit-bounded linkers, and
unexpanded canonical barcode lists. Refer to the analysis repository rather
than copying thresholds from this abbreviated recipe.

## Review checklist

Before using a new geometry at scale:

1. Run `seqproc validate` and archive the successful geometry.
2. Inspect `seqproc explain` for the effective graph.
3. Test exact, truncated, substitution, insertion/deletion, reverse-orientation,
   and equal-best synthetic reads.
4. Confirm output lengths and sequence content, not only retained read counts.
5. Record checksums for geometry, input FASTQs, maps, and whitelists.
