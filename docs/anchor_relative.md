# anchor_relative() Function

## Overview

The `anchor_relative()` function enables flexible linker/anchor searching from position 0 in a read, allowing preceding fixed-length elements to be extracted relative to the found anchor position. This is particularly useful for protocols like SPLiT-seq where linker positions may vary.

## Syntax

```
anchor_relative(hamming(f[SEQUENCE], distance))
```

Or as a labeled definition:
```
linker = anchor_relative(hamming(f[SEQUENCE], distance))
```

## Parameters

- **SEQUENCE**: The nucleotide sequence to search for (anchor/linker sequence)
- **distance**: Maximum Hamming distance allowed for matching (typically 2-3)

## Behavior

1. Searches for the anchor sequence starting from position 0
2. Performs a 3-way split: `before | anchor | after`
3. Preceding elements are extracted relative to the found anchor position
4. If the anchor is not found within the allowed Hamming distance, the read fails filtering

## Example: SPLiT-seq Geometry

```
# Define linkers with anchor_relative
l1 = anchor_relative(hamming(f[GTGGCCGATGTTTCGCATCGGCGTACGACT], 3))
l2 = anchor_relative(hamming(f[ATCCACGTGCTTGAGAGGCCAGAGCATTCG], 3))

# Read structure
1{r:}
2{u[10]b[8]<l1>b[8]<l2>b[8]}

# Output transformation (linkers removed)
-> 1{r:} 2{u[10]b[8]b[8]b[8]}
```

## Use Cases

- **SPLiT-seq**: Finding L1/L2 linkers that may be offset by 1-2bp due to sequencing variability
- **Any protocol** where anchor sequences may appear at slightly variable positions
- **Barcode extraction** where fixed elements precede a searchable anchor

## Comparison with Regular Search

| Feature | `search()` | `anchor_relative()` |
|---------|-----------|---------------------|
| Search start | Current position | Position 0 |
| Element extraction | After anchor only | Before and after anchor |
| Use case | Known position | Variable position |

## Combined with Whitelist Filtering

For highest accuracy, combine `anchor_relative()` with `filter_within_dist()`:

```
l1 = anchor_relative(hamming(f[LINKER_SEQ], 3))
bc = filter_within_dist(b[8], "whitelist.txt", 1)
```

This ensures both flexible linker finding AND validated barcode extraction.
