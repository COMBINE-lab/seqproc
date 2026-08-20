---
title: Annotations and ambiguity
description: Match annotations, orientation handling, and equal-best barcode policies.
---

Annotations modify a definition or read declaration. Operation-like annotations
use call syntax:

```text
#[search(relative)]
#[edit(2)]
anchor = f[CAGAGC]
```

The ambiguity policy is a property and uses assignment syntax:

```text
#[ambig_policy = no_match]
bc = filter_within_dist(b[8], "barcodes.txt", 1)
```

`seqproc validate` rejects a policy placed on a definition that contains no map
or filter operation.

## Orientation-aware matching

Attach `#[match_ori(either)]` to a read layout to try the specified structure in
the forward orientation and, if needed, the reverse-complement orientation:

```text
umi = u[10]
bc = b[8]

#[match_ori(either)]
1{<umi><bc>r:}
-> 1{<umi><bc>}
```

This is a native either-orientation operation. It avoids requiring users to
reverse-complement an entire FASTQ and run a second external pass.

### Orientation-conditional output

When the emitted layout must differ according to the successful orientation,
branch on the `ori` attribute produced by `match_ori(either)`:

```text
header { efgdl = 2 }

#[match_ori(either)]
1{b<bc>[8]f[CAGAGC]r<read>:}
-> match 1.ori {
  fw => #[header = append(" ORI:Z:fw")] 1{f[A]<bc><read>},
  rc => #[header = append(" ORI:Z:rc")] 1{f[T]revcomp(<bc>)<read>}
}
```

The `fw` arm runs for a forward match and the `rc` arm for a successful
reverse-complement retry. Each arm can independently transform captured
labels, arrange output reads, construct EFGDL 2 fixed sequence, and set an
output FASTQ-name template. A `match 1.ori` block is rejected unless read 1 has
`#[match_ori(either)]`, because otherwise the selector attribute cannot exist.

Use conditional output only when the desired emitted representation genuinely
depends on input orientation. If both orientations should be normalized to the
same output, a direct `-> 1{...}` transformation is shorter and easier to
review.

## What “ambiguous” means

Ambiguity is an **equal-best match to two or more distinct whitelist or mapping
entries**. It is not merely a repeated identical line in an input file:
identical whitelist rows and identical mapping rows are normalized. Repeated
mapping keys with conflicting replacement values are configuration errors.

Default behavior depends on the operation:

- filters accept equal-best set membership;
- maps treat an equal-best tie as no match and execute their fallback.

Set an explicit policy when these defaults are not the protocol's intended
behavior.

## Policies

| Policy | Behavior |
| --- | --- |
| `accept` | Accept and select the first candidate in normalized input order. |
| `no_match` | Treat the tie as unmatched; filters reject and maps use their fallback. |
| `first` | Select the first candidate in normalized input order. |
| `random(seed = N)` | Choose deterministically from the seed, query sequence, and read identity. |
| `quality(min_delta = N)` | Prefer the candidate whose mismatching positions have the lower summed Phred score, if it beats the runner-up by at least `N`. |
| `error` | Stop graph execution when a tie occurs. |

`random` defaults to seed 0. Its result is deterministic across thread
schedules for the same read identities and inputs; it is not intended as a
cryptographic random choice.

`quality` defaults to `min_delta = 1`. It currently requires equal-length
Hamming candidates and valid FASTQ qualities. If the best candidate is not
strictly better by the required delta, the tie is dropped.

Examples:

```text
#[ambig_policy = accept]
bc = filter_within_dist(b[8], "barcodes.txt", 1)

#[ambig_policy = random(seed = 2026)]
sample = map_with_mismatch(s[8], "sample-map.tsv", self, 1)

#[ambig_policy = quality(min_delta = 2)]
cell = map_with_mismatch(b[16], "cell-map.tsv", self, 1)
```

Detailed summaries report how many equal-best events were accepted, dropped,
resolved by first/random/quality, or raised as errors.
