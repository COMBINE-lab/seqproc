---
title: Layout algebra
description: Bounded alternatives, optional structure, and fixed repetition in EFGDL 2 input reads.
---

EFGDL 2 can describe a family of related input layouts without duplicating a
whole protocol file. Four operators form a small, bounded algebra:

| Syntax | Meaning |
| --- | --- |
| `A B` | concatenate adjacent terms: match `A`, then immediately match `B` |
| `A \| B` | try `A`, then try `B` if `A` rejects the read |
| `(A)?` | prefer `A`, but allow it to be absent |
| `(A)*N` | repeat `A` exactly `N` times |

Grouping uses parentheses. Layout operators are valid only inside input-read
braces in a document with `header { efgdl = 2 }`; output construction remains
deterministic and linear.

```text
header { efgdl = 2 }

1{b<bc>[8](f[AAA] | f[CCC])(x[2])?r<read>:}
-> 1{<bc><read>}
```

This layout captures the same barcode and biological read around either of two
linkers and permits an optional two-base spacer. Choice is ordered: if more
than one alternative succeeds, the first source alternative wins.

## Compilation and cost

seqproc normalizes the algebra at validation time. It never performs unbounded
grammar search at runtime. Each normalized alternative becomes an isolated
ANTISEQUENCE subgraph, and copy-on-write `TryOp` branching retries only rejected
records. A linear layout still compiles directly and pays no branch overhead.

Normalization is deliberately bounded:

- at most 64 alternatives per read;
- at most 64 repetitions for one `*N`; and
- at most 4096 segments in one alternative.

`seqproc validate` rejects a Cartesian expansion beyond these limits instead
of constructing an unexpectedly large graph. `seqproc explain` reports the
alternative count and maximum normalized segment count for each input read.

## Capture compatibility

Every alternative must expose the same named labels with the same interval and
function definition. This makes later output templates and transformations
well-typed regardless of the successful branch. Place shared captures outside
a choice, or reference the same named definition in every arm:

```text
header { efgdl = 2 }

bc = b[8]
1{(<bc>f[AAA] | f[CCC]<bc>)r<read>:}
-> 1{<bc><read>}
```

Repeated terms may contain named captures when their cardinality is statically
fixed. Refer to an occurrence with a one-based index:

```text
header { efgdl = 2 }

1{(b<round>[8]f[AAA])*3r<read>:}
-> #[header = append(" round3:", <round[3]>)]
   1{<round[1]><round[2]><round[3]><read>}
```

Every normalized alternative must expose the same number of occurrences for
each public capture name. When a capture occurs more than once, an unindexed
reference such as `<round>` is rejected; indices start at 1 and are checked at
compile time. The compiler lowers each `(capture, index)` pair to an ordinary,
short internal interval label, so indexing adds no per-read lookup or dynamic
collection. Dynamically sized capture collections remain outside the bounded
layout algebra.
