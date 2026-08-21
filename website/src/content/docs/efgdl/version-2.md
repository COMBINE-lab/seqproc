---
title: EFGDL 2
description: Versioned documents, constructed output sequence, FASTQ-name templates, and migration from EFGDL 1.
---

EFGDL 2 adds an explicit document header and output-construction features while
preserving the established geometry, matching, mapping, filtering, annotation,
and conditional-output language.

## Declare the language version

An EFGDL 2 document starts with a header:

```text
header {
  efgdl = 2,
  name = "example protocol",
  chemistry = v2,
  revision = 3,
}
```

`efgdl = 2` is required and must be an integer. Other fields are optional
metadata retained in the parsed and compiled representation for provenance
tools. Metadata values may be integers, quoted strings, or bare identifiers.
Field names must be unique; a missing version, duplicate field, or unsupported
version is an error.

Headerless files remain valid and use legacy EFGDL 1 semantics. The header is
the version-negotiation point for future language revisions; there is no
separate version marker elsewhere in the file.

## Describe alternative input layouts

EFGDL 2 adds bounded layout algebra inside input reads: ordered choice (`|`),
optional terms (`?`), fixed repetition (`*N`), grouping, and implicit
concatenation. The compiler normalizes these constructs into a bounded list of
linear alternatives and executes them with copy-on-write fallback branches.
See [Layout algebra](/seqproc/efgdl/layout-algebra/) for syntax, limits, and
capture compatibility.

## Construct fixed sequence in an output read

On the right side of `->`, `f[...]` inserts literal bases between captured
intervals:

```text
header { efgdl = 2 }

1{b<bc>[8]u<umi>[10]r<read>:}
-> 1{f[ACGT]<bc><umi>f[T]<read>}
```

The output sequence is `ACGT`, followed by `bc`, `umi`, `T`, and `read`.
Captured intervals keep their original quality scores. Each constructed base
receives quality `I`. This makes adapters, separators, protocol tags, and other
fixed sequence explicit in the geometry instead of requiring a postprocessing
script.

Fixed sequence in an **input** layout continues to mean a sequence that must be
matched, such as an adapter or linker. Fixed sequence in an **output** layout
means sequence to construct. The output meaning requires EFGDL 2 so a legacy
file cannot silently acquire new behavior.

## Modify FASTQ names

Attach a typed `header` template to an output read:

```text
header { efgdl = 2 }

1{b<bc>[8]u<umi>[10]r<read>:}
-> #[header = append(" CB:Z:", <bc>, " UB:Z:", <umi>)]
   1{<read>}
```

Quoted arguments are literal text. `<label>` arguments insert the sequence of
a label captured by the input geometry. Separators are never implicit, so put
the desired spaces, colons, or other punctuation in quoted arguments.

The three modes are:

| Mode | Resulting FASTQ name |
| --- | --- |
| `append(parts...)` | original name followed by the template |
| `prepend(parts...)` | template followed by the original name |
| `replace(parts...)` | template only |

For paired output, each output read may have its own template:

```text
-> #[header = append(" CB:Z:", <bc>)] 1{<bc><umi>}
   #[header = append(" CB:Z:", <bc>)] 2{<bio>}
```

Template labels must have been matched in an input read. Newlines and carriage
returns are rejected in literal parts so a template cannot create a malformed
FASTQ record.

When no output-header template is present, seqproc adds no corresponding graph
operation and passes the existing name directly to the writer. Merely opting a
document into EFGDL 2 therefore does not add per-read header work.

## Complete paired-end example

This geometry constructs a fixed protocol tag in output read 1 and places the
captured cell barcode and UMI in both output FASTQ names:

```text
header {
  efgdl = 2,
  name = "tagged paired-end example",
}

bc = b[16]
umi = u[10]
bio = r:

1{<bc><umi>}
2{<bio>}
-> #[header = append(" CB:Z:", <bc>, " UB:Z:", <umi>)]
   1{f[ACGT]<bc><umi>}
   #[header = append(" CB:Z:", <bc>, " UB:Z:", <umi>)]
   2{<bio>}
```

Use `seqproc validate` before processing and `seqproc explain` to inspect the
normalized geometry and compiled graph:

```console
seqproc validate tagged.geom
seqproc explain tagged.geom
seqproc run --geom tagged.geom \
  --file1 reads_R1.fastq.gz --file2 reads_R2.fastq.gz \
  --out1 tagged_R1.fastq.gz --out2 tagged_R2.fastq.gz \
  --threads 8
```

The concise `Normalized EFGDL` line reports the effective emitted sequence
layout; it is not a lossless serialization of the source document. The
subsequent compiled representation reports `efgdl_version`, the retained
document header, output sequence parts, and FASTQ-name transformations.

## Migration and provenance

To migrate an existing file, add `header { efgdl = 2 }`, validate it, and
compare byte-level output before adopting any new output construction. Existing
EFGDL 1 matching and transformation syntax retains its behavior under EFGDL 2.

Run summaries identify geometry content with an algorithm-tagged BLAKE3 digest
such as `blake3:...`. The digest covers the geometry text, including the
header. It does not replace archiving the geometry and all referenced map or
whitelist files.

## EFGDL 2 feature boundary

The document header, bounded input-layout algebra, fixed output construction,
and FASTQ-name templates are the EFGDL 2 additions. Approximate anchors, maps,
filters, ambiguity policies, native either-orientation matching, and
orientation-conditional output remain available with their existing semantics;
new files should nevertheless declare EFGDL 2 so their language contract is
explicit.
