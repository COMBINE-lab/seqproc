---
title: seqspec import
description: Convert supported seqspec 0.3/0.4 input layouts to reviewable EFGDL 2.
---

seqproc's seqspec importer is a compile-time bridge, not a runtime dependency
and not a replacement implementation of seqspec. It translates the observable
FASTQ input structure that seqproc can represent exactly. It intentionally
leaves output construction, barcode correction distance, and other policies
not specified by seqspec for the user to add to the generated EFGDL.

## Assess before writing

```console
seqproc import seqspec protocol.yaml --check-only
```

Check-only is filesystem- and network-read-only. It prints the versioned JSON
assessment, including one status per modality, source paths, capability codes,
and remediation. A blocked modality does not produce approximate EFGDL.

For multi-modal documents whose read association is not explicit, select one
modality and give read IDs in input-lane order:

```console
seqproc import seqspec protocol.yaml --check-only \
  --modality rna --read R1.fastq.gz --read R2.fastq.gz --read I1.fastq.gz
```

## Create an import bundle

```console
seqproc import seqspec protocol.yaml \
  --output-dir protocol-import \
  --resources auto
```

The output directory must not already exist. seqproc stages the entire bundle
beside its destination and publishes it by one rename, so a failed resource or
geometry validation does not leave a seemingly complete directory. The bundle
contains:

- the byte-identical `source.seqspec.yaml`;
- one `<modality>.geom` per supported modality;
- resolved onlists under `resources/`; and
- `import-report.json`, with schema versions, BLAKE3 source and EFGDL digests,
  diagnostics, inputs, resource checks, and emitted filenames.

Resource modes are:

| Mode | Behavior |
| --- | --- |
| `auto` | Copy local and fetch HTTP(S) onlists when possible; leave failures as explicit runtime bindings. |
| `offline` | Never use the network; copy local onlists and report the rest as bindings. |
| `required` | Require and verify every onlist before publishing the bundle. |

Declared nonzero sizes are checked against the stored object. As required by
seqspec, MD5 is checked against uncompressed content. The report records both
stored and uncompressed byte counts and separate BLAKE3 digests, so a bundle
identifies the exact archived bytes as well as the matcher-visible content.
FASTQ examples in seqspec are reported as input hints and are never downloaded
by import.

Use `--onlist-policy capture` to capture declared onlist regions without
filtering. The default, `exact`, does not infer a mismatch allowance or perform
barcode correction.

## Variable and clipped onlists

Variable-length onlists do not require a hamming-expanded or normalized input
file. Generated EFGDL uses an exact heterogeneous-length matcher and lets the
matched pattern determine the interval boundary. Equal exact alternatives use
`ambig_policy = no_match` conservatively.

If a sequencing window observes only part of an onlist region, seqproc projects
the appropriate prefix or suffix and reverse-complements entries when the read
strand requires it. Projection and orientation happen once during graph
construction; geometries that do not request them pay no per-read cost.

## Compatibility matrix

The repository pins both the current pachterlab seqspec examples and the IGVF
0.3 corpus. Regenerate JSON, CSV, and Markdown reports with:

```console
cargo run -p xtask -- seqspec-compat --fetch
```

The fetch uses sparse, commit-pinned checkouts of YAML specifications only—it
does not download example FASTQs or large onlists. The report ranks missing
seqproc capabilities by the number of blocked modalities and specifications.
This ranking is the input to the importer roadmap, not a claim that every
source document is semantically valid or unambiguous.

Current explicit gaps include read windows that can terminate across multiple
named regions, unidentified boundaries between consecutive variable regions,
partial fixed-sequence prefixes, and variable sequencing windows that terminate
within an onlist. All-`N` fixed fields normalize exactly to unconstrained
intervals; inconsistent fixed sequence/length declarations are reported as
source errors. These cases are kept separate from read-selection diagnostics.
