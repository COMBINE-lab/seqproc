---
title: Introduction
description: What seqproc does, and where it fits in a sequencing workflow.
---

`seqproc` turns a declarative read geometry into an executable FASTQ
preprocessing graph. It is designed for protocols in which useful sequence is
distributed among fixed positions, variable intervals, linker-delimited
regions, and one or more read orientations.

The project has two deliberately separate layers:

1. The **Extended Fragment Geometry Description Language (EFGDL)** describes
   the input layout and desired output.
2. The **ANTISEQUENCE execution backend** performs matching, filtering,
   extraction, rewriting, and FASTQ I/O.

That separation makes a geometry inspectable and reusable without tying it to
a shell pipeline or protocol-specific program. `seqproc validate` catches
syntax and semantic errors before processing; `seqproc explain` shows the
normalized description and compiled graph.

## Suitable workloads

`seqproc` is particularly useful when a workflow needs one or more of:

- fixed or variable-length barcode and UMI extraction;
- anchor searches with Hamming or edit-distance tolerance;
- whitelist filtering and barcode replacement;
- forward, reverse, or either-orientation matching;
- explicit handling of equal-best barcode matches;
- paired-end transformations and demultiplexed output;
- bounded multithreading, compressed input/output, or preserved read order;
- machine-readable processing summaries.

It is not a read aligner, basecaller, cell caller, or quantifier. Its output is
FASTQ prepared for those downstream tools.

## Project status

`seqproc` is under active development alongside the
[seqproc preprint](https://www.biorxiv.org/content/10.64898/2026.07.28.741211v1).
The command-based CLI (`validate`, `explain`, and `run`) is the preferred
interface. A legacy flag-only invocation remains temporarily available for
compatibility.

The language's normative reference is the independent
[EFGDL specification](https://efgdl-spec.readthedocs.io/). This site focuses on
the currently implemented `seqproc` interface and practical use.

## Related repositories

- [seqproc source](https://github.com/COMBINE-lab/seqproc)
- [ANTISEQUENCE backend](https://github.com/COMBINE-lab/ANTISEQUENCE)
- [paper analysis and benchmark configurations](https://github.com/COMBINE-lab/seqproc-paper-analysis)
