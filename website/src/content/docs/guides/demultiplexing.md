---
title: Demultiplexing
description: Route accepted records to per-sample FASTQ files.
---

Demultiplexing maps a labeled interval from the compiled geometry to a sample
name and uses that name in output paths.

## Sample map format

Create a tab-separated file with `barcode<TAB>sample` and no header:

```text
AACGTGAT\tsample_A
TGGTGGTA\tsample_B
AACAACCA\tsample_C
```

Blank lines and lines beginning with `#` are ignored. Any other line without
two tab-separated columns is an error.

## Run demultiplexing

Assume the geometry labels the sample barcode as `seq2.bc1`:

```console
seqproc run \
  --geom protocol.geom \
  --file1 reads_R1.fastq.gz --file2 reads_R2.fastq.gz \
  --threads 16 \
  --demux-map sample-map.tsv \
  --demux-label seq2.bc1 \
  --demux-out-dir demux
```

`--demux-label` defaults to `seq2.bc1`, but specifying it explicitly makes a
workflow easier to audit. Outputs are named
`demux/{sample}_R1.fastq` and, for paired input,
`demux/{sample}_R2.fastq`. An unmatched barcode is routed to the sample name
`unassigned`.

Demultiplexed output is expression-routed rather than written through fixed
`--out1`/`--out2` paths. The current filenames end in `.fastq` and therefore
are uncompressed.

## Rejected versus unassigned-by-map reads

Two concepts are distinct:

- `--unassigned1` and `--unassigned2` retain records rejected while evaluating
  the geometry;
- the demultiplexing sample `unassigned` contains accepted records whose
  barcode was absent from the sample map.

Keep these outputs separate when calculating protocol pass rates or per-sample
yield.
