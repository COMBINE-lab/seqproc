### What is `seqproc`?

`seqproc` is a program for interpreting and transforming sequencing data according to the extended fragment geometry description language (EFGDL). In fact, the `seqproc` executable is a rather thin wrapper around the underlying `seqproc` library, whose main purpose is to accept an input stream of reads (consisting of single-end or paired-end reads) and an EFGDL specification, and to transform the input reads into the desired output formation according to the specification. 

Thus the important information regarding `seqproc` actually pertains to the EFGDL, which has its own documentation. For detailed documentation describing EFDGL, please visit the EFGDL specification documentation [here](https://efgdl-spec.readthedocs.io/en/latest/)

### Using `seqproc`

The CLI provides `validate`, `explain`, and `run` subcommands. The legacy
flag-only invocation remains available during the compatibility window.

```console
seqproc validate protocol.geom
seqproc explain protocol.geom
seqproc run --geom protocol.geom --file1 reads_R1.fastq.gz \
  --file2 reads_R2.fastq.gz --out1 clean_R1.fastq.gz \
  --out2 clean_R2.fastq.gz --threads 8 --preserve-order
```

`--summary report.json` runs the same processing pipeline and emits the
versioned schema documented in [`schemas/`](schemas/). Statistics are disabled
unless a summary is requested.

### Compressed I/O

Gzip level 3 is the measured speed/size default. Two opt-in parallel output
backends cover different interoperability and performance requirements:

- `--parallel-gzip` compresses read batches on transform workers and emits a
  concatenated multi-member gzip file. This is typically the fastest choice,
  but consumers must support concatenated members.
- `--parallel-gzip-stream` emits one logical gzip member with dictionary
  continuity across read batches. Its compression pool defaults to
  `min(--threads, 4)`; use `--gzip-threads` and `--gzip-block-size` to tune it.

FASTQ input is parsed by `needletail`, which also provides transparent
decompression by default. `--accelerated-gzip-input` instead feeds `needletail`
from `rapidgzip-core`'s speculative decoder for regular `.gz` files while
retaining the same transformation graph. Decoder workers are created adaptively
up to the `--gzip-input-threads` ceiling; real-data profiling currently favors the
default ceiling of one worker per input when transform workers share a fixed
CPU allocation. `--gzip-input-chunk-size` controls decoded handoff chunks, with
a measured 256-KiB default. Plain input files are unchanged by the option.

Run `seqproc run --help` for the complete set of pipeline, ordering,
demultiplexing, and compressed-I/O options.
