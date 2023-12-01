# Documentation for `seqproc`

`seqproc` is a program for interpreting and transforming sequencing data 
according to the extended fragment geometry description language (EFGDL). 
In fact, currently, the `seqproc` executable is a rather thin wrapper around
the underlying `seqproc` library, whose main purpose it to accept an input 
stream of reads (consisting of single-end or paired-end reads) and an EFGDL
specification, and to transform the input reads into the desired output format
according to the specification.

Thus, most of the important information regarding `seqproc` actually pertains 
to the EFGDL, which has its own documentation. For detailed documentation 
describing EFGDL, please visit the EFGDL specification documentation 
[here](https://efgdl-spec.readthedocs.io/en/latest/).

## Using `seqproc`

The `seqproc` program is an executable that exposes a single command that takes 
several command line arguments.  You can pass the `-h` or `--help` flag to `seqproc`
to see the various command line arguments and their descriptions.

    $ ./target/release/seqproc -h
    General puprose sequence preprocessor

    Usage: seqproc [OPTIONS] --geom <GEOM> --file1 <FILE1> --file2 <FILE2>

    Options:
      -g, --geom <GEOM>                 EFGDL string
      -1, --file1 <FILE1>               r1 fastq file
      -2, --file2 <FILE2>               r2 fastq file
      -o, --out1 <OUT1>                 r1 out fastq file [default: ]
      -w, --out2 <OUT2>                 r2 out fastq file [default: ]
      -t, --threads <THREADS>           number of threads to use [default: 1]
      -a, --additional <ADDITIONAL>...
      -h, --help                        Print help


