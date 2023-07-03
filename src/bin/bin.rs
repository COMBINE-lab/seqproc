use antisequence::{iter_fastq2, sel, Reads};
use chumsky::prelude::*;
use clap::{arg, Parser as cParser};
use std::time::Instant;

use seqproc::syntax::Read;

/// General puprose sequence preprocessor
#[derive(Debug, cParser)]
pub struct Args {
    /// FGDL string
    #[arg(short, long)]
    geom: String,

    /// r1 fastq file
    #[arg(short = '1', long)]
    file1: String,

    /// r2 fastq file
    #[arg(short = '2', long)]
    file2: String,

    /// r1 out fastq file
    #[arg(short = 'o', long, default_value = "")]
    out1: String,

    /// r2 out fastq file
    #[arg(short = 'w', long, default_value = "")]
    out2: String,

    /// number of threads to use
    #[arg(short, long, default_value = "1")]
    threads: usize,
}

pub fn interpret(args: Args, reads: Vec<Read>) {
    let Args {
        geom: _,
        file1,
        file2,
        out1,
        out2,
        threads,
    } = args;

    let read_one = reads.first().unwrap().to_owned();
    let read_two = reads.last().unwrap().to_owned();

    let read = iter_fastq2(file1, file2, 256)
        .unwrap_or_else(|e| panic!("{e}"))
        .boxed();

    let read = read_one.interpret(read);
    let read = read_two.interpret(read);

    if out1.is_empty() && out2.is_empty() {
        return read
            .collect_fastq1(sel!(), "/dev/null")
            .run_with_threads(threads);
    }

    read.collect_fastq2(sel!(), out1, out2)
        .run_with_threads(threads)
}

fn main() {
    let args: Args = Args::parse();

    let start = Instant::now();
    let geom = std::fs::read_to_string(args.geom).unwrap();

    match seqproc::parse::parser().parse(geom) {
        Ok(reads) => {
            println!("{:?}", reads);
            // interpret(args, reads)
        }
        Err(errs) => println!("Error: {:?}", errs),
    }
    let duration = start.elapsed();
    println!("tranformation completed in {:.2}s", duration.as_secs_f32());
}
