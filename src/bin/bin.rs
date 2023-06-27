use clap::{Parser as cParser, arg};
use antisequence::*;
use chumsky::prelude::*;
use seqproc::ReadDescription;
use std::time::Instant;


/// General puprose sequence preprocessor
#[derive(Debug, cParser)]
pub struct Args {
    /// FGDL string
    #[arg(short, long)]
    geom: String,

    /// r1 fastq file
    #[arg(short = '1', long)]
    read1: String,

    /// r2 fastq file
    #[arg(short = '2', long)]
    read2: String,

    /// r1 out fastq file
    #[arg(short = 'o', long)]
    out1: String,

    /// r2 out fastq file
    #[arg(short = 'w', long)]
    out2: String,

    /// number of threads to use
    #[arg(short, long, default_value = "1")]
    threads: usize
}

pub fn interpret(
    args: Args,
    read_descriptions: Vec<ReadDescription>,
) {
    let Args { geom: _, read1, read2, out1, out2, threads } = args;

    let read_description_one = read_descriptions.first().unwrap().to_owned();
    let read_description_two = read_descriptions.last().unwrap().to_owned();
    
    let mut pipeline = iter_fastq2(read1, read2, 256)
        .unwrap_or_else(|e| panic!("{e}"))
        .boxed();

    pipeline = ReadDescription::build_pipeline(
        read_description_two,
        ReadDescription::build_pipeline(read_description_one, pipeline),
    );

    if out1.eq("/dev/null") | out2.eq("/dev/null") {
        pipeline
            .run_with_threads(threads);
    } else {
        // writing to /dev/null stalls with ANTISEQUENCE
        pipeline
            .collect_fastq2(sel!(), out1, out2)
            .run_with_threads(threads);
    }
}

fn main() {
    let args: Args = Args::parse();

    let start = Instant::now();
    let geom = args.geom.as_str();

    match seqproc::parser().parse(geom) {
        Ok(read_description) => interpret(
            args,
            read_description,
        ),
        Err(errs) => println!("Error: {:?}", errs),
    }
    let duration = start.elapsed();
    println!("tranformation completed in {:.2}s", duration.as_secs_f32());
}
