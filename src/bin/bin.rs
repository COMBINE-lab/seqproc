use antisequence::{iter_fastq2, Reads, sel};
use chumsky::prelude::*;
use clap::{arg, Parser as cParser};
use std::time::Instant;
use std::io;
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};
use tracing::{warn, info};


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
    #[arg(short = 'o', long)]
    out1: String,

    /// r2 out fastq file
    #[arg(short = 'w', long)]
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

    // TODO if passed /dev/null
    read.collect_fastq2(sel!(), out1, out2)
        .run_with_threads(threads)
}

fn main() {
    // set up the logging.  Here we will take the 
    // logging level from the environment variable if 
    // it is set.  Otherwise, we'll set the default 
    tracing_subscriber::registry()
        // log level to INFO.
        .with(fmt::layer().with_writer(io::stderr))
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    let args: Args = Args::parse();

    let start = Instant::now();
    let geom = args.geom.as_str();

    match seqproc::parse::parser().parse(geom) {
        Ok(reads) => interpret(args, reads),
        Err(errs) => warn!("Error: {:?}", errs),
    }
    let duration = start.elapsed();
    info!("tranformation completed in {:.2}s", duration.as_secs_f32());
}
