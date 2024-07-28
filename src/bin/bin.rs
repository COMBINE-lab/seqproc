use clap::arg;
use std::io;
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};

use seqproc::{
    error::handle_errors,
    execute::{compile_geom, interpret},
};

/// General puprose sequence preprocessor
#[derive(Debug, clap::Parser)]
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

    #[arg(short, long, value_parser, num_args = 1.., value_delimiter = ' ')]
    additional: Vec<String>,
}

fn main() {
    // set up the logging. Here we will take the
    // logging level from the environment variable if
    // it is set. Otherwise we will set the default
    tracing_subscriber::registry()
        // log level to INFO
        .with(fmt::layer().with_writer(io::stderr))
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    let args: Args = <Args as clap::Parser>::parse();

    let geom = std::fs::read_to_string(&args.geom).unwrap();

    let compiled_efgdl = compile_geom(geom.clone());

    match compiled_efgdl {
        Ok(geom) => interpret(
            args.file1,
            args.file2,
            args.out1,
            args.out2,
            args.threads,
            args.additional,
            geom,
        ),
        Err(e) => handle_errors(e, geom),
    }
}
