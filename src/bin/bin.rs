use clap::{arg, Parser as cParser};

use seqproc::{
    error::handle_errors,
    execute::{compile_geom, interpret},
};

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

    #[arg(short, long, value_parser, num_args = 1.., value_delimiter = ' ')]
    additional: Vec<String>,
}

fn main() {
    let args: Args = Args::parse();

    let geom = std::fs::read_to_string(args.geom.clone()).unwrap();

    let compiled_fgdl = compile_geom(geom.clone());

    match compiled_fgdl {
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
