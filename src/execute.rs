use std::{path::PathBuf, thread};

use antisequence::*;
use anyhow::{bail, Result};
use chumsky::{prelude::Simple, Parser, Stream};
use tempfile::tempdir;

use crate::{
    compile::{compile, CompiledData},
    lexer,
    parser::parser,
};

#[derive(Debug)]
pub struct FifoSeqprocData {
    pub r1_fifo: PathBuf,
    pub r2_fifo: PathBuf,
    pub join_handle: thread::JoinHandle<Result<SeqprocStats>>,
}

#[derive(Debug)]
pub struct SeqprocStats {
    pub total_fragments: u64,
    pub failed_parsing: u64,
}

pub fn read_pairs_to_file(
    compiled_data: CompiledData,
    in1: String,
    in2: String,
    out1: String,
    out2: String,
    threads: usize,
    additional_args: Vec<String>,
) -> Result<SeqprocStats> {
    interpret(in1, in2, out1, out2, threads, additional_args, compiled_data);

    Ok(SeqprocStats { total_fragments: 0, failed_parsing: 0 })
}

pub fn read_pairs_to_fifo(
    _compiled_data: CompiledData,
    _r1: Vec<PathBuf>,
    _r2: Vec<PathBuf>,
) -> Result<FifoSeqprocData> {
    // create the fifos

    let tmp_dir = tempdir()?;
    let r1_fifo = tmp_dir.path().join("r1.pipe");
    let r2_fifo = tmp_dir.path().join("r2.pipe"); 

    // todo: create the fifos

    let join_handle: thread::JoinHandle<Result<SeqprocStats>> = thread::spawn(move || {
        // let seqproc_stats = sinpleaf_interpret(file1, file2, out1, out2, threads, additional_args, compiled_data);

        match tmp_dir.close() {
            Ok(_) => Ok(SeqprocStats {
                total_fragments: 0,
                failed_parsing: 0,
            }),
            Err(e) => {
                bail!("When closing (deleting) the temp directory, the following error was encountered {:?}", e);
            }
        }
    });

    Ok(FifoSeqprocData {
        r1_fifo,
        r2_fifo,
        join_handle,
    })
}

pub fn interpret(
    file1: String,
    file2: String,
    out1: String,
    out2: String,
    threads: usize,
    additional_args: Vec<String>,
    compiled_data: CompiledData,
) {
    let read = iter_fastq2(file1, file2, 256)
        .unwrap_or_else(|e| panic!("{e}"))
        .boxed();

    let read = compiled_data.interpret(read, out1, out2, additional_args);

    read.run_with_threads(threads)
}

pub fn compile_geom(geom: String) -> Result<CompiledData, Vec<Simple<String>>> {
    let (tokens, mut errs) = lexer::lexer().parse_recovery(geom.clone());

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser().parse_recovery(Stream::from_iter(
            tokens.len()..tokens.len() + 1,
            tokens.clone().into_iter(),
        ));

        if let Some((ast, _)) = &ast {
            let res = compile(ast.clone());

            if let Err(e) = res {
                errs.push(Simple::custom(e.span, e.msg));
            } else {
                return Ok(res.ok().unwrap());
            }
        };

        parse_errs
    } else {
        Vec::new()
    };

    let errors = errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
        .collect::<Vec<_>>();

    Err(errors)
}
