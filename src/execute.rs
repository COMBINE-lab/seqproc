use std::{fs::File, io::BufWriter, path::PathBuf, thread};

use antisequence::graph::*;
use anyhow::{bail, Result};
use chumsky::{prelude::Simple, Parser, Stream};
use nix::sys::stat;
use nix::unistd;
use tempfile::tempdir;
use tracing::info;

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

pub fn interpret(
    file1: String,
    file2: String,
    out1: String,
    out2: String,
    threads: usize,
    additional_args: Vec<String>,
    compiled_data: CompiledData,
) {
    let mut graph = antisequence::graph::Graph::new();
    graph.add(
        antisequence::graph::InputFastqOp::from_files([file1, file2])
            .unwrap_or_else(|e| panic!("{e}")),
    );

    compiled_data.interpret(&mut graph, &additional_args);

    if out1.is_empty() && out2.is_empty() {
        graph.add(OutputFastqFileOp::from_file("/dev/null"));
    } else if out2.is_empty() {
        graph.add(OutputFastqFileOp::from_file(out1));
    } else {
        graph.add(OutputFastqFileOp::from_files([out1, out2]));
    }

    graph.run_with_threads(threads);
}

fn interpret_to_pipes(
    files1: Vec<String>,
    files2: Vec<String>,
    out1: PathBuf,
    out2: PathBuf,
    threads: usize,
    additional_args: Vec<String>,
    compiled_data: CompiledData,
) -> SeqprocStats {
    let f1 = File::create(out1).expect("Unable to open read 1 file");
    let f2 = File::create(out2).expect("Unable to open read 2 file");

    let stream1 = BufWriter::new(f1);
    let stream2 = BufWriter::new(f2);

    let files = files1
        .iter()
        .zip(files2.iter())
        .flat_map(|tup| std::iter::once(tup.0).chain(std::iter::once(tup.1)))
        .collect::<Vec<_>>();

    let mut graph = antisequence::graph::Graph::new();
    graph.add(
        antisequence::graph::InputFastqOp::from_files(files).unwrap_or_else(|e| panic!("{e}")),
    );

    compiled_data.interpret(&mut graph, &additional_args);

    graph.add(OutputFastqOp::from_writers([stream1, stream2]));

    graph.run_with_threads(threads);

    SeqprocStats {
        total_fragments: 0,
        failed_parsing: 0,
    }
}

pub fn compile_geom(geom: String) -> Result<CompiledData, Vec<Simple<String>>> {
    let (tokens, mut errs) = lexer::lexer().parse_recovery(geom.clone());

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser().parse_recovery(Stream::from_iter(
            tokens.len()..tokens.len() + 1,
            tokens.clone().into_iter(),
        ));

        if let Some(ast) = &ast {
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

pub fn read_pairs_to_file(
    compiled_data: CompiledData,
    in1: String,
    in2: String,
    out1: String,
    out2: String,
    threads: usize,
    additional_args: Vec<String>,
) -> Result<SeqprocStats> {
    interpret(
        in1,
        in2,
        out1,
        out2,
        threads,
        additional_args,
        compiled_data,
    );

    Ok(SeqprocStats {
        total_fragments: 0,
        failed_parsing: 0,
    })
}

pub fn read_pairs_to_fifo(
    compiled_data: CompiledData,
    r1: Vec<String>,
    r2: Vec<String>,
    additional_args: Vec<String>,
) -> Result<FifoSeqprocData> {
    if r1.len() != r2.len() {
        bail!(
            "The number of R1 files ({}) must match the number of R2 files ({})",
            r1.len(),
            r2.len()
        );
    }

    let tmp_dir = tempdir()?;
    let r1_fifo = tmp_dir.path().join("r1.pipe");
    let r2_fifo = tmp_dir.path().join("r2.pipe");

    // create the fifos
    // create new fifo and give read, write and execute rights to the owner
    match unistd::mkfifo(&r1_fifo, stat::Mode::S_IRWXU) {
        Ok(_) => {
            info!("created {:?}", r1_fifo);
            assert!(std::path::Path::new(&r1_fifo).exists());
        }
        Err(err) => bail!("Error creating read 1 fifo: {}", err),
    }
    // create new fifo and give read, write and execute rights to the owner
    match unistd::mkfifo(&r2_fifo, stat::Mode::S_IRWXU) {
        Ok(_) => {
            info!("created {:?}", r2_fifo);
            assert!(std::path::Path::new(&r2_fifo).exists());
        }
        Err(err) => bail!("Error creating read 2 fifo: {}", err),
    }

    // we clone this here because we want to move these into
    // the thread that will do the transformation but we need
    // to retain a copy to pass to the FifoXFormData that we
    // will return.
    let r1_fifo_clone = r1_fifo.clone();
    let r2_fifo_clone = r2_fifo.clone();

    let join_handle: thread::JoinHandle<Result<SeqprocStats>> = thread::spawn(move || {
        let seqproc_stats = interpret_to_pipes(
            r1,
            r2,
            r1_fifo_clone,
            r2_fifo_clone,
            1,
            additional_args,
            compiled_data,
        );

        // Explicitly check for and propagate any errors encountered in the
        // closing and deleting of the temporary directory.  The directory
        // will be deleted when the handle goes out of scope, but without
        // calling this method, any encountered errors will be silently
        // ignored.
        // see: https://docs.rs/tempfile/latest/tempfile/struct.TempDir.html#method.close
        match tmp_dir.close() {
            Ok(_) => Ok(seqproc_stats),
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