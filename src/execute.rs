use std::{
    fs::File,
    io::BufWriter,
    panic,
    path::{Path, PathBuf},
    thread,
};

use antisequence::graph::*;
use anyhow::{bail, Result};
use chumsky::{error::Rich, input::Input, Parser};
use nix::sys::stat;
use nix::unistd;
use tempfile::tempdir;
use tracing::info;

use crate::{
    compile::{compile, CompiledData},
    error::parse_failure,
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
    file1: &Path,
    file2: &Path,
    out1: &Path,
    out2: &Path,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
) {
    let additional_args = additional_args.into_iter().collect::<Vec<_>>();

    if let Some(transformations) = &compiled_data.transformation {
        if transformations.len() == 2
            && (out1.as_os_str().is_empty() || out2.as_os_str().is_empty())
        {
            tracing::error!(
                "You defined a transformation into two files - you must provide two outputs"
            );
            return;
        }
    }

    let mut graph = antisequence::graph::Graph::new();
    let file1_str = file1.to_str().unwrap_or("");
    let file2_str = file2.to_str().unwrap_or("");
    graph.add(
        antisequence::graph::InputFastqOp::from_files([file1_str, file2_str])
            .unwrap_or_else(|e| panic!("{e}")),
    );

    compiled_data.interpret(&mut graph, &additional_args);

    let out1_str = out1.to_str().unwrap_or("");
    let out2_str = out2.to_str().unwrap_or("");

    match (out1_str, out2_str) {
        ("", "") => {
            graph.add(OutputFastqFileOp::from_file("/dev/null"));
        }
        (out1_str, "") => {
            graph.add(OutputFastqFileOp::from_file(out1_str.to_owned()));
        }
        (out1_str, out2_str) => {
            graph.add(OutputFastqFileOp::from_files([
                out1_str.to_owned(),
                out2_str.to_owned(),
            ]));
        }
    }

    graph.run_with_threads(threads);
}

fn interpret_to_pipes(
    files1: Vec<String>,
    files2: Vec<String>,
    out1: PathBuf,
    out2: PathBuf,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
) -> SeqprocStats {
    let f1 = File::create(out1).expect("Unable to open read 1 file");
    let f2 = File::create(out2).expect("Unable to open read 2 file");

    let stream1 = BufWriter::new(f1);
    let stream2 = BufWriter::new(f2);

    let readers = files1
        .iter()
        .chain(files2.iter())
        .map(|f| File::open(f).expect("Failed to open file"));

    let additional_args = additional_args.into_iter().collect::<Vec<_>>();

    let mut graph = antisequence::graph::Graph::new();
    graph.add(
        antisequence::graph::InputFastqOp::from_readers(readers).unwrap_or_else(|e| panic!("{e}")),
    );

    compiled_data.interpret(&mut graph, &additional_args);

    graph.add(OutputFastqOp::from_writers([stream1, stream2]));

    graph.run_with_threads(threads);

    SeqprocStats {
        total_fragments: 0,
        failed_parsing: 0,
    }
}

pub fn compile_geom(geom: String) -> Result<CompiledData, Vec<Rich<'static, String>>> {
    // lex input
    let tokens = lexer::lexer()
        .parse(&geom)
        .into_result()
        .unwrap_or_else(|errs| parse_failure(&errs[0], geom.clone()));

    let tokens = tokens
        .into_iter()
        .map(|(tok, span)| chumsky::span::Spanned { inner: tok, span })
        .collect::<Vec<_>>();
    let input = tokens[..].split_spanned((0..geom.len()).into());

    // parse token
    let description = parser()
        .parse(input)
        .into_result()
        .unwrap_or_else(|errs| parse_failure(&errs[0], geom.clone()));

    // compile ast
    compile(description)
        .map_err(|e| parse_failure(&Rich::<String>::custom(e.span, e.msg), geom.clone()))
}

pub fn read_pairs_to_file(
    compiled_data: CompiledData,
    in1: &Path,
    in2: &Path,
    out1: &Path,
    out2: &Path,
    threads: usize,
    additional_args: Vec<&str>,
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

pub fn read_pairs_to_fifo<'a: 'static>(
    compiled_data: CompiledData,
    r1: Vec<String>,
    r2: Vec<String>,
    additional_args: Vec<&'a str>,
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
            6, // default to 6 threads
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
