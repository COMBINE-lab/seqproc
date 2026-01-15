use std::{
    fs::File,
    io::BufWriter,
    panic,
    path::{Path, PathBuf},
    thread,
};

use antisequence::expr::fmt_expr;
use antisequence::graph::*;
use antisequence::graph::TryOp;
use anyhow::{bail, Result};
use chumsky::{error::Rich, input::Input, Parser};
use nix::sys::stat;
use nix::unistd;
use serde::Serialize;
use tempfile::tempdir;
use tracing::info;

use crate::{
    compile::{compile, CompiledData},
    demux::DemuxConfig,
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

#[derive(Debug, Serialize)]
pub struct SeqprocStats {
    pub seqproc_version: String,
    pub call: Option<String>,

    pub n_fastqs: u32,
    pub n_processed: u64,
    pub n_reads_max: u64,

    pub total_fragments: u64,
    pub failed_parsing: u64,
    pub read_length_mean: Vec<f64>,
    pub read_length_min: Vec<u64>,
    pub read_length_max: Vec<u64>,
    pub match_distance_stats: Vec<MatchDistanceStats>,
}

#[derive(Debug, Serialize)]
pub struct MatchDistanceStats {
    pub label: String,
    pub unmatched: u64,
    pub distance_histogram: Vec<DistanceBin>,
}

#[derive(Debug, Serialize)]
pub struct DistanceBin {
    pub distance: usize,
    pub count: u64,
}

pub fn interpret(
    file1: &Path,
    file2: Option<&Path>,
    out1: &Path,
    out2: &Path,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
) {
    interpret_with_unassigned(file1, file2, out1, out2, None, None, threads, additional_args, compiled_data, None);
}

/// Interpret geometry with optional unassigned output and demultiplexing support.
pub fn interpret_with_unassigned(
    file1: &Path,
    file2: Option<&Path>,
    out1: &Path,
    out2: &Path,
    unassigned1: Option<&Path>,
    unassigned2: Option<&Path>,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
    demux_config: Option<DemuxConfig>,
) {
    let additional_args = additional_args.into_iter().collect::<Vec<_>>();

    // Skip output check when demux is enabled (demux handles its own output routing)
    if demux_config.is_none() {
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
    }

    // Build main processing graph
    let mut main_graph = antisequence::graph::Graph::new();
    compiled_data.interpret(&mut main_graph, &additional_args);

    // If unassigned output is requested, wrap in TryOp
    let has_unassigned = unassigned1.is_some() || unassigned2.is_some();
    
    let mut graph = antisequence::graph::Graph::new();
    let file1_str = file1.to_str().unwrap_or("");
    
    let mut input_files = vec![file1_str];
    if let Some(f2) = file2 {
        input_files.push(f2.to_str().unwrap_or(""));
    }

    graph.add(
        antisequence::graph::InputFastqOp::from_files(input_files)
            .unwrap_or_else(|e| panic!("{e}")),
    );

    if has_unassigned {
        // Build catch graph for unassigned reads
        let mut catch_graph = antisequence::graph::Graph::new();
        let unassigned1_str = unassigned1.map(|p| p.to_str().unwrap_or("")).unwrap_or("/dev/null");
        let unassigned2_str = unassigned2.map(|p| p.to_str().unwrap_or("")).unwrap_or("/dev/null");
        
        let mut unassigned_files = vec![unassigned1_str.to_owned()];
        if file2.is_some() && !unassigned2_str.is_empty() && unassigned2_str != "/dev/null" {
            unassigned_files.push(unassigned2_str.to_owned());
        }

        if !unassigned1_str.is_empty() && unassigned1_str != "/dev/null" {
             catch_graph.add(OutputFastqFileOp::from_files(unassigned_files));
        }

        // Use TryOp to route failed reads to catch graph
        graph.add(TryOp::new(main_graph, catch_graph));
    } else {
        // No unassigned output - just add main graph nodes
        compiled_data.interpret(&mut graph, &additional_args);
    }

    // Add LookupOp for demultiplexing if configured
    if let Some(ref config) = demux_config {
        if let Err(e) = config.add_lookup_op(&mut graph) {
            tracing::error!("Failed to add demux LookupOp: {}", e);
            return;
        }
        tracing::info!("Demultiplexing enabled with label: {}", config.barcode_label);
    }

    let out1_str = out1.to_str().unwrap_or("");
    let out2_str = out2.to_str().unwrap_or("");

    // When demux is enabled, use expression-based output routing
    if let Some(ref config) = demux_config {
        // Create output directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(&config.output_dir) {
            tracing::error!("Failed to create demux output directory: {}", e);
            return;
        }

        let out_dir = config.output_dir.to_string_lossy();
        let sample_attr_path = format!("{}.{}", config.barcode_label, config.sample_attr);
        
        let out1_expr = format!("{}/{{{}}}_R1.fastq", out_dir, sample_attr_path);
        let mut out_exprs = vec![fmt_expr(out1_expr.clone())];

        if file2.is_some() {
            let out2_expr = format!("{}/{{{}}}_R2.fastq", out_dir, sample_attr_path);
            tracing::info!("Demux output: {} and {}", out1_expr, out2_expr);
            out_exprs.push(fmt_expr(out2_expr));
        } else {
            tracing::info!("Demux output: {}", out1_expr);
        }
        
        graph.add(OutputFastqFileOp::from_files(out_exprs));
    } else {
        // Standard output (no demux)
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
    }

    graph.run_with_threads(threads);
}

/// Interpret geometry with optional demultiplexing support.
pub fn interpret_with_demux(
    file1: &Path,
    file2: Option<&Path>,
    out1: &Path,
    out2: &Path,
    threads: usize,
    additional_args: Vec<&str>,
    compiled_data: CompiledData,
    demux_config: Option<DemuxConfig>,
) {
    let additional_args = additional_args.into_iter().collect::<Vec<_>>();

    // Skip output check when demux is enabled (demux handles its own output routing)
    if demux_config.is_none() {
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
    }

    let mut graph = antisequence::graph::Graph::new();
    let file1_str = file1.to_str().unwrap_or("");
    
    let mut input_files = vec![file1_str];
    if let Some(f2) = file2 {
        input_files.push(f2.to_str().unwrap_or(""));
    }

    graph.add(
        antisequence::graph::InputFastqOp::from_files(input_files)
            .unwrap_or_else(|e| panic!("{e}")),
    );

    compiled_data.interpret(&mut graph, &additional_args);

    // Add LookupOp for demultiplexing if configured
    if let Some(ref config) = demux_config {
        if let Err(e) = config.add_lookup_op(&mut graph) {
            tracing::error!("Failed to add demux LookupOp: {}", e);
            return;
        }
        tracing::info!("Demultiplexing enabled with label: {}", config.barcode_label);
    }

    let out1_str = out1.to_str().unwrap_or("");
    let out2_str = out2.to_str().unwrap_or("");

    // When demux is enabled, use expression-based output routing
    if let Some(ref config) = demux_config {
        // Create output directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(&config.output_dir) {
            tracing::error!("Failed to create demux output directory: {}", e);
            return;
        }

        // Build format expressions for dynamic file routing based on sample attribute
        // Format: {output_dir}/{sample}_R1.fastq and {output_dir}/{sample}_R2.fastq
        let out_dir = config.output_dir.to_string_lossy();
        
        // The sample attribute is set on the barcode label (e.g., seq2.bc1.sample)
        let sample_attr_path = format!("{}.{}", config.barcode_label, config.sample_attr);
        
        let out1_expr = format!("{}/{{{}}}_R1.fastq", out_dir, sample_attr_path);
        let mut out_exprs = vec![fmt_expr(out1_expr.clone())];

        if file2.is_some() {
            let out2_expr = format!("{}/{{{}}}_R2.fastq", out_dir, sample_attr_path);
            tracing::info!("Demux output: {} and {}", out1_expr, out2_expr);
            out_exprs.push(fmt_expr(out2_expr));
        } else {
            tracing::info!("Demux output: {}", out1_expr);
        }
        
        graph.add(OutputFastqFileOp::from_files(out_exprs));
    } else {
        // Standard output (no demux)
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
    
    // Handle second output stream optionally if files2 is present?
    // But this function return signature doesn't change easily.
    // And it is used by read_pairs_to_fifo which has r1_fifo and r2_fifo.
    // We should probably keep assuming paired output if called via this path,
    // OR we assume that if files2 is empty, we don't write to out2?
    // But out2 is passed as PathBuf.
    // Let's create f2 only if needed?
    // But InputFastqOp needs readers.

    let mut readers = files1
        .iter()
        .map(|f| File::open(f).expect("Failed to open file"))
        .collect::<Vec<_>>();
        
    for f in &files2 {
         readers.push(File::open(f).expect("Failed to open file"));
    }

    let additional_args = additional_args.into_iter().collect::<Vec<_>>();

    let mut graph = antisequence::graph::Graph::new();
    graph.add(
        antisequence::graph::InputFastqOp::from_readers(readers).unwrap_or_else(|e| panic!("{e}")),
    );

    compiled_data.interpret(&mut graph, &additional_args);

    let stream1 = BufWriter::new(f1);
    
    if !files2.is_empty() {
        let f2 = File::create(out2).expect("Unable to open read 2 file");
        let stream2 = BufWriter::new(f2);
        graph.add(OutputFastqOp::from_writers([stream1, stream2]));
    } else {
        graph.add(OutputFastqOp::from_writer(stream1));
    }

    graph.run_with_threads(threads);

    let input_stats = graph.input_stats();

    let (n_fastqs, n_processed, n_reads_max, read_length_min, read_length_max, read_length_mean) =
        if let Some(s) = input_stats {
            let n_fastqs = s.n_fastqs as u32;

            let mut read_length_min_u64 = Vec::with_capacity(s.n_fastqs);
            let mut read_length_max_u64 = Vec::with_capacity(s.n_fastqs);
            let mut read_length_mean_f64 = Vec::with_capacity(s.n_fastqs);

            let mut max_count = 0usize;

            for i in 0..s.n_fastqs {
                let count = *s.read_counts.get(i).unwrap_or(&0);
                if count > max_count {
                    max_count = count;
                }

                let min = *s.read_length_min.get(i).unwrap_or(&0);
                let max = *s.read_length_max.get(i).unwrap_or(&0);
                let sum = *s.read_length_sum.get(i).unwrap_or(&0);

                read_length_min_u64.push(min as u64);
                read_length_max_u64.push(max as u64);

                let mean = if count > 0 {
                    sum as f64 / (count as f64)
                } else {
                    0.0
                };
                read_length_mean_f64.push(mean);
            }

            let n_processed = max_count as u64;
            let n_reads_max = n_processed;

            (
                n_fastqs,
                n_processed,
                n_reads_max,
                read_length_min_u64,
                read_length_max_u64,
                read_length_mean_f64,
            )
        } else {
            (0, 0, 0, Vec::new(), Vec::new(), Vec::new())
        };

    let match_distance_stats = graph
        .match_distance_counts()
        .into_iter()
        .map(|c| {
            let matched_total: u64 = c.counts.iter().map(|&x| x as u64).sum();
            let unmatched = (c.total as u64).saturating_sub(matched_total);

            let distance_histogram = c
                .counts
                .into_iter()
                .enumerate()
                .filter_map(|(distance, count)| {
                    if count == 0 {
                        None
                    } else {
                        Some(DistanceBin {
                            distance,
                            count: count as u64,
                        })
                    }
                })
                .collect();

            MatchDistanceStats {
                label: c.label,
                unmatched,
                distance_histogram,
            }
        })
        .collect();

    SeqprocStats {
        seqproc_version: env!("CARGO_PKG_VERSION").to_string(),
        call: None,

        n_fastqs,
        n_processed,
        n_reads_max,

        total_fragments: n_processed,
        failed_parsing: 0,
        read_length_mean,
        read_length_min,
        read_length_max,
        match_distance_stats,
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
    in2: Option<&Path>,
    out1: &Path,
    out2: &Path,
    threads: usize,
    additional_args: Vec<&str>,
) -> Result<SeqprocStats> {
    let files1 = vec![in1.to_str().unwrap_or("").to_owned()];
    let files2 = if let Some(i2) = in2 {
        vec![i2.to_str().unwrap_or("").to_owned()]
    } else {
        vec![]
    };

    let stats = interpret_to_pipes(
        files1,
        files2,
        out1.to_path_buf(),
        out2.to_path_buf(),
        threads,
        additional_args,
        compiled_data,
    );

    Ok(stats)
}

pub fn read_pairs_to_fifo<'a: 'static>(
    compiled_data: CompiledData,
    r1: Vec<String>,
    r2: Vec<String>,
    additional_args: Vec<&'a str>,
) -> Result<FifoSeqprocData> {
    if !r2.is_empty() && r1.len() != r2.len() {
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
