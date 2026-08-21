use std::path::PathBuf;

use seqproc::{
    demux::{DemuxConfig, DemuxError},
    error::{ExecutionConfigError, GeometryStage, InputTopologyError, SeqprocError},
    execute::{compile_geom_typed, run, RunConfig},
    io_config::InputLane,
    resources::ResourceError,
};
use std::fs;
use tempfile::tempdir;

fn error_from_geometry(source: &str) -> SeqprocError {
    match compile_geom_typed(source) {
        Ok(_) => panic!("geometry unexpectedly compiled: {source}"),
        Err(error) => error,
    }
}

#[test]
fn geometry_errors_are_owned_source_located_and_stage_specific() {
    for (source, expected_stage) in [
        ("1{b[16]r:@}", GeometryStage::Lexing),
        ("1{b[16]", GeometryStage::Parsing),
        (
            "1{b<bc>[2]r:} -> 1{f[AC]<bc>}",
            GeometryStage::SemanticCompilation,
        ),
    ] {
        let error = error_from_geometry(source);
        let SeqprocError::Geometry { stage, diagnostics } = error else {
            panic!("expected a geometry error")
        };
        assert_eq!(stage, expected_stage);
        assert!(!diagnostics.is_empty());
        assert!(diagnostics.iter().all(|diagnostic| {
            diagnostic.span.start <= diagnostic.span.end && diagnostic.span.end <= source.len()
        }));
    }
}

#[test]
fn execution_configuration_errors_are_matchable_without_display_parsing() {
    let compiled = compile_geom_typed("1{r:}").unwrap();
    let mut config = RunConfig::new("unused.fastq");
    config.threads = 0;
    assert!(matches!(
        run(config, compiled),
        Err(SeqprocError::InvalidExecutionConfiguration(
            ExecutionConfigError::ThreadCount(0)
        ))
    ));
}

#[test]
fn input_topology_errors_are_matchable_before_io_starts() {
    let compiled = compile_geom_typed("1{r:}").unwrap();
    let config = RunConfig::new("unused.fastq").with_input_lanes(Vec::<InputLane>::new());
    assert!(matches!(
        run(config, compiled),
        Err(SeqprocError::InputTopology(
            InputTopologyError::MissingLanes
        ))
    ));
}

#[test]
fn missing_positional_resources_remain_typed() {
    let compiled = compile_geom_typed("1{filter_within_dist(b[4], $0, 1)r:}").unwrap();
    let config = RunConfig::new(PathBuf::from("unused.fastq"));
    assert!(matches!(
        run(config, compiled),
        Err(SeqprocError::Resource(ResourceError::MissingPositional {
            index: 0,
            supplied: 0
        }))
    ));
}

#[test]
fn malformed_demultiplexing_labels_return_errors_instead_of_panicking() {
    let directory = tempdir().unwrap();
    let input = directory.path().join("reads.fastq");
    let sample_map = directory.path().join("samples.tsv");
    fs::write(&input, "@r1\nACGT\n+\nIIII\n").unwrap();
    fs::write(&sample_map, "ACGT\tsample\n").unwrap();

    let compiled = compile_geom_typed("1{b<bc>[4]}").unwrap();
    let mut config = RunConfig::new(input);
    config.demux = Some(
        DemuxConfig::new(sample_map, "not-a-qualified-label")
            .with_output_dir(directory.path().join("must-not-exist")),
    );
    assert!(matches!(
        run(config, compiled),
        Err(SeqprocError::Demultiplex(
            DemuxError::InvalidBarcodeLabel { .. }
        ))
    ));
    assert!(!directory.path().join("must-not-exist").exists());
}

#[test]
fn malformed_fastq_returns_a_typed_runtime_error() {
    let directory = tempdir().unwrap();
    let input = directory.path().join("truncated.fastq");
    fs::write(&input, "@r1\nACGT\n+\n").unwrap();

    let compiled = compile_geom_typed("1{r:}").unwrap();
    let result = run(RunConfig::new(input), compiled);
    assert!(
        matches!(
            result,
            Err(SeqprocError::FastqInput {
                context: "runtime",
                ..
            })
        ),
        "unexpected result: {result:?}"
    );
}
