use antisequence::{
    graph::{CountOp, Graph, InputFastqOp, OutputFastqOp, TryOp},
    trace::NoTrace,
};
use seqproc::execute::compile_geom;
use std::{
    io::{Cursor, Write},
    sync::{Arc, Mutex},
};

const V2: &str = "header { efgdl = 2 }\n";

#[test]
fn ordered_choice_compiles_to_two_isolated_alternatives() {
    let geometry = format!("{V2}1{{b<bc>[2](f[AAA] | f[CCC])r<read>:}} -> 1{{<bc><read>}}");
    let compiled = compile_geom(geometry).unwrap();
    assert_eq!(compiled.geometry.len(), 1);
    assert_eq!(compiled.layout_alternatives[0].len(), 2);
    assert!(compiled.layout_report[0].used_layout_algebra);
    assert_eq!(compiled.layout_report[0].alternatives, 2);
}

#[test]
fn optional_term_and_fixed_repeat_normalize_without_runtime_combinatorics() {
    let geometry = format!("{V2}1{{(f[AAA])?x[2]*2r:}}");
    let compiled = compile_geom(geometry).unwrap();
    assert_eq!(compiled.layout_alternatives[0].len(), 2);
    assert_eq!(compiled.layout_report[0].max_segments, 4);
}

#[test]
fn layout_algebra_requires_v2_header() {
    let errors = compile_geom("1{(f[AAA] | f[CCC])r:}".to_string()).unwrap_err();
    assert!(errors
        .iter()
        .any(|error| error.to_string().contains("EFGDL 2")));
}

#[test]
fn alternatives_must_expose_compatible_labels() {
    let geometry = format!("{V2}1{{(b<short>[8]f[AAA] | b<long>[10]f[CCC])r:}}");
    let errors = compile_geom(geometry).unwrap_err();
    assert!(errors
        .iter()
        .any(|error| error.to_string().contains("same capture cardinality")));
}

#[test]
fn alternatives_can_reuse_the_same_definition_labels() {
    let geometry = format!("{V2}bc = b[8]\n1{{(<bc>f[AAA] | f[CCC]<bc>)r:}} -> 1{{<bc>}}");
    let compiled = compile_geom(geometry).unwrap();
    assert_eq!(compiled.layout_alternatives[0].len(), 2);
}

#[test]
fn normalization_rejects_exponential_expansion() {
    let mut body = String::new();
    for _ in 0..7 {
        body.push_str("(f[A] | f[C])");
    }
    body.push_str("r:");
    let errors = compile_geom(format!("{V2}1{{{body}}}")).unwrap_err();
    assert!(errors
        .iter()
        .any(|error| error.to_string().contains("limit is 64")));
}

#[test]
fn runtime_falls_back_to_later_alternative_and_rejects_nonmatches() {
    let compiled = compile_geom(format!("{V2}1{{b<bc>[2](f[AAA] | f[CCC])r:}}")).unwrap();
    let fastq = b"@first\nGGAAATT\n+\nIIIIIII\n@second\nTTCCCGG\n+\nIIIIIII\n@reject\nAACACGG\n+\nIIIIIII\n";
    let mut graph = Graph::<NoTrace>::new();
    graph.add(InputFastqOp::from_reader(Cursor::new(fastq)).unwrap());
    compiled.interpret(&mut graph, &[]);
    let count = graph.add(CountOp::new([true]));
    graph.run().unwrap();
    assert_eq!(count.counts(), [2]);
}

#[derive(Clone, Default)]
struct SharedWriter(Arc<Mutex<Vec<u8>>>);

impl Write for SharedWriter {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().write(bytes)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn run_layout_graph(graph: impl FnOnce(&mut Graph<NoTrace>), fastq: &[u8]) -> Vec<u8> {
    let output = SharedWriter::default();
    let bytes = Arc::clone(&output.0);
    let mut runtime = Graph::<NoTrace>::new();
    runtime.add(InputFastqOp::from_reader(Cursor::new(fastq.to_vec())).unwrap());
    graph(&mut runtime);
    runtime.add(OutputFastqOp::from_writer(output));
    runtime.try_run_with_threads(1).unwrap();
    let result = bytes.lock().unwrap().clone();
    result
}

#[test]
fn layout_choice_is_byte_identical_to_manually_expanded_try_graph() {
    let fastq = b"@first\nGGAAATT\n+\nIIIIIII\n@second\nTTCCCGG\n+\nIIIIIII\n@reject\nAACACGG\n+\nIIIIIII\n";
    let algebra = compile_geom(format!("{V2}1{{b<bc>[2](f[AAA] | f[CCC])r<read>:}}")).unwrap();
    let algebra_bytes = run_layout_graph(|graph| algebra.interpret(graph, &[]), fastq);

    let first = compile_geom("1{b<bc>[2]f[AAA]r<read>:}".to_string()).unwrap();
    let second = compile_geom("1{b<bc>[2]f[CCC]r<read>:}".to_string()).unwrap();
    let manual_bytes = run_layout_graph(
        |graph| {
            let mut first_arm = Graph::<NoTrace>::new();
            first.interpret(&mut first_arm, &[]);
            let mut second_arm = Graph::<NoTrace>::new();
            second.interpret(&mut second_arm, &[]);
            graph.add(TryOp::new(first_arm, second_arm).return_catch_output());
        },
        fastq,
    );

    assert_eq!(algebra_bytes, manual_bytes);
}

#[test]
fn v2_without_new_features_is_byte_identical_to_headerless_protocol() {
    let fastq = b"@legacy\nAACCGGTTACGT\n+\nIIIIIIIIIIII\n";
    let legacy =
        compile_geom("1{b<bc>[4]u<umi>[4]r<read>:}->1{<bc><umi><read>}".to_string()).unwrap();
    let v2 = compile_geom(format!(
        "{V2}1{{b<bc>[4]u<umi>[4]r<read>:}}->1{{<bc><umi><read>}}"
    ))
    .unwrap();
    let legacy_bytes = run_layout_graph(|graph| legacy.interpret(graph, &[]), fastq);
    let v2_bytes = run_layout_graph(|graph| v2.interpret(graph, &[]), fastq);
    assert_eq!(legacy_bytes, v2_bytes);
}

#[test]
fn repeated_named_captures_are_lowered_and_addressable_by_index() {
    let geometry = format!(
        "{V2}1{{(b<bc>[2])*2r<read>:}} -> #[header = append(\" second:\", <bc[2]>)] 1{{<bc[2]>f[TT]<bc[1]>}}"
    );
    let compiled = compile_geom(geometry).unwrap();
    let captures = &compiled.capture_registry["bc"];
    assert_eq!(captures.len(), 2);
    assert_eq!(captures[0].index, 1);
    assert_eq!(captures[1].index, 2);
    assert_ne!(captures[0].physical_label, captures[1].physical_label);

    let output = run_layout_graph(
        |graph| compiled.interpret(graph, &[]),
        b"@indexed\nAACCAG\n+\nIIIIII\n",
    );
    let output = String::from_utf8(output).unwrap();
    assert!(
        output.starts_with("@indexed second:CC\nCCTTAA\n+\n"),
        "{output}"
    );
}

#[test]
fn repeated_capture_requires_index_and_checks_bounds() {
    let unindexed = compile_geom(format!("{V2}1{{(b<bc>[2])*2}} -> 1{{<bc>}}")).unwrap_err();
    assert!(unindexed
        .iter()
        .any(|error| error.to_string().contains("has 2 occurrences")));

    let zero = compile_geom(format!("{V2}1{{(b<bc>[2])*2}} -> 1{{<bc[0]>}}")).unwrap_err();
    assert!(zero
        .iter()
        .any(|error| error.to_string().contains("one-based")));

    let out_of_bounds = compile_geom(format!("{V2}1{{(b<bc>[2])*2}} -> 1{{<bc[3]>}}")).unwrap_err();
    assert!(out_of_bounds
        .iter()
        .any(|error| error.to_string().contains("out of bounds")));
}

#[test]
fn alternatives_require_equal_repeated_capture_cardinality() {
    let errors = compile_geom(format!("{V2}1{{((b<bc>[2])*2 | b<bc>[2])r:}}")).unwrap_err();
    assert!(errors
        .iter()
        .any(|error| error.to_string().contains("same capture cardinality")));
}
