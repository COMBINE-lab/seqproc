use antisequence::{
    graph::{CountOp, Graph, InputFastqOp},
    trace::NoTrace,
};
use seqproc::execute::compile_geom;
use std::io::Cursor;

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
        .any(|error| error.to_string().contains("same labels")));
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
