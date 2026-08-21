use antisequence::{graph::Graph, AmbiguityPolicy, PositionAmbiguityPolicy};
use seqproc::{
    compile::functions::CompiledFunction,
    execute::{compile_geom, read_pairs_to_file},
};
use std::fs;

fn anchor_geometry() -> String {
    "header { efgdl = 2 }
#[edit(1)]
#[ambig_policy = first]
#[position_policy = rightmost]
#[anchor_set($0)]
#[search(relative)]
linker = f[AAAA]
1{b<bc>[2]<linker>r<read>:}
-> 1{<bc><read>}"
        .to_string()
}

#[test]
fn anchor_set_and_both_ambiguity_axes_compile_independently() {
    let compiled = compile_geom(anchor_geometry()).unwrap();
    let linker = compiled.geometry[0]
        .iter()
        .find(|piece| piece.get_label().as_deref() == Some("linker"))
        .unwrap();
    assert!(linker.stack.iter().any(|function| {
        matches!(
            function.0,
            CompiledFunction::AmbiguityPolicy(AmbiguityPolicy::First)
        )
    }));
    assert!(linker.stack.iter().any(|function| {
        matches!(
            function.0,
            CompiledFunction::PositionAmbiguityPolicy(PositionAmbiguityPolicy::Rightmost)
        )
    }));
    assert!(linker.stack.iter().any(|function| {
        matches!(
            function.0,
            CompiledFunction::AnchorSet(seqproc::parser::ResourceRef::Positional(0))
        )
    }));
    assert!(linker
        .stack
        .iter()
        .any(|function| matches!(function.0, CompiledFunction::Edit(1))));
}

#[test]
fn anchor_set_is_loaded_once_while_building_the_graph() {
    let path = std::env::temp_dir().join(format!(
        "seqproc-anchor-set-{}-{}.txt",
        std::process::id(),
        std::thread::current().name().unwrap_or("test")
    ));
    std::fs::write(&path, "AAAA\nCCCC\nCCCC\n").unwrap();

    let compiled = compile_geom(anchor_geometry()).unwrap();
    let mut graph = Graph::new();
    compiled.interpret(&mut graph, &[path.to_str().unwrap()]);

    std::fs::remove_file(path).ok();
}

#[test]
fn position_policy_requires_a_search_anchor() {
    let errors = compile_geom(
        "header { efgdl = 2 }
#[anchor_set($0)]
#[position_policy = rightmost]
linker = f[AAAA]
1{<linker>r:}"
            .to_string(),
    )
    .unwrap_err();
    assert!(errors
        .iter()
        .any(|error| error.to_string().contains("requires #[search(relative)]")));
}

#[test]
fn best_is_an_explicit_spelling_of_global_best_then_leftmost() {
    let compiled = compile_geom(
        "header { efgdl = 2 }
#[hamming(1)]
#[position_policy = best]
#[search(relative)]
linker = f[AAAA]
1{<linker>r:}"
            .to_string(),
    )
    .unwrap();
    let linker = compiled.geometry[0]
        .iter()
        .find(|piece| piece.get_label().as_deref() == Some("linker"))
        .unwrap();
    assert!(linker.stack.iter().any(|function| {
        matches!(
            function.0,
            CompiledFunction::PositionAmbiguityPolicy(PositionAmbiguityPolicy::Leftmost)
        )
    }));
}

#[test]
fn position_quality_arguments_compile_for_hamming_search() {
    let compiled = compile_geom(
        "header { efgdl = 2 }
#[hamming(1)]
#[position_policy = quality(min_delta = 7)]
#[search(relative)]
linker = f[AAAA]
1{<linker>r:}"
            .to_string(),
    )
    .unwrap();
    let linker = compiled.geometry[0]
        .iter()
        .find(|piece| piece.get_label().as_deref() == Some("linker"))
        .unwrap();
    assert!(linker.stack.iter().any(|function| {
        matches!(
            function.0,
            CompiledFunction::PositionAmbiguityPolicy(PositionAmbiguityPolicy::Quality {
                min_delta: 7
            })
        )
    }));
}

#[test]
fn position_quality_rejects_edit_search() {
    let errors = compile_geom(
        "header { efgdl = 2 }
#[edit(1)]
#[position_policy = quality]
#[search(relative)]
linker = f[AAAA]
1{<linker>r:}"
            .to_string(),
    )
    .unwrap_err();
    assert!(errors.iter().any(|error| error
        .to_string()
        .contains("edit-distance gap qualities are not defined")));
}

#[test]
fn anchor_set_quality_requires_exact_or_hamming_search() {
    let errors = compile_geom(
        "header { efgdl = 2 }
#[edit(1)]
#[ambig_policy = quality]
#[anchor_set($0)]
#[search(relative)]
linker = f[AAAA]
1{<linker>r:}"
            .to_string(),
    )
    .unwrap_err();
    assert!(errors.iter().any(|error| error
        .to_string()
        .contains("edit-distance gap qualities are not defined")));
}

fn run_anchor_pattern_policy(policy: &str) -> Result<Vec<String>, String> {
    let temporary = tempfile::tempdir().unwrap();
    let input = temporary.path().join("input.fastq");
    let anchors = temporary.path().join("anchors.txt");
    let output = temporary.path().join("output.fastq");
    let unused = temporary.path().join("unused.fastq");
    fs::write(&input, "@read1\nTCGTNNNNTCGA\n+\nIIIIIIII!III\n").unwrap();
    fs::write(&anchors, "ACGT\nACGA\n").unwrap();
    let geometry = format!(
        "header {{ efgdl = 2 }}
#[hamming(1)]
#[ambig_policy = {policy}]
#[anchor_set($0)]
#[search(relative)]
linker = f[AAAA]
1{{<linker>r:}}
-> 1{{<linker>}}"
    );
    let compiled = compile_geom(geometry).map_err(|errors| format!("{errors:?}"))?;
    let anchors_path = anchors.to_string_lossy().into_owned();
    read_pairs_to_file(
        compiled,
        &input,
        None,
        &output,
        &unused,
        1,
        vec![anchors_path.as_str()],
    )
    .map_err(|error| error.to_string())?;
    let text = fs::read_to_string(output).map_err(|error| error.to_string())?;
    Ok(text
        .lines()
        .enumerate()
        .filter_map(|(index, line)| (index % 4 == 1).then_some(line.to_owned()))
        .collect())
}

#[test]
fn every_anchor_set_pattern_ambiguity_policy_executes_as_documented() {
    assert_eq!(run_anchor_pattern_policy("accept").unwrap(), ["TCGT"]);
    assert_eq!(run_anchor_pattern_policy("first").unwrap(), ["TCGT"]);
    assert!(run_anchor_pattern_policy("no_match").unwrap().is_empty());
    assert_eq!(
        run_anchor_pattern_policy("quality(min_delta = 1)").unwrap(),
        ["TCGA"]
    );

    let random_once = run_anchor_pattern_policy("random(seed = 2026)").unwrap();
    let random_twice = run_anchor_pattern_policy("random(seed = 2026)").unwrap();
    assert_eq!(random_once, random_twice);
    assert!(matches!(random_once.as_slice(), [value] if value == "TCGT" || value == "TCGA"));

    let error = run_anchor_pattern_policy("error").unwrap_err();
    assert!(error.contains("ambiguous equal-best match"));
}

fn run_anchor_position_policy(policy: &str) -> Result<Vec<String>, String> {
    let temporary = tempfile::tempdir().unwrap();
    let input = temporary.path().join("input.fastq");
    let anchors = temporary.path().join("anchors.txt");
    let output = temporary.path().join("output.fastq");
    let unused = temporary.path().join("unused.fastq");
    fs::write(&input, "@read1\nTAAAXTAAAEND\n+\nIIIII!IIIIII\n").unwrap();
    fs::write(&anchors, "AAAA\n").unwrap();
    let geometry = format!(
        "header {{ efgdl = 2 }}
#[hamming(1)]
#[position_policy = {policy}]
#[anchor_set($0)]
#[search(relative)]
linker = f[AAAA]
1{{<linker>r<tail>:}}
-> 1{{<tail>}}"
    );
    let compiled = compile_geom(geometry).map_err(|errors| format!("{errors:?}"))?;
    let anchors_path = anchors.to_string_lossy().into_owned();
    read_pairs_to_file(
        compiled,
        &input,
        None,
        &output,
        &unused,
        1,
        vec![anchors_path.as_str()],
    )
    .map_err(|error| error.to_string())?;
    let text = fs::read_to_string(output).map_err(|error| error.to_string())?;
    Ok(text
        .lines()
        .enumerate()
        .filter_map(|(index, line)| (index % 4 == 1).then_some(line.to_owned()))
        .collect())
}

#[test]
fn every_anchor_set_position_ambiguity_policy_executes_as_documented() {
    assert_eq!(run_anchor_position_policy("best").unwrap(), ["XTAAAEND"]);
    assert_eq!(
        run_anchor_position_policy("leftmost").unwrap(),
        ["XTAAAEND"]
    );
    assert_eq!(run_anchor_position_policy("rightmost").unwrap(), ["ND"]);
    assert_eq!(
        run_anchor_position_policy("quality(min_delta = 1)").unwrap(),
        ["END"]
    );
    assert!(run_anchor_position_policy("no_match").unwrap().is_empty());

    let error = run_anchor_position_policy("error").unwrap_err();
    assert!(error.contains("multiple equal-best positions"));
}
