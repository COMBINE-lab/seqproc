use antisequence::{graph::Graph, AmbiguityPolicy, PositionAmbiguityPolicy};
use seqproc::{compile::functions::CompiledFunction, execute::compile_geom};

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
        matches!(function.0, CompiledFunction::AnchorSet(ref path) if path == "0")
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
