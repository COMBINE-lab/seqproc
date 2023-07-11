use std::ops::RangeBounds;

use antisequence::{
    expr::{Label, SelectorExpr, TransformExpr},
    *,
};

pub fn cut(
    pipeline: Box<dyn Reads>,
    starting_label: String,
    label: &str,
    index: EndIdx,
) -> Box<dyn Reads> {
    let transform_expression =
        TransformExpr::new(format!("{0} -> {1}_l, {1}_r", starting_label, label).as_bytes())
            .unwrap();

    pipeline.cut(sel!(), transform_expression, index).boxed()
}

pub fn pad(
    read: Box<dyn antisequence::Reads>,
    labels: Vec<Label>,
    to_length: usize,
) -> Box<dyn antisequence::Reads> {
    read.pad(sel!(), labels, to_length).boxed()
}

pub fn trim(
    read: Box<dyn antisequence::Reads>,
    labels: Vec<Label>,
) -> Box<dyn antisequence::Reads> {
    read.trim(sel!(), labels).boxed()
}

pub fn trim_leftover(
    read: Box<dyn antisequence::Reads>,
    labels: &mut [&str],
) -> Box<dyn antisequence::Reads> {
    let labels = vec![Label::new(labels.join("_").as_bytes()).unwrap()];

    trim(read, labels)
}

pub fn make_label(prefix: &str, suffix: &str) -> Label {
    Label::new(format!("{prefix}_{suffix}").as_bytes()).unwrap()
}

pub fn validate_length<B>(pipeline: Box<dyn Reads>, label: &str, bound: B) -> Box<dyn Reads>
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    let transform_expression =
        TransformExpr::new(format!("{0}_l -> {0}_l.v_len", label).as_bytes()).unwrap();
    let selector_expression = SelectorExpr::new(format!("{}_l.v_len", label).as_bytes()).unwrap();

    pipeline
        .length_in_bounds(sel!(), transform_expression, bound)
        .retain(selector_expression)
        .boxed()
}

pub fn process_sequence(
    pipeline: Box<dyn Reads>,
    sequence: String,
    starting_label: String,
    label: &str,
    match_type: iter::MatchType,
) -> Box<dyn Reads> {
    let transform = match match_type {
        PrefixAln { .. } => TransformExpr::new(
            format!("{0} -> {1}_anchor, {1}_r", starting_label, label).as_bytes(),
        ),
        LocalAln { .. } | BoundedAln { .. } => TransformExpr::new(
            format!("{0} -> {1}_l, {1}_anchor, {1}_r", starting_label, label).as_bytes(),
        ),
        _ => panic!(
            "Currently supports Local and Prefix alignment, found: {:?}",
            match_type
        ),
    };

    match transform {
        Ok(t) => {
            let pattern =
                format!("\n    name: _anchor\n    patterns:\n        - pattern: \"{sequence}\"\n");

            let selector_expression =
                SelectorExpr::new(format!("{label}_anchor").as_bytes()).unwrap();

            let labels = vec![Label::new(format!("{}_anchor", label).as_bytes()).unwrap()];

            trim(
                pipeline
                    .match_any(sel!(), t, pattern, match_type)
                    .retain(selector_expression)
                    .boxed(),
                labels,
            )
        }
        Err(e) => panic!("{e}"),
    }
}
