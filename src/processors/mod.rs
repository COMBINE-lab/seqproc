use std::ops::{Bound, RangeBounds};

use antisequence::{
    expr::{Label, SelectorExpr, TransformExpr},
    *,
};

use crate::interpret::BoxedReads;

fn set(read: BoxedReads, sel_expr: SelectorExpr, label: Label, transform: String) -> BoxedReads {
    read.set(sel_expr, label, transform).boxed()
}

fn cut(
    read: BoxedReads,
    sel_expr: SelectorExpr,
    tr_expr: TransformExpr,
    index: EndIdx,
) -> BoxedReads {
    read.cut(sel_expr, tr_expr, index).boxed()
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

fn validate_length<B>(
    read: BoxedReads,
    sel_expr: SelectorExpr,
    tr_expr: TransformExpr,
    r_sel_expr: SelectorExpr,
    bound: B,
) -> BoxedReads
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    read.length_in_bounds(sel_expr, tr_expr, bound)
        .retain(r_sel_expr)
        .boxed()
}

pub fn process_sequence(
    pipeline: Box<dyn Reads>,
    sequence: String,
    starting_label: String,
    this_label: String,
    prev_label: String,
    next_label: String,
    match_type: iter::MatchType,
) -> Box<dyn Reads> {
    let tr_expr = match match_type {
        PrefixAln { .. } => TransformExpr::new(
            format!("{} -> {}, {}", starting_label, this_label, next_label).as_bytes(),
        )
        .unwrap(),
        ExactSearch | HammingSearch(_) => TransformExpr::new(
            format!(
                "{} -> {}, {}, {}",
                starting_label, prev_label, this_label, next_label
            )
            .as_bytes(),
        )
        .unwrap(),
        _ => unreachable!(),
    };

    let sel_expr = SelectorExpr::new(starting_label.as_bytes()).unwrap();

    pipeline
        .match_one(sel_expr, tr_expr, sequence, match_type)
        .boxed()
}

fn process_sized<B>(
    read: BoxedReads,
    init_label: String,
    this_label: String,
    next_label: String,
    range: B,
) -> BoxedReads
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    let cut_sel_expr = SelectorExpr::new(init_label.as_bytes()).unwrap();
    let cut_tr_expr =
        TransformExpr::new(format!("{init_label} -> {this_label}, {next_label}").as_bytes())
            .unwrap();

    let end = match RangeBounds::<usize>::end_bound(&range) {
        Bound::Included(end) => *end,
        _ => unreachable!(),
    };
    let cut_read = cut(read, cut_sel_expr, cut_tr_expr, LeftEnd(end));

    let len_sel_expr = SelectorExpr::new(this_label.as_bytes()).unwrap();
    let len_tr_expr =
        TransformExpr::new(format!("{this_label} -> {this_label}.v_len").as_bytes()).unwrap();
    let r_sel_expr = SelectorExpr::new(format!("{this_label}.v_len").as_bytes()).unwrap();

    let val_len_read = validate_length(cut_read, len_sel_expr, len_tr_expr, r_sel_expr, range);

    val_len_read
}

pub fn process_fixed_len(
    read: BoxedReads,
    init_label: String,
    this_label: String,
    next_label: String,
    len: usize,
) -> BoxedReads {
    process_sized(read, init_label, this_label, next_label, len..=len)
}

pub fn process_ranged_len<B>(
    read: BoxedReads,
    init_label: String,
    this_label: String,
    next_label: String,
    range: B,
) -> BoxedReads
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    process_sized(read, init_label, this_label, next_label, range)
}

pub fn process_unbounded(read: BoxedReads, init_label: String, this_label: String) -> BoxedReads {
    // set init_label to this_label
    // cut left end 0
    let sel_expr = SelectorExpr::new(init_label.as_bytes()).unwrap();
    let cut_tr_expr =
        TransformExpr::new(format!("{init_label} -> _, {this_label}").as_bytes()).unwrap();
    let label = Label::new(init_label.as_bytes()).unwrap();
    let tr = format!("{{{this_label}}}");

    let cut_read = cut(read, sel_expr.clone(), cut_tr_expr, LeftEnd(0));

    set(cut_read, sel_expr, label, tr)
}
