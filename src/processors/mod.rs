use std::ops::{Bound, RangeBounds};

use antisequence::{
    expr::{Label, SelectorExpr, TransformExpr},
    *,
};

use crate::interpret::BoxedReads;

fn get_selector(label: String, attr: String) -> SelectorExpr {
    if attr.is_empty() {
        return SelectorExpr::new(label.as_bytes()).unwrap();
    }
    SelectorExpr::new(format!("{}.{}", label, attr).as_bytes()).unwrap()
}

pub fn set(
    read: BoxedReads,
    sel_expr: SelectorExpr,
    label: String,
    transform: String,
) -> BoxedReads {
    let label = Label::new(label.as_bytes()).unwrap();

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

pub fn remove(read: BoxedReads, label: String, attr: String) -> BoxedReads {
    let sel_expr = get_selector(label.clone(), attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.trim(sel_expr, vec![label]).boxed()
}

pub fn pad(read: BoxedReads, label: String, attr: String, by: EndIdx) -> BoxedReads {
    let sel_expr = get_selector(label.clone(), attr);
    let label = Label::new(label.as_bytes()).unwrap();

    // this is a pad by function
    read.pad(sel_expr, vec![label], by, b'A').boxed()
}

pub fn truncate(read: BoxedReads, label: String, attr: String, by: EndIdx) -> BoxedReads {
    let sel_expr = get_selector(label.clone(), attr);
    let label = Label::new(label.as_bytes()).unwrap();

    // this is a truncate by function
    read.trunc(sel_expr, vec![label], by).boxed()
}

pub fn reverse(read: BoxedReads, label: String, attr: String) -> BoxedReads {
    let sel_expr = get_selector(label.clone(), attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.reverse(sel_expr, vec![label]).boxed()
}

pub fn reverse_comp(read: BoxedReads, label: String, attr: String) -> BoxedReads {
    let sel_expr = get_selector(label.clone(), attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.revcomp(sel_expr, vec![label]).boxed()
}

pub fn normalize<B>(read: BoxedReads, label: String, attr: String, range: B) -> BoxedReads
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    let sel_expr = get_selector(label.clone(), attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.norm(sel_expr, label, range).boxed()
}

pub fn filter(
    read: BoxedReads,
    label: String,
    attr: String,
    filename: String,
    mismatch: usize,
) -> BoxedReads {
    let sel_expr = get_selector(label.clone(), attr);
    let sel_retain_expr = get_selector(label.clone(), "_f".to_string());

    let tr_expr = TransformExpr::new(format!("{0} -> {0}._f", label).as_bytes()).unwrap();

    read.filter(sel_expr, tr_expr, filename, mismatch)
        .retain(sel_retain_expr)
        .boxed()
}

pub fn map(
    read: BoxedReads,
    label: String,
    attr: String,
    file: String,
    mismatch: usize,
) -> BoxedReads {
    let sel_expr = get_selector(label.clone(), attr);
    let tr_expr = TransformExpr::new(format!("{0} -> {0}.not_mapped", label).as_bytes()).unwrap();

    read.map(sel_expr, tr_expr, file, mismatch).boxed()
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
    let r_sel_expr = SelectorExpr::new(this_label.as_bytes()).unwrap();
    println!("{:?}", match_type);
    pipeline
        .match_one(sel_expr, tr_expr, sequence, match_type)
        .retain(r_sel_expr)
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

    let tr = format!("{{{this_label}}}");

    let cut_read = cut(read, sel_expr.clone(), cut_tr_expr, LeftEnd(0));

    set(cut_read, sel_expr, init_label, tr)
}

pub fn process_ranged_len_no_cut<B>(read: BoxedReads, this_label: String, range: B) -> BoxedReads
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    let len_sel_expr = SelectorExpr::new(this_label.as_bytes()).unwrap();
    let len_tr_expr =
        TransformExpr::new(format!("{this_label} -> {this_label}.v_len").as_bytes()).unwrap();
    let r_sel_expr = SelectorExpr::new(format!("{this_label}.v_len").as_bytes()).unwrap();

    validate_length(read, len_sel_expr, len_tr_expr, r_sel_expr, range)
}

pub fn process_unbounded_no_cut(
    read: BoxedReads,
    init_label: String,
    this_label: String,
) -> BoxedReads {
    // set init_label to this_label
    // cut left end 0
    let sel_expr = SelectorExpr::new(init_label.as_bytes()).unwrap();

    let tr = format!("{{{this_label}}}");

    set(read, sel_expr, init_label, tr)
}
