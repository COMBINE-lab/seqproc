use crate::{geometry::compile::functions::CompiledFunction, parser::IntervalShape};

use std::ops::{Bound, RangeBounds};

use antisequence::{
    expr::{label, Label, SelectorExpr, TransformExpr},
    node::*,
    *,
};
use chumsky::chain::Chain;

use crate::{interpret::BoxedReads, Nucleotide};

impl CompiledFunction {
    fn to_expr(
        self,
        interval_name: &[u8],
        meta_data: Option<IntervalShape>,
    ) -> antisequence::expr::Expr {
        use antisequence::expr::Expr;
        match self {
            CompiledFunction::Reverse => Expr::from(label(interval_name)).rev(),
            CompiledFunction::ReverseComp => Expr::from(label(interval_name)).revcomp(),
            // trunc by
            CompiledFunction::Truncate(n) => Expr::from(label(interval_name)).slice(..=(-1 * n)),
            CompiledFunction::TruncateLeft(n) => Expr::from(label(interval_name)).slice((-1 * n)..),
            // trunc to
            CompiledFunction::TruncateTo(n) => Expr::from(label(interval_name)).slice(..=n),
            CompiledFunction::TruncateToLeft(n) => Expr::from(label(interval_name))
                .slice(label(interval_name).len().sub(Expr::from(n))..),
            CompiledFunction::Pad(n, nuc) => {
                Expr::from(label(interval_name)).concat(Expr::bytes(nuc as u8).repeat(n))
            }
            CompiledFunction::PadLeft(n, nuc) => Expr::bytes(nuc as u8)
                .repeat(n)
                .concat(Expr::from(label(interval_name))),
            CompiledFunction::PadTo(n, nuc) => Expr::from(label(interval_name)).pad(
                Expr::bytes(nuc as u8),
                Expr::from(n),
                End::Right,
            ),
            CompiledFunction::PadToLeft(n, nuc) => Expr::from(label(interval_name)).pad(
                Expr::bytes(nuc as u8),
                Expr::from(n),
                End::Left,
            ),
            CompiledFunction::Normalize => {
                let range = if let Some(r) = meta_data {
                    match r {
                        IntervalShape::RangedLen(r) => r.0..=r.1,
                        _ => panic!("Expected a ranged length"),
                    }
                };
                Expr::from(label(interval_name)).normalize(range)
            }
            // these cannot be exprs
            CompiledFunction::Remove => todo!(),
            CompiledFunction::Map(_, _) => todo!(),
            CompiledFunction::MapWithMismatch(_, _, _) => todo!(),
            CompiledFunction::FilterWithinDist(_, _) => todo!(),
            CompiledFunction::Hamming(_) => todo!(),
        }
    }
}

fn get_interval(label: &str, attr: &str) -> &'static [u8] {
    if attr.is_empty() {
        label.as_bytes()
    } else {
        format!("{label}_{attr}").as_bytes()
    }
}

fn cut_node(tr_expr: TransformExpr, index: EndIdx) -> Box<dyn antisequence::GraphNode> {
    CutNode::new(tr_expr, index)
}

fn set_node(label_name: &str, expr: antisequence::expr::Expr) -> Box<dyn antisequence::GraphNode> {
    SetNode::new(label(label_name), expr)
}

fn retain_node(expr: antisequence::expr::Expr) -> Box<dyn antisequence::GraphNode> {
    RetainNode::new(expr)
}


fn get_selector(label: &str, attr: &str) -> SelectorExpr {
    if attr.is_empty() {
        return SelectorExpr::new(label.as_bytes()).unwrap();
    }
    SelectorExpr::new(format!("{label}.{attr}").as_bytes()).unwrap()
}

pub fn set(read: BoxedReads, sel_expr: SelectorExpr, label: &str, transform: &str) -> BoxedReads {
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

pub fn remove(read: BoxedReads, label: &str, attr: &str) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.trim(sel_expr, vec![label]).boxed()
}

pub fn pad_by(
    read: BoxedReads,
    label: &str,
    attr: &str,
    by: EndIdx,
    nuc: Nucleotide,
) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let a_label = Label::new(label.as_bytes()).unwrap();

    match by {
        LeftEnd(n) => read
            .set(sel_expr, a_label, format!("{{'{nuc}';{n}}}{{{label}}}"))
            .boxed(),
        RightEnd(n) => read
            .set(sel_expr, a_label, format!("{{{label}}}{{'{nuc}';{n}}}"))
            .boxed(),
    }
}

pub fn pad_to(
    read: BoxedReads,
    label: &str,
    attr: &str,
    to: EndIdx,
    nuc: Nucleotide,
) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.pad(sel_expr, vec![label], to, nuc as u8).boxed()
}

pub fn truncate_by(read: BoxedReads, label: &str, attr: &str, by: EndIdx) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.trunc_by(sel_expr, vec![label], by).boxed()
}

pub fn truncate_to(read: BoxedReads, label: &str, attr: &str, to: EndIdx) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.trunc_to(sel_expr, vec![label], to).boxed()
}

pub fn reverse(read: BoxedReads, label: &str, attr: &str) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.reverse(sel_expr, vec![label]).boxed()
}

pub fn reverse_comp(read: BoxedReads, label: &str, attr: &str) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.revcomp(sel_expr, vec![label]).boxed()
}

pub fn normalize<B>(read: BoxedReads, label: &str, attr: &str, range: B) -> BoxedReads
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    let sel_expr = get_selector(label, attr);
    let label = Label::new(label.as_bytes()).unwrap();

    read.norm(sel_expr, label, range).boxed()
}

pub fn filter(
    read: BoxedReads,
    label: &str,
    attr: &str,
    filename: String,
    mismatch: usize,
) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let sel_retain_expr = get_selector(label, "_f");

    let tr_expr = TransformExpr::new(format!("{label} -> {label}._f").as_bytes()).unwrap();

    read.filter(sel_expr, tr_expr, filename, mismatch)
        .retain(sel_retain_expr)
        .boxed()
}

pub fn map(read: BoxedReads, label: &str, attr: &str, file: String, mismatch: usize) -> BoxedReads {
    let sel_expr = get_selector(label, attr);
    let tr_expr = TransformExpr::new(format!("{label} -> {label}.not_mapped").as_bytes()).unwrap();

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
    sequence: &[Nucleotide],
    starting_label: &str,
    this_label: &str,
    prev_label: &str,
    next_label: &str,
    match_type: iter::MatchType,
) -> Box<dyn Reads> {
    let tr_expr = match match_type {
        PrefixAln { .. } => {
            TransformExpr::new(format!("{starting_label} -> {this_label}, {next_label}").as_bytes())
                .unwrap()
        }
        ExactSearch | HammingSearch(_) => TransformExpr::new(
            format!("{starting_label} -> {prev_label}, {this_label}, {next_label}").as_bytes(),
        )
        .unwrap(),
        _ => unreachable!(),
    };

    let sel_expr = SelectorExpr::new(starting_label.as_bytes()).unwrap();
    let r_sel_expr = SelectorExpr::new(this_label.as_bytes()).unwrap();

    pipeline
        .match_one(sel_expr, tr_expr, Nucleotide::as_str(sequence), match_type)
        .retain(r_sel_expr)
        .boxed()
}

fn process_sized<B>(
    read: BoxedReads,
    init_label: &str,
    this_label: &str,
    next_label: &str,
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

    validate_length(cut_read, len_sel_expr, len_tr_expr, r_sel_expr, range) as _
}

pub fn process_fixed_len(
    read: BoxedReads,
    init_label: &str,
    this_label: &str,
    next_label: &str,
    len: usize,
) -> BoxedReads {
    process_sized(read, init_label, this_label, next_label, len..=len)
}

pub fn process_ranged_len<B>(
    read: BoxedReads,
    init_label: &str,
    this_label: &str,
    next_label: &str,
    range: B,
) -> BoxedReads
where
    B: RangeBounds<usize> + Send + Sync + 'static,
{
    process_sized(read, init_label, this_label, next_label, range)
}

pub fn process_unbounded(read: BoxedReads, init_label: &str, this_label: &str) -> BoxedReads {
    // set init_label to this_label
    // cut left end 0
    let sel_expr = SelectorExpr::new(init_label.as_bytes()).unwrap();
    let cut_tr_expr =
        TransformExpr::new(format!("{init_label} -> _, {this_label}").as_bytes()).unwrap();

    let tr = format!("{{{this_label}}}");

    let cut_read = cut(read, sel_expr.clone(), cut_tr_expr, LeftEnd(0));

    set(cut_read, sel_expr, init_label, &tr)
}

pub fn process_ranged_len_no_cut<B>(read: BoxedReads, this_label: &str, range: B) -> BoxedReads
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
    init_label: &str,
    this_label: &str,
) -> BoxedReads {
    // set init_label to this_label
    // cut left end 0
    let sel_expr = SelectorExpr::new(init_label.as_bytes()).unwrap();

    let tr = format!("{{{this_label}}}");

    set(read, sel_expr, init_label, &tr)
}
