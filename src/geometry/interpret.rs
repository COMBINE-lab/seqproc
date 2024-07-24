use antisequence::{
    graph::{
        MatchType::{ExactSearch, HammingSearch, PrefixAln},
        Threshold::Frac,
    },
    *,
};
use expr::label_exists;

use crate::{
    compile::{
        functions::CompiledFunction,
        utils::{GeometryMeta, GeometryPiece},
        CompiledData,
    },
    parser::{IntervalKind, IntervalShape},
    processors::*,
    S,
};

// use these consts for left and right
static LEFT: &str = "l";
static RIGHT: &str = "r";
static VOID_LABEL: &str = "_";

fn labels(read_label: &[String]) -> (String, String) {
    let len = read_label.len();
    let next_label = read_label.to_vec().as_slice().join("");

    if len == 1 {
        (format!("{}*", read_label.first().unwrap()), next_label)
    } else {
        (next_label.clone(), next_label)
    }
}

impl CompiledData {
    pub fn interpret(&self, additional_args: &[String]) -> Vec<GraphNodes> {
        let Self {
            geometry,
            transformation,
        } = self;

        let mut nodes: Vec<GraphNodes> = vec![];

        for (i, read_geometry) in geometry.iter().enumerate() {
            interpret_geometry(
                &mut nodes,
                read_geometry,
                format!("seq{}.", i + 1),
                additional_args,
            );
        }

        if let Some(transformation) = transformation {
            for (i, tr) in transformation.iter().enumerate() {
                let seq_name = format!("seq{}.*", i + 1);
                let tr = format!("{{{}}}", tr.join("}{"));
                nodes.push(set_node(&seq_name, antisequence::expr::fmt_expr(tr)));
            }
        }

        nodes
    }
}

fn interpret_geometry(
    nodes: &mut Vec<GraphNodes>,
    geometry: &[GeometryMeta],
    init_label: String,
    additional_args: &[String],
) {
    let mut geometry_iter = geometry.iter();

    let mut label: Vec<String> = vec![init_label];

    while let Some(gp) = geometry_iter.next() {
        let (_, size, _, _) = gp.unpack();

        match size {
            IntervalShape::FixedSeq(_) | IntervalShape::FixedLen(_) => {
                gp.interpret(&label, additional_args, nodes);
            }
            IntervalShape::RangedLen(_) | IntervalShape::UnboundedLen => {
                // by rules of geometry this should either be None or a sequence
                if let Some(next) = geometry_iter.next() {
                    next.interpret_dual(gp, &label, additional_args, nodes);
                } else {
                    gp.interpret(&label, additional_args, nodes);
                }
            }
        };

        label.push(format!("_{RIGHT}"));
    }
}

fn parse_additional_args(arg: String, args: &[String]) -> String {
    match arg.parse::<usize>() {
        Ok(n) => args
            .get(n)
            .unwrap_or_else(|| {
                panic!(
                    "Expected {n} additional arguments with `--additional` tag. Found only {}.",
                    args.len()
                )
            })
            .clone(),
        _ => arg,
    }
}

fn execute_stack(
    stack: Vec<S<CompiledFunction>>,
    label: String,
    attr: String,
    size: &IntervalShape,
    additional_args: &[String],
    nodes: &mut Vec<GraphNodes>,
) {
    let range = if let IntervalShape::RangedLen(S((a, b), _)) = size {
        Some(*a..=*b)
    } else {
        None
    };

    let interval_name = if attr.is_empty() {
        label.clone()
    } else {
        [label.clone(), ".".to_string(), attr].concat()
    };

    for S(fn_, _) in stack.into_iter().rev() {
        match fn_ {
            CompiledFunction::Remove => {
                // let interval_name = get_interval(label, attr);
                nodes.push(trim_node([antisequence::expr::label(
                    interval_name.clone(),
                )]));
            }
            CompiledFunction::Hamming(_) => {
                panic!("Hamming requires to be bound to a sequence cannot operate in isolation")
            }
            CompiledFunction::Map(file, fns) => {
                let file = parse_additional_args(file, additional_args);

                // map node
                // let mapped = map(read, label, attr, file, 0);
                execute_stack(
                    fns,
                    label.clone(),
                    "not_mapped".to_string(),
                    size,
                    additional_args,
                    nodes,
                );
            }
            CompiledFunction::MapWithMismatch(file, fns, mismatch) => {
                let file = parse_additional_args(file, additional_args);

                // map node
                // let mapped = map(read, label, attr, file, mismatch);
                execute_stack(
                    fns,
                    label.clone(),
                    "not_mapped".to_string(),
                    size,
                    additional_args,
                    nodes,
                );
            }
            CompiledFunction::FilterWithinDist(file, mismatch) => {
                let file = parse_additional_args(file, additional_args);

                // filter
                // retain
                // filter(read, label, attr, file, mismatch)
            }
            // for the rest of the compliled functions which translate exactly to a single node
            _ => {
                // let interval_name = get_interval(label, attr);
                nodes.push(set_node(
                    &interval_name,
                    fn_.to_expr(&interval_name, &range),
                ));
            }
        };
    }
}

impl GeometryMeta {
    fn unpack(
        &self,
    ) -> (
        IntervalKind,
        IntervalShape,
        Option<String>,
        Vec<S<CompiledFunction>>,
    ) {
        let GeometryMeta {
            expr: S(GeometryPiece { type_, size, label }, _),
            stack,
        } = self.clone();

        (type_, size, label, stack)
    }

    fn interpret_no_cut(
        &self,
        label: Vec<String>,
        additional_args: &[String],
        nodes: &mut Vec<GraphNodes>,
    ) {
        let (type_, size, self_label, mut stack) = self.unpack();

        let (init_label, cur_label) = labels(&label);
        let seq_name = label.first().unwrap();

        let this_label = if let Some(l) = self_label {
            format!("{seq_name}{l}")
        } else {
            cur_label
        };

        if type_ == IntervalKind::Discard {
            stack.push(S(CompiledFunction::Remove, 0..1));
        }

        // this is only called from `interpret_dual` which is for variable to fixedSeq
        // thus this is only for variable sized segments
        match size {
            IntervalShape::RangedLen(S((a, b), _)) => {
                nodes.push(valid_label_length(&this_label, a, Some(b)));
            }
            IntervalShape::UnboundedLen => {
                // set init -> this
                nodes.push(set_node(
                    &init_label,
                    antisequence::expr::Expr::from(antisequence::expr::label(this_label.clone())),
                ));
            }
            _ => unreachable!(),
        };

        execute_stack(
            stack,
            this_label,
            "".to_string(),
            &size,
            additional_args,
            nodes,
        );
    }

    fn interpret(&self, label: &[String], additional_args: &[String], nodes: &mut Vec<GraphNodes>) {
        let (type_, size, self_label, mut stack) = self.unpack();

        let (init_label, cur_label) = labels(label);
        let seq_name = label.first().unwrap();

        let this_label = if let Some(l) = self_label {
            format!("{seq_name}{l}")
        } else {
            format!("{cur_label}_{LEFT}")
        };
        let next_label = format!("{cur_label}_{RIGHT}");

        if type_ == IntervalKind::Discard {
            stack.push(S(CompiledFunction::Remove, 0..1));
        }

        // execute the requisite process here
        match size.clone() {
            IntervalShape::FixedSeq(S(seq, _)) => {
                let match_type = if !stack.is_empty() {
                    match stack.last().unwrap() {
                        S(CompiledFunction::Hamming(n), _) => {
                            let dist = Frac(1.0 - (*n as f64 / seq.len() as f64));
                            HammingSearch(dist)
                        }
                        _ => PrefixAln {
                            identity: 1.0,
                            overlap: 1.0,
                        },
                    }
                } else {
                    ExactSearch
                };

                // a match
                // a retain
                nodes.push(match_node(
                    &seq,
                    &init_label,
                    &this_label,
                    "_",
                    &next_label,
                    match_type,
                ));
                nodes.push(retain_node(label_exists(this_label.clone())));
            }
            IntervalShape::FixedLen(S(len, _)) => {
                // a cut
                // a validate length
                nodes.push(cut_node(
                    into_transform_expr(&init_label, [this_label.clone(), next_label]),
                    LeftEnd(len),
                ));
                nodes.push(valid_label_length(&this_label, len, None));
            }
            IntervalShape::RangedLen(S((a, b), _)) => {
                // a cut
                // a validate length
                nodes.push(cut_node(
                    into_transform_expr(&init_label, [this_label.clone(), next_label]),
                    LeftEnd(b),
                ));
                nodes.push(valid_label_length(&this_label, a, Some(b)));
            }
            // a cut
            // a set
            IntervalShape::UnboundedLen => {
                nodes.push(cut_node(
                    into_transform_expr(&init_label, [VOID_LABEL.to_owned(), this_label.clone()]),
                    LeftEnd(0),
                ));
                nodes.push(set_node(
                    &init_label,
                    antisequence::expr::Expr::from(antisequence::expr::label(this_label.clone())),
                ));
            }
        };

        execute_stack(
            stack,
            this_label,
            "".to_string(),
            &size,
            additional_args,
            nodes,
        );
    }

    fn interpret_dual(
        &self,
        prev: &Self,
        label: &Vec<String>,
        additional_args: &[String],
        nodes: &mut Vec<GraphNodes>,
    ) {
        // unpack label for self
        let (_, size, this_label, mut stack) = self.unpack();
        let (_, _, prev_label, _) = prev.unpack();
        // execute the processing for next

        let (init_label, cur_label) = labels(label);
        let seq_name = label.first().unwrap();

        let mut left_label = label.to_owned();

        let prev_label = if let Some(l) = prev_label {
            left_label.push(l.clone());
            format!("{seq_name}{l}")
        } else {
            left_label.push(format!("_{LEFT}"));
            format!("{cur_label}_{LEFT}")
        };
        let this_label = if let Some(l) = this_label {
            format!("{seq_name}{l}")
        } else {
            format!("{cur_label}_anchor")
        };
        let next_label = format!("{cur_label}_{RIGHT}");

        match size.clone() {
            IntervalShape::FixedSeq(S(seq, _)) => {
                // check if the first function on the stack is a hamming search
                // else do an exact match
                let match_type = if !stack.is_empty() {
                    match stack.pop().unwrap() {
                        S(CompiledFunction::Hamming(n), _) => {
                            let dist = Frac(1.0 - (n as f64 / seq.len() as f64));
                            HammingSearch(dist)
                        }
                        _ => ExactSearch,
                    }
                } else {
                    ExactSearch
                };

                nodes.push(match_node(
                    &seq,
                    &init_label,
                    &this_label,
                    &prev_label,
                    &next_label,
                    match_type,
                ));
                nodes.push(retain_node(label_exists(this_label.clone())));

                execute_stack(
                    stack,
                    this_label,
                    "".to_string(),
                    &size,
                    additional_args,
                    nodes,
                );
            }
            _ => unreachable!(),
        };

        // call interpret for self
        // this is just an unbounded or ranged segment. No cut just set or validate
        prev.interpret_no_cut(left_label, additional_args, nodes);
    }
}
