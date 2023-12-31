use antisequence::{
    MatchType::{ExactSearch, HammingSearch, PrefixAln},
    Threshold::Frac,
    *,
};

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

fn labels(read_label: &[String]) -> (String, String) {
    let next_label = read_label.join("");

    if read_label.len() == 1 {
        (format!("{}*", read_label.first().unwrap()), next_label)
    } else {
        (next_label.clone(), next_label)
    }
}

pub type BoxedReads = Box<dyn antisequence::Reads>;

impl CompiledData {
    pub fn interpret(
        &self,
        read: BoxedReads,
        out1: &str,
        out2: &str,
        additional_args: &[String],
    ) -> BoxedReads {
        let Self {
            geometry,
            transformation,
        } = self;

        let mut read = read;

        for (i, read_geometry) in geometry.iter().enumerate() {
            read = interpret_geometry(
                read_geometry,
                read,
                &format!("seq{}.", i + 1),
                "r",
                "l",
                additional_args,
            );
        }

        read = if let Some(trs) = transformation {
            for (i, tr) in trs.iter().enumerate() {
                let seq_name = format!("seq{}.*", i + 1);
                let tr = format!("{{{}}}", tr.join("}{"));
                read = set(read, sel!(), &seq_name, &tr);
            }

            if trs.len() == 1 {
                read.collect_fastq1(sel!(), out1).boxed()
            } else if out1.is_empty() && out2.is_empty() {
                read.collect_fastq1(sel!(), "/dev/null").boxed()
            } else {
                read.collect_fastq2(sel!(), out1, out2).boxed()
            }
        } else if out1.is_empty() && out2.is_empty() {
            read.collect_fastq1(sel!(), "/dev/null").boxed()
        } else if out2.is_empty() {
            read.collect_fastq1(sel!(), out1).boxed()
        } else {
            read.collect_fastq2(sel!(), out1, out2).boxed()
        };

        read
    }
}

fn interpret_geometry(
    geometry: &[GeometryMeta],
    read: BoxedReads,
    init_label: &str,
    right: &str,
    left: &str,
    additional_args: &[String],
) -> BoxedReads {
    let mut geometry_iter = geometry.iter();

    let mut read = read;

    let mut label: Vec<String> = vec![init_label.to_owned()];

    while let Some(gp) = geometry_iter.next() {
        let (_, size, _, _) = gp.unpack();

        read = match size {
            IntervalShape::FixedSeq(_) | IntervalShape::FixedLen(_) => {
                gp.interpret(read, &label, left, right, additional_args)
            }
            IntervalShape::RangedLen(_) | IntervalShape::UnboundedLen => {
                // by rules of geometry this should either be None or a sequence
                if let Some(next) = geometry_iter.next() {
                    next.interpret_dual(gp, read, &label, right, left, additional_args)
                } else {
                    gp.interpret(read, &label, left, right, additional_args)
                }
            }
        };

        label.push(format!("_{right}"));
    }

    read
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
    label: &str,
    attr: &str,
    read: BoxedReads,
    size: IntervalShape,
    additional_args: &[String],
) -> BoxedReads {
    let mut read = read;

    let range = if let IntervalShape::RangedLen(S((a, b), _)) = size {
        Some(a..=b)
    } else {
        None
    };

    for S(fn_, _) in stack.into_iter().rev() {
        read = match fn_ {
            CompiledFunction::Reverse => reverse(read, label, attr),
            CompiledFunction::ReverseComp => reverse_comp(read, label, attr),
            CompiledFunction::Truncate(n) => truncate_by(read, label, attr, RightEnd(n)),
            CompiledFunction::TruncateLeft(n) => truncate_by(read, label, attr, LeftEnd(n)),
            CompiledFunction::TruncateTo(n) => truncate_to(read, label, attr, RightEnd(n)),
            CompiledFunction::TruncateToLeft(n) => truncate_to(read, label, attr, LeftEnd(n)),
            CompiledFunction::Remove => remove(read, label, attr),
            CompiledFunction::Pad(n, nuc) => pad_by(read, label, attr, RightEnd(n), nuc),
            CompiledFunction::PadLeft(n, nuc) => pad_by(read, label, attr, LeftEnd(n), nuc),
            CompiledFunction::PadTo(n, nuc) => pad_to(read, label, attr, RightEnd(n), nuc),
            CompiledFunction::PadToLeft(n, nuc) => pad_to(read, label, attr, LeftEnd(n), nuc),
            CompiledFunction::Normalize => normalize(read, label, attr, range.clone().unwrap()),
            CompiledFunction::Map(file, fns) => {
                let file = parse_additional_args(file, additional_args);

                let mapped = map(read, label, attr, file, 0);
                execute_stack(
                    fns,
                    label,
                    "not_mapped",
                    mapped,
                    size.clone(),
                    additional_args,
                )
            }
            CompiledFunction::MapWithMismatch(file, fns, mismatch) => {
                let file = parse_additional_args(file, additional_args);

                let mapped = map(read, label, attr, file, mismatch);
                execute_stack(fns, label, "mapped", mapped, size.clone(), additional_args)
            }
            CompiledFunction::FilterWithinDist(file, mismatch) => {
                let file = parse_additional_args(file, additional_args);

                filter(read, label, attr, file, mismatch)
            }
            CompiledFunction::Hamming(_) => unreachable!(),
        };
    }

    read
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
        read: BoxedReads,
        label: &[String],
        additional_args: &[String],
    ) -> BoxedReads {
        let (type_, size, self_label, mut stack) = self.unpack();

        let (init_label, cur_label) = labels(label);
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
        let read = match size {
            IntervalShape::RangedLen(S((a, b), _)) => {
                process_ranged_len_no_cut(read, &this_label, a..=b)
            }
            IntervalShape::UnboundedLen => process_unbounded_no_cut(read, &init_label, &this_label),
            _ => unreachable!(),
        };

        execute_stack(stack, &this_label, "", read, size, additional_args)
    }

    fn interpret(
        &self,
        read: BoxedReads,
        label: &[String],
        left: &str,
        right: &str,
        additional_args: &[String],
    ) -> BoxedReads {
        let (type_, size, self_label, mut stack) = self.unpack();

        let (init_label, cur_label) = labels(label);
        let seq_name = label.first().unwrap();

        let this_label = if let Some(l) = self_label {
            format!("{seq_name}{l}")
        } else {
            format!("{cur_label}_{left}")
        };
        let next_label = format!("{cur_label}_{right}");

        if type_ == IntervalKind::Discard {
            stack.push(S(CompiledFunction::Remove, 0..1));
        }

        // execute the requisite process here
        let read = match size.clone() {
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

                process_sequence(
                    read,
                    &seq,
                    &init_label,
                    &this_label,
                    "_",
                    &next_label,
                    match_type,
                )
            }
            IntervalShape::FixedLen(S(len, _)) => {
                process_fixed_len(read, &init_label, &this_label, &next_label, len)
            }
            IntervalShape::RangedLen(S((a, b), _)) => {
                process_ranged_len(read, &init_label, &this_label, &next_label, a..=b)
            }
            IntervalShape::UnboundedLen => process_unbounded(read, &init_label, &this_label),
        };

        execute_stack(stack, &this_label, "", read, size, additional_args)
    }

    fn interpret_dual(
        &self,
        prev: &Self,
        read: BoxedReads,
        label: &[String],
        right: &str,
        left: &str,
        additional_args: &[String],
    ) -> BoxedReads {
        // unpack label for self
        let (_, size, this_label, mut stack) = self.unpack();
        let (_, _, prev_label, _) = prev.unpack();
        // execute the processing for next

        let (init_label, cur_label) = labels(label);
        let seq_name = label.first().unwrap();

        let mut left_label = label.to_owned();

        let prev_label = if let Some(l) = prev_label {
            left_label.push(l.to_string());
            format!("{seq_name}{l}")
        } else {
            left_label.push(format!("_{left}"));
            format!("{cur_label}_{left}")
        };
        let this_label = if let Some(l) = this_label {
            format!("{seq_name}{l}")
        } else {
            format!("{cur_label}_anchor")
        };
        let next_label = format!("{cur_label}_{right}");

        let read = match size.clone() {
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

                let read = process_sequence(
                    read,
                    &seq,
                    &init_label,
                    &this_label,
                    &prev_label,
                    &next_label,
                    match_type,
                );

                execute_stack(stack, &this_label, "", read, size, additional_args)
            }
            _ => unreachable!(),
        };

        // call interpret for self
        // this is just an unbounded or ranged segment. No cut just set or validate
        prev.interpret_no_cut(read, &left_label, additional_args)
    }
}
