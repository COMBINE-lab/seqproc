use antisequence::{
    MatchType::{ExactSearch, HammingSearch, PrefixAln},
    Threshold::Frac,
};

use crate::{
    compile::{
        utils::{GeometryMeta, GeometryPiece},
        CompiledData,
    },
    parser::{Function, Size, Spanned, Type},
    processors::*,
};

fn labels(read_label: &mut Vec<String>) -> (String, String) {
    let next_label = read_label.join("");

    if read_label.len() == 1 {
        (format!("{}*", read_label.first().unwrap()), next_label)
    } else {
        (next_label.clone(), next_label)
    }
}

pub type BoxedReads = Box<dyn antisequence::Reads>;

impl CompiledData {
    pub fn interpret(&self, read: BoxedReads) -> BoxedReads {
        let Self {
            geometry,
            transformation,
        } = self;

        let mut read = read;

        for (i, read_geometry) in geometry.into_iter().enumerate() {
            read = interpret_geometry(read_geometry.to_vec(), read, format!("seq{}.", i + 1));
        }

        if let Some(trs) = transformation {
            for tr in trs {
                println!("{{{}}}", tr.join("}{"))
            }
        }

        read
    }
}

fn interpret_geometry(
    geometry: Vec<GeometryMeta>,
    read: BoxedReads,
    init_label: String,
) -> BoxedReads {
    let mut geometry_iter = geometry.into_iter();

    let mut read = read;

    let mut label: Vec<String> = vec![init_label];

    while let Some(gp) = geometry_iter.next() {
        let (_, size, _, _) = gp.unpack();

        read = match size {
            Size::FixedSeq(_) | Size::FixedLen(_) => gp.interpret(read, &mut label),
            Size::RangedLen(_) | Size::UnboundedLen => {
                // by rules of geometry this should either be None or a sequence
                if let Some(next) = geometry_iter.next() {
                    next.interpret_dual(gp, read, &mut label)
                } else {
                    gp.interpret(read, &mut label)
                }
            }
        };

        label.push("_r".to_string())
    }

    read
}

fn execute_stack(_stack: Vec<Spanned<Function>>, _label: String, read: BoxedReads) -> BoxedReads {
    read
}

impl GeometryMeta {
    fn unpack(&self) -> (Type, Size, Option<String>, Vec<Spanned<Function>>) {
        let GeometryMeta {
            expr: (GeometryPiece { type_, size, label }, _),
            stack,
        } = self.clone();

        (type_, size, label, stack)
    }

    fn interpret(&self, read: BoxedReads, label: &mut Vec<String>) -> BoxedReads {
        let (type_, size, self_label, stack) = self.unpack();

        let mut stack = stack.to_owned();

        let (init_label, cur_label) = labels(label);
        let seq_name = label.get(0).unwrap();

        let this_label = if let Some(l) = self_label {
            format!("{seq_name}{l}")
        } else {
            format!("{seq_name}_l")
        };
        let next_label = format!("{cur_label}_r");

        if type_ == Type::Discard {
            stack.push((Function::Remove, 0..1))
        }

        // execute the requisite process here
        let read = match size {
            Size::FixedSeq((seq, _)) => {
                let match_type = if !stack.is_empty() {
                    match stack.first().unwrap() {
                        (Function::Hamming(n), _) => {
                            let dist = Frac((seq.len() / n) as f64);
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
                    seq,
                    init_label,
                    this_label.clone(),
                    "_".to_string(),
                    next_label,
                    match_type,
                )
            }
            Size::FixedLen((len, _)) => {
                process_fixed_len(read, init_label, this_label.clone(), next_label, len)
            }
            Size::RangedLen(((a, b), _)) => {
                process_ranged_len(read, init_label, this_label.clone(), next_label, a..=b)
            }
            Size::UnboundedLen => process_unbounded(read, init_label, this_label.clone()),
        };

        execute_stack(stack, this_label, read)
    }

    fn interpret_dual(&self, prev: Self, read: BoxedReads, label: &mut Vec<String>) -> BoxedReads {
        // unpack label for self
        let (_, size, this_label, stack) = self.unpack();
        let (_, _, prev_label, _) = prev.unpack();
        // execute the processing for next

        let (init_label, cur_label) = labels(label);
        let seq_name = label.get(0).unwrap();

        let mut left_label = label.clone();

        let prev_label = if let Some(l) = prev_label {
            left_label.push(format!("_{l}"));
            format!("{seq_name}_{l}")
        } else {
            left_label.push(format!("_l"));
            format!("{cur_label}_l")
        };
        let this_label = if let Some(l) = this_label {
            format!("{seq_name}{l}")
        } else {
            format!("{cur_label}_anchor")
        };
        let next_label = format!("{cur_label}_r");

        let read = match size {
            Size::FixedSeq((seq, _)) => {
                // check if the first function on the stack is a hamming search
                // else do an exact match
                let match_type = if !stack.is_empty() {
                    match stack.first().unwrap() {
                        (Function::Hamming(n), _) => {
                            let dist = Frac((seq.len() / n) as f64);
                            HammingSearch(dist)
                        }
                        _ => ExactSearch,
                    }
                } else {
                    ExactSearch
                };

                process_sequence(
                    read, seq, init_label, this_label, prev_label, next_label, match_type,
                )
            }
            _ => unreachable!(),
        };

        // call interpret for self
        prev.interpret(read, &mut left_label)
    }
}
