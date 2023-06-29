use antisequence::{
    expr::Label,
    EndIdx::LeftEnd,
    MatchType::{BoundedAln, LocalAln},
};

use crate::processors::*;

use super::syntax::*;

fn get_labels(read_label: &mut Vec<&str>) -> (String, String) {
    let next_label = read_label.join("_");

    if read_label.len() == 1 {
        (format!("{}*", read_label.first().unwrap()), next_label)
    } else {
        (next_label.clone(), next_label)
    }
}

impl FixedGeomPiece {
    fn interpret(
        self,
        read: Box<dyn antisequence::Reads>,
        read_label: &mut Vec<&str>,
    ) -> Box<dyn antisequence::Reads> {
        let (starting_label, next_label) = get_labels(read_label);

        match self {
            Self::Fixed(_, _) => self.sequence_interpret(read, read_label, None),
            Self::Discard(FixedGeom::Len(len), _label) => trim(
                validate_length(
                    cut(read, starting_label, &next_label, LeftEnd(len)),
                    &next_label,
                    len..=len,
                ),
                vec![make_label(&next_label, "l")],
            ),
            Self::Barcode(FixedGeom::Len(len), _label)
            | Self::Umi(FixedGeom::Len(len), _label)
            | Self::ReadSeq(FixedGeom::Len(len), _label) => validate_length(
                cut(read, starting_label, &next_label, LeftEnd(len)),
                &next_label,
                len..=len,
            ),
        }
    }

    fn sequence_interpret(
        self,
        read: Box<dyn antisequence::Reads>,
        read_label: &mut Vec<&str>,
        bound: Option<(usize, usize)>,
    ) -> Box<dyn antisequence::Reads> {
        let (starting_label, next_label) = get_labels(read_label);

        if let Self::Fixed(NucStr::Seq(sequence), _label) = self {
            let match_type = if let Some((i, j)) = bound {
                BoundedAln {
                    identity: 1.0,
                    overlap: 1.0,
                    from: i,
                    to: j + sequence.len(),
                }
            } else {
                LocalAln {
                    identity: 1.0,
                    overlap: 1.0,
                }
            };

            process_sequence(read, sequence, starting_label, &next_label, match_type)
        } else {
            unreachable!()
        }
    }
}

impl VariableGeomPiece {
    fn context_interpret(
        self,
        read: Box<dyn antisequence::Reads>,
        read_label: &mut Vec<&str>,
        fixed: FixedGeomPiece,
    ) -> Box<dyn antisequence::Reads> {
        match self {
            Self::Barcode(VariableGeom::Range(i, j), _)
            | Self::Discard(VariableGeom::Range(i, j), _)
            | Self::ReadSeq(VariableGeom::Range(i, j), _)
            | Self::Umi(VariableGeom::Range(i, j), _) => {
                let read = fixed.sequence_interpret(read, read_label, Some((i, j)));
                read_label.push("l");
                let read = self.interpret(read, read_label);
                read_label.pop();
                read
            }
            Self::Barcode(VariableGeom::Unbounded, _)
            | Self::Discard(VariableGeom::Unbounded, _)
            | Self::ReadSeq(VariableGeom::Unbounded, _)
            | Self::Umi(VariableGeom::Unbounded, _) => {
                let read = fixed.sequence_interpret(read, read_label, None);
                read_label.push("l");
                let read = self.interpret(read, read_label);
                read_label.pop();
                read
            }
        }
    }

    fn interpret(
        self,
        read: Box<dyn antisequence::Reads>,
        read_label: &mut Vec<&str>,
    ) -> Box<dyn antisequence::Reads> {
        let (starting_label, next_label) = get_labels(read_label);
        match self {
            Self::Discard(VariableGeom::Range(i, j), _label) => trim(
                validate_length(
                    cut(read, starting_label, &next_label, LeftEnd(j)),
                    &next_label,
                    i..j + 1,
                ),
                vec![make_label(&next_label, "l")],
            ),
            Self::Discard(VariableGeom::Unbounded, _label) => {
                trim(read, vec![Label::new(next_label.as_bytes()).unwrap()])
            }
            Self::Barcode(VariableGeom::Range(i, j), _label)
            | Self::ReadSeq(VariableGeom::Range(i, j), _label)
            | Self::Umi(VariableGeom::Range(i, j), _label) => {
                let read = cut(read, starting_label, &next_label, LeftEnd(j));
                let read = validate_length(read, &next_label, i..j + 1);
                pad(read, vec![make_label(&next_label, "l")], j + 1)
            }
            Self::Barcode(VariableGeom::Unbounded, _label)
            | Self::ReadSeq(VariableGeom::Unbounded, _label)
            | Self::Umi(VariableGeom::Unbounded, _label) => read,
        }
    }
}

impl BoundedGeom {
    fn interpret(
        self,
        read: Box<dyn antisequence::Reads>,
        read_label: &mut Vec<&str>,
    ) -> Box<dyn antisequence::Reads> {
        if let Some(variable) = self.variable {
            variable.context_interpret(read, read_label, self.fixed)
        } else {
            self.fixed.interpret(read, read_label)
        }
    }
}

impl CompositeGeom {
    fn interpret(
        self,
        read: Box<dyn antisequence::Reads>,
        read_label: &mut Vec<&str>,
    ) -> Box<dyn antisequence::Reads> {
        let mut read = read;

        for bounded_geom in self.bounded {
            read = bounded_geom.interpret(read, read_label);
            read_label.push("r")
        }

        match self.variable {
            Some(v) => v.interpret(read, read_label),
            None => trim_leftover(read, read_label),
        }
    }
}

impl Description {
    fn interpret(
        self,
        read: Box<dyn antisequence::Reads>,
        read_label: &mut Vec<&str>,
    ) -> Box<dyn antisequence::Reads> {
        match self {
            Self::CompositeGeom(composite) => composite.interpret(read, read_label),
            Self::Variable(variable) => {
                let read = variable.clone().interpret(read, read_label);

                if !variable.is_unbounded() {
                    trim_leftover(read, read_label)    
                } else {
                    read
                }                
            }
        }
    }
}

impl Read {
    pub fn interpret(self, read: Box<dyn antisequence::Reads>) -> Box<dyn antisequence::Reads> {
        self.description
            .interpret(read, &mut vec![format!("seq{}.", self.num).as_str()])
    }
}
