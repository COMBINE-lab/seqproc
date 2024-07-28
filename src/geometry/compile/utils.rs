use std::fmt;

use crate::{
    parser::{IntervalKind, IntervalShape},
    Span, S,
};

use super::functions::{ChangeAs, CompiledFunction};

pub type Geometry = Vec<Vec<(Interval, usize)>>;

pub type Transformation = Vec<Vec<String>>;

fn log4_roundup(n: usize) -> usize {
    (n.ilog2() + 1).div_ceil(2) as usize
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReturnType {
    Ranged,
    FixedLen,
    Unbounded,
    FixedSeq,
    Void,
}

impl fmt::Display for ReturnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ReturnType::*;
        match self {
            Ranged => write!(f, "Ranged"),
            FixedLen => write!(f, "Fixed Length"),
            Unbounded => write!(f, "Unbounded"),
            FixedSeq => write!(f, "Fixed Sequence"),
            Void => write!(f, "Void"),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error: {}, at {}-{}",
            self.msg, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Interval {
    Named(String),
    Temporary(GeometryMeta),
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Interval::*;
        match self {
            Named(s) => write!(f, "Label: {s}"),
            Temporary(gp) => write!(f, "{gp}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GeometryMeta {
    pub expr: S<GeometryPiece>,
    pub stack: Vec<S<CompiledFunction>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GeometryPiece {
    pub type_: IntervalKind,
    pub size: IntervalShape,
    pub label: Option<String>,
}

impl fmt::Display for GeometryMeta {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Geometry Meta: {}, {:?}", self.expr.0, self.stack)
    }
}

impl fmt::Display for GeometryPiece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}, {}", self.label, self.type_, self.size)
    }
}

impl GeometryMeta {
    pub fn validate_expr(&self) -> Result<(), Error> {
        let S(expr, expr_span) = &self.expr;

        let expr_type = {
            if let IntervalKind::Discard = expr.type_ {
                ReturnType::Void
            } else {
                match expr.size {
                    IntervalShape::FixedSeq(_) => ReturnType::FixedSeq,
                    IntervalShape::FixedLen(_) => ReturnType::FixedLen,
                    IntervalShape::RangedLen(_) => ReturnType::Ranged,
                    IntervalShape::UnboundedLen => ReturnType::Unbounded,
                }
            }
        };

        let mut return_type = S(expr_type, expr_span.clone());

        for S(fn_, span) in self.stack.iter().rev() {
            return_type = validate_composition(S(fn_, span.clone()), return_type, &expr.size)?;
        }

        Ok(())
    }
}

pub fn validate_composition(
    S(fn_, fn_span): S<&CompiledFunction>,
    S(return_type, return_type_span): S<ReturnType>,
    size: &IntervalShape,
) -> Result<S<ReturnType>, Error> {
    let (min, max) = match size {
        IntervalShape::FixedSeq(S(seq, _)) => (0, seq.len()),
        &IntervalShape::FixedLen(S(n, _)) => (0, n),
        &IntervalShape::RangedLen(S((a, b), _)) => (a, b),
        IntervalShape::UnboundedLen => (100, 100),
    };

    match *fn_ {
        CompiledFunction::ReverseComp => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Reverse Complement cannot take void element as an argument"
                    .to_string(),
            }),
            _ => Ok(S(return_type, fn_span)),
        },
        CompiledFunction::Reverse => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Reverse cannot take void element as an argument".to_string(),
            }),
            _ => Ok(S(return_type, fn_span)),
        },
        CompiledFunction::Truncate(by) | CompiledFunction::TruncateLeft(by) => {
            if min <= by && max <= by {
                return Err(Error {
                    span: return_type_span,
                    msg: "Cannot truncate by more than the length of the segment".to_string(),
                });
            }

            match return_type {
                ReturnType::Void => Err(Error {
                    span: return_type_span,
                    msg:
                        "Function Truncate and TruncateLeft cannot take void element as an argument"
                            .to_string(),
                }),
                _ => Ok(S(return_type, fn_span)),
            }
        }
        CompiledFunction::TruncateTo(to) | CompiledFunction::TruncateToLeft(to) => {
            if to > max {
                return Err(Error {
                    span: return_type_span,
                    msg: "Cannot truncate to a length greater than the elements length."
                        .to_string(),
                });
            }

            match return_type {
                ReturnType::FixedLen | ReturnType::Unbounded | ReturnType::Ranged => Ok(S(ReturnType::FixedLen, fn_span)),
                ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
                ReturnType::Void => Err(Error {
                    span: return_type_span,
                    msg: "Function TruncateTo and TruncateToLeft cannot take void element as an argument".to_string(),
                }),
            }
        }
        CompiledFunction::Remove => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Remove cannot recieve a void element as an argument".to_string(),
            }),
            _ => Ok(S(ReturnType::Void, fn_span)),
        },
        // todo: should this take only fixed length segments so to lead ot known ending length?
        CompiledFunction::Pad(..) | CompiledFunction::PadLeft(..) => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Pad and PadLeft cannot take void element as an argument".to_string(),
            }),
            _ => Ok(S(return_type, fn_span)),
        },
        CompiledFunction::PadTo(to, ..) | CompiledFunction::PadToLeft(to, ..) => {
            if to < max {
                return Err(Error {
                    span: return_type_span,
                    msg: "Cannot pad to a length less than the elements length.".to_string(),
                });
            }

            match return_type {
                ReturnType::FixedLen |  ReturnType::Unbounded | ReturnType::Ranged  => Ok(S(ReturnType::FixedLen, fn_span)),
                ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
                ReturnType::Void => Err(Error {
                    span: return_type_span,
                    msg: "Function PadTo and PadToLeft cannot take a void element as an argument"
                        .to_string(),
                }),
            }
        }
        CompiledFunction::Normalize => match return_type {
            ReturnType::Ranged => Ok(S(ReturnType::FixedLen, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Normalize must take ranged element an argument, found: {return_type}"
                ),
            }),
        },
        CompiledFunction::Map(..) | CompiledFunction::MapWithMismatch(..) => match return_type {
            ReturnType::Ranged | ReturnType::FixedLen | ReturnType::FixedSeq => {
                Ok(S(ReturnType::FixedLen, fn_span))
            }
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Map can recieve a Ranged or Fixed piece as an argument, found: {return_type}"
                ),
            }),
        },
        CompiledFunction::FilterWithinDist(..) => match return_type {
            ReturnType::FixedLen => Ok(S(ReturnType::FixedLen, fn_span)),
            ReturnType::Ranged => Ok(S(ReturnType::Ranged, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Filter can recieve a Ranged or Fixed piece as an argument, found: {return_type}"
                ),
            }),
        },
        CompiledFunction::Hamming(_) => match return_type {
            ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Hamming must take Sequence element an argument, found: {return_type}"
                ),
            }),
        },
    }
}

impl GeometryMeta {
    pub fn is_complex(&self) -> bool {
        self.expr.0.is_complex()
    }

    pub fn get_label(&self) -> Option<String> {
        self.expr.0.label.clone()
    }

    // show normalize when seen, then at the end if something is still variable then normalize
    pub fn get_simplified_description_string(&self) -> String {
        if self.expr.0.is_seq() {
            return String::from("");
        }

        let mut size: Option<IntervalShape> = Some(self.expr.0.clone().size);

        for (n, change_as) in self
            .stack
            .iter()
            .map(|S(f, _)| f.clone().get_change_in_len())
        {
            size = match change_as {
                ChangeAs::TO => Some(size.unwrap().update_size_to(n)),
                ChangeAs::ADD => Some(size.unwrap().update_size_add(n)),
                ChangeAs::SUB => Some(size.unwrap().update_size_sub(n)),
                ChangeAs::REMOVE => None,
            }
        }

        match size {
            Some(s) => self
                .expr
                .0
                .get_simplified_description_string(s.get_normalized()),
            None => String::from(""),
        }
    }
}

impl IntervalShape {
    pub fn update_size_to(&self, n: usize) -> Self {
        IntervalShape::FixedLen(S(n, 0..1))
    }

    pub fn update_size_add(self, n: usize) -> Self {
        match self {
            IntervalShape::FixedLen(S(l, s)) => IntervalShape::FixedLen(S(n + l, s)),
            IntervalShape::RangedLen(S((a, b), s)) => {
                IntervalShape::RangedLen(S((a + n, b + n), s))
            }
            _ => self,
        }
    }

    pub fn update_size_sub(self, n: usize) -> Self {
        match self {
            IntervalShape::FixedLen(S(l, s)) => IntervalShape::FixedLen(S(l - n, s)),
            _ => self,
        }
    }

    pub fn get_normalized(self) -> Self {
        match self {
            IntervalShape::RangedLen(S((a, b), s)) => {
                IntervalShape::FixedLen(S(b + log4_roundup(b - a + 1), s))
            }
            _ => self,
        }
    }
}

impl GeometryPiece {
    pub fn is_complex(&self) -> bool {
        matches!(
            self.size,
            IntervalShape::RangedLen(..) | IntervalShape::FixedSeq(..)
        )
    }

    pub fn is_seq(&self) -> bool {
        matches!(self.size, IntervalShape::FixedSeq(..))
    }

    pub fn get_simplified_description_string(&self, size: IntervalShape) -> String {
        let type_ = match self.type_ {
            IntervalKind::Barcode => "b",
            IntervalKind::Umi => "u",
            IntervalKind::Discard => "x",
            IntervalKind::ReadSeq => "r",
            _ => unreachable!(),
        };

        match size {
            IntervalShape::FixedLen(S(n, ..)) => format!("{type_}[{n}]"),
            IntervalShape::UnboundedLen => format!("{type_}:"),
            _ => unreachable!(),
        }
    }
}
