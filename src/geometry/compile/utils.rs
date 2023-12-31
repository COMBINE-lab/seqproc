use std::fmt;

use crate::{
    parser::{IntervalKind, IntervalShape},
    Span, S,
};

use super::functions::CompiledFunction;

pub type Geometry = Vec<Vec<(Interval, usize)>>;

pub type Transformation = Vec<Vec<String>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReturnType {
    Ranged,
    FixedLen,
    Unbounded,
    FixedSeq,
    Void,
}

impl fmt::Display for ReturnType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Geometry Meta: {}, {:?}", self.expr.0, self.stack)
    }
}

impl fmt::Display for GeometryPiece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
                ReturnType::FixedLen => Ok(S(ReturnType::FixedLen, fn_span)),
                ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
                ReturnType::Unbounded | ReturnType::Ranged => Ok(S(ReturnType::FixedLen, fn_span)),
                _ => Err(Error {
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
                ReturnType::FixedLen => Ok(S(ReturnType::FixedLen, fn_span)),
                ReturnType::FixedSeq => Ok(S(ReturnType::FixedSeq, fn_span)),
                ReturnType::Unbounded | ReturnType::Ranged => Ok(S(ReturnType::FixedLen, fn_span)),
                _ => Err(Error {
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
