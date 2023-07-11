use std::fmt;

use crate::{
    lexer::Span,
    parser::{Expr, Function, Size, Spanned, Type},
};

pub type Geometry = Vec<Vec<(Interval, i32)>>;

pub type Transformation = Vec<Vec<String>>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Interval {
    Named(String),
    Temporary(GeometryPiece),
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Interval::*;
        match self {
            Named(s) => write!(f, "Label: {}", s),
            Temporary(gp) => write!(f, "{}", gp),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GeometryPiece {
    pub expr: Spanned<Expr>,
    pub stack: Vec<Spanned<Function>>,
}

impl fmt::Display for GeometryPiece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Geometry Piece: {}, {:?}", self.expr.0, self.stack)
    }
}

pub fn validate_expr(gp: GeometryPiece) -> Result<GeometryPiece, Error> {
    if let Err(e) = gp_return_type(gp.clone()) {
        return Err(e);
    }

    Ok(gp)
}

pub fn gp_return_type(gp: GeometryPiece) -> Result<ReturnType, Error> {
    let (expr, expr_span) = gp.clone().expr;

    let expr_type = if let Expr::GeomPiece(type_, size) = expr.clone() {
        if let Type::Discard = type_ {
            ReturnType::Void
        } else {
            match size {
                Size::FixedSeq(_) => ReturnType::FixedSeq,
                Size::FixedLen(_) => ReturnType::FixedLen,
                Size::RangedLen(_) => ReturnType::Ranged,
                Size::UnboundedLen => ReturnType::Unbounded,
            }
        }
    } else {
        unreachable!();
    };

    let mut return_type = (expr_type.clone(), expr_span.clone());

    for fn_ in gp.clone().stack.into_iter() {
        let res = validate_composition(fn_, return_type);

        if let Err(e) = res {
            return Err(e);
        }

        return_type = res.ok().unwrap();
    }

    Ok(return_type.0)
}

pub fn validate_composition(
    fn_: Spanned<Function>,
    return_type: Spanned<ReturnType>,
) -> Result<Spanned<ReturnType>, Error> {
    let (fn_, fn_span) = fn_;
    let (return_type, return_type_span) = return_type;

    match fn_ {
        Function::ReverseComp => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Reverse Complement cannot take void element as an argument"
                    .to_string(),
            }),
            _ => Ok((return_type, fn_span)),
        },
        Function::Reverse => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Reverse cannot take void element as an argument".to_string(),
            }),
            _ => Ok((return_type, fn_span)),
        },
        Function::Trim(_) => match return_type {
            ReturnType::FixedLen => Ok((ReturnType::FixedLen, fn_span)),
            ReturnType::FixedSeq => Ok((ReturnType::FixedSeq, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Trim must take fixed element an argument, found: {}",
                    return_type
                ),
            }),
        },
        Function::Remove => match return_type {
            ReturnType::Void => Err(Error {
                span: return_type_span,
                msg: "Function Remove cannot recieve a void element as an argument".to_string(),
            }),
            _ => Ok((ReturnType::Void, fn_span)),
        },
        Function::Pad(_) => match return_type {
            ReturnType::FixedLen => Ok((ReturnType::FixedLen, fn_span)),
            ReturnType::FixedSeq => Ok((ReturnType::FixedSeq, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Pad must take fixed element an argument, found: {}",
                    return_type
                ),
            }),
        },
        Function::Normalize => match return_type {
            ReturnType::Ranged => Ok((ReturnType::FixedLen, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Normalize must take ranged element an argument, found: {}",
                    return_type
                ),
            }),
        },
        Function::Map(_, _) => match return_type {
            ReturnType::Ranged | ReturnType::FixedLen | ReturnType::FixedSeq => {
                Ok((ReturnType::FixedLen, fn_span))
            }
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Map can recieve a Ranged or Fixed piece as an argument, found: {}",
                    return_type
                ),
            }),
        },
        Function::Hamming(_) => match return_type {
            ReturnType::FixedSeq => Ok((ReturnType::FixedSeq, fn_span)),
            _ => Err(Error {
                span: return_type_span,
                msg: format!(
                    "Function Hamming must take Sequence element an argument, found: {}",
                    return_type
                ),
            }),
        },
    }
}
