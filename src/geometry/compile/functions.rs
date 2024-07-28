//! Compile functions read from parser
//! This is specifically for map as this point
//! but in the future when functions can accept
//! expressions with label references a seperate
//! place for this to happen will be useful

use crate::{
    compile::utils::{Error, GeometryMeta, GeometryPiece},
    parser::{Expr, Function},
    Nucleotide, S,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompiledFunction {
    Reverse,
    ReverseComp,
    Truncate(usize),
    TruncateLeft(usize),
    TruncateTo(usize),
    TruncateToLeft(usize),
    Remove,
    Pad(usize, Nucleotide),
    PadLeft(usize, Nucleotide),
    PadTo(usize, Nucleotide),
    PadToLeft(usize, Nucleotide),
    Normalize,
    Map(String, Vec<S<CompiledFunction>>),
    MapWithMismatch(String, Vec<S<CompiledFunction>>, usize),
    FilterWithinDist(String, usize),
    Hamming(usize),
}

pub enum ChangeAs {
    TO,
    ADD,
    SUB,
    REMOVE,
}

impl CompiledFunction {
    pub fn get_change_in_len(&self) -> (usize, ChangeAs) {
        match self {
            CompiledFunction::Truncate(n) | CompiledFunction::TruncateLeft(n) => {
                (*n, ChangeAs::SUB)
            }
            CompiledFunction::PadTo(n, _)
            | CompiledFunction::PadToLeft(n, _)
            | CompiledFunction::TruncateTo(n)
            | CompiledFunction::TruncateToLeft(n) => (*n, ChangeAs::TO),
            CompiledFunction::Pad(n, _) | CompiledFunction::PadLeft(n, _) => (*n, ChangeAs::ADD),
            CompiledFunction::Remove => (0, ChangeAs::REMOVE),
            _ => (0, ChangeAs::ADD),
        }
    }
}

pub fn compile_fn(
    S(fn_, span): S<Function>,
    S(parent_expr, expr_span): S<Expr>,
) -> Result<S<CompiledFunction>, Error> {
    let comp_fn = match fn_ {
        Function::Reverse => CompiledFunction::Reverse,
        Function::ReverseComp => CompiledFunction::ReverseComp,
        Function::Truncate(n) => CompiledFunction::Truncate(n),
        Function::TruncateLeft(n) => CompiledFunction::TruncateLeft(n),
        Function::TruncateTo(n) => CompiledFunction::TruncateTo(n),
        Function::TruncateToLeft(n) => CompiledFunction::TruncateToLeft(n),
        Function::Remove => CompiledFunction::Remove,
        Function::Pad(n, nuc) => CompiledFunction::Pad(n, nuc),
        Function::PadLeft(n, nuc) => CompiledFunction::PadLeft(n, nuc),
        Function::PadTo(n, nuc) => CompiledFunction::PadTo(n, nuc),
        Function::PadToLeft(n, nuc) => CompiledFunction::PadToLeft(n, nuc),
        Function::Normalize => CompiledFunction::Normalize,
        Function::MapWithMismatch(path, expr, mismatch) => CompiledFunction::MapWithMismatch(
            path,
            compile_inner_expr(expr.unboxed(), S(parent_expr, expr_span))?,
            mismatch,
        ),
        Function::Map(path, expr) => CompiledFunction::Map(
            path,
            compile_inner_expr(expr.unboxed(), S(parent_expr, expr_span))?,
        ),
        Function::FilterWithinDist(path, mismatch) => {
            CompiledFunction::FilterWithinDist(path, mismatch)
        }
        Function::Hamming(n) => CompiledFunction::Hamming(n),
    };

    Ok(S(comp_fn, span))
}

fn compile_inner_expr(
    mut expr: S<Expr>,
    parent_expr: S<Expr>,
) -> Result<Vec<S<CompiledFunction>>, Error> {
    // if we are here in a map then the expr passed into the expr should be a geom_piece or labeled geom_piece
    // either way we can extract the size and type of it
    let mut stack: Vec<S<CompiledFunction>> = Vec::new();

    loop {
        match expr.0 {
            Expr::Self_ => break,
            Expr::Function(inner_fn, inner_expr) => {
                expr = inner_expr.unboxed();
                let inner_fn = compile_fn(inner_fn.clone(), expr.clone());
                if inner_fn.is_ok() {
                    stack.push(inner_fn.ok().unwrap());
                } else {
                    return Err(Error {
                        span: expr.1,
                        msg: "Invalid function composition".to_string(),
                    });
                }
            }
            _ => {
                return Err(Error {
                    span: expr.1,
                    msg: "Must refernce only `self` in `map` fallback argument".to_string(),
                })
            }
        }
    }

    let geom_piece = {
        let S(mut expr, span) = parent_expr;
        loop {
            match expr {
                Expr::LabeledGeomPiece(_, b) => {
                    let S(gp, _) = b.unboxed();
                    expr = gp.clone();
                }
                Expr::GeomPiece(_, _) => break,
                Expr::Label(_) => todo!(),
                _ => todo!(), // throw error
            };
        }

        if let Expr::GeomPiece(type_, size) = expr {
            GeometryPiece {
                type_,
                size,
                label: None,
            }
        } else {
            return Err(Error {
                span,
                msg: format!("Expected geometry peice found: {expr}"),
            });
        }
    };

    // check this and return result from this function
    let gm = GeometryMeta {
        expr: S(geom_piece, expr.1),
        stack,
    };

    gm.validate_expr()?;

    Ok(gm.stack)
}
