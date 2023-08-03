/*
   Compile functions read from parser
   This is specifically for map as this point
   but in the future when functions can accept
   expressions with label references a seperate
   place for this to happen will be useful
*/

use std::ops::Deref;

use crate::parser::{Expr, Function, Spanned};

use super::utils::{validate_expr, Error, GeometryMeta, GeometryPiece};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompiledFunction {
    Reverse,
    ReverseComp,
    Truncate(usize),
    TruncateLeft(usize),
    TruncateTo(usize),
    TruncateToLeft(usize),
    Remove,
    Pad(usize, char),
    PadLeft(usize, char),
    PadTo(usize, char),
    PadToLeft(usize, char),
    Normalize,
    Map(String, Vec<Spanned<CompiledFunction>>),
    MapWithMismatch(String, Vec<Spanned<CompiledFunction>>, usize),
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
    pub fn get_change_in_len(self) -> (usize, ChangeAs) {
        match self {
            CompiledFunction::Truncate(n)
            | CompiledFunction::TruncateLeft(n)
            | CompiledFunction::PadTo(n, _)
            | CompiledFunction::PadToLeft(n, _) => (n, ChangeAs::SUB),
            CompiledFunction::TruncateTo(n) | CompiledFunction::TruncateToLeft(n) => {
                (n, ChangeAs::TO)
            }
            CompiledFunction::Pad(n, _) | CompiledFunction::PadLeft(n, _) => (n, ChangeAs::ADD),
            CompiledFunction::Remove => (0, ChangeAs::REMOVE),
            _ => (0, ChangeAs::ADD),
        }
    }
}

pub fn compile_fn(
    fn_: Spanned<Function>,
    parent_expr: Spanned<Expr>,
) -> Result<Spanned<CompiledFunction>, Error> {
    let (fn_, span) = fn_;
    let (parent_expr, expr_span) = parent_expr;
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
            compile_inner_expr(expr.deref().clone(), (parent_expr, expr_span))?,
            mismatch,
        ),
        Function::Map(path, expr) => CompiledFunction::Map(
            path,
            compile_inner_expr(expr.deref().clone(), (parent_expr, expr_span))?,
        ),
        Function::FilterWithinDist(path, mismatch) => {
            CompiledFunction::FilterWithinDist(path, mismatch)
        }
        Function::Hamming(n) => CompiledFunction::Hamming(n),
    };

    Ok((comp_fn, span))
}

fn compile_inner_expr(
    mut expr: Spanned<Expr>,
    parent_expr: Spanned<Expr>,
) -> Result<Vec<Spanned<CompiledFunction>>, Error> {
    // if we are here in a map then the expr passed into the expr should be a geom_piece or labeled geom_piece
    // either way we can extract the size and type of it
    let mut stack: Vec<Spanned<CompiledFunction>> = Vec::new();

    loop {
        match expr.0 {
            Expr::Self_ => break,
            Expr::Function(inner_fn, inner_expr) => {
                expr = inner_expr.deref().clone();
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
        let (mut expr, span) = parent_expr;
        loop {
            match expr {
                Expr::LabeledGeomPiece(_, b) => {
                    let (gp, _) = b.deref();
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
                msg: format!("Expected geometry peice found: {}", expr),
            });
        }
    };

    // check this and return esult from this function
    validate_expr(GeometryMeta {
        expr: (geom_piece, expr.1),
        stack: stack.clone(),
    })?;

    Ok(stack)
}
