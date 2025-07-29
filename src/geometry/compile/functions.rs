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

/// expr: transformed interval with reference to 'self'
/// parent_expr: outside interval which will acted on first
fn compile_inner_expr(
    mut expr: S<Expr>,
    parent_expr: S<Expr>,
) -> Result<Vec<S<CompiledFunction>>, Error> {
    // if we are here in a map then the expr passed into the expr should be a geom_piece or labeled geom_piece
    // either way we can extract the size and type of it
    let mut self_stack: Vec<S<CompiledFunction>> = Vec::new();
    let mut inner_stack: Vec<S<CompiledFunction>> = Vec::new();

    loop {
        match expr.0 {
            Expr::Self_ => break,
            Expr::Function(inner_fn, inner_expr) => {
                expr = inner_expr.unboxed();
                let inner_fn = compile_fn(inner_fn.clone(), expr.clone());
                if inner_fn.is_ok() {
                    self_stack.push(inner_fn.ok().unwrap());
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
        let S(mut expr, mut span) = parent_expr;
        loop {
            match expr {
                Expr::Function(fn_, fn_expr) => {
                    expr = fn_expr.unboxed().0;
                    span = fn_.1;
                    let compiled_fn = compile_fn(fn_.clone(), S(expr.clone(), span));
                    if compiled_fn.is_ok() {
                        inner_stack.push(compiled_fn.ok().unwrap());
                    } else {
                        return Err(Error {
                            span,
                            msg: "Invalid function composition".to_string(),
                        });
                    }
                }
                Expr::LabeledGeomPiece(_, b) => {
                    let S(gp, _) = b.unboxed();
                    expr = gp.clone();
                }
                Expr::GeomPiece(_, _) => break,
                _ => return Err(Error {
                    span,
                    msg: "Something internal went wrong -- please post EFGDL specification on Github issues".to_string(),
                })
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

    GeometryMeta {
        expr: S(geom_piece, expr.1),
        stack: self_stack
            .clone()
            .into_iter()
            .chain(inner_stack.into_iter())
            .collect::<Vec<_>>(),
    }
    .validate_expr()?;

    Ok(self_stack)
}
