use std::{collections::HashMap, fmt, ops::Deref};

use crate::{
    lexer::Span,
    parser::{Expr, Function, Size, Spanned},
};

/*
   Take a description and validate the composition of the following components
       1. Definitions
           - must contain valid functional composition
           - cannot reuse label
           - must be a valid geom_piece not a label
           - labels must be less than 16 characters
       2. Read
           - must abide by the grammar, no ambiguous reads
           - cannot reuse label
           - inline functions must be valid
           - labels must be defined above
           - labels must be less than 16 characters //
       3. Transformation
           - must refer to a created label -- must have been captured in read
           - may use labels multiple times
           - must contain valid functional composition
*/

pub enum ReturnType {
    Ranged,
    Fixed,
    Unbounded,
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

fn val_trim_pad(fn_: Function, expr: Spanned<Expr>) -> Result<(), Error> {
    let (expr, expr_span) = expr;

    match expr {
        Expr::GeomPiece(_, size) => match size {
            Size::FixedLen(_) | Size::FixedSeq(_) => Ok(()),
            _ => Err(Error {
                span: expr_span,
                msg: format!(
                    "Function: {}, Expected a fixed length geometry piece, found {} geometry peice",
                    fn_,
                    size
                )
            })
        },
        Expr::LabeledGeomPiece(_, inner_expr) => val_trim_pad(fn_, inner_expr.deref().clone()),
        Expr::Function(inner_fn_, _) => match inner_fn_.0 {
            Function::Remove => Err(Error {
                span: inner_fn_.1,
                msg: format!(
                    "Function: {}, expected to be composed with valid geometry or any function but Remove, found: {}",
                    fn_,
                    Function::Remove
                )
            }),
            _ => Ok(())
        },
        _ => Err(Error {
            span: expr_span,
            msg: format!(
                "Function: {}, expected to be composed with valid geometry or any function but Remove, found: {}",
                fn_,
                expr
            ),
        }),
    }
}

// must be variable no unbounded, or rev/revcomp functions
fn val_norm(expr: Spanned<Expr>) -> Result<(), Error> {
    let (expr, expr_span) = expr;

    match expr {
        Expr::GeomPiece(_, size) => match size {
            Size::RangedLen(_) => Ok(()),
            _ => Err(Error {
                span: expr_span,
                msg: format!(
                    "Function: Normalize, expected to be composed with a ranged geometry piece of functions: `rev` or `revcomp`. Found: {} geometry piece",
                    size
                )
            })
        },
        Expr::LabeledGeomPiece(_, inner_expr) => val_norm(inner_expr.deref().clone()),
        Expr::Function(fn_, _) => match fn_.0 {
            Function::Reverse | Function::ReverseComp => Ok(()),
            _ => Err(Error {
                span: expr_span,
                msg: format!(
                    "Function: Normalize, expected to be composed with a ranged geometry piece of functions: `rev` or `revcomp`. Found: {}",
                    fn_.0
                )
            })
        },
        _ => Err(Error {
            span: expr_span,
            msg: format!(
                "Function: Normalize, expected to be composed with a ranged geometry piece of functions: `rev` or `revcomp`. Found: {}",
                expr
            )
        })
    }
}

fn val_hamming(expr: Spanned<Expr>) -> Result<(), Error> {
    let (expr, expr_span) = expr;

    match expr {
        Expr::GeomPiece(_, size) => match size {
            Size::FixedSeq(_) => Ok(()),
            _ => Err(Error {
                span: expr_span,
                msg: format!(
                    "Function: Hamming, expected to be provided with a fixed sequence geometry, found: {} geometry",
                    size
                )
            })
        },
        Expr::LabeledGeomPiece(_, inner_expr) => val_hamming(inner_expr.deref().clone()),
        _ => Err(Error {
            span: expr_span,
            msg: format!(
                "Function: Hamming, expected to be provided with a fixed sequence geometry, found: {}",
                expr
            )
        })
    }
}

fn valid_rev_revcomp(fn_: Function, expr: Spanned<Expr>) -> Result<(), Error> {
    let (expr, expr_span) = expr;

    match expr {
        Expr::Function(inner_fn, _) => match inner_fn.0 {
            Function::Remove => Err(Error {
                span: inner_fn.1,
                msg: format!(
                    "Function: {0}, cannot composite Remove inside of {0}",
                    fn_
                )
            }),
            _ => Ok(())
        },
        Expr::GeomPiece(_, _) | Expr::LabeledGeomPiece(_, _) => Ok(()),
        _ => Err(Error {
            span: expr_span,
            msg: format!(
                "Function: {}, expected to be provided with a valid geometry or function, expect Remove, found: {}",
                fn_,
                expr
            )
        })
    }
}

fn validate_composition(fn_: Spanned<Function>, expr: Spanned<Expr>) -> Result<(), Error> {
    let (fn_, _fn_span) = fn_;

    // match function
    match fn_ {
        Function::Reverse => valid_rev_revcomp(Function::Reverse, expr),
        Function::ReverseComp => valid_rev_revcomp(Function::ReverseComp, expr),
        Function::Trim(n) => val_trim_pad(Function::Trim(n), expr),
        Function::Pad(n) => val_trim_pad(Function::Pad(n), expr),
        Function::Normalize => val_norm(expr),
        Function::Map(_, _) => todo!(), // can be anything but remove, or unbounded
        Function::Hamming(_) => val_hamming(expr),
        Function::Remove => Ok(()),
    }
}

/*
   Replace labels with their value, return the bottom level expr at the end
*/

pub fn unwrap_val_fn(expr: Expr, map: HashMap<String, Spanned<Expr>>) -> Result<Expr, Error> {
    let mut expr = expr;
    let mut err: Option<Error> = None;
    let mut stack: Vec<Spanned<Function>> = Vec::new();
    let mut labels: Vec<String> = Vec::new();

    // create a vector of labels which have already been used. help with errors
    // labels and span!

    loop {
        match expr {
            Expr::Function(fn_, gp) => {
                expr = gp.deref().clone().0;
                stack.push(fn_);
            }
            Expr::LabeledGeomPiece(_, gp) => {
                expr = gp.deref().clone().0;
            }
            Expr::Label((ref l, ref span)) => {
                if labels.contains(l) {
                    err = Some(Error {
                        span: span.clone(),
                        msg: format!(
                            "`{}` has already been used. Cannot use same variable more than once.",
                            l
                        ),
                    });

                    break;
                } else {
                    if let Some((inner_expr, _)) = map.get(l) {
                        labels.push(l.clone());
                        expr = inner_expr.clone();
                    } else {
                        err = Some(Error {
                            span: span.clone(),
                            msg: format!("No variable declared with label: {}", l),
                        });

                        break;
                    }
                }
            }
            _ => break,
        }
    }

    for (fn_, _) in stack.into_iter().rev() {
        println!("{}", fn_);
    }

    if let Some(e) = err {
        return Err(e);
    }

    Ok(expr)

    // match expr {
    //     Expr::Function(fn_, gp) => {
    //         let res = match &gp.deref().0 {
    //             Expr::Label((ref l, span)) => {
    //                 if let Some((_, expr)) = map.remove_entry(l) {
    //                     validate_composition(fn_, expr.clone())
    //                 } else {
    //                     Err(Error {
    //                         span: span.clone(),
    //                         msg: format!("Either no variable declared with label: {}. Or this variable has already been used in the Read.", l),
    //                     })
    //                 }
    //             }
    //             _ => validate_composition(fn_, gp.deref().clone()),
    //         };

    //         match res {
    //             Ok(_) => unwrap_val_fn(gp.deref().0.clone(), map),
    //             Err(e) => Err(e),
    //         }
    //     }
    //     Expr::Label((ref l, span)) => {
    //         if let Some((_, (expr, _))) = map.remove_entry(l) {
    //             Ok(expr.clone())
    //         } else {
    //             Err(Error {
    //                 span: span.clone(),
    //                 msg: format!("Either no variable declared with label: {}. Or this variable has already been used in the Read.", l),
    //             })
    //         }
    //     }
    //     _ => Ok(expr),
    // }
}

pub fn compile_definitions(expr: Spanned<Expr>) -> Result<HashMap<String, Spanned<Expr>>, Error> {
    let (expr, expr_span) = expr;

    if let Expr::Definitions(defs) = expr {
        let mut map = HashMap::new();

        let mut err: Option<Error> = None;

        for (def, def_span) in defs {
            if let Expr::LabeledGeomPiece(label, expr) = def.clone() {
                let expr = expr.deref().clone();
                let label = label.deref().clone();

                if let Expr::Label((l, span)) = label {
                    // at this point there should not be any labels if there are there should be an error
                    if let Err(e) = unwrap_val_fn(expr.0.clone(), HashMap::new()) {
                        err = Some(e);
                        break;
                    } else {
                        if map.insert(l.clone(), expr).is_some() {
                            err = Some(Error {
                                // span labels
                                span,
                                msg: format!(
                                    "Repeated label in definition block: \"{}\" already defined",
                                    l
                                ),
                            });
                            break;
                        }
                    }
                } else {
                    err = Some(Error {
                        span: def_span,
                        msg: format!("Expected a Labeled Geometry piece, found: {}", def),
                    })
                }
            }
        }

        if let Some(e) = err {
            return Err(e);
        }

        Ok(map)
    } else {
        Err(Error {
            span: expr_span,
            msg: format!("Expected a definition block, found: {}", expr),
        })
    }
}
