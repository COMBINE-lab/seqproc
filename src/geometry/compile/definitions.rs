use super::utils::*;

use std::{collections::HashMap, ops::Deref};

use crate::parser::{Expr, Function, Spanned};

// validate definitions there should be no labels, just labeled geom peices and functions
fn validate_definition(expr: Spanned<Expr>) -> Result<GeometryMeta, Error> {
    let mut stack: Vec<Spanned<Function>> = vec![];
    let mut expr = expr;

    loop {
        match expr.0 {
            Expr::Function(fn_, gp) => {
                stack.insert(0, fn_);
                expr = gp.deref().clone();
            }
            Expr::Label(_) => {
                return Err(Error {
                    span: expr.1,
                    msg: "Unexpected label in definition block".to_string(),
                })
            }
            _ => break,
        }
    }

    let gp = if let (Expr::GeomPiece(type_, size), span) = expr {
        (GeometryPiece { type_, size }, span)
    } else {
        unreachable!()
    };

    let gp = GeometryMeta { expr: gp, stack };

    validate_expr(gp)
}

pub fn compile_definitions(expr: Spanned<Expr>) -> Result<HashMap<String, GeometryMeta>, Error> {
    let (expr, expr_span) = expr;

    if let Expr::Definitions(defs) = expr {
        let mut map = HashMap::new();

        let mut err: Option<Error> = None;

        for (def, def_span) in defs {
            if let Expr::LabeledGeomPiece(label, expr) = def.clone() {
                let expr = expr.deref().clone();
                let label = label.deref().clone();

                if let Expr::Label((l, span)) = label {
                    let res = validate_definition(expr.clone());
                    if let Err(e) = res {
                        err = Some(e);
                        break;
                    } else if map.insert(l.clone(), res.ok().unwrap()).is_some() {
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
