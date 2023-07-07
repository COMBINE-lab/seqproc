use std::{collections::HashMap, fmt, ops::Deref};

use crate::{
    lexer::Span,
    parser::{Expr, Function, Size, Spanned, Type},
};

/*
   Take a description and validate the composition of the following components
       1. Definitions
           - must contain valid functional composition /
           - cannot reuse label /
           - must be a valid geom_piece not a label /
       2. Read
           - must abide by the grammar, no ambiguous reads /
           - cannot reuse label /
           - inline functions must be valid /
           - labels must be defined above /
       3. Transformation
           - must refer to a created label, must have been captured in read /
           - may use labels multiple times [TODO]
           - must contain valid functional composition /
           - use return type from read for composition [TODO]
*/

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

fn validate_composition(
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

/*
   Replace labels with their value, return the bottom level expr at the end

   TODO: after a read segment the transformations occur:
    - thus references labels should have new return values for validation
*/

pub fn unwrap_val_fn(
    exprs: Vec<Spanned<Expr>>,
    map: Option<HashMap<String, Spanned<Expr>>>,
) -> Result<(Vec<Spanned<Expr>>, HashMap<String, Spanned<Expr>>), Error> {
    let mut err: Option<Error> = None;
    let mut geom: Vec<Spanned<Expr>> = Vec::new();
    let mut labels: Vec<String> = Vec::new();
    let mut label_map: HashMap<String, Spanned<Expr>> = HashMap::new();

    // create a vector of labels which have already been used. help with errors
    // labels and span!

    'outer: for expr in exprs {
        let mut expr = expr;
        let mut stack: Vec<Spanned<Function>> = Vec::new();

        'inner: loop {
            match expr.0 {
                Expr::Function(fn_, gp) => {
                    expr = gp.deref().clone();
                    stack.push(fn_);
                }
                Expr::LabeledGeomPiece(l, gp) => {
                    if let Expr::Label((l, span)) = l.deref() {
                        if labels.contains(&l)
                            || (map.is_some() && map.clone().unwrap().contains_key(l))
                        {
                            err = Some(Error {
                                span: span.clone(),
                                msg: format!("Variable: {}, already defined above.", l),
                            });

                            break 'outer;
                        }

                        label_map.insert(l.clone(), gp.deref().clone());
                    }
                    // maybe return from this and add labeled elements to the map outside of this
                    // would have to unpack labeled values to validate at the end
                    expr = gp.deref().clone();
                }
                Expr::Label((ref l, ref span)) => {
                    if map.is_none() {
                        err = Some(Error {
                            span: span.clone(),
                            msg: "Cannot refernce labels in Definition block".to_string(),
                        });

                        break 'outer;
                    }

                    let map = map.as_ref().clone().unwrap();

                    if labels.contains(l) {
                        err = Some(Error {
                            span: span.clone(),
                            msg: format!(
                                "`{}` has already been used. Cannot use same variable more than once.",
                                l
                            ),
                        });

                        break 'outer;
                    } else {
                        if let Some(inner_expr) = map.get(l) {
                            labels.push(l.clone());
                            expr = inner_expr.clone();
                        } else {
                            err = Some(Error {
                                span: span.clone(),
                                msg: format!("No variable declared with label: {}", l),
                            });

                            break 'outer;
                        }
                    }
                }
                _ => break 'inner,
            }
        }

        let (expr, expr_span) = expr;

        let return_type = if let Expr::GeomPiece(type_, size) = expr.clone() {
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

        let mut inner_type = (return_type.clone(), expr_span.clone());

        for fn_ in stack.into_iter().rev() {
            let res = validate_composition(fn_, inner_type);

            if let Err(e) = res {
                err = Some(e);
                break 'outer;
            }

            inner_type = res.ok().unwrap();
        }

        geom.push((expr, expr_span));
    }

    if let Some(e) = err {
        return Err(e);
    }

    Ok((geom, label_map))
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
                    if let Err(e) = unwrap_val_fn(vec![expr.clone()], None) {
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

pub fn validate_geometry(geom: Vec<Spanned<Expr>>) -> Result<(), Error> {
    let mut expect_next = vec![
        ReturnType::FixedLen,
        ReturnType::FixedSeq,
        ReturnType::Unbounded,
        ReturnType::Ranged,
    ];
    let mut iter = geom.iter();

    loop {
        let next = iter.next();

        if next.is_none() {
            break;
        }

        if let (Expr::GeomPiece(_, geom_type), span) = next.unwrap() {
            let type_ = match geom_type {
                Size::FixedSeq(_) => ReturnType::FixedSeq,
                Size::FixedLen(_) => ReturnType::FixedLen,
                Size::RangedLen(_) => ReturnType::Ranged,
                Size::UnboundedLen => ReturnType::Unbounded,
            };

            if !expect_next.contains(&type_) {
                return Err(Error {
                    span: span.clone(),
                    msg: format!(
                        "Ambiguous Geometry: expected {:?}, found: {}",
                        expect_next, type_
                    ),
                });
            }

            expect_next = match type_ {
                ReturnType::FixedLen | ReturnType::FixedSeq => {
                    vec![
                        ReturnType::FixedLen,
                        ReturnType::FixedSeq,
                        ReturnType::Unbounded,
                        ReturnType::Ranged,
                    ]
                }
                ReturnType::Ranged | ReturnType::Unbounded => {
                    vec![ReturnType::FixedSeq]
                }
                _ => unreachable!(),
            };
        }
    }

    Ok(())
}

pub fn validate(expr: Expr) -> Result<(), Error> {
    let mut err: Option<Error> = None;

    if let Expr::Description(d, mut r, t) = expr {
        // validate defintion block
        let map = if let Some(expr) = d.deref() {
            let def_res = compile_definitions(expr.clone());

            if let Err(e) = def_res {
                return Err(e);
            } else {
                Some(def_res.ok().unwrap())
            }
        } else {
            None
        };

        // validate read block
        // should only have two reads
        let second_read = r.0.pop().unwrap();
        let first_read = r.0.pop().unwrap();

        let read_one_map = if let Expr::Read(_, exprs) = first_read {
            let read_res = unwrap_val_fn(exprs, map.clone());

            if let Err(e) = read_res {
                return Err(e);
            } else {
                // validate read for ambiguity
                let (geom, read_one_map) = read_res.ok().unwrap();
                // validate read for ambiguity
                let res_geom = validate_geometry(geom);

                if res_geom.is_err() {
                    return Err(res_geom.err().unwrap());
                }

                read_one_map
            }
        } else {
            HashMap::new()
        };

        let read_two_map = if let Expr::Read(_, exprs) = second_read {
            let read_res = unwrap_val_fn(exprs, map.clone());

            if let Err(e) = read_res {
                return Err(e);
            } else {
                let (geom, read_two_map) = read_res.ok().unwrap();
                // validate read for ambiguity
                let res_geom = validate_geometry(geom);

                if res_geom.is_err() {
                    return Err(res_geom.err().unwrap());
                }

                read_two_map
            }
        } else {
            HashMap::new()
        };

        // ensure there are no similarities between the two maps
        for k in read_one_map.keys() {
            let val = read_two_map.get(k);

            if val.is_some() {
                err = Some(Error {
                    span: val.unwrap().clone().1,
                    msg: format!(
                        "Variable: {}, already in use. Cannot use a variable more than once.",
                        k
                    ),
                });
            }
        }

        let all_vars = if map.is_some() {
            map.unwrap()
                .into_iter()
                .chain(read_one_map)
                .chain(read_two_map)
                .collect::<HashMap<_, _>>()
        } else {
            read_one_map
                .into_iter()
                .chain(read_two_map)
                .collect::<HashMap<_, _>>()
        };

        // validate transformation block
        // this has to be a bit more complex
        // may need to keep track of the total return value of some label and use that to validate
        if let Some(trans) = t.deref() {
            if let (Expr::Transform(ts), _) = trans {
                'outer: for transformation in ts {
                    if let Expr::Read(_, exprs) = transformation {
                        let read_res = unwrap_val_fn(exprs.clone(), Some(all_vars.clone()));

                        if let Err(e) = read_res {
                            err = Some(e);

                            break 'outer;
                        }
                    }
                }
            }
        }
    } else {
        unreachable!();
    }

    if let Some(e) = err {
        return Err(e);
    }

    Ok(())
}
