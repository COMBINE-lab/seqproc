use std::{collections::HashMap, ops::Deref};

use chumsky::{prelude::*, Stream};
use seqproc::{
    lexer::lexer,
    parser::{parser, Expr},
    validate::{compile_definitions, unwrap_val_fn},
};

#[test]
fn no_err() {
    let src = "1{remove(hamming(f[CAG], 1))}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r.0
    } else {
        unreachable!()
    };

    for read in &res {
        if let Expr::Read(_, exprs) = read {
            for (expr, _) in exprs {
                let res = unwrap_val_fn(expr.clone(), HashMap::new());

                assert_eq!(true, res.is_ok());
            }
        } else {
            todo!()
        }
    }
}

#[test]
fn fail_norm() {
    let src = "1{norm(r:)}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r.0
    } else {
        unreachable!()
    };

    for read in &res {
        if let Expr::Read(_, exprs) = read {
            for (expr, _) in exprs {
                let res = unwrap_val_fn(expr.clone(), HashMap::new());

                assert_eq!(false, res.is_ok());
            }
        } else {
            todo!()
        }
    }
}

#[test]
fn fail_composition() {
    let src = "1{trim(rev(r:), 1)}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r.0
    } else {
        unreachable!()
    };

    for read in &res {
        if let Expr::Read(_, exprs) = read {
            for (expr, _) in exprs {
                let res = unwrap_val_fn(expr.clone(), HashMap::new());

                assert_eq!(true, res.is_ok());
            }
        } else {
            todo!()
        }
    }
}

#[test]
fn fail_remove() {
    let src = "1{rev(remove(r:))}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r.0
    } else {
        unreachable!()
    };

    for read in &res {
        if let Expr::Read(_, exprs) = read {
            for (expr, _) in exprs {
                let res = unwrap_val_fn(expr.clone(), HashMap::new());

                assert_eq!(false, res.is_ok());
            }
        } else {
            todo!()
        }
    }
}

#[test]
fn ok_definition() {
    let src = "
brc = b[10]
brc1 = b[1-4]
1{<brc>}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(d, _r, _t) = res.clone().unwrap().0 {
        d
    } else {
        unreachable!()
    };

    let res = if let Some(def) = res.deref() {
        def
    } else {
        panic!("No definitions in {}", src.clone())
    };

    let def_map = compile_definitions(res.clone());

    assert!(def_map.is_ok());
    assert_eq!(2, def_map.unwrap().len())
}

#[test]
fn duplicate_def() {
    let src = "
brc = b[10]
brc = b[1-4]
1{<brc>}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(d, _r, _t) = res.clone().unwrap().0 {
        d
    } else {
        unreachable!()
    };

    let res = if let Some(def) = res.deref() {
        def
    } else {
        panic!("No definitions in {}", src.clone())
    };

    let def_map = compile_definitions(res.clone());

    assert!(def_map.is_err());
}

#[test]
fn label_replacement() {
    let src = "test = r: 
    1{pad(<test>, 5)}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let (def, (reads, _)) = if let Expr::Description(d, r, _t) = res.clone().unwrap().0 {
        (d.deref().clone().unwrap(), r)
    } else {
        unreachable!()
    };

    let def_map = if let Ok(d) = compile_definitions(def.clone()) {
        d
    } else {
        todo!()
    };

    for read in &reads {
        if let Expr::Read(_, exprs) = read {
            for (expr, _) in exprs {
                let res = unwrap_val_fn(expr.clone(), def_map.clone());
                assert!(res.is_err());
            }
        } else {
            todo!()
        }
    }
}

#[test]
fn no_variable() {
    let src = "testing = r: 
    1{pad(<test>, 5)}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let (def, (reads, _)) = if let Expr::Description(d, r, _t) = res.clone().unwrap().0 {
        (d.deref().clone().unwrap(), r)
    } else {
        unreachable!()
    };

    let def_map = if let Ok(d) = compile_definitions(def.clone()) {
        d
    } else {
        todo!()
    };

    for read in &reads {
        if let Expr::Read(_, exprs) = read {
            for (expr, _) in exprs {
                let res = unwrap_val_fn(expr.clone(), def_map.clone());
                assert!(res.is_err());
            }
        } else {
            todo!()
        }
    }
}

#[test]
fn expr_unwrap() {
    let src = "1{norm(b[9-10])remove(f[CAGAGC])u[8]remove(b[10])}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let (reads, _) = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let mut geom: Vec<Expr> = vec![];
    for read in &reads {
        if let Expr::Read(_, exprs) = read {
            for (expr, _) in exprs {
                let res = unwrap_val_fn(expr.clone(), HashMap::new());

                if let Ok(expr) = res {
                    geom.push(expr);
                }
            }
        } else {
            todo!()
        }
    }

    assert_eq!(4, geom.len());
}

#[test]
fn fail_reuse_label() {
    let src = "
brc = b[10]
1{<brc><brc>}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let (reads, _) = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    for read in &reads {
        if let Expr::Read(_, exprs) = read {
            for (expr, _) in exprs {
                let res = unwrap_val_fn(expr.clone(), HashMap::new());

                assert!(res.is_err());
            }
        } else {
            todo!()
        }
    }
}
