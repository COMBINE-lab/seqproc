use std::{collections::HashMap, ops::Deref};

use chumsky::{prelude::*, Stream};
use seqproc::{
    lexer::lexer,
    parser::{parser, Expr},
    validate::{compile_definitions, unwrap_val_fn, validate},
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
            let res = unwrap_val_fn(exprs.clone(), Some(HashMap::new()));

            assert_eq!(true, res.is_ok());
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
            let res = unwrap_val_fn(exprs.clone(), Some(HashMap::new()));

            assert_eq!(false, res.is_ok());
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
            let res = unwrap_val_fn(exprs.clone(), Some(HashMap::new()));

            assert!(res.is_err());
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
            let res = unwrap_val_fn(exprs.clone(), Some(HashMap::new()));

            assert!(res.is_err());
        } else {
            todo!()
        }
    }
}

#[test]
fn discard_as_void() {
    let src = "1{rev(x[10])}";

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
            let res = unwrap_val_fn(exprs.clone(), Some(HashMap::new()));

            assert!(res.is_err());
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
            let res = unwrap_val_fn(exprs.clone(), Some(def_map.clone()));
            assert!(res.is_err());
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
            let res = unwrap_val_fn(exprs.clone(), Some(def_map.clone()));
            assert!(res.is_err());
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

    for read in &reads {
        if let Expr::Read(_, exprs) = read {
            let res = unwrap_val_fn(exprs.clone(), Some(HashMap::new()));

            assert!(res.is_ok());

            if let Ok((expr, _)) = res {
                assert_eq!(4, expr.len());
            }
        } else {
            todo!()
        }
    }
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
            let res = unwrap_val_fn(exprs.clone(), Some(def_map.clone()));

            dbg!(&res);
            assert!(res.is_err());
        } else {
            todo!()
        }
    }
}

#[test]
fn def_block_fail() {
    let src = "
brc = b[10]
brc1 = pad(<brc>, 1)
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
fn validate_description() {
    let src = "
brc = b[10]
umi = pad(u[10], 1)
1{<brc>}2{<umi>}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = validate(desc);

    assert!(res.is_ok())
}

#[test]
fn fail_description() {
    let src = "
brc = b[10]
umi = pad(u[10], 1)
1{<brc><brc>}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = validate(desc);

    assert!(res.is_err())
}

#[test]
fn valid_geom() {
    let src = "1{b<brc1>[9-11]remove(f[CAGAGC])u<umi>[8]b<brc2>[10]}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = validate(desc);

    assert!(res.is_ok())
}

#[test]
fn invalid_geom_one() {
    let src = "1{b[9-11]f[CAGAGC]r:u[8]b[10]}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = validate(desc);

    assert!(res.is_err())
}

#[test]
fn invalid_geom_two() {
    let src = "1{f[GAG]b[10-11]b[10]}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = validate(desc);

    assert!(res.is_err())
}
