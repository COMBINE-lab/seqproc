use std::{collections::HashMap, ops::Deref};

use chumsky::{prelude::*, Stream};
use seqproc::{
    compile::{compile, definitions::compile_definitions, reads::compile_reads},
    lexer::lexer,
    parser::{parser, Expr},
};

#[test]
fn no_err() {
    let src = "1{remove(hamming(f[CAG], 1))}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = compile_reads(res, &mut HashMap::new());

    assert_eq!(true, res.is_ok());
}

#[test]
fn fail_norm() {
    let src = "1{norm(r:)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = compile_reads(res.clone(), &mut HashMap::new());

    assert_eq!(false, res.is_ok());
}

#[test]
fn fail_composition() {
    let src = "1{trunc_to(rev(r:), 1)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = compile_reads(res.clone(), &mut HashMap::new());

    assert_eq!(false, res.is_ok());
}

#[test]
fn fail_remove() {
    let src = "1{rev(remove(r:))}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = compile_reads(res.clone(), &mut HashMap::new());

    assert_eq!(false, res.is_ok());
}

#[test]
fn discard_as_void() {
    let src = "1{rev(x[10])}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let res = if let Expr::Description(_d, r, _t) = res.clone().unwrap().0 {
        r
    } else {
        unreachable!()
    };

    let res = compile_reads(res.clone(), &mut HashMap::new());

    assert_eq!(false, res.is_ok());
}

#[test]
fn ok_definition() {
    let src = "
brc = b[10]
brc1 = b[1-4]
1{<brc>}2{r:}";

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
1{<brc>}2{r:}";

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
    1{pad_to(<test>, 5)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let (def, reads) = if let Expr::Description(d, r, _t) = res.clone().unwrap().0 {
        (d.deref().clone().unwrap(), r)
    } else {
        unreachable!()
    };

    let mut def_map = if let Ok(d) = compile_definitions(def.clone()) {
        d
    } else {
        todo!()
    };

    let res = compile_reads(reads, &mut def_map);

    assert_eq!(false, res.is_ok());
}

#[test]
fn no_variable() {
    let src = "testing = r: 
    1{pad(<test>, 5)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let (def, reads) = if let Expr::Description(d, r, _t) = res.clone().unwrap().0 {
        (d.deref().clone().unwrap(), r)
    } else {
        unreachable!()
    };

    let mut def_map = if let Ok(d) = compile_definitions(def.clone()) {
        d
    } else {
        todo!()
    };

    let res = compile_reads(reads, &mut def_map);

    assert_eq!(false, res.is_ok());
}

#[test]
fn expr_unwrap() {
    let src = "1{pad(norm(b[9-10]), 1)remove(f[CAGAGC])u[8]remove(b[10])}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = compile(desc);

    assert_eq!(true, res.is_ok());
}

#[test]
fn fail_reuse_label() {
    let src = "
brc = b[10]
1{<brc><brc>}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let (def, reads) = if let Expr::Description(d, r, _t) = res.clone().unwrap().0 {
        (d.deref().clone().unwrap(), r)
    } else {
        unreachable!()
    };

    let mut def_map = if let Ok(d) = compile_definitions(def.clone()) {
        d
    } else {
        todo!()
    };

    let res = compile_reads(reads, &mut def_map);

    assert_eq!(false, res.is_ok());
}

#[test]
fn def_block_fail() {
    let src = "
brc = b[10]
brc1 = pad(<brc>, 1)
1{<brc>}2{r:}";

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
fn compile_description() {
    let src = "
brc = b[10]
umi = pad(u[10], 1)
1{<brc>}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = compile(desc);

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

    let res = compile(desc);

    assert!(res.is_err())
}

#[test]
fn fail_label_composition() {
    let src = "
brc = remove(trunc(b[10], 3))
1{pad(<brc>, 1)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = compile(desc);

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

    let res = compile(desc);

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

    let res = compile(desc);

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

    let res = compile(desc);

    assert!(res.is_err())
}

#[test]
fn transform_update_map() {
    let src = "
brc = b[10]
umi = norm(u[9-11])
test = r:
1{pad(<brc>, 1)f<read1>[CAGAGC]<umi>f<another>[CAGA]}2{r<read>:}
 -> 1{<brc>remove(<read1>)remove(<umi>)<read>}
";
    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = compile(desc);

    assert!(res.is_ok())
}

#[test]
fn stack_orientation() {
    let src = "
brc = b[10]
umi = norm(u[9-11])
1{pad(<brc>, 1)f<read1>[CAGAGC]<umi>f<another>[CAGA]}2{r<read>:}
 -> 1{<brc>remove(<read1>)remove(pad(<umi>, 1))<read>}
";
    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = compile(desc);

    assert!(res.is_ok())
}

#[test]
fn compile_map_arguments() {
    let src = "1{map(b[10-11], \"file\", norm(self))}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = compile(desc);

    assert!(res.is_ok())
}

#[test]
fn compile_map_arguments_with_label() {
    let src = "
brc = b[10-11]    
1{map(<brc>>, \"file\", norm(self))}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let (res, _) = parser().parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()));

    let desc = res.clone().unwrap().0;

    let res = compile(desc);

    assert!(res.is_ok())
}
