use std::collections::HashMap;

use chumsky::{prelude::*, Stream};
use seqproc::{
    compile::{compile, definitions::compile_definitions, reads::compile_reads, utils::Error},
    execute::compile_geom,
    lexer::lexer,
    parser::parser,
};

#[test]
fn no_err() -> Result<(), Error> {
    let src = "1{remove(hamming(f[CAG], 1))}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    compile_reads(res.reads, HashMap::new())?;

    Ok(())
}

#[test]
fn fail_norm() {
    let src = "1{norm(r:)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let res = compile_reads(res.reads, HashMap::new());

    assert!(res.is_err());
}

#[test]
fn pass_composition() {
    let src = "1{trunc_to(rev(r:), 1)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let res = compile_reads(res.reads, HashMap::new());

    assert!(res.is_ok());
}

#[test]
fn fail_remove() {
    let src = "1{rev(remove(r:))}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let res = compile_reads(res.reads, HashMap::new());

    assert!(res.is_err());
}

#[test]
fn discard_as_void() {
    let src = "1{rev(x[10])}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let res = compile_reads(res.reads, HashMap::new());

    assert!(res.is_err());
}

#[test]
fn ok_definition() -> Result<(), Error> {
    let src = "
brc = b[10]
brc1 = b[1-4]
1{<brc>}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let def_map = compile_definitions(res.definitions)?;

    assert_eq!(2, def_map.len());

    Ok(())
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

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let def_map = compile_definitions(res.definitions);

    assert!(def_map.is_err());
}

#[test]
fn label_replacement() {
    let src = "test = r: 
    1{pad_to(<test>, 5, A)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let def_map = compile_definitions(res.definitions).unwrap();

    let res = compile_reads(res.reads, def_map);

    assert!(res.is_err());
}

#[test]
fn no_variable() {
    let src = "testing = r: 
    1{pad(<test>, 5, A)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let def_map = compile_definitions(res.definitions).unwrap();

    let res = compile_reads(res.reads, def_map);

    assert!(res.is_err());
}

#[test]
fn expr_unwrap() -> Result<(), Error> {
    let src = "1{pad(norm(b[9-10]), 1, A)remove(f[CAGAGC])u[8]remove(b[10])}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn fail_reuse_label() {
    let src = "
brc = b[10]
1{<brc><brc>}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let def_map = compile_definitions(res.definitions).unwrap();

    let res = compile_reads(res.reads, def_map);

    assert!(res.is_err());
}

#[test]
fn def_block_fail() {
    let src = "
brc = b[10]
brc1 = pad(<brc>, 1, A)
1{<brc>}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let def_map = compile_definitions(res.definitions);

    assert!(def_map.is_err());
}

#[test]
fn compile_description() -> Result<(), Error> {
    let src = "
brc = b[10]
umi = pad(u[10], 1, A)
1{<brc>}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn fail_description() {
    let src = "
brc = b[10]
umi = pad(u[10], 1, A)
1{<brc><brc>}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let res = compile(res);

    assert!(res.is_err());
}

#[test]
fn fail_label_composition() {
    let src = "
brc = remove(trunc(b[10], 3))
1{pad(<brc>, 1, A)}2{r:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let res = compile(res);

    assert!(res.is_err());
}

#[test]
fn valid_geom() -> Result<(), Error> {
    let src = "1{b<brc1>[9-11]remove(f[CAGAGC])u<umi>[8]b<brc2>[10]}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn invalid_geom_one() {
    let src = "1{b[9-11]f[CAGAGC]r:u[8]b[10]}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let res = compile(res);

    assert!(res.is_err());
}

#[test]
fn invalid_geom_two() {
    let src = "1{f[GAG]b[10-11]b[10]}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    let res = compile(res);

    assert!(res.is_err());
}

#[test]
fn transform_update_map() -> Result<(), Error> {
    let src = "
brc = b[10]
umi = norm(u[9-11])
test = r:
1{pad(<brc>, 1, A)f<read1>[CAGAGC]<umi>f<another>[CAGA]}2{r<read>:}
 -> 1{<brc>remove(<read1>)remove(<umi>)<read>}
";
    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn stack_orientation() -> Result<(), Error> {
    let src = "
brc = b[10]
umi = norm(u[9-11])
1{pad(<brc>, 1, A)f<read1>[CAGAGC]<umi>f<another>[CAGA]}2{r<read>:}
 -> 1{<brc>remove(<read1>)remove(pad(<umi>, 1, A))<read>}
";
    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn compile_map_arguments() -> Result<(), Error> {
    let src = "1{map(b[10-11], \"file\", norm(self))}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn compile_map_arguments_with_label() -> Result<(), Error> {
    let src = "
brc = b[10-11]    
1{map(<brc>>, \"file\", norm(self))}2{r<read>:}";

    let (res, _) = lexer().parse_recovery(src);

    let res = res.unwrap();

    let len = res.len();

    let res = parser()
        .parse_recovery(Stream::from_iter(len..len + 1, res.into_iter()))
        .0
        .unwrap();

    compile(res)?;

    Ok(())
}

#[test]
fn test_simplified_geom() {
    let geom = String::from("1{b[9-10]f[CAGAGC]u[8]b[10]}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]u[8]b[10]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_simplified_geom_with_transformation() {
    let geom = String::from(
        "1{b<brc>[9-10]f[CAGAGC]u<umi>[8]b<brc2>[10]}2{r<read>:} -> 1{<brc><brc2><umi>}2{<read>}",
    );

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[10]u[8]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_desc_with_remove() {
    let geom = String::from("1{b[9-10]f[CAGAGC]remove(u[8])b[10]}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[10]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_desc_with_pad() {
    let geom = String::from("1{b[9-10]f[CAGAGC]remove(u[8])pad_to(b[10], 13, A)}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[13]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_desc_with_trunc() {
    let geom = String::from("1{b[9-10]f[CAGAGC]remove(u[8])trunc(b[10], 3)}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[7]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}

#[test]
fn test_simplified_geom_from_def() {
    let geom = String::from("brc = b[9-10] 1{<brc>f[CAGAGC]remove(u[8])b[10]}2{r:}");

    let res = compile_geom(geom);

    assert!(res.is_ok());
    assert_eq!(
        "1{b[11]b[10]}2{r:}",
        res.ok().unwrap().get_simplified_description_string()
    );
}
