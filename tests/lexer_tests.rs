use chumsky::prelude::*;
use seqproc::lexer::{lexer, Token};

#[test]
fn nucs() {
    let src = "GCA";

    let expected_res = vec![
        (Token::G, SimpleSpan::from(0..1)),
        (Token::C, SimpleSpan::from(1..2)),
        (Token::A, SimpleSpan::from(2..3)),
    ];

    assert_eq!(expected_res, lexer().parse(src).into_output().unwrap());
}

#[test]
fn token() {
    let src = "1";

    assert_eq!(
        vec![(Token::Num(1), SimpleSpan::from(0..1))],
        lexer().parse(src).unwrap()
    );
}

#[test]
fn tokens() {
    let src = "bc1 = b[10]";

    assert_eq!(
        vec![
            (Token::Label("bc1".to_string()), SimpleSpan::from(0..3)),
            (Token::Equals, SimpleSpan::from(4..5)),
            (Token::Barcode, SimpleSpan::from(6..7)),
            (Token::LBracket, SimpleSpan::from(7..8)),
            (Token::Num(10), SimpleSpan::from(8..10)),
            (Token::RBracket, SimpleSpan::from(10..11)),
        ],
        lexer().parse(src).unwrap()
    );
}

#[test]
fn fail() {
    let src = "1 ? 2";

    let (_, err) = lexer().parse(src).into_output_errors();

    assert_eq!(err.len(), 1);
}

#[test]
fn label() {
    let src = "barcode";

    let (res, err) = lexer().parse(src).into_output_errors();

    assert_eq!(err.len(), 0);
    assert_eq!(
        vec![(Token::Label("barcode".to_string()), SimpleSpan::from(0..7))],
        res.unwrap()
    );
}

#[test]
fn precidence() {
    let src = "b[1-2] -> 1{}";

    let (res, err) = lexer().parse(src).into_output_errors();

    assert_eq!(err.len(), 0);

    let result = vec![
        &Token::Barcode,
        &Token::LBracket,
        &Token::Num(1),
        &Token::Dash,
        &Token::Num(2),
        &Token::RBracket,
        &Token::TransformTo,
        &Token::Num(1),
        &Token::LBrace,
        &Token::RBrace,
    ];

    assert_eq!(
        result,
        res.unwrap().iter().map(|(tok, _)| tok).collect::<Vec<_>>()
    );
}

#[test]
fn map_vs_with_mismatch() {
    let src = "map()map_with_mismatch()";

    let (res, err) = lexer().parse(src).into_output_errors();

    assert_eq!(err.len(), 0);

    let result = vec![
        &Token::Map,
        &Token::LParen,
        &Token::RParen,
        &Token::MapWithMismatch,
        &Token::LParen,
        &Token::RParen,
    ];

    assert_eq!(
        result,
        res.unwrap().iter().map(|(tok, _)| tok).collect::<Vec<_>>()
    );
}

#[test]
fn arguments() {
    let src = "map(f[ATG], $0, self)";

    let (res, err) = lexer().parse(src).into_output_errors();
    dbg!(&err);
    assert_eq!(err.len(), 0);

    let result = vec![
        &Token::Map,
        &Token::LParen,
        &Token::FixedSeq,
        &Token::LBracket,
        &Token::A,
        &Token::T,
        &Token::G,
        &Token::RBracket,
        &Token::Comma,
        &Token::Arg(0),
        &Token::Comma,
        &Token::Self_,
        &Token::RParen,
    ];

    assert_eq!(
        result,
        res.unwrap().iter().map(|(tok, _)| tok).collect::<Vec<_>>()
    );
}

#[test]
fn comments() {
    let src = "bc1 = b[10] # this is a comment\nbc2 = u[8]";

    let (res, err) = lexer().parse(src).into_output_errors();

    assert_eq!(err.len(), 0);

    let tokens: Vec<Token> = res.unwrap().into_iter().map(|(tok, _)| tok).collect();
    
    assert_eq!(tokens[0], Token::Label("bc1".to_string()));
    assert_eq!(tokens[1], Token::Equals);
    assert_eq!(tokens[2], Token::Barcode);
    assert_eq!(tokens[6], Token::Label("bc2".to_string()));
    assert_eq!(tokens[8], Token::Umi);
    assert_eq!(tokens.len(), 12);
}

#[test]
fn comment_only_line() {
    let src = "# Full line comment\nbc1 = b[8]";

    let (res, err) = lexer().parse(src).into_output_errors();

    assert_eq!(err.len(), 0);

    let tokens: Vec<Token> = res.unwrap().into_iter().map(|(tok, _)| tok).collect();
    
    assert_eq!(tokens[0], Token::Label("bc1".to_string()));
    assert_eq!(tokens[1], Token::Equals);
    assert_eq!(tokens[2], Token::Barcode);
    assert_eq!(tokens.len(), 6);
}
