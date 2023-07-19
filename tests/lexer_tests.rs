use chumsky::prelude::*;
use seqproc::lexer::{lexer, Token};

#[test]
fn nucs() {
    let src = "GCA";

    let expected_res = vec![(Token::G, 0..1), (Token::C, 1..2), (Token::A, 2..3)];

    assert_eq!(expected_res, lexer().parse(src).unwrap());
}

#[test]
fn token() {
    let src = "1";

    assert_eq!(vec![(Token::Num(1), 0..1)], lexer().parse(src).unwrap());
}

#[test]
fn tokens() {
    let src = "bc1 = b[10]";

    assert_eq!(
        vec![
            (Token::Label("bc1".to_string()), 0..3),
            (Token::Special('='), 4..5),
            (Token::Barcode, 6..7),
            (Token::Ctrl('['), 7..8),
            (Token::Num(10), 8..10),
            (Token::Ctrl(']'), 10..11),
        ],
        lexer().parse(src).unwrap()
    )
}

#[test]
fn fail() {
    let src = "1 ? 2";

    let (_, err) = lexer().parse_recovery(src);

    assert_eq!(err.len(), 1)
}

#[test]
fn label() {
    let src = "barcode";

    let (res, err) = lexer().parse_recovery(src);

    assert_eq!(err.len(), 0);
    assert_eq!(
        vec![(Token::Label("barcode".to_string()), 0..7)],
        res.unwrap()
    )
}

#[test]
fn precidence() {
    let src = "b[1-2] -> 1{}";

    let (res, err) = lexer().parse_recovery(src);

    assert_eq!(err.len(), 0);

    let result = vec![
        &Token::Barcode,
        &Token::Ctrl('['),
        &Token::Num(1),
        &Token::Special('-'),
        &Token::Num(2),
        &Token::Ctrl(']'),
        &Token::TransformTo,
        &Token::Num(1),
        &Token::Ctrl('{'),
        &Token::Ctrl('}'),
    ];

    assert_eq!(
        result,
        res.unwrap().iter().map(|(tok, _)| tok).collect::<Vec<_>>()
    )
}

#[test]
fn map_vs_with_mismatch() {
    let src = "map()map_with_mismatch()";

    let (res, err) = lexer().parse_recovery(src);

    assert_eq!(err.len(), 0);

    let result = vec![
        &Token::Map,
        &Token::Ctrl('('),
        &Token::Ctrl(')'),
        &Token::MapWithMismatch,
        &Token::Ctrl('('),
        &Token::Ctrl(')'),
    ];

    assert_eq!(
        result,
        res.unwrap().iter().map(|(tok, _)| tok).collect::<Vec<_>>()
    )
}

#[test]
fn arguments() {
    let src = "map(f[ATG], $0, self)";

    let (res, err) = lexer().parse_recovery(src);
    dbg!(&err);
    assert_eq!(err.len(), 0);

    let result = vec![
        &Token::Map,
        &Token::Ctrl('('),
        &Token::FixedSeq,
        &Token::Ctrl('['),
        &Token::A,
        &Token::T,
        &Token::G,
        &Token::Ctrl(']'),
        &Token::Ctrl(','),
        &Token::Arg(0),
        &Token::Ctrl(','),
        &Token::Self_,
        &Token::Ctrl(')'),
    ];

    assert_eq!(
        result,
        res.unwrap().iter().map(|(tok, _)| tok).collect::<Vec<_>>()
    )
}
