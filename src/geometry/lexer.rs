use chumsky::prelude::*;
use std::{fmt, ops::Range};

pub type Span = Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(usize),
    Ctrl(char),
    Label(String),
    File(String),
    Special(char),
    Barcode,
    Umi,
    Discard,
    ReadSeq,
    FixedSeq,
    Reverse,
    ReverseComp,
    Trim,
    Remove,
    Pad,
    Normalize,
    Map,
    Hamming,
    TransformTo,
    Nuc(char),
    U,
    G,
    T,
    C,
    A,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match self {
            Num(n) => write!(f, "{}", n),
            Ctrl(c) => write!(f, "{}", c),
            Label(s) => write!(f, "{}", s),
            Nuc(n) => write!(f, "{}", n),
            A => write!(f, "A"),
            T => write!(f, "T"),
            G => write!(f, "G"),
            C => write!(f, "C"),
            U => write!(f, "U"),
            File(p) => write!(f, "{}", p),
            Special(s) => write!(f, "{}", s),
            Reverse => write!(f, "Reverse"),
            ReverseComp => write!(f, "ReverseComp"),
            Trim => write!(f, "Trim"),
            Remove => write!(f, "Remove"),
            Pad => write!(f, "Pad"),
            Normalize => write!(f, "Normalize"),
            Map => write!(f, "Map"),
            Hamming => write!(f, "Hamming"),
            Barcode => write!(f, "Barcode"),
            Umi => write!(f, "UMI"),
            Discard => write!(f, "Discard"),
            ReadSeq => write!(f, "ReadSeq"),
            FixedSeq => write!(f, "FixedSeq"),
            TransformTo => write!(f, "Transform into"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let int = text::int(10).from_str().unwrapped().map(Token::Num);

    let ctrl = one_of("()[]{},").map(Token::Ctrl);

    let label = just('<')
        .ignore_then(text::ident())
        .then_ignore(just('>'))
        .map(Token::Label);

    let special = one_of(":-=").map(Token::Special);

    let file = just('"')
        .ignored()
        .then(take_until(just('"').ignored()))
        .padded()
        .map(|(_, (f, _))| Token::File(f.into_iter().collect::<String>()));

    let transformto = just('-').then(just('>')).to(Token::TransformTo);

    let nucs = choice((
        just('A').to(Token::A),
        just('T').to(Token::T),
        just('G').to(Token::G),
        just('C').to(Token::C),
        just('U').to(Token::U),
    ));

    let ident = text::ident().map(|s: String| match s.as_str() {
        "rev" => Token::Reverse,
        "revcomp" => Token::ReverseComp,
        "remove" => Token::Remove,
        "trim" => Token::Trim,
        "pad" => Token::Pad,
        "norm" => Token::Normalize,
        "map" => Token::Map,
        "hamming" => Token::Hamming,
        "b" => Token::Barcode,
        "u" => Token::Umi,
        "r" => Token::ReadSeq,
        "x" => Token::Discard,
        "f" => Token::FixedSeq,
        _ => Token::Label(s),
    });

    let token = nucs
        .or(ident)
        .or(label)
        .or(transformto)
        .or(int)
        .or(ctrl)
        .or(special)
        .or(file)
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
        .collect()
}
