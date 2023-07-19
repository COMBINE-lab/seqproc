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
    Self_,
    Reverse,
    ReverseComp,
    Truncate,
    TruncateLeft,
    TruncateTo,
    TruncateToLeft,
    Remove,
    Pad,
    PadLeft,
    PadTo,
    PadToLeft,
    Normalize,
    Map,
    MapWithMismatch,
    Hamming,
    TransformTo,
    Arg(usize),
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
            A => write!(f, "A"),
            T => write!(f, "T"),
            G => write!(f, "G"),
            C => write!(f, "C"),
            U => write!(f, "U"),
            File(p) => write!(f, "{}", p),
            Special(s) => write!(f, "{}", s),
            Reverse => write!(f, "Reverse"),
            ReverseComp => write!(f, "ReverseComp"),
            Truncate => write!(f, "Truncate"),
            TruncateLeft => write!(f, "TruncateLeft"),
            TruncateTo => write!(f, "TruncateTo"),
            TruncateToLeft => write!(f, "TruncateToLeft"),
            Remove => write!(f, "Remove"),
            Pad => write!(f, "Pad"),
            PadLeft => write!(f, "PadLeft"),
            PadTo => write!(f, "PadTo"),
            PadToLeft => write!(f, "PadToLeft"),
            Normalize => write!(f, "Normalize"),
            Map => write!(f, "Map"),
            MapWithMismatch => write!(f, "MapWithMismatch"),
            Hamming => write!(f, "Hamming"),
            Barcode => write!(f, "Barcode"),
            Umi => write!(f, "UMI"),
            Discard => write!(f, "Discard"),
            ReadSeq => write!(f, "ReadSeq"),
            FixedSeq => write!(f, "FixedSeq"),
            TransformTo => write!(f, "Transform into"),
            Self_ => write!(f, "Self"),
            Arg(n) => write!(f, "argument {n}"),
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

    let argument = just('$')
        .then(text::int(10).from_str().unwrapped())
        .map(|(_, n)| Token::Arg(n));

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
        "trunc" => Token::Truncate,
        "trunc_left" => Token::TruncateLeft,
        "trunc_to" => Token::TruncateTo,
        "trunc_to_left" => Token::TruncateToLeft,
        "pad" => Token::Pad,
        "pad_left" => Token::PadLeft,
        "pad_to" => Token::PadTo,
        "pad_to_left" => Token::PadToLeft,
        "norm" => Token::Normalize,
        "map_with_mismatch" => Token::MapWithMismatch,
        "map" => Token::Map,
        "hamming" => Token::Hamming,
        "self" => Token::Self_,
        "b" => Token::Barcode,
        "u" => Token::Umi,
        "r" => Token::ReadSeq,
        "x" => Token::Discard,
        "f" => Token::FixedSeq,
        _ => Token::Label(s),
    });

    let token = nucs
        .or(argument)
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
