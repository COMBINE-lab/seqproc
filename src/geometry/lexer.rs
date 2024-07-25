//! Defines the lexer for EFGDL.
use std::fmt::{self, Write};

use chumsky::prelude::*;

use super::Span;

/// A token produced by the EFGDL lexer.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    /// A numeric literal.
    Num(usize),
    /// `(`.
    LParen,
    /// `)`.
    RParen,
    /// `[`.
    LBracket,
    /// `]`.
    RBracket,
    /// `{`.
    LBrace,
    /// `}`.
    RBrace,
    /// `,`.
    Comma,
    /// `<label_text>`.
    Label(String),
    /// `"file_path"`.
    File(String),
    /// `=`.
    Equals,
    /// `-`.
    Dash,
    /// `:`.
    Colon,
    /// `b`.
    Barcode,
    /// `u`.
    Umi,
    /// `x`.
    Discard,
    /// `r`.
    ReadSeq,
    /// `f`.
    FixedSeq,
    /// `self`.
    Self_,
    /// `rev`.
    Reverse,
    /// `revcomp`.
    ReverseComp,
    /// `trunc`.
    Truncate,
    /// `trunc_left`.
    TruncateLeft,
    /// `trunc_to`.
    TruncateTo,
    /// `trunc_to_left`.
    TruncateToLeft,
    /// `remove`.
    Remove,
    /// `pad`.
    Pad,
    /// `pad_left`.
    PadLeft,
    /// `pad_to`.
    PadTo,
    /// `pad_to_left`.
    PadToLeft,
    /// `norm`.
    Normalize,
    /// `norm`.
    Map,
    /// `map_with_mismatch`.
    MapWithMismatch,
    /// `filter_within_dist`.
    FilterWithinDist,
    /// `hamming`.
    Hamming,
    /// `->`.
    TransformTo,
    /// `$n`, where `n` is a numeric literal.
    Arg(usize),
    /// Nucleotide `U`.
    U,
    /// Nucleotide `G`.
    G,
    /// Nucleotide `T`.
    T,
    /// Nucleotide `C`.
    C,
    /// Nucleotide `A`.
    A,
    /// Reserved label beginning - cannot being label with '_'
    Reserved(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Reserved(s) => write!(f, "cannot prefix labed with _: {s}"),
            Num(n) => write!(f, "{n}"),
            LParen => f.write_char('('),
            RParen => f.write_char(')'),
            LBracket => f.write_char('['),
            RBracket => f.write_char(']'),
            LBrace => f.write_char('{'),
            RBrace => f.write_char('}'),
            Comma => f.write_char(','),
            Label(s) => write!(f, "<{s}>"),
            A => f.write_char('A'),
            T => f.write_char('T'),
            G => f.write_char('G'),
            C => f.write_char('C'),
            U => f.write_char('U'),
            File(p) => write!(f, "\"{p}\""),
            Equals => f.write_char('='),
            Dash => f.write_char('-'),
            Colon => f.write_char(':'),
            Reverse => f.write_str("rev"),
            ReverseComp => f.write_str("revcomp"),
            Truncate => f.write_str("trunc"),
            TruncateLeft => f.write_str("trunc_left"),
            TruncateTo => f.write_str("trunc_to"),
            TruncateToLeft => f.write_str("trunc_to_left"),
            Remove => f.write_str("remove"),
            Pad => f.write_str("pad"),
            PadLeft => f.write_str("pad_left"),
            PadTo => f.write_str("pad_to"),
            PadToLeft => f.write_str("pad_to_left"),
            Normalize => f.write_str("norm"),
            Map => f.write_str("map"),
            MapWithMismatch => f.write_str("map_with_mismatch"),
            FilterWithinDist => f.write_str("filter_within_dist"),
            Hamming => f.write_str("hamming"),
            Barcode => f.write_char('b'),
            Umi => f.write_char('u'),
            Discard => f.write_char('x'),
            ReadSeq => f.write_char('r'),
            FixedSeq => f.write_char('f'),
            TransformTo => f.write_str("->"),
            Self_ => f.write_str("self"),
            Arg(n) => write!(f, "${n}"),
        }
    }
}

/// Returns a lexer for EFGDL.
pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let int = text::int(10).from_str().unwrapped().map(Token::Num);

    let ctrl = choice((
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just(',').to(Token::Comma),
    ));

    let label = just('<')
        .ignore_then(text::ident())
        .then_ignore(just('>'))
        .map(|s: String| {
            if s.starts_with("_") {
                Token::Reserved(s)
            } else {
                Token::Label(s)
            }
        });

    let special = choice((
        just('=').to(Token::Equals),
        just('-').to(Token::Dash),
        just(':').to(Token::Colon),
    ));

    let file = just('"')
        .ignored()
        .then(take_until(just('"').ignored()))
        .padded()
        .map(|((), (f, _))| Token::File(f.into_iter().collect::<String>()));

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
        "filter_within_dist" => Token::FilterWithinDist,
        "map" => Token::Map,
        "hamming" => Token::Hamming,
        "self" => Token::Self_,
        "b" => Token::Barcode,
        "u" => Token::Umi,
        "r" => Token::ReadSeq,
        "x" => Token::Discard,
        "f" => Token::FixedSeq,
        _ => {
            if s.starts_with("_") {
                Token::Reserved(s)
            } else {
                Token::Label(s)
            }
        }
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
