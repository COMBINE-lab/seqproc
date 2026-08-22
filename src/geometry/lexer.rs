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
    /// '<'
    LAngle,
    /// '>'
    RAngle,
    /// `,`.
    Comma,
    /// `|`, used for ordered layout alternatives in EFGDL 2.
    Pipe,
    /// `?`, used for optional layout terms in EFGDL 2.
    Question,
    /// `*`, used for fixed layout repetition in EFGDL 2.
    Star,
    /// `label_text`.
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
    /// `s`.
    SampleBarcode,
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
    /// `filter`
    Filter,
    /// `hamming`.
    Hamming,
    /// `edit` - edit distance (Levenshtein) matching.
    Edit,
    /// `map_with_edit` - map with edit distance tolerance.
    MapWithEdit,
    /// `anchor_relative` - search for anchor from position 0 and extract preceding elements with flexible length.
    Anchor,
    /// `#[` - annotation start.
    HashBracket,
    /// `match` keyword.
    Match,
    /// `=>` - fat arrow for match arms.
    FatArrow,
    /// `.` - dot for attribute access.
    Dot,
    /// `fw` - forward orientation.
    Fw,
    /// `rc` - reverse complement orientation.
    Rc,
    /// `->`.
    TransformTo,
    /// `$n`, where `n` is a numeric literal.
    Arg(usize),
    /// `$name`, where `name` is a declared resource identifier.
    NamedArg(String),
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
    /// Reserved label beginning - cannot begin label with '_'
    Reserved(String),
    /// End of File token for parser
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            EOF => write!(f, "EOF"),
            Reserved(s) => write!(f, "cannot prefix labed with '_': {s}"),
            Num(n) => write!(f, "{n}"),
            LParen => f.write_char('('),
            RParen => f.write_char(')'),
            LBracket => f.write_char('['),
            RBracket => f.write_char(']'),
            LBrace => f.write_char('{'),
            RBrace => f.write_char('}'),
            LAngle => f.write_char('<'),
            RAngle => f.write_char('>'),
            Comma => f.write_char(','),
            Pipe => f.write_char('|'),
            Question => f.write_char('?'),
            Star => f.write_char('*'),
            Label(s) => write!(f, "{s}"),
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
            Filter => f.write_str("filter"),
            Hamming => f.write_str("hamming"),
            Edit => f.write_str("edit"),
            MapWithEdit => f.write_str("map_with_edit"),
            Anchor => f.write_str("anchor_relative"),
            HashBracket => f.write_str("#["),
            Match => f.write_str("match"),
            FatArrow => f.write_str("=>"),
            Dot => f.write_char('.'),
            Fw => f.write_str("fw"),
            Rc => f.write_str("rc"),
            Barcode => f.write_char('b'),
            SampleBarcode => f.write_char('s'),
            Umi => f.write_char('u'),
            Discard => f.write_char('x'),
            ReadSeq => f.write_char('r'),
            FixedSeq => f.write_char('f'),
            TransformTo => f.write_str("->"),
            Self_ => f.write_str("self"),
            Arg(n) => write!(f, "${n}"),
            NamedArg(name) => write!(f, "${name}"),
        }
    }
}

fn identifier_token(s: &str) -> Token {
    match s {
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
        "filter" => Token::Filter,
        "map" => Token::Map,
        "hamming" => Token::Hamming,
        "edit" => Token::Edit,
        "map_with_edit" => Token::MapWithEdit,
        "anchor_relative" => Token::Anchor,
        "match" => Token::Match,
        "fw" => Token::Fw,
        "rc" => Token::Rc,
        "self" => Token::Self_,
        "b" => Token::Barcode,
        "s" => Token::SampleBarcode,
        "u" => Token::Umi,
        "r" => Token::ReadSeq,
        "x" => Token::Discard,
        "f" => Token::FixedSeq,
        _ if s.starts_with('_') => Token::Reserved(s.to_owned()),
        _ => Token::Label(s.to_owned()),
    }
}

/// Returns a lexer for EFGDL.
pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token, Span)>, extra::Err<Rich<'src, char>>> {
    let integer = || {
        text::int(10).try_map(|digits: &str, span| {
            digits.parse::<usize>().map_err(|_| {
                Rich::custom(
                    span,
                    "integer literal exceeds the maximum supported platform value",
                )
            })
        })
    };
    let int = integer().map(Token::Num);

    let ctrl = choice((
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just(',').to(Token::Comma),
        just('|').to(Token::Pipe),
        just('?').to(Token::Question),
        just('*').to(Token::Star),
        just('<').to(Token::LAngle),
        just('>').to(Token::RAngle),
    ));

    let special = choice((
        just('=').to(Token::Equals),
        just('-').to(Token::Dash),
        just(':').to(Token::Colon),
        just('.').to(Token::Dot),
    ));

    let file = just('"')
        .ignored()
        .then(
            any()
                .and_is(just('"').not())
                .repeated()
                .collect::<Vec<_>>()
                .then(just('"')),
        )
        // .then(take_until(just('"').ignored()))
        .padded()
        .map(|((), (f, _))| Token::File(f.into_iter().collect::<String>()));

    let transformto = just('-').then(just('>')).to(Token::TransformTo);

    let fatarrow = just('=').then(just('>')).to(Token::FatArrow);

    let hash_bracket = just('#').then(just('[')).to(Token::HashBracket);

    let argument = just('$').then(integer()).map(|(_, n)| Token::Arg(n));

    let named_argument = just('$')
        .ignore_then(text::ident())
        .map(|name: &str| Token::NamedArg(name.to_owned()));

    let nucs = choice((
        just('A').to(Token::A),
        just('T').to(Token::T),
        just('G').to(Token::G),
        just('C').to(Token::C),
        just('U').to(Token::U),
    ));

    let ident = text::ident().map(identifier_token);

    // A fixed sequence such as `ACGT` must remain four nucleotide tokens,
    // while an identifier such as `Anchor1` must remain one label. Because
    // nucleotide tokens otherwise win after their first character, recognize
    // the latter class before the single-nucleotide parser.
    let nucleotide_prefixed_ident = text::ident().try_map(|s: &str, span| {
        let starts_with_nucleotide = s
            .as_bytes()
            .first()
            .is_some_and(|base| matches!(base, b'A' | b'C' | b'G' | b'T' | b'U'));
        let is_fixed_sequence = s
            .as_bytes()
            .iter()
            .all(|base| matches!(base, b'A' | b'C' | b'G' | b'T' | b'U'));
        if starts_with_nucleotide && !is_fixed_sequence {
            Ok(identifier_token(s))
        } else {
            Err(Rich::custom(span, "not a nucleotide-prefixed identifier"))
        }
    });

    // TODO: remove recovery
    let token = choice((
        nucleotide_prefixed_ident,
        nucs,
        argument,
        named_argument,
        ident,
        hash_bracket,
        fatarrow,
        transformto,
        int,
        ctrl,
        special,
        file,
    ));

    // Comments: # to end of line (but not #[ which starts an annotation)
    let comment = just('#')
        .then(none_of("[\n\r").then(none_of("\n\r").repeated()).or_not())
        .to(())
        .padded();

    token
        .map_with(|tok, state| Some((tok, state.span())))
        .or(comment.map(|_| None))
        .padded_by(text::whitespace())
        .repeated()
        .collect::<Vec<_>>()
        .map(|tokens| tokens.into_iter().flatten().collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::Parser;

    fn lex(input: &str) -> Vec<Token> {
        lexer()
            .parse(input)
            .into_result()
            .unwrap()
            .into_iter()
            .map(|(tok, _)| tok)
            .collect()
    }

    #[test]
    fn test_lex_simple_geometry() {
        let tokens = lex("1{b[16]}2{r:}");
        assert!(tokens.contains(&Token::Num(1)));
        assert!(tokens.contains(&Token::Num(16)));
        assert!(tokens.contains(&Token::Barcode));
        assert!(tokens.contains(&Token::ReadSeq));
        assert!(tokens.contains(&Token::LBrace));
        assert!(tokens.contains(&Token::RBrace));
        assert!(tokens.contains(&Token::LBracket));
        assert!(tokens.contains(&Token::RBracket));
        assert!(tokens.contains(&Token::Colon));
    }

    #[test]
    fn test_lex_nucleotides() {
        let tokens = lex("A T G C U");
        assert_eq!(
            tokens,
            vec![Token::A, Token::T, Token::G, Token::C, Token::U]
        );
    }

    #[test]
    fn test_lex_interval_types() {
        let tokens = lex("b s u x r f");
        assert_eq!(
            tokens,
            vec![
                Token::Barcode,
                Token::SampleBarcode,
                Token::Umi,
                Token::Discard,
                Token::ReadSeq,
                Token::FixedSeq
            ]
        );
    }

    #[test]
    fn test_lex_functions() {
        let tokens = lex("rev revcomp trunc trunc_left trunc_to trunc_to_left");
        assert!(tokens.contains(&Token::Reverse));
        assert!(tokens.contains(&Token::ReverseComp));
        assert!(tokens.contains(&Token::Truncate));
        assert!(tokens.contains(&Token::TruncateLeft));
        assert!(tokens.contains(&Token::TruncateTo));
        assert!(tokens.contains(&Token::TruncateToLeft));
    }

    #[test]
    fn test_lex_more_functions() {
        let tokens = lex("remove pad pad_left pad_to pad_to_left norm");
        assert!(tokens.contains(&Token::Remove));
        assert!(tokens.contains(&Token::Pad));
        assert!(tokens.contains(&Token::PadLeft));
        assert!(tokens.contains(&Token::PadTo));
        assert!(tokens.contains(&Token::PadToLeft));
        assert!(tokens.contains(&Token::Normalize));
    }

    #[test]
    fn test_lex_map_functions() {
        let tokens = lex("map map_with_mismatch filter filter_within_dist hamming edit map_with_edit anchor_relative");
        assert!(tokens.contains(&Token::Map));
        assert!(tokens.contains(&Token::MapWithMismatch));
        assert!(tokens.contains(&Token::Filter));
        assert!(tokens.contains(&Token::FilterWithinDist));
        assert!(tokens.contains(&Token::Hamming));
        assert!(tokens.contains(&Token::Edit));
        assert!(tokens.contains(&Token::MapWithEdit));
        assert!(tokens.contains(&Token::Anchor));
    }

    #[test]
    fn test_lex_transform_to() {
        let tokens = lex("->");
        assert_eq!(tokens, vec![Token::TransformTo]);
    }

    #[test]
    fn test_lex_argument() {
        let tokens = lex("$1 $2 $3");
        assert_eq!(tokens, vec![Token::Arg(1), Token::Arg(2), Token::Arg(3)]);
    }

    #[test]
    fn test_lex_file() {
        let tokens = lex("\"path/to/file.txt\"");
        assert_eq!(tokens, vec![Token::File("path/to/file.txt".to_string())]);
    }

    #[test]
    fn test_lex_label() {
        let tokens = lex("myLabel");
        assert_eq!(tokens, vec![Token::Label("myLabel".to_string())]);
    }

    #[test]
    fn test_lex_reserved() {
        let tokens = lex("_reserved");
        assert_eq!(tokens, vec![Token::Reserved("_reserved".to_string())]);
    }

    #[test]
    fn test_lex_self() {
        let tokens = lex("self");
        assert_eq!(tokens, vec![Token::Self_]);
    }

    #[test]
    fn test_lex_comment() {
        let tokens = lex("b # this is a comment\nu");
        assert_eq!(tokens, vec![Token::Barcode, Token::Umi]);
    }

    #[test]
    fn test_lex_ctrl_tokens() {
        let tokens = lex("( ) [ ] { } , < >");
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::RParen,
                Token::LBracket,
                Token::RBracket,
                Token::LBrace,
                Token::RBrace,
                Token::Comma,
                Token::LAngle,
                Token::RAngle,
            ]
        );
    }

    #[test]
    fn test_lex_special() {
        let tokens = lex("= -");
        assert_eq!(tokens, vec![Token::Equals, Token::Dash]);
    }

    #[test]
    fn test_lex_complex_geometry() {
        let tokens = lex("1{b[16]u[10]r:}2{x[10]b[8]r:}");
        assert!(tokens.len() > 10);
        // Verify key tokens are present
        assert!(tokens.contains(&Token::Num(16)));
        assert!(tokens.contains(&Token::Num(10)));
        assert!(tokens.contains(&Token::Num(8)));
        assert!(tokens.contains(&Token::Umi));
        assert!(tokens.contains(&Token::Discard));
    }

    #[test]
    fn test_lex_ranged_len() {
        let tokens = lex("b[8-12]");
        assert!(tokens.contains(&Token::Barcode));
        assert!(tokens.contains(&Token::Num(8)));
        assert!(tokens.contains(&Token::Dash));
        assert!(tokens.contains(&Token::Num(12)));
    }

    #[test]
    fn test_lex_fixed_seq() {
        let tokens = lex("f[ACGT]");
        assert!(tokens.contains(&Token::FixedSeq));
        assert!(tokens.contains(&Token::A));
        assert!(tokens.contains(&Token::C));
        assert!(tokens.contains(&Token::G));
        assert!(tokens.contains(&Token::T));
    }

    #[test]
    fn test_token_display() {
        assert_eq!(format!("{}", Token::EOF), "EOF");
        assert_eq!(format!("{}", Token::Num(42)), "42");
        assert_eq!(format!("{}", Token::LParen), "(");
        assert_eq!(format!("{}", Token::RParen), ")");
        assert_eq!(format!("{}", Token::LBracket), "[");
        assert_eq!(format!("{}", Token::RBracket), "]");
        assert_eq!(format!("{}", Token::LBrace), "{");
        assert_eq!(format!("{}", Token::RBrace), "}");
        assert_eq!(format!("{}", Token::LAngle), "<");
        assert_eq!(format!("{}", Token::RAngle), ">");
        assert_eq!(format!("{}", Token::Comma), ",");
        assert_eq!(format!("{}", Token::Equals), "=");
        assert_eq!(format!("{}", Token::Dash), "-");
        assert_eq!(format!("{}", Token::Colon), ":");
        assert_eq!(format!("{}", Token::Reverse), "rev");
        assert_eq!(format!("{}", Token::ReverseComp), "revcomp");
        assert_eq!(format!("{}", Token::Truncate), "trunc");
        assert_eq!(format!("{}", Token::TruncateLeft), "trunc_left");
        assert_eq!(format!("{}", Token::TruncateTo), "trunc_to");
        assert_eq!(format!("{}", Token::TruncateToLeft), "trunc_to_left");
        assert_eq!(format!("{}", Token::Remove), "remove");
        assert_eq!(format!("{}", Token::Pad), "pad");
        assert_eq!(format!("{}", Token::PadLeft), "pad_left");
        assert_eq!(format!("{}", Token::PadTo), "pad_to");
        assert_eq!(format!("{}", Token::PadToLeft), "pad_to_left");
        assert_eq!(format!("{}", Token::Normalize), "norm");
        assert_eq!(format!("{}", Token::Map), "map");
        assert_eq!(format!("{}", Token::MapWithMismatch), "map_with_mismatch");
        assert_eq!(format!("{}", Token::FilterWithinDist), "filter_within_dist");
        assert_eq!(format!("{}", Token::Filter), "filter");
        assert_eq!(format!("{}", Token::Hamming), "hamming");
        assert_eq!(format!("{}", Token::Edit), "edit");
        assert_eq!(format!("{}", Token::MapWithEdit), "map_with_edit");
        assert_eq!(format!("{}", Token::Anchor), "anchor_relative");
        assert_eq!(format!("{}", Token::Barcode), "b");
        assert_eq!(format!("{}", Token::SampleBarcode), "s");
        assert_eq!(format!("{}", Token::Umi), "u");
        assert_eq!(format!("{}", Token::Discard), "x");
        assert_eq!(format!("{}", Token::ReadSeq), "r");
        assert_eq!(format!("{}", Token::FixedSeq), "f");
        assert_eq!(format!("{}", Token::TransformTo), "->");
        assert_eq!(format!("{}", Token::Self_), "self");
        assert_eq!(format!("{}", Token::Arg(1)), "$1");
        assert_eq!(format!("{}", Token::A), "A");
        assert_eq!(format!("{}", Token::T), "T");
        assert_eq!(format!("{}", Token::G), "G");
        assert_eq!(format!("{}", Token::C), "C");
        assert_eq!(format!("{}", Token::U), "U");
        assert_eq!(format!("{}", Token::Label("foo".into())), "foo");
        assert_eq!(format!("{}", Token::File("bar.txt".into())), "\"bar.txt\"");
        assert_eq!(
            format!("{}", Token::Reserved("_r".into())),
            "cannot prefix labed with '_': _r"
        );
        assert_eq!(format!("{}", Token::HashBracket), "#[");
        assert_eq!(format!("{}", Token::Match), "match");
        assert_eq!(format!("{}", Token::FatArrow), "=>");
        assert_eq!(format!("{}", Token::Dot), ".");
        assert_eq!(format!("{}", Token::Fw), "fw");
        assert_eq!(format!("{}", Token::Rc), "rc");
    }

    #[test]
    fn test_lex_annotation() {
        let tokens = lex("#[match_ori(either)]");
        assert_eq!(
            tokens,
            vec![
                Token::HashBracket,
                Token::Label("match_ori".to_string()),
                Token::LParen,
                Token::Label("either".to_string()),
                Token::RParen,
                Token::RBracket,
            ]
        );
    }

    #[test]
    fn test_lex_match_block_tokens() {
        let tokens = lex("match 1.ori { fw => rc => }");
        assert!(tokens.contains(&Token::Match));
        assert!(tokens.contains(&Token::Dot));
        assert!(tokens.contains(&Token::Label("ori".to_string())));
        assert!(tokens.contains(&Token::Fw));
        assert!(tokens.contains(&Token::FatArrow));
        assert!(tokens.contains(&Token::Rc));
    }

    #[test]
    fn test_lex_comment_vs_annotation() {
        // # followed by non-[ is a comment
        let tokens = lex("b # this is a comment\nu");
        assert_eq!(tokens, vec![Token::Barcode, Token::Umi]);

        // #[ starts an annotation, not a comment
        let tokens = lex("#[foo(bar)]");
        assert_eq!(
            tokens,
            vec![
                Token::HashBracket,
                Token::Label("foo".to_string()),
                Token::LParen,
                Token::Label("bar".to_string()),
                Token::RParen,
                Token::RBracket,
            ]
        );
    }

    #[test]
    fn test_lex_fat_arrow_vs_equals() {
        // => should be FatArrow, not Equals + RAngle
        let tokens = lex("=>");
        assert_eq!(tokens, vec![Token::FatArrow]);

        // = alone is Equals
        let tokens = lex("=");
        assert_eq!(tokens, vec![Token::Equals]);
    }

    // ---------------------------------------------------------------
    // SampleBarcode (`s`) token tests
    // ---------------------------------------------------------------

    #[test]
    fn test_lex_sample_barcode_alone() {
        assert_eq!(lex("s"), vec![Token::SampleBarcode]);
    }

    #[test]
    fn test_lex_sample_barcode_with_length() {
        let tokens = lex("s[8]");
        assert_eq!(
            tokens,
            vec![
                Token::SampleBarcode,
                Token::LBracket,
                Token::Num(8),
                Token::RBracket,
            ]
        );
    }

    #[test]
    fn test_lex_sample_barcode_with_label() {
        let tokens = lex("s<sample>[8]");
        assert_eq!(
            tokens,
            vec![
                Token::SampleBarcode,
                Token::LAngle,
                Token::Label("sample".into()),
                Token::RAngle,
                Token::LBracket,
                Token::Num(8),
                Token::RBracket,
            ]
        );
    }

    #[test]
    fn test_sample_barcode_not_reserved() {
        // Confirm bare `s` lexes to SampleBarcode, never as a generic Label.
        let tokens = lex("s");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::SampleBarcode);
        // And that it's not a Label either (would be Label("s") if the match
        // order were ever wrong).
        assert!(!matches!(tokens[0], Token::Label(_)));
    }

    #[test]
    fn test_10x_flex_geometry_lexes() {
        // 10x Chromium Flex: cell barcode (b), UMI (u), read (r), sample barcode (s).
        let tokens = lex("1{b[16]u[12]}2{r:x[28]s[8]}");
        assert!(tokens.contains(&Token::Barcode));
        assert!(tokens.contains(&Token::SampleBarcode));
        assert!(tokens.contains(&Token::Umi));
        assert!(tokens.contains(&Token::ReadSeq));
        assert!(tokens.contains(&Token::Discard));
        assert!(tokens.contains(&Token::Num(16)));
        assert!(tokens.contains(&Token::Num(12)));
        assert!(tokens.contains(&Token::Num(28)));
        assert!(tokens.contains(&Token::Num(8)));
    }
}

#[cfg(test)]
mod proptests {
    //! Property-based tests for the SampleBarcode (`s`) token and its
    //! interaction with the broader lexer/parser pipeline.
    use super::*;
    use chumsky::input::Input;
    use chumsky::Parser;
    use proptest::prelude::*;

    use crate::parser::{parser, Expr, IntervalKind, IntervalShape};
    use crate::S;

    fn lex(input: &str) -> Vec<Token> {
        lexer()
            .parse(input)
            .into_result()
            .unwrap()
            .into_iter()
            .map(|(tok, _)| tok)
            .collect()
    }

    fn parse_ok(src: &str) -> crate::parser::Description {
        let (lex_res, lex_errs) = lexer().parse(src).into_output_errors();
        assert!(lex_errs.is_empty(), "lex errors for {src:?}: {lex_errs:?}");
        let tokens = lex_res.unwrap();
        let tokens = tokens
            .into_iter()
            .map(|(tok, span)| chumsky::span::Spanned { inner: tok, span })
            .collect::<Vec<_>>();
        let input = tokens[..].split_spanned((0..src.len()).into());
        let (parse_res, parse_errs) = parser().parse(input).into_output_errors();
        assert!(
            parse_errs.is_empty(),
            "parse errors for {src:?}: {parse_errs:?}"
        );
        parse_res.unwrap()
    }

    // Words/tokens we must filter out of random label strategies because they
    // lex to keyword tokens rather than `Token::Label(_)`.
    const KEYWORDS: &[&str] = &[
        "b",
        "s",
        "u",
        "x",
        "r",
        "f",
        "self",
        "rev",
        "revcomp",
        "trunc",
        "trunc_left",
        "trunc_to",
        "trunc_to_left",
        "remove",
        "pad",
        "pad_left",
        "pad_to",
        "pad_to_left",
        "norm",
        "map",
        "map_with_mismatch",
        "filter_within_dist",
        "filter",
        "hamming",
        "edit",
        "map_with_edit",
        "anchor_relative",
        "match",
        "fw",
        "rc",
    ];

    proptest! {
        #![proptest_config(ProptestConfig { cases: 64, .. ProptestConfig::default() })]

        /// Property 1: lex-display roundtrip for `s[n]`.
        #[test]
        fn prop_lex_sample_barcode_fixed_len(n in 1usize..=256) {
            let src = format!("s[{}]", n);
            let tokens = lex(&src);
            prop_assert_eq!(
                tokens,
                vec![
                    Token::SampleBarcode,
                    Token::LBracket,
                    Token::Num(n),
                    Token::RBracket,
                ]
            );
        }

        /// Property 2: Display of IntervalKind::SampleBarcode is `"s"`, and
        /// feeding that back into the lexer produces a single SampleBarcode token.
        #[test]
        fn prop_display_lex_idempotent(_dummy in 0u8..1) {
            let kind = IntervalKind::SampleBarcode;
            let rendered = format!("{}", kind);
            prop_assert_eq!(&rendered, "s");
            prop_assert_eq!(lex(&rendered), vec![Token::SampleBarcode]);
        }

        /// Property 3: full parser roundtrip for `1{s[n]r:}` produces an AST
        /// containing IntervalKind::SampleBarcode and the Display round-trips
        /// to `s[n]`.
        #[test]
        fn prop_parser_roundtrip_sample_barcode(n in 1usize..=32) {
            let src = format!("1{{s[{}]r:}}", n);
            let desc = parse_ok(&src);
            let reads = &desc.reads.0;
            prop_assert_eq!(reads.len(), 1);
            let first_expr = &reads[0].0.exprs[0].0;
            match first_expr {
                Expr::GeomPiece(IntervalKind::SampleBarcode, IntervalShape::FixedLen(S(got_n, _))) => {
                    prop_assert_eq!(*got_n, n);
                }
                other => prop_assert!(false, "expected GeomPiece(SampleBarcode, FixedLen), got {:?}", other),
            }
            // Also check direct AST display (decoupled from parser choice).
            let manual = Expr::GeomPiece(
                IntervalKind::SampleBarcode,
                IntervalShape::FixedLen(S(n, (0..1).into())),
            );
            prop_assert_eq!(format!("{}", manual), format!("s[{}]", n));
        }

        /// Property 4: Label equivalence -- any valid short identifier that is
        /// NOT a keyword lexes cleanly as `Token::Label(label)` in the
        /// position of an inline label.
        #[test]
        fn prop_sample_barcode_inline_label(label in "[a-z][a-z_0-9]{0,15}") {
            // Filter out keywords / reserved identifiers and labels beginning
            // with '_' (which become Token::Reserved).
            prop_assume!(!KEYWORDS.contains(&label.as_str()));
            prop_assume!(!label.starts_with('_'));

            let src = format!("s<{}>[8]", label);
            let tokens = lex(&src);
            prop_assert_eq!(
                tokens,
                vec![
                    Token::SampleBarcode,
                    Token::LAngle,
                    Token::Label(label.clone()),
                    Token::RAngle,
                    Token::LBracket,
                    Token::Num(8),
                    Token::RBracket,
                ]
            );
        }

        /// Property 5: `b` and `s` are structurally equivalent modulo the
        /// IntervalKind. Parsing `1{b[n]r:}` and `1{s[n]r:}` yields ASTs
        /// that differ only in that one field.
        #[test]
        fn prop_b_and_s_are_structurally_equivalent(n in 1usize..=64) {
            let b_src = format!("1{{b[{}]r:}}", n);
            let s_src = format!("1{{s[{}]r:}}", n);
            let b_desc = parse_ok(&b_src);
            let s_desc = parse_ok(&s_src);

            let b_reads = &b_desc.reads.0;
            let s_reads = &s_desc.reads.0;
            prop_assert_eq!(b_reads.len(), s_reads.len());
            prop_assert_eq!(b_reads.len(), 1);

            let b_exprs = &b_reads[0].0.exprs;
            let s_exprs = &s_reads[0].0.exprs;
            prop_assert_eq!(b_exprs.len(), s_exprs.len());

            // First expr: b -> Barcode, s -> SampleBarcode; both FixedLen(n).
            match (&b_exprs[0].0, &s_exprs[0].0) {
                (
                    Expr::GeomPiece(IntervalKind::Barcode, IntervalShape::FixedLen(S(bn, _))),
                    Expr::GeomPiece(IntervalKind::SampleBarcode, IntervalShape::FixedLen(S(sn, _))),
                ) => {
                    prop_assert_eq!(*bn, n);
                    prop_assert_eq!(*sn, n);
                    prop_assert_eq!(*bn, *sn);
                }
                (b, s) => prop_assert!(
                    false,
                    "expected matching GeomPiece variants, got b={:?} s={:?}",
                    b, s
                ),
            }

            // Second expr: should be identical (ReadSeq, Unbounded) on both sides.
            prop_assert_eq!(&b_exprs[1].0, &s_exprs[1].0);
        }
    }
}
