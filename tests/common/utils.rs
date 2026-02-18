use chumsky::{error::Rich, input::Input, Parser};
use seqproc::{
    lexer::{self, Token},
    parser::{parser, Description},
};

pub struct ParsedInput<'a> {
    pub parse_res: Option<Description>,
    pub lex_errs: Vec<Rich<'a, char>>,
    pub parse_errs: Vec<Rich<'a, Token>>,
}

pub fn result_with_errs<'a>(input: &'a str) -> ParsedInput<'a> {
    // lex input
    let (lex_res, lex_errs) = lexer::lexer().parse(input).into_output_errors();
    let tokens = lex_res.unwrap();

    let tokens = tokens
        .into_iter()
        .map(|(tok, span)| chumsky::span::Spanned { inner: tok, span })
        .collect::<Vec<_>>();
    let input = tokens[..].split_spanned((0..input.len()).into());

    // parse token
    let (parse_res, parse_errs) = parser().parse(input).into_output_errors();

    let parse_errs = parse_errs
        .into_iter()
        .map(|r| Rich::custom(*r.span(), r.reason()))
        .collect::<Vec<_>>();

    ParsedInput {
        parse_res,
        lex_errs,
        parse_errs,
    }
}
