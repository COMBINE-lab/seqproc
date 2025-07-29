use chumsky::{error::Rich, Parser};
use seqproc::lexer::lexer;

pub fn into_input_tokens(i: &str) -> (Vec<seqproc::lexer::Token>, Vec<Rich<'_, char>>) {
    let (res, lex_err) = lexer().parse(i).into_output_errors();

    let res = res.unwrap();

    let input_tokens = res.iter().map(|(t, _)| t.clone()).collect::<Vec<_>>();

    (input_tokens, lex_err)
}
