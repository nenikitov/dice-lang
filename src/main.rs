mod parser;
mod token;

use std::error::Error;

use chumsky::{
    input::{Input, Stream},
    span::SimpleSpan,
    Parser,
};
use logos::Logos;
use token::Token;

fn main() -> Result<(), Box<dyn Error>> {
    let src = std::fs::read_to_string(
        std::env::args()
            .nth(1)
            .expect("First argument should be passed"),
    )?;

    let tokens = token::Token::lexer(&src)
        .spanned()
        .map(|(token, span)| match token {
            Ok(token) => (token, span),
            Err(error) => (Token::Error(error), span),
        })
        .map(|(token, span)| (token, span.into()));
    let tokens = Stream::from_iter(tokens).spanned((src.len()..src.len()).into());

    match parser::parser().parse(tokens).into_result() {
        Ok(ast) => println!("{ast:#?}"),
        Err(errors) => {
            for error in errors {
                println!("{error:?}");
            }
        }
    };

    Ok(())
}
