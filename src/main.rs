#![feature(let_chains)]

mod parser;
mod token;

mod token_new;
mod util_new;

use std::error::Error;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Input, Stream},
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
                Report::build(ReportKind::Error, (), error.span().start)
                    .with_code(3)
                    .with_message(error.to_string())
                    .with_label(
                        Label::new(error.span().into_range())
                            .with_message(error.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(&src))
                    .unwrap();
            }
        }
    };

    Ok(())
}
