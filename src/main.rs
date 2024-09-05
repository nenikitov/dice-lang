#![feature(let_chains, trait_alias)]
#![allow(dead_code)]

mod token;

mod token_new;
mod util_new;

use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let src = std::fs::read_to_string(
        std::env::args()
            .nth(1)
            .expect("First argument should be passed"),
    )?;

    let tokens = crate::token_new::tok::tokenize(&src);
    // let tokens = Stream::from_iter(tokens).spanned((src.len()..src.len()).into());
    //let tokens = Stream::from_iter(tokens).spanned((src.len()..src.len()).into());

    // match parser::parser().parse(tokens).into_result() {
    //     Ok(ast) => println!("{ast:#?}"),
    //     Err(errors) => {
    //         for error in errors {
    //             Report::build(ReportKind::Error, (), error.span().start)
    //                 .with_code(3)
    //                 .with_message(error.to_string())
    //                 .with_label(
    //                     Label::new(error.span().into_range())
    //                         .with_message(error.reason().to_string())
    //                         .with_color(Color::Red),
    //                 )
    //                 .finish()
    //                 .eprint(Source::from(&src))
    //                 .unwrap();
    //         }
    //     }
    // };

    Ok(())
}
