use std::num::NonZeroU8;

use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"\s+")]
pub enum Token {
    // TODO(nenikitov): Remove this `unwrap` and do error handling
    #[regex(r"\d+", |l| l.slice().parse::<NonZeroU8>().unwrap())]
    Number(NonZeroU8),

    #[token("d", priority = 3)]
    DieIndicator,

    #[token(":")]
    Colon,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Multiply,

    #[token("/")]
    #[token("/-")]
    DivideFloor,

    #[token("/+")]
    DivideCeil,

    #[token("/~")]
    DivideRound,

    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[regex(r"[a-z]+", |l| l.slice().to_owned())]
    Identifier(String),
}

