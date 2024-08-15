use std::num::{NonZeroU8, ParseIntError};

use logos::Logos;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum TokenError {
    #[default]
    Unrecognized,
    InvalidNumber(ParseIntError),
}

impl From<ParseIntError> for TokenError {
    fn from(value: ParseIntError) -> Self {
        Self::InvalidNumber(value)
    }
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenError::Unrecognized => "unrecognized token".to_owned(),
                TokenError::InvalidNumber(e) => format!("{e}"),
            }
        )
    }
}

impl std::error::Error for TokenError {}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"\s+", error = TokenError)]
pub enum Token {
    // TODO(nenikitov): Remove this `unwrap` and do error handling
    #[regex(r"\d+", |l| l.slice().parse::<NonZeroU8>())]
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
