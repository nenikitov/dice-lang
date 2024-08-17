use std::{
    fmt::Display,
    num::{NonZeroU8, ParseIntError},
};

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

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"\s+", error = TokenError)]
pub enum Token<'src> {
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

    #[token("/~")]
    DivideRound,

    #[token("/+")]
    DivideCeil,

    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[regex(r"[a-z]+")]
    Symbol(&'src str),

    Error(TokenError),
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Number(n) => format!("{n}"),
                Token::DieIndicator => "d".to_string(),
                Token::Colon => ":".to_string(),
                Token::Plus => "+".to_string(),
                Token::Minus => "-".to_string(),
                Token::Multiply => "*".to_string(),
                Token::DivideFloor => "/-".to_string(),
                Token::DivideRound => "/~".to_string(),
                Token::DivideCeil => "/+".to_string(),
                Token::ParenOpen => "(".to_string(),
                Token::ParenClose => ")".to_string(),
                Token::Symbol(s) => s.to_string(),
                Token::Error(e) => e.to_string(),
            }
        )
    }
}
