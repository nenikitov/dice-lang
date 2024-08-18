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

    #[token("<")]
    LessThan,

    #[token("<=")]
    LessThanEqual,

    #[token(">")]
    GreaterThan,

    #[token(">=")]
    GreaterThanEqual,

    #[token("==")]
    Equal,

    #[token("!=")]
    NotEqual,

    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[regex(r"[a-z_]+")]
    Symbol(&'src str),

    Error(TokenError),
}

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Number(n) => n.to_string(),
                Token::DieIndicator => "d".to_string(),
                Token::Colon => ";".to_string(),
                Token::Plus => "+".to_string(),
                Token::Minus => "-".to_string(),
                Token::Multiply => "*".to_string(),
                Token::DivideFloor => "/".to_string(),
                Token::DivideRound => "/~".to_string(),
                Token::DivideCeil => "/+".to_string(),
                Token::LessThan => "<".to_string(),
                Token::LessThanEqual => "<=".to_string(),
                Token::GreaterThan => ">".to_string(),
                Token::GreaterThanEqual => ">=".to_string(),
                Token::Equal => "==".to_string(),
                Token::NotEqual => "!=".to_string(),
                Token::ParenOpen => "(".to_string(),
                Token::ParenClose => ")".to_string(),
                Token::Symbol(s) => s.to_string(),
                Token::Error(e) => format!("<error: {e}>"),
            }
        )
    }
}
