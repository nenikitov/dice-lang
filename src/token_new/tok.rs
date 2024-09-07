use std::{
    fmt,
    num::{NonZeroU8, ParseIntError},
};

use itertools::Itertools;
use logos::Logos;

use crate::util_new::Spanned;

#[derive(Default, Debug, PartialEq, Clone)]
pub enum TokenError {
    #[default]
    Unrecognized,

    InvalidInteger(ParseIntError),
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenError::Unrecognized => "unrecognized".to_string(),
                TokenError::InvalidInteger(e) => format!("{e}"),
            }
        )
    }
}

#[derive(Logos, PartialEq, Debug, Clone)]
#[logos(skip r"\s+", error = TokenError)]
pub enum TokenKind<'src> {
    // Error
    Err(TokenError),

    // Comments
    #[regex(r#""([^"\\\n]|\\["\\t])*""#, |l| &l.slice()[1..l.slice().len() - 1])]
    Label(&'src str),

    // Literals
    #[regex(r"[\d]+", |l| l.slice().parse().map_err(TokenError::InvalidInteger))]
    Integer(NonZeroU8),

    // Operators
    #[token(":")]
    Colon,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("/-")]
    SlashMinus,
    #[token("/~")]
    SlashTilde,
    #[token("/+")]
    SlashPlus,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanEqual,

    // Separators
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,

    // Other
    #[regex(r"[a-z_]+")]
    Identifier(&'src str),
}

impl<'src> fmt::Display for TokenKind<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // Error
                TokenKind::Err(err) => format!("Invalid token: {err}"),

                // Comments
                TokenKind::Label(label) => format!("\"{}\"", label),

                // Literals
                TokenKind::Integer(integer) => integer.to_string(),

                // Operators
                TokenKind::Colon => ":".to_string(),
                TokenKind::Plus => "+".to_string(),
                TokenKind::Minus => "-".to_string(),
                TokenKind::Star => "*".to_string(),
                TokenKind::Slash => "/".to_string(),
                TokenKind::SlashMinus => "/-".to_string(),
                TokenKind::SlashTilde => "/~".to_string(),
                TokenKind::SlashPlus => "/+".to_string(),
                TokenKind::Equal => "==".to_string(),
                TokenKind::NotEqual => "!=".to_string(),
                TokenKind::LessThan => "<".to_string(),
                TokenKind::LessThanEqual => "<=".to_string(),
                TokenKind::GreaterThan => ">".to_string(),
                TokenKind::GreaterThanEqual => ">=".to_string(),

                // Separators
                TokenKind::ParenOpen => "(".to_string(),
                TokenKind::ParenClose => ")".to_string(),

                // Other
                TokenKind::Identifier(identifier) => identifier.to_string(),
            }
        )
    }
}
pub type Token<'src> = Spanned<TokenKind<'src>>;

pub fn tokenize<'src>(source: &'src str) -> impl Iterator<Item = Token<'src>> {
    TokenKind::lexer(source)
        .spanned()
        .map(|(token, span)| match token {
            Ok(token) => (token, span),
            Err(error) => (TokenKind::Err(error), span),
        })
        .peekable()
        .batching(|it| {
            let (token, span) = it.next()?;

            if token != TokenKind::Err(TokenError::Unrecognized) {
                Some((token, span))
            } else {
                let mut end = span.end;

                while let Some((TokenKind::Err(TokenError::Unrecognized), span_next)) = it.peek()
                    && span_next.start == end
                {
                    end = span_next.end;
                    it.next();
                }

                Some((TokenKind::Err(TokenError::Unrecognized), span.start..end))
            }
        })
        .map(|(token, span)| (token, span.into()))
}
