use std::num::{NonZeroU8, ParseIntError};

use itertools::Itertools;
use logos::Logos;

use crate::util_new::Spanned;

#[derive(Default, Debug, PartialEq, Clone)]
pub enum TokenError {
    #[default]
    Unrecognized,

    InvalidInteger(ParseIntError),
}

#[derive(Logos)]
#[logos(skip r"\s+", error = TokenError)]
enum TokenKind<'src> {
    // Comments
    #[regex(r#""(?:[^"\\\n]|\\["\\t])*""#)]
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

    // Other
    #[regex(r"[a-z_]+")]
    Identifier(&'src str),
}

pub type Token<'src> = Spanned<Result<TokenKind<'src>, TokenError>>;

impl<'src> Token<'src> {
    pub fn parse(source: &'src str) -> impl Iterator<Item = Self> {
        TokenKind::lexer(source)
            .spanned()
            .chunk_by(|(token, _)| matches!(token, Err(TokenError::Unrecognized)))
            .into_iter()
            .flat_map(|(is_unrecognized, chunk)| {
                let chunk: Vec<_> = chunk.collect();
                if is_unrecognized {
                    chunk
                } else {
                    let (_, first) = chunk.first().unwrap();
                    let (_, last) = chunk.last().unwrap();
                    vec![(Err(TokenError::Unrecognized), first.start..last.end)]
                }
            })
            .map(|e| e.into())
    }
}
