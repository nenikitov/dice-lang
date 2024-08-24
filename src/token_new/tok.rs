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

#[derive(Logos, PartialEq, Debug)]
#[logos(skip r"\s+", error = TokenError)]
pub enum TokenKind<'src> {
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
            .peekable()
            .batching(|it| match it.next() {
                None => None,
                Some((token, span)) => {
                    if token != Err(TokenError::Unrecognized) {
                        Some((token, span))
                    } else {
                        let mut end = span.end;

                        while let Some((token_n, span_n)) = it.peek() {
                            if let Err(TokenError::Unrecognized) = token_n {
                                if span_n.start == end {
                                    end = span_n.end;
                                    it.next();
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }

                        Some((Err(TokenError::Unrecognized), span.start..end))
                    }
                }
            })
            .map(|e| e.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_token() {
        for t in Token::parse("10d40 + HELLO WORLD + WORLD + 20") {
            eprintln!("{t:?}");
        }
        panic!("Hey");
    }
}
