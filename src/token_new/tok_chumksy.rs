use std::{
    cell::LazyCell,
    collections::HashMap,
    num::{NonZeroU8, ParseIntError},
    str::FromStr,
};

use crate::util_new::Spanned;

use chumsky::{error::Rich, prelude::*};
use itertools::Itertools;

use regex::Regex;

pub struct ErrorMessage {
    message: String,
    span: Option<SimpleSpan>,
}

pub trait ParsingError {
    fn header(self) -> String;
    fn error(self) -> ErrorMessage;
    fn hints(self) -> Vec<ErrorMessage>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntegerParsingError {
    Parse(ParseIntError),
}

impl ParsingError for IntegerParsingError {
    fn header(self) -> String {
        "Invalid integer".to_string()
    }

    fn error(self) -> ErrorMessage {
        match self {
            IntegerParsingError::Parse(e) => ErrorMessage {
                message: e.to_string(),
                span: None,
            },
        }
    }

    fn hints(self) -> Vec<ErrorMessage> {
        match self {
            IntegerParsingError::Parse(_) => vec![ErrorMessage {
                message: "Valid numbers are integers `>= 1` and `<= 255`".to_string(),
                span: None,
            }],
        }
    }
}

pub type Integer = Result<NonZeroU8, IntegerParsingError>;

const VALID_ESCAPES: LazyCell<HashMap<char, char>> =
    LazyCell::new(|| HashMap::from([('"', '"'), ('\\', '\\'), ('t', '\t'), ('n', '\n')]));

#[derive(Debug, Clone, PartialEq)]
pub enum LabelParsingError {
    Unterminated(usize),
    Escape(SimpleSpan),
}

impl ParsingError for LabelParsingError {
    fn header(self) -> String {
        match self {
            LabelParsingError::Unterminated(_) => "Unterminated label".to_string(),
            LabelParsingError::Escape(_) => "Invalid escape sequence".to_string(),
        }
    }

    fn error(self) -> ErrorMessage {
        match self {
            LabelParsingError::Unterminated(_) => ErrorMessage {
                message: "Quote start".to_string(),
                span: Some((0..1).into()),
            },
            LabelParsingError::Escape(span) => ErrorMessage {
                message: "Escape sequence".to_string(),
                span: Some(span),
            },
        }
    }

    fn hints(self) -> Vec<ErrorMessage> {
        match self {
            LabelParsingError::Unterminated(len) => {
                vec![ErrorMessage {
                    message: "Should have been closed by here".to_string(),
                    span: Some(((len - 1)..len).into()),
                }]
            }
            LabelParsingError::Escape(span) => {
                vec![ErrorMessage {
                    message: format!(
                        "Valid escapes are {}",
                        VALID_ESCAPES.keys().map(|c| format!("`{c}`")).join(",")
                    ),
                    span: Some(span),
                }]
            }
        }
    }
}

pub type Label = Result<String, Vec<LabelParsingError>>;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'src> {
    // Literals
    Integer(Integer),
    Label(Label),
    Identifier(&'src str),
    // Operators
    Colon,
    Plus,
    Minus,
    Star,
    SlashPlus,
    SlashTilde,
    SlashMinus,
    Slash,
    Equal,
    NotEqual,
    LessThanEqual,
    LessThan,
    GreaterThanEqual,
    GreaterThan,
    // Separators
    ParenOpen,
    ParenClose,
}

pub type Token<'src> = Spanned<Result<TokenKind<'src>, ()>>;

macro_rules! parser_fn {
    (fn $name:ident<'src>() -> $out:ty $body:block) => {
        fn $name<'src>(
        ) -> impl Parser<'src, &'src str, $out, extra::Err<Rich<'src, char>>> + Clone
        $body
    };
}

trait SpannedParser<'src, I, O, E>
where
    I: Input<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
    fn spanned(self) -> impl Parser<'src, I, Spanned<O>, E> + Clone;
}

impl<'src, T, I, O, E> SpannedParser<'src, I, O, E> for T
where
    T: Parser<'src, I, O, E> + Clone,
    I: Input<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
    fn spanned(self) -> impl Parser<'src, I, Spanned<O>, E> + Clone {
        self.map_with(|it, e| (it, e.span()))
    }
}

parser_fn!(
    fn integer<'src>() -> Integer {
        text::digits(10)
            .labelled("digits")
            .to_slice()
            .validate(|token, _, _| NonZeroU8::from_str(token).map_err(IntegerParsingError::Parse))
    }
);

parser_fn!(
    fn any_or_end<'src>() -> &'src str {
        any().to_slice().or(end().to_slice())
    }
);

parser_fn!(
    fn label<'src>() -> Label {
        just('"')
            .ignore_then(regex(r#"([^\\"]|\\.?)"#).repeated().to_slice())
            .then(choice((just('"').to(true), end().to(false))))
            .validate(|(label, terminator): (&str, bool), _, _| {
                let mut errors = label
                    .chars()
                    .enumerate()
                    .batching(|it| {
                        while let Some((i, current)) = it.next() {
                            if current == '\\' {
                                return match it.next() {
                                    Some((_, c)) if VALID_ESCAPES.contains_key(&c) => continue,
                                    Some(_) => {
                                        Some(LabelParsingError::Escape(((i + 1)..(i + 3)).into()))
                                    }
                                    None => {
                                        Some(LabelParsingError::Escape(((i + 1)..(i + 2)).into()))
                                    }
                                };
                            }
                        }

                        None
                    })
                    .collect_vec();

                if !terminator {
                    errors.push(LabelParsingError::Unterminated(label.len() + 1))
                }

                if !errors.is_empty() {
                    Err(errors)
                } else {
                    Ok(
                        Regex::new(&VALID_ESCAPES.keys().map(|c| format!(r"\\{c}")).join("|"))
                            .unwrap()
                            .replace_all(label, |caps: &regex::Captures| {
                                VALID_ESCAPES[&caps[0].chars().nth(1).unwrap()].to_string()
                            })
                            .into(),
                    )
                }
            })
    }
);

parser_fn!(
    fn identifier<'src>() -> &'src str {
        text::ascii::ident().labelled("identifier")
    }
);

parser_fn!(
    fn token<'src>() -> () {
        let token = choice((
            // Literals
            integer().map(TokenKind::Integer),
            label().map(TokenKind::Label),
            identifier().map(TokenKind::Identifier),
            // Operators
            just(':').to(TokenKind::Colon),
            just('+').to(TokenKind::Plus),
            just('-').to(TokenKind::Minus),
            just('*').to(TokenKind::Star),
            just("/+").to(TokenKind::SlashPlus),
            just("/~").to(TokenKind::SlashTilde),
            just("/-").to(TokenKind::SlashMinus),
            just('/').to(TokenKind::Slash),
            just("==").to(TokenKind::Equal),
            just("!=").to(TokenKind::NotEqual),
            just("<=").to(TokenKind::LessThanEqual),
            just('<').to(TokenKind::LessThan),
            just(">=").to(TokenKind::GreaterThanEqual),
            just('>').to(TokenKind::GreaterThan),
            // Separators
            just('(').to(TokenKind::ParenOpen),
            just(')').to(TokenKind::ParenClose),
        ));

        custom(move |input| {
            let error = input.parse(token.clone())().unwrap_err();

            loop {
                if matches!(input.peek(), Some(char) if char.is_whitespace()) {
                    input.next();
                    
                }
                if matches!(input.check(token.clone()), Err(_)) {
                    input.parse(token.clone());
                    continue;
                }
            }

            let a = input.check(token.clone());
            dbg!(a);
            Ok(())
        })
        .ignored()

        // token.recover_with(via_parser(any().map(|_| TokenKind::Plus)))

        // token
        //     .padded()
        //     // .recover_with(via_parser(none_of(token).repeated()).map(|_| Err(())))
        //     .recover_with(skip_then_retry_until(any().ignored(), end()))
    }
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing() {
        let source = r#"
             $
        "#
        .trim();
        let source = token().parse(source);
        dbg!(source);
        panic!()
    }
}
