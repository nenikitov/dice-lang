use std::{
    num::{NonZeroU8, ParseIntError},
    str::FromStr,
};

use crate::util_new::Spanned;

use chumsky::{
    error::{Error, Rich},
    prelude::*,
};

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
                    span: Some((len - 1..len).into()),
                }]
            }
            LabelParsingError::Escape(span) => {
                vec![ErrorMessage {
                    message: r#"Valid escapes are `\"`, `\\`, `\t`, and `\n`"#.to_string(),
                    span: Some(span),
                }]
            }
        }
    }
}

pub type Integer = Spanned<Result<NonZeroU8, IntegerParsingError>>;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'src> {
    Integer(Integer),

    Phantom(&'src str),
}

pub type Token<'src> = Spanned<TokenKind<'src>>;

macro_rules! parser_fn {
    (fn $name:ident()<'src> -> $out:ty $body:block) => {
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

parser_fn! {
    fn integer()<'src> -> Integer {
        text::digits(10)
            .labelled("digits")
            .to_slice()
            .validate(|token, _, _| {
                NonZeroU8::from_str(token)
                    .map_err(IntegerParsingError::Parse)
            })
            .spanned()
    }
}

parser_fn! {
    fn label()<'src> -> () {
        todo()
    }
}

//parser_fn! {
//    fn label()<'src> -> Token<'src> {
//        just('"')
//            .ignore_then(
//                choice((
//                    none_of(r#""\"#).to_slice(),
//                    just('\\').then(any()).to_slice(),
//                ))
//                .repeated()
//                .to_slice(),
//            )
//            .then(choice((
//                just('"').map(|_| None),
//                end().map_with(|_, extra| Some(extra.span())),
//            )))
//            .validate(|(label, reached_eof), _, emitter| {
//                if let Some(reached_eof) = reached_eof {
//                    emitter.emit(<Rich<_> as Error<'src, &'src str>>::expected_found(
//                        [Some('"'.into())],
//                        None,
//                        reached_eof,
//                    ))
//                }
//                label
//            })
//            .map(TokenKind::Label)
//            .spanned()
//    }
//}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing() {
        let source = r#"0000000000"#;
        let source = integer().lazy().parse(source);
        dbg!(source);
        panic!()
    }
}
