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

#[cfg(test)]
mod tests {
    use super::*;

    use std::num::IntErrorKind;

    use paste::paste;
    use rstest::rstest;

    fn parse_int_error(kind: IntErrorKind) -> ParseIntError {
        match kind {
            IntErrorKind::Empty => "",
            IntErrorKind::InvalidDigit => "A",
            IntErrorKind::PosOverflow => "1000",
            IntErrorKind::NegOverflow => "-1000",
            IntErrorKind::Zero => "0",
            _ => unreachable!("All kind types are covered"),
        }
        .parse::<NonZeroU8>()
        .expect_err("This method always produces an error")
    }

    macro_rules! test_token {
        ($name:ident, $($input:literal ), +) => {
            paste! {
                #[rstest]
                $(#[case($input)])+
                fn [<$name:snake _valid_parses>](#[case] input: &str) {
                    assert_eq!(
                        Token::lexer(input).next(),
                        Some(Ok(Token::[<$name:camel>]))
                    );
                }
            }
        };
    }

    #[rstest]
    #[case("1", 1)]
    #[case("10", 10)]
    #[case("100", 100)]
    fn number_valid_parses(#[case] input: &str, #[case] result: u8) {
        assert_eq!(
            Token::lexer(input).next(),
            Some(Ok(Token::Number(unsafe {
                NonZeroU8::new_unchecked(result)
            })))
        )
    }

    #[rstest]
    #[case("0", IntErrorKind::Zero)]
    #[case("1000", IntErrorKind::PosOverflow)]
    fn number_invalid_fails(#[case] input: &str, #[case] error: IntErrorKind) {
        assert_eq!(
            Token::lexer(input).next(),
            Some(Err(TokenError::InvalidNumber(parse_int_error(error))))
        )
    }

    test_token!(DieIndicator, "d");
    test_token!(Colon, ":");
    test_token!(Plus, "+");
    test_token!(Minus, "-");
    test_token!(Multiply, "*");
    test_token!(DivideFloor, "/", "/-");
    test_token!(DivideRound, "/~");
    test_token!(DivideCeil, "/+");
    test_token!(LessThan, "<");
    test_token!(LessThanEqual, "<=");
    test_token!(GreaterThan, ">");
    test_token!(GreaterThanEqual, ">=");
    test_token!(Equal, "==");
    test_token!(NotEqual, "!=");
    test_token!(ParenOpen, "(");
    test_token!(ParenClose, ")");

    #[rstest]
    #[case("adv")]
    #[case("dis")]
    #[case("hello_world")]
    fn symbol_valid_parses(#[case] input: &str) {
        assert_eq!(Token::lexer(input).next(), Some(Ok(Token::Symbol(input))))
    }
}
