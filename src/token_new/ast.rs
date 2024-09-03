use std::num::NonZeroU8;

use chumsky::{
    error::Rich,
    input::{Stream, ValueInput},
    prelude::*,
};

use crate::{token_new::tok::*, util_new::Spanned};

pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    DivideFloor,
    DivideRound,
    DivideCeil,
}

pub enum LogicalUnaryOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

pub struct ModifierCall<'src> {
    label: Option<&'src str>,
    name: &'src str,
    argument: Option<Expression<'src>>,
}

pub enum ExpressionKind<'src> {
    // Error
    Err,

    // Literals
    Integer {
        label: Option<&'src str>,
        value: NonZeroU8,
    },
    Dice {
        label: Option<&'src str>,
        count: NonZeroU8,
        sides: NonZeroU8,
        modifiers: Vec<ModifierCall<'src>>,
    },

    // Operations
    Binary {
        op: BinaryOperator,
        lhs: Box<Expression<'src>>,
        rhs: Box<Expression<'src>>,
    },
    LogicalUnary {
        op: LogicalUnaryOperator,
        rhs: Box<Expression<'src>>,
    },
}

pub type Expression<'src> = Spanned<ExpressionKind<'src>>;

trait TokenInput<'src> = ValueInput<'src, Token = TokenKind<'src>, Span = SimpleSpan>;
trait TokenParser<'src, I: TokenInput<'src>, O = Expression<'src>> =
    Parser<'src, I, O, extra::Err<Rich<'src, TokenKind<'src>>>>;

trait SpannedParser<'src, I, O, E>
where
    I: Input<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
    fn spanned(self) -> impl Parser<'src, I, Spanned<O>, E>;
}

impl<'src, T, I, O, E> SpannedParser<'src, I, O, E> for T
where
    T: Parser<'src, I, O, E>,
    I: Input<'src, Span = SimpleSpan>,
    E: extra::ParserExtra<'src, I>,
{
    fn spanned(self) -> impl Parser<'src, I, Spanned<O>, E> {
        self.map_with(|it, e| (it, e.span()))
    }
}

fn label<'src, I: TokenInput<'src>>() -> impl TokenParser<'src, I, Option<&'src str>> {
    select! { TokenKind::Label(l) => l}.or_not()
}

fn integer<'src, I: TokenInput<'src>>() -> impl TokenParser<'src, I> {
    let integer = select! { TokenKind::Integer(n) => n };
    label()
        .then(integer)
        .map(|(l, n)| ExpressionKind::Integer { label: l, value: n })
        .spanned()
}

fn dice<'src, I: TokenInput<'src>>() -> impl TokenParser<'src, I> {
    todo()
}

pub fn parse<'src>(source: Vec<Token<'src>>) -> Expression<'src> {
    if let (Some((_, start)), Some((_, end))) = (source.first(), source.last()) {
        let start = start.start;
        let end = end.end;
        let source = Stream::from_iter(source).spanned((end..end).into());

        //let expression: ParseResult<_, _> = integer().parse(source);
        let expression: ParseResult<(ExpressionKind, chumsky::span::SimpleSpan), Rich<'_, _>> =
            integer().parse(source);

        (Ok(todo!()), (start..end).into())
    } else {
        (Err(ExpressionError::Empty), (0..0).into())
    }
}
