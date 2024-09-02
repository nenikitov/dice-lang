use std::num::NonZeroU8;

use chumsky::{input::ValueInput, prelude::*};

use crate::{token_new::tok::*, util_new::Spanned};

pub enum ExpressionErrorKind<'src> {
    Phantom(&'src str),
}

pub struct ExpressionError<'src> {
    errors: Vec<ExpressionErrorKind<'src>>,
}

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

pub struct DieModifier<'src> {
    label: Option<&'src str>,
    name: &'src str,
    args: Vec<ExpressionSpanned<'src>>,
}

pub enum LitKind<'src> {
    Number(NonZeroU8),
    Die {
        count: NonZeroU8,
        sides: NonZeroU8,
        modifiers: Vec<DieModifier<'src>>,
    },
}

pub enum ExprKind<'src> {
    Literal {
        label: Option<&'src str>,
        value: LitKind<'src>,
    },
    Binary {
        op: BinaryOperator,
        lhs: Box<ExpressionSpanned<'src>>,
        rhs: Box<ExpressionSpanned<'src>>,
    },
    LogicalUnary {
        op: LogicalUnaryOperator,
        rhs: Box<ExpressionSpanned<'src>>,
    },
}

pub type Expression<'src> = Result<ExprKind<'src>, ExpressionError<'src>>;
pub type ExpressionSpanned<'src> = Spanned<Expression<'src>>;

pub trait TokenInput<'src> = ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>;
pub trait TokenParser<'src, I: TokenInput<'src>, O> =
    Parser<'src, I, O, extra::Err<Rich<'src, Token<'src>>>>;

pub fn label<'src, I: TokenInput<'src>>() -> impl TokenParser<'src, I, Option<&'src str>> {
    select! { Ok(TokenKind::Label(l)) => l }.or_not()
}
