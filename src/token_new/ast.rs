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

macro_rules! parser_fn {
    (fn $name:ident()<'src> -> $out:ty $body:block) => {
        fn $name<'src, I>() -> impl Parser<'src, I, $out, extra::Err<Rich<'src, TokenKind<'src>>>>
        where I: ValueInput<'src, Token = TokenKind<'src>, Span = SimpleSpan> $body
    };
}

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

parser_fn! {
    fn label()<'src> -> Option<&'src str> {
        select! { TokenKind::Label(l) => l}.or_not()
    }
}

parser_fn! {
    fn integer_primitive()<'src> -> NonZeroU8 {
        select! { TokenKind::Integer(n) => n }
    }
}

parser_fn! {
    fn integer()<'src> -> Expression<'src> {
        label()
            .then(integer_primitive())
            .map(|(l, n)| ExpressionKind::Integer { label: l, value: n })
            .spanned()
    }
}

parser_fn! {
    fn modifier_call()<'src> -> ModifierCall<'src> {
        just(TokenKind::Colon)
            .ignore_then(label())
            .then(select! {
                TokenKind::Identifier(i) => i
            })
            .then(
                expression()
                    .delimited_by(just(TokenKind::ParenOpen), just(TokenKind::ParenClose))
                    .or_not()
            )
            .map(|((label, name), argument)| ModifierCall { label, name, argument })
    }
}

parser_fn! {
    fn dice()<'src> -> Expression<'src> {
        label()
            .then(integer_primitive().or_not())
            .then_ignore(just(TokenKind::Identifier("d")))
            .then(integer_primitive())
            .then(modifier_call().repeated().collect::<Vec<_>>())
            .map(|(((label, count), sides), modifiers)| { ExpressionKind::Dice {
                label,
                count: count.unwrap_or(NonZeroU8::new(1).unwrap()),
                sides,
                modifiers
            }})
            .spanned()
    }
}

parser_fn! {
    fn atom()<'src> -> Expression<'src> {
        choice((
            dice(),
            integer(),
            expression()
                .delimited_by(just(TokenKind::ParenOpen), just(TokenKind::ParenClose))
        ))
    }
}

parser_fn! {
    fn expression()<'src> -> Expression<'src> {
        todo()
    }
}

pub fn parse<'src>(source: Vec<Token<'src>>) -> Expression<'src> {
    if let (Some((_, start)), Some((_, end))) = (source.first(), source.last()) {
        let start = start.start;
        let end = end.end;
        let source = Stream::from_iter(source).spanned((end..end).into());

        //let expression: ParseResult<_, _> = integer().parse(source);
        //let expression: ParseResult<(ExpressionKind, chumsky::span::SimpleSpan), Rich<'_, _>> =
        //    integer_primitive().parse(source);

        todo!()
        //(Ok(todo!()), (start..end).into())
    } else {
        todo!()
        // (Err(ExpressionError::Empty), (0..0).into())
    }
}
