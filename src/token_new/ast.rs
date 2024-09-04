use std::num::NonZeroU8;

use chumsky::{
    error::Rich,
    input::{Stream, ValueInput},
    prelude::*,
};

use crate::{token_new::tok::*, util_new::Spanned};

#[derive(Clone, Debug)]
pub enum UnaryArithmeticOperator {
    Negation,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    DivideFloor,
    DivideRound,
    DivideCeil,
}

#[derive(Clone, Debug)]
pub enum UnaryLogicalOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(Debug)]
pub struct ModifierCall<'src> {
    label: Option<&'src str>,
    name: &'src str,
    argument: Option<Expression<'src>>,
}

#[derive(Debug)]
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
    UnaryArithmetic {
        op: UnaryArithmeticOperator,
        rhs: Box<Expression<'src>>,
    },
    Binary {
        op: BinaryOperator,
        lhs: Box<Expression<'src>>,
        rhs: Box<Expression<'src>>,
    },
    UnaryLogical {
        op: UnaryLogicalOperator,
        rhs: Box<Expression<'src>>,
    },
}

pub type Expression<'src> = Spanned<ExpressionKind<'src>>;

macro_rules! parser_fn {
    (fn $name:ident($expression:ident)<'src> -> $out:ty $body:block) => {
        fn $name<'src, I>(
            $expression: impl Parser<'src, I, Expression<'src>, extra::Err<Rich<'src, TokenKind<'src>>>> + Clone
        ) -> impl Parser<'src, I, $out, extra::Err<Rich<'src, TokenKind<'src>>>> + Clone
            where
                I: ValueInput<'src, Token = TokenKind<'src>, Span = SimpleSpan>
        {
            let $expression = || $expression.clone();
            $body
        }
    };
    (fn $name:ident()<'src> -> $out:ty $body:block) => {
        fn $name<'src, I>(
        ) -> impl Parser<'src, I, $out, extra::Err<Rich<'src, TokenKind<'src>>>> + Clone
            where
                I: ValueInput<'src, Token = TokenKind<'src>, Span = SimpleSpan>
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
    fn modifier_call(expression)<'src> -> ModifierCall<'src> {
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
    fn dice(expression)<'src> -> Expression<'src> {
        label()
            .then(integer_primitive().or_not())
            .then_ignore(just(TokenKind::Identifier("d")))
            .then(integer_primitive())
            .then(modifier_call(expression()).repeated().collect::<Vec<_>>())
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
    fn atom(expression)<'src> -> Expression<'src> {
        choice((
            dice(expression()),
            integer(),
            expression()
                .delimited_by(just(TokenKind::ParenOpen), just(TokenKind::ParenClose))
        ))
    }
}

parser_fn! {
    fn unary_arithmetic(expression)<'src> -> Expression<'src> {
        let op = choice((
            just(TokenKind::Minus).to(UnaryArithmeticOperator::Negation),
        ));
        let operand = atom(expression());

        op.repeated().foldr_with(operand, |op, rhs, e| {
            (
                ExpressionKind::UnaryArithmetic {
                    op,
                    rhs: Box::new(rhs)
                },
                e.span()
            )
        })
    }
}

parser_fn! {
    fn binary_product(expression)<'src> -> Expression<'src> {
        let op = choice((
            just(TokenKind::Star).to(BinaryOperator::Multiply),
            just(TokenKind::Slash).to(BinaryOperator::DivideFloor),
            just(TokenKind::SlashMinus).to(BinaryOperator::DivideFloor),
            just(TokenKind::SlashTilde).to(BinaryOperator::DivideRound),
            just(TokenKind::SlashPlus).to(BinaryOperator::DivideCeil),
        ));
        let operand = || unary_arithmetic(expression());

        operand().foldl_with(op.then(operand()).repeated(), |lhs, (op, rhs), e| {
            (
                ExpressionKind::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                e.span(),
            )
        })
    }
}

parser_fn! {
    fn binary_sum(expression)<'src> -> Expression<'src> {
        let op = choice((
            just(TokenKind::Plus).to(BinaryOperator::Add),
            just(TokenKind::Minus).to(BinaryOperator::Subtract),
        ));
        let operand = || binary_product(expression());

        operand().foldl_with(op.then(operand()).repeated(), |lhs, (op, rhs), e| {
            (
                ExpressionKind::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                e.span(),
            )
        })
    }
}

parser_fn! {
    fn logical_unary(expression)<'src> -> Expression<'src> {
        let op = choice((
            just(TokenKind::Equal).to(UnaryLogicalOperator::Equal),
            just(TokenKind::NotEqual).to(UnaryLogicalOperator::NotEqual),
            just(TokenKind::LessThan).to(UnaryLogicalOperator::LessThan),
            just(TokenKind::LessThanEqual).to(UnaryLogicalOperator::LessThanEqual),
            just(TokenKind::GreaterThan).to(UnaryLogicalOperator::GreaterThan),
            just(TokenKind::GreaterThanEqual).to(UnaryLogicalOperator::GreaterThanEqual),
        ));
        let operand = || binary_sum(expression());

        choice((
            op
                .then(operand())
                .map(|(op, rhs)| ExpressionKind::UnaryLogical {
                    op,
                    rhs: Box::new(rhs)
                })
                .spanned(),
            operand()
        ))
    }
}

parser_fn! {
    fn expression()<'src> -> Expression<'src> {
        recursive(logical_unary)
    }
}

pub fn parse<'src>(source: Vec<Token<'src>>) -> Expression<'src> {
    if let (Some((_, start)), Some((_, end))) = (source.first(), source.last()) {
        let start = start.start;
        let end = end.end;
        let source = Stream::from_iter(source).spanned((end..end).into());

        dbg!(expression().parse(source));

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsing() {
        let source = r#"
            "base roll"2d20:"being helped"adv:reroll(<= 3) + "ability score"5 + -3
        "#;
        let source = tokenize(source).collect();
        parse(source);
    }
}
