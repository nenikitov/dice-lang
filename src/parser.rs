use std::num::NonZeroU8;

use chumsky::{input::ValueInput, prelude::*};

use crate::token::Token;

#[derive(Debug)]
pub struct ModifierCall<'src> {
    name: &'src str,
    arg: Option<Ast<'src>>,
}

#[derive(Debug)]
pub struct Dice<'src> {
    count: NonZeroU8,
    sides: NonZeroU8,
    modifiers: Vec<ModifierCall<'src>>,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    DivideFloor,
    DivideCeil,
    DivideRound,
}

#[derive(Debug)]
pub enum UnaryOperator {
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub enum Ast<'src> {
    Constant(NonZeroU8),
    Dice(Dice<'src>),

    UnaryOperation {
        rhs: Box<Ast<'src>>,
        op: UnaryOperator,
    },

    BinaryOperation {
        lhs: Box<Ast<'src>>,
        rhs: Box<Ast<'src>>,
        op: BinaryOperator,
    },
}

pub fn parser<'src, I>() -> impl Parser<'src, I, Ast<'src>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let number = select! { Token::Number(n) => n };

    recursive(|ast| {
        let constant = number.map(Ast::Constant).labelled("constant");
        let modifier = just(Token::Colon)
            .ignore_then(select! { Token::Symbol(s) => s })
            .then(
                ast.clone()
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
                    .or_not(),
            )
            .map(|(name, arg)| ModifierCall { name, arg });
        let dice = number
            .or_not()
            .labelled("die count")
            .then_ignore(just(Token::DieIndicator))
            .then(number.labelled("die sides"))
            .then(modifier.repeated().collect::<Vec<_>>())
            .map(|((count, sides), modifiers)| {
                Ast::Dice(Dice {
                    count: count.unwrap_or(unsafe { NonZeroU8::new_unchecked(1) }),
                    sides,
                    modifiers,
                })
            })
            .labelled("dice set");
        let atom = choice((
            dice,
            constant,
            ast.clone()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
        ));

        let product = choice((
            just(Token::Multiply),
            just(Token::DivideFloor),
            just(Token::DivideRound),
            just(Token::DivideCeil),
        ));
        let product =
            atom.clone()
                .foldl(product.then(atom.clone()).repeated(), |lhs, (op, rhs)| {
                    Ast::BinaryOperation {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: match op {
                            Token::Multiply => BinaryOperator::Multiply,
                            Token::DivideFloor => BinaryOperator::DivideFloor,
                            Token::DivideRound => BinaryOperator::DivideRound,
                            Token::DivideCeil => BinaryOperator::DivideCeil,
                            _ => {
                                unreachable!(
                                    "Only multiplication and division operators are captured"
                                )
                            }
                        },
                    }
                });

        let summation = choice((just(Token::Plus), just(Token::Minus)));
        let summation = product.clone().foldl(
            summation.then(product.clone()).repeated(),
            |lhs, (op, rhs)| Ast::BinaryOperation {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: match op {
                    Token::Plus => BinaryOperator::Add,
                    Token::Minus => BinaryOperator::Subtract,
                    _ => {
                        unreachable!("Only addition and subtraction operators are captured")
                    }
                },
            },
        );

        let logical_unary = choice((
            just(Token::LessThan),
            just(Token::LessThanEqual),
            just(Token::GreaterThan),
            just(Token::GreaterThanEqual),
            just(Token::Equal),
            just(Token::NotEqual),
        ));
        let logical_unary = choice((
            logical_unary
                .then(summation.clone())
                .map(|(op, rhs)| Ast::UnaryOperation {
                    rhs: Box::new(rhs),
                    op: match op {
                        Token::LessThan => UnaryOperator::LessThan,
                        Token::LessThanEqual => UnaryOperator::LessThanEqual,
                        Token::GreaterThan => UnaryOperator::GreaterThan,
                        Token::GreaterThanEqual => UnaryOperator::GreaterThanEqual,
                        Token::Equal => UnaryOperator::Equal,
                        Token::NotEqual => UnaryOperator::NotEqual,
                        _ => {
                            unreachable!("Only logical unary operators are captured")
                        }
                    },
                }),
            summation.clone(),
        ));

        logical_unary
    })
}
