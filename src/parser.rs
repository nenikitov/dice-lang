use std::num::NonZeroU8;

use chumsky::{input::ValueInput, prelude::*};

use crate::token::Token;

#[derive(Debug)]
pub enum Modifier {
    Todo,
}

#[derive(Debug)]
pub struct Dice {
    count: NonZeroU8,
    sides: NonZeroU8,
    modifiers: Vec<Modifier>,
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
pub enum Ast {
    Constant(NonZeroU8),
    Dice(Dice),

    Operation {
        lhs: Box<Ast>,
        rhs: Box<Ast>,
        op: BinaryOperator,
    },
}

pub fn parser<'src, I>() -> impl Parser<'src, I, Ast, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let number = select! { Token::Number(n) => n };

    recursive(|ast| {
        let constant = number.map(Ast::Constant).labelled("constant");
        let dice = number
            .or_not()
            .labelled("die count")
            .then_ignore(just(Token::DieIndicator))
            .then(number.labelled("die sides"))
            .map(|(count, sides)| {
                Ast::Dice(Dice {
                    count: count.unwrap_or(unsafe { NonZeroU8::new_unchecked(1) }),
                    sides,
                    modifiers: vec![],
                })
            })
            .labelled("dice set");
        let atom = choice((
            dice,
            constant,
            ast.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
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
                    Ast::Operation {
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
            |lhs, (op, rhs)| Ast::Operation {
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

        summation
    })
}
