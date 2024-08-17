use std::num::NonZeroU8;

use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use std::fmt;

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

// fn parser() -> impl Parser<Token, Ast, Error = Simple<Token>> {
//     let number = select! { Token::Number(n) => n };
//
//     number.map(Ast::Constant)
// }

/*
fn parser<'a, I>() -> impl Parser<'a, I, SExpr, extra::Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
*/

pub fn parser<'src, I>() -> impl Parser<'src, I, Ast, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let number = select! { Token::Number(n) => n };

    recursive(|ast| {
        let constant = number.map(Ast::Constant);
        let dice = number
            .or_not()
            .then_ignore(just(Token::DieIndicator))
            .then(number)
            .map(|(count, sides)| {
                Ast::Dice(Dice {
                    count: count.unwrap_or(unsafe { NonZeroU8::new_unchecked(1) }),
                    sides,
                    modifiers: vec![],
                })
            });
        let atom = choice((
            dice,
            constant,
            ast.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
        ));

        let product = choice((
            just(Token::Multiply),
            just(Token::DivideCeil),
            just(Token::DivideFloor),
            just(Token::DivideRound),
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
                            Token::DivideCeil => BinaryOperator::DivideCeil,
                            Token::DivideRound => BinaryOperator::DivideRound,
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

        // let constant = number.map(Ast::Constant);
        // let dice = number
        //     .or_not()
        //     .then_ignore(just(Token::DieIndicator))
        //     .then(number)
        //     .map(|(count, sides)| {
        //         Ast::Dice(Dice {
        //             count: count.unwrap_or(unsafe { NonZeroU8::new_unchecked(1) }),
        //             sides,
        //             modifiers: vec![],
        //         })
        //     });
        // let atom = choice((dice, constant, ast.clone()));

        // let multiplication_division = choice((
        //     just(Token::Multiply),
        //     just(Token::DivideFloor),
        //     just(Token::DivideCeil),
        //     just(Token::DivideRound),
        // ));
        // let multiplication_division = choice((
        //     atom.clone()
        //         .then(multiplication_division)
        //         .then(atom.clone())
        //         .map(|((lhs, op), rhs)| Ast::Operation {
        //             lhs: Box::new(lhs),
        //             rhs: Box::new(rhs),
        //             op: match op {
        //                 Token::Multiply => BinaryOperator::Multiply,
        //                 Token::DivideFloor => BinaryOperator::DivideFloor,
        //                 Token::DivideCeil => BinaryOperator::DivideCeil,
        //                 Token::DivideRound => BinaryOperator::DivideRound,
        //                 _ => {
        //                     unreachable!("Only multiplication and division operators are captured")
        //                 }
        //             },
        //         }),
        //     atom.clone(),
        // ));

        // multiplication_division
    })
}
