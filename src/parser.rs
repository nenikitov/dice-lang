use std::num::NonZeroU8;

use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use std::fmt;

use crate::token::Token;

#[derive(Debug)]
pub struct Die {
    sides: NonZeroU8,
}

#[derive(Debug)]
pub enum Modifier {
    Todo,
}

pub type Dice = (Vec<Die>, Vec<Modifier>);

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
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
    select! {
        Token::Number(n) => Ast::Constant(n)
    }
}
