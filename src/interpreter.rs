use crate::parser::Ast;

pub enum Value {
    Boolean(bool),
    Number { value: i32, deterministic: bool },
}

pub trait Evaluate {}

impl<'src> Evaluate for Ast<'src> {}
