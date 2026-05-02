use std::result;

pub type Result<T> = result::Result<T, T>;

pub trait Node
where
    Self: Sized,
{
    fn into_result(self) -> Result<Self>;
}

impl<T> Node for Vec<Result<T>> {
    fn into_result(self) -> Result<Self> {
        let is_ok = self.iter().all(result::Result::is_ok);
        if is_ok { Ok(self) } else { Err(self) }
    }
}

impl<T> Node for Option<Result<T>> {
    fn into_result(self) -> Result<Self> {
        let is_ok = self.as_ref().is_none_or(result::Result::is_ok);
        if is_ok { Ok(self) } else { Err(self) }
    }
}

#[derive(Debug)]
pub struct Program(pub Result<Block>);

impl Node for Program {
    fn into_result(self) -> Result<Self> {
        let is_ok = self.0.is_ok();
        if is_ok { Ok(self) } else { Err(self) }
    }
}

#[derive(Debug)]
pub struct Block {
    pub body: Result<Vec<Result<Expression>>>,
    pub last: Result<Option<Result<Expression>>>,
}

impl Node for Block {
    fn into_result(self) -> Result<Self> {
        let is_ok = self.body.is_ok() && self.last.is_ok();
        if is_ok { Ok(self) } else { Err(self) }
    }
}

pub type Lexeme = lrlex::DefaultLexeme<u32>;

#[derive(Debug)]
pub enum Expression {
    Declaration {
        left: Result<Lexeme>,
        right: Box<Result<Expression>>,
    },
    Assignment {
        left: Result<Lexeme>,
        right: Box<Result<Expression>>,
    },
    Binary {
        left: Box<Result<Expression>>,
        op: Result<Lexeme>,
        right: Box<Result<Expression>>,
    },
    Tagged {
        tag: Result<Lexeme>,
        expression: Box<Result<Expression>>,
    },
    Field {
        expression: Box<Result<Expression>>,
        field: Result<Lexeme>,
    },
    Call {
        expression: Box<Result<Expression>>,
        arguments: Result<Vec<Result<Expression>>>,
    },
    Literal(Result<Lexeme>),
    Block(Box<Result<Block>>),
}

impl Node for Expression {
    fn into_result(self) -> Result<Self> {
        let is_ok = match &self {
            Expression::Declaration { left, right } | Expression::Assignment { left, right } => {
                left.is_ok() && right.is_ok()
            }
            Expression::Binary { left, op, right } => left.is_ok() && op.is_ok() && right.is_ok(),
            Expression::Tagged { tag, expression } => tag.is_ok() && expression.is_ok(),
            Expression::Field { expression, field } => expression.is_ok() && field.is_ok(),
            Expression::Call {
                expression,
                arguments,
            } => expression.is_ok() && arguments.is_ok(),
            Expression::Literal(lexeme) => lexeme.is_ok(),
            Expression::Block(block) => block.is_ok(),
        };

        if is_ok { Ok(self) } else { Err(self) }
    }
}
