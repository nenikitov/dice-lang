use std::num::NonZeroU8;

use chumsky::{input::ValueInput, prelude::*};

use crate::token::Token;

#[derive(Debug, PartialEq)]
pub struct ModifierCall<'src> {
    name: &'src str,
    arg: Option<Ast<'src>>,
}

#[derive(Debug, PartialEq)]
pub struct Dice<'src> {
    count: NonZeroU8,
    sides: NonZeroU8,
    modifiers: Vec<ModifierCall<'src>>,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    DivideFloor,
    DivideCeil,
    DivideRound,
}

#[derive(Debug, PartialEq)]
pub enum LogicalUnaryOperator {
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, PartialEq)]
pub enum Ast<'src> {
    Constant(NonZeroU8),
    Dice(Dice<'src>),

    LogicalUnaryOperation {
        rhs: Box<Ast<'src>>,
        op: LogicalUnaryOperator,
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
                .map(|(op, rhs)| Ast::LogicalUnaryOperation {
                    rhs: Box::new(rhs),
                    op: match op {
                        Token::LessThan => LogicalUnaryOperator::LessThan,
                        Token::LessThanEqual => LogicalUnaryOperator::LessThanEqual,
                        Token::GreaterThan => LogicalUnaryOperator::GreaterThan,
                        Token::GreaterThanEqual => LogicalUnaryOperator::GreaterThanEqual,
                        Token::Equal => LogicalUnaryOperator::Equal,
                        Token::NotEqual => LogicalUnaryOperator::NotEqual,
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

#[cfg(test)]
mod tests {
    use super::*;

    use chumsky::input::Stream;
    use rstest::rstest;

    #[rstest]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(1) }),
        ],
        Ast::Constant(unsafe { NonZeroU8::new_unchecked(1) }),
    )]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(5) }),
        ],
        Ast::Constant(unsafe { NonZeroU8::new_unchecked(5) }),
    )]
    fn constant_valid_parses(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
            Token::DieIndicator,
            Token::Number(unsafe { NonZeroU8::new_unchecked(20) }),
        ],
        Ast::Dice(Dice {
            count: unsafe { NonZeroU8::new_unchecked(2) },
            sides: unsafe { NonZeroU8::new_unchecked(20) },
            modifiers: vec![],
        }),
    )]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(4) }),
            Token::DieIndicator,
            Token::Number(unsafe { NonZeroU8::new_unchecked(6) }),
        ],
        Ast::Dice(Dice {
            count: unsafe { NonZeroU8::new_unchecked(4) },
            sides: unsafe { NonZeroU8::new_unchecked(6) },
            modifiers: vec![],
        }),
    )]
    fn dice_valid_simple_parses(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::DieIndicator,
            Token::Number(unsafe { NonZeroU8::new_unchecked(20) }),
        ],
        Ast::Dice(Dice {
            count: unsafe { NonZeroU8::new_unchecked(1) },
            sides: unsafe { NonZeroU8::new_unchecked(20) },
            modifiers: vec![],
        }),
    )]
    #[case(
        vec![
            Token::DieIndicator,
            Token::Number(unsafe { NonZeroU8::new_unchecked(6) }),
        ],
        Ast::Dice(Dice {
            count: unsafe { NonZeroU8::new_unchecked(1) },
            sides: unsafe { NonZeroU8::new_unchecked(6) },
            modifiers: vec![],
        }),
    )]
    fn dice_valid_short_parses(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
            Token::DieIndicator,
            Token::Number(unsafe { NonZeroU8::new_unchecked(20) }),
            Token::Colon,
            Token::Symbol("adv"),
            Token::Colon,
            Token::Symbol("min"),
        ],
        Ast::Dice(Dice {
            count: unsafe { NonZeroU8::new_unchecked(2) },
            sides: unsafe { NonZeroU8::new_unchecked(20) },
            modifiers: vec![
                ModifierCall { name: "adv", arg: None },
                ModifierCall { name: "min", arg: None },
            ],
        }),
    )]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(4) }),
            Token::DieIndicator,
            Token::Number(unsafe { NonZeroU8::new_unchecked(6) }),
            Token::Colon,
            Token::Symbol("adv"),
        ],
        Ast::Dice(Dice {
            count: unsafe { NonZeroU8::new_unchecked(4) },
            sides: unsafe { NonZeroU8::new_unchecked(6) },
            modifiers: vec![
                ModifierCall { name: "adv", arg: None },
            ],
        }),
    )]
    fn dice_valid_modifiers_parses(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(4) }),
            Token::DieIndicator,
            Token::Number(unsafe { NonZeroU8::new_unchecked(6) }),
            Token::Colon,
            Token::Symbol("adv"),
            Token::ParenOpen,
            Token::Number(unsafe { NonZeroU8::new_unchecked(3) }),
            Token::ParenClose,
            Token::Colon,
            Token::Symbol("min"),
            Token::ParenOpen,
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
            Token::ParenClose,
        ],
        Ast::Dice(Dice {
            count: unsafe { NonZeroU8::new_unchecked(4) },
            sides: unsafe { NonZeroU8::new_unchecked(6) },
            modifiers: vec![
                ModifierCall {
                    name: "adv",
                    arg: Some(Ast::Constant(unsafe {NonZeroU8::new_unchecked(3)})),
                },
                ModifierCall {
                    name: "min",
                    arg: Some(Ast::Constant(unsafe {NonZeroU8::new_unchecked(2)})),
                },
            ],
        }),
    )]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
            Token::DieIndicator,
            Token::Number(unsafe { NonZeroU8::new_unchecked(20) }),
            Token::Colon,
            Token::Symbol("adv"),
            Token::ParenOpen,
            Token::Number(unsafe { NonZeroU8::new_unchecked(3) }),
            Token::ParenClose,
        ],
        Ast::Dice(Dice {
            count: unsafe { NonZeroU8::new_unchecked(2) },
            sides: unsafe { NonZeroU8::new_unchecked(20) },
            modifiers: vec![
                ModifierCall {
                    name: "adv",
                    arg: Some(Ast::Constant(unsafe { NonZeroU8::new_unchecked(3)})),
                },
            ],
        }),
    )]
    fn dice_valid_modifiers_with_args_parses(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::Equal,
            Token::Number(unsafe { NonZeroU8::new_unchecked(3) }),
        ],
        Ast::LogicalUnaryOperation {
            rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(3) })),
            op: LogicalUnaryOperator::Equal,
        },
    )]
    #[case(
        vec![
            Token::LessThanEqual,
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
        ],
        Ast::LogicalUnaryOperation {
            rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(2) })),
            op: LogicalUnaryOperator::LessThanEqual,
        },
    )]
    fn logical_unary_operation_valid_parses(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(1) }),
            Token::Multiply,
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
        ],
        Ast::BinaryOperation {
            lhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(1) })),
            rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(2) })),
            op: BinaryOperator::Multiply,
        },
    )]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(9) }),
            Token::Plus,
            Token::Number(unsafe { NonZeroU8::new_unchecked(10) }),
        ],
        Ast::BinaryOperation {
            lhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(9) })),
            rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(10) })),
            op: BinaryOperator::Add,
        },
    )]
    fn binary_operation_valid_parses(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::LessThanEqual,
            Token::Number(unsafe { NonZeroU8::new_unchecked(1) }),
            Token::Multiply,
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
        ],
        Ast::LogicalUnaryOperation {
            rhs: Box::new(Ast::BinaryOperation {
                lhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(1) })),
                rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(2) })),
                op: BinaryOperator::Multiply,
            }),
            op: LogicalUnaryOperator::LessThanEqual
        },
    )]
    #[case(
        vec![
            Token::Equal,
            Token::Number(unsafe { NonZeroU8::new_unchecked(9) }),
            Token::Plus,
            Token::Number(unsafe { NonZeroU8::new_unchecked(10) }),
        ],
        Ast::LogicalUnaryOperation {
            rhs: Box::new(Ast::BinaryOperation {
                lhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(9) })),
                rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(10) })),
                op: BinaryOperator::Add,
            }),
            op: LogicalUnaryOperator::Equal,
        }
    )]
    fn precedence_parenthesis_over_logical_unary(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::ParenOpen,
            Token::LessThanEqual,
            Token::Number(unsafe { NonZeroU8::new_unchecked(1) }),
            Token::ParenClose,
            Token::Multiply,
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
        ],
        Ast::BinaryOperation {
            lhs: Box::new(Ast::LogicalUnaryOperation {
                rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(1) })),
                op: LogicalUnaryOperator::LessThanEqual,
            }),
            rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(2) })),
            op: BinaryOperator::Multiply
        },
    )]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
            Token::Multiply,
            Token::ParenOpen,
            Token::LessThanEqual,
            Token::Number(unsafe { NonZeroU8::new_unchecked(1) }),
            Token::ParenClose,
        ],
        Ast::BinaryOperation {
            lhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(2) })),
            rhs: Box::new(Ast::LogicalUnaryOperation {
                rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(1) })),
                op: LogicalUnaryOperator::LessThanEqual,
            }),
            op: BinaryOperator::Multiply
        },
    )]
    fn precedence_logical_unary_over_multiplication(
        #[case] input: Vec<Token>,
        #[case] output: Ast,
    ) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }

    #[rstest]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(1) }),
            Token::Multiply,
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
            Token::Plus,
            Token::Number(unsafe { NonZeroU8::new_unchecked(3) }),
        ],
        Ast::BinaryOperation {
            lhs: Box::new(Ast::BinaryOperation {
                lhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(1) }) ),
                rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(2) }) ),
                op: BinaryOperator::Multiply
            }),
            rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(3) }) ),
            op: BinaryOperator::Add
        },
    )]
    #[case(
        vec![
            Token::Number(unsafe { NonZeroU8::new_unchecked(1) }),
            Token::Minus,
            Token::Number(unsafe { NonZeroU8::new_unchecked(2) }),
            Token::DivideFloor,
            Token::Number(unsafe { NonZeroU8::new_unchecked(3) }),
        ],
        Ast::BinaryOperation {
            lhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(1) }) ),
            rhs: Box::new(Ast::BinaryOperation {
                lhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(2) }) ),
                rhs: Box::new(Ast::Constant(unsafe { NonZeroU8::new_unchecked(3) }) ),
                op: BinaryOperator::DivideFloor
            }),
            op: BinaryOperator::Subtract
        },
    )]
    fn precedence_multiplication_over_summation(#[case] input: Vec<Token>, #[case] output: Ast) {
        assert_eq!(
            parser().parse(Stream::from_iter(input)).into_result(),
            Ok(output)
        )
    }
}
