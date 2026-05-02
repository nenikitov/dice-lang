%grmtools {
    yacckind: Grmtools,
}

%expect-unused Unmatched "UNMATCHED"

%start Program

%%
// Entry
Program                 ->  ast::Result<ast::Program>
                        :   ExpressionBlock
                                {
                                    ast::Program($1).into_result()
                                }
                        ;

// Expressions
ExpressionBlock         ->  ast::Result<ast::Block>
                        :   ExpressionBlockList
                                {
                                    ast::Block {
                                        body: $1,
                                        last: None.into_result(),
                                    }.into_result()
                                }
                        |   ExpressionBlockList Expression
                                {
                                    ast::Block {
                                        body: $1,
                                        last: Some($2).into_result(),
                                    }.into_result()
                                }
                        ;
ExpressionBlockList     ->  ast::Result<Vec<ast::Result<ast::Expression>>>
                        :
                                {
                                    vec![].into_result()
                                }
                        |   ExpressionBlockList Expression ";"
                                {
                                    let is_ok = $1.is_ok();
                                    let mut result = match $1 {
                                        Ok(r) => r,
                                        Err(r) => r,
                                    };
                                    result.push($2);
                                    if is_ok { Ok(result) } else { Err(result) }
                                }
                        ;

ExpressionList          ->  ast::Result<Vec<ast::Result<ast::Expression>>>
                        :   ExpressionListList
                                {
                                    $1
                                }
                        |   ExpressionListList Expression
                                {
                                    let is_ok = $1.is_ok();
                                    let mut result = match $1 {
                                        Ok(r) => r,
                                        Err(r) => r,
                                    };
                                    result.push($2);
                                    if is_ok { Ok(result) } else { Err(result) }
                                }
                        ;
ExpressionListList    ->  ast::Result<Vec<ast::Result<ast::Expression>>>
                        :
                                {
                                    Ok(vec![])
                                }
                        |   ExpressionListList Expression ","
                                {
                                    let is_ok = $1.is_ok();
                                    let mut result = match $1 {
                                        Ok(r) => r,
                                        Err(r) => r,
                                    };
                                    result.push($2);
                                    if is_ok { Ok(result) } else { Err(result) }
                                }
                        ;

// Expression
// By precedence, from lowest to highest
Expression              ->  ast::Result<ast::Expression>
                        :   Expression1
                                {
                                    $1
                                }
                        |   "LET" "LIT_IDENTIFIER" "=" Expression
                                {
                                    ast::Expression::Declaration {
                                        left: $2,
                                        right: Box::new($4),
                                    }.into_result()
                                }
                        |   "LIT_IDENTIFIER" "=" Expression
                                {
                                    ast::Expression::Assignment {
                                        left: $1,
                                        right: Box::new($3),
                                    }.into_result()
                                }
                        ;
Expression1             ->  ast::Result<ast::Expression>
                        :   Expression2
                                {
                                    $1
                                }
                        |   Expression1 Expression1Op Expression2
                                {
                                    ast::Expression::Binary {
                                        left: Box::new($1),
                                        op: $2,
                                        right: Box::new($3),
                                    }.into_result()
                                }
                        ;
Expression1Op           ->  ast::Result<ast::Lexeme>
                        :   "+"
                                {
                                    $1
                                }
                        |   "-"
                                {
                                    $1
                                }
                        ;
Expression2             ->  ast::Result<ast::Expression> 
                        :   Expression3
                                {
                                    $1
                                }
                        |   Expression2 Expression2Op Expression3
                                {
                                    ast::Expression::Binary {
                                        left: Box::new($1),
                                        op: $2,
                                        right: Box::new($3),
                                    }.into_result()
                                }
                        ;
Expression2Op           ->  ast::Result<ast::Lexeme> 
                        :   "*"
                                {
                                    $1
                                }
                        |   "/-"
                                {
                                    $1
                                }
                        |   "/+"
                                {
                                    $1
                                }
                        |   "/~"
                                {
                                    $1
                                }
                        |   "/"
                                {
                                    $1
                                }
                        ;
Expression3             ->  ast::Result<ast::Expression>
                        :   Expression4
                                {
                                    $1
                                }
                        |   Expression3 "." "LIT_IDENTIFIER"
                                {
                                    ast::Expression::Field {
                                        expression: Box::new($1),
                                        field: $3,
                                    }.into_result()
                                }
                        |   Expression3 "(" ExpressionList ")"
                                {
                                    ast::Expression::Call {
                                        expression: Box::new($1),
                                        arguments: $3,
                                    }.into_result()
                                }
                        ;
Expression4             ->  ast::Result<ast::Expression>
                        :   Expression5
                                {
                                    $1
                                }
                        |   "LIT_DOUBLE_QUOTED" Expression5
                                {
                                    ast::Expression::Tagged {
                                        tag: $1,
                                        expression: Box::new($2),
                                    }.into_result()
                                }
                        ;
Expression5             ->  ast::Result<ast::Expression>
                        :   "LIT_IDENTIFIER"
                                {
                                    ast::Expression::Literal($1).into_result()
                                }
                        |   "LIT_NUMBER"
                                {
                                    ast::Expression::Literal($1).into_result()
                                }
                        |   "LIT_DIE"
                                {
                                    ast::Expression::Literal($1).into_result()
                                }
                        |   "(" Expression ")"
                                {
                                    $2
                                }
                        |   "{" ExpressionBlock "}"
                                {
                                    ast::Expression::Block(Box::new($2)).into_result()
                                }
                        ;

// Other
Unmatched               ->  ()
                        :   "UNMATCHED"
                                {}
                        ;
%%

use crate::ast;
use crate::ast::Node as _;
