%grmtools {
    yacckind: Grmtools,
}

%expect-unused Unmatched "UNMATCHED"

%start Program

%%
// Entry
Program                 ->  ()
                        :   ExpressionsSemi
                                {}
                        ;

// Expressions
ExpressionsSemi         ->  ()
                        :   ExpressionsSemiList
                                {}
                        |   ExpressionsSemiList Expression
                                {}
                        ;
ExpressionsSemiList     ->  ()
                        :
                                {}
                        |   ExpressionsSemiList Expression ";"
                                {}
                        ;

ExpressionsComma        ->  ()
                        :   ExpressionsCommaList
                                {}
                        |   ExpressionsCommaList Expression
                                {}
                        ;
ExpressionsCommaList    ->  ()
                        :
                                {}
                        |   ExpressionsCommaList Expression ","
                                {}
                        ;

// Expression
// By precedence, from lowest to highest
Expression              ->  ()
                        :   Expression1
                                {}
                        |   "LET" "LIT_IDENTIFIER" "=" Expression
                                {}
                        |   "LIT_IDENTIFIER" "=" Expression
                                {}
                        ;
Expression1             ->  ()
                        :   Expression2
                                {}
                        |   Expression1 Expression1Op Expression2
                                {}
                        ;
Expression1Op           ->  ()
                        :   "+"
                                {}
                        |   "-"
                                {}
                        ;
Expression2             ->  ()
                        :   Expression3
                                {}
                        |   Expression2 Expression2Op Expression3
                                {}
                        ;
Expression2Op           ->  ()
                        :   "*"
                                {}
                        |   "/-"
                                {}
                        |   "/+"
                                {}
                        |   "/~"
                                {}
                        |   "/"
                                {}
                        ;
Expression3             ->  ()
                        :   Expression4
                                {}
                        |   Expression3 "." "LIT_IDENTIFIER"
                                {}
                        |   Expression3 "(" ExpressionsComma ")"
                                {}
                        ;
Expression4             ->  ()
                        :   Expression5
                                {}
                        |   "LIT_DOUBLE_QUOTED" Expression5
                                {}
                        ;
Expression5             ->  ()
                        :   "LIT_IDENTIFIER"
                                {}
                        |   "LIT_NUMBER"
                                {}
                        |   "LIT_DIE"
                                {}
                        |   "(" Expression ")"
                                {}
                        |   "{" ExpressionsSemi "}"
                                {}
                        ;

// Other
Unmatched               ->  ()
                        :   "UNMATCHED"
                                {}
                        ;
%%
