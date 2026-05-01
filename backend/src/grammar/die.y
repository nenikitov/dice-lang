%grmtools {
    yacckind: Grmtools,
}

%expect-unused Unmatched "UNMATCHED"

%start Program

%%
// Program
Program         -> ()
                : Statement
                    {}
                ;

Statement       -> ()
                : Expr0
                    {}
                ;

// Precedences
Expr0           -> ()
                : Expr1
                    {}
                | Expr0 "+" Expr1
                    {}
                | Expr0 "-" Expr1
                    {}
                ;

Expr1           -> ()
                : Expr2
                    {}
                | Expr1 "*" Expr2
                    {}
                | Expr1 "/" Expr2
                    {}
                | Expr1 "/+" Expr2
                    {}
                | Expr1 "/-" Expr2
                    {}
                | Expr1 "/~" Expr2
                    {}
                ;

Expr2           -> ()
                : "(" Expr0 ")"
                    {}
                | ExprNote ExprAtom ExprAtomTail
                    {}
                ;

ExprNote        -> ()
                : "LIT_DOUBLE_QUOTED"
                    {}
                |
                    {}
                ;

ExprAtom        -> ()
                : "LIT_DIE"
                    {}
                | "LIT_NUMBER"
                    {}
                ;

ExprAtomTail    -> ()
                : ":" "LIT_IDENTIFIER" ExprArgs ExprAtomTail
                    {}
                |
                    {}
                ;

ExprArgs        -> ()
                :   "(" ExprArgsInner ")"
                    {}
                |
                    {}
                ;

ExprArgsInner   -> ()
                : Expr0 "," ExprArgsInner
                    {}
                | Expr0
                    {}
                |
                    {}
                ;

// Other
Unmatched       -> ()
                : "UNMATCHED"
                    {}
                ;
%%
