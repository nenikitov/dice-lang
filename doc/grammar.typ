#set raw(syntaxes: (
  "./res/ebnf.sublime-syntax",
  "./res/regex.sublime-syntax",
))

#show raw: it => {
  let display = if it.block {
    block
  } else {
    box
  }

  display(
    fill: gray.lighten(80%),
    radius: 2pt,
    inset: (x: 2pt),
    outset: (y: 2pt),
    it,
  )
}

= Tokens

```regex
// Keywords
"LET"               ::= let

// Operators
"="                 ::= =
"."                 ::= \.
"+"                 ::= \+
"-"                 ::= \-
"*"                 ::= \*
"/+"                ::= \/\+
"/-"                ::= \/\-
"/~"                ::= \/~
"/"                 ::= \/

// Punctuation
"("                 ::= \(
")"                 ::= \)
"{"                 ::= \{
"}"                 ::= \}
";"                 ::= ;
","                 ::= ,

// Literals
"LIT_DIE"           ::= (\d+)?d\d+
"LIT_NUMBER"        ::= \d+
"LIT_IDENTIFIER"    ::= [a-z][a-z_]*
"LIT_DOUBLE_QUOTED" ::= \"(\\.|[^\"])*\"
```

= Grammar

```ebnf
<program>           ::= <expressions_semi>

// Expressions
<expressions_semi>  ::= { <expression> ";" } [ <expression> [ ";" ] ]
<expressions_comma> ::= { <expression> "," } [ <expression> [ "," ] ]

// Expression
// By precedence, from lowest to highest
<expression>        ::= "let" LIT_IDENTIFIER "=" <expression>
                      | LIT_IDENTIFIER "=" <expression>
                      | <expression_1>
<expression_1>      ::= <expression_1> ( "+" | "-" ) <expression_2>
                      | <expression_2>
<expression_2>      ::= <expression_2> ( "*" | "/-" | "/+" | "/") <expression_3>
                      | <expression_3>
<expression_3>      ::= <expression_3> "." LIT_IDENTIFIER
                      | <expression_3> "(" [ <expressions_comma> ] ")"
                      | <expression_4>
<expression_4>      ::= <expression_5>
                      | LIT_DOUBLE_QUOTED <expression_5>
<expression_5>      ::= LIT_IDENTIFIER
                      | LIT_NUMBER
                      | LIT_DIE
                      | "(" <expression> ")"
                      | "{" <expressions_semi> "}"
```
