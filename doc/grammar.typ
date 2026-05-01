#set raw(syntaxes: ("./res/ebnf.sublime-syntax",))
#set page(margin: 0.5in)
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

= Token

#table(
  columns: 2,
  table.header([Token], [Regular Expression]),

  [Colon],
  [```re
  :
  ```],

  [Plus],
  [```re
  \+
  ```],

  [Minus],
  [```re
  -
  ```],

  [Star],
  [```re
  \*
  ```],

  [Slash Plus],
  [```re
  \/\+
  ```],

  [Slash Minus],
  [```re
  \/\-
  ```],

  [Slash Tilde],
  [```re
  \/\~
  ```],

  [Slash],
  [```re
  \/
  ```],

  [Parenthesis Open],
  [```re
  \(
  ```],

  [Parenthesis Close],
  [```re
  \)
  ```],

  [Comma],
  [```re
  ,
  ```],

  [Literal die],
  [```re
  (\d+)?d\d+
  ```],

  [Literal number],
  [```re
  \d+
  ```],

  [Literal identifier],
  [```re
  [a-z][a-z_]*
  ```],

  [Literal double quoted],
  [```re
  (\\.|[^"])*
  ```],
)

= Grammar

#table(
  columns: 2,
  table.header([Rule], [EBNF Productions]),

  [```ebnf
  <program>
  ```],
  [```ebnf
  <expression>
  ```],

  [```ebnf
  <expression>
  ```],
  [```ebnf
  <sum>
  ```],

  [```ebnf
  <sum>
  ```],
  [```ebnf
  <sum> ( PLUS | MINUS ) <product>
  | <product>
  ```],

  [```ebnf
  <product>
  ```],
  [```ebnf
  <product> ( STAR | SLASH_PLUS | SLASH_MINUS | SLASH_TILDE | SLASH ) <atom>
  | <atom>
  ```],

  [```ebnf
  <atom>
  ```],
  [```ebnf
  PAREN_OPEN <expression> PAREN_CLOSE
  | [ LIT_DOUBLE_QUOTED ] ( LIT_NUMBER | <atom_die> )
  ```],

  [```ebnf
  <atom_die>
  ```],
  [```ebnf
  LIT_DIE { COLON LIT_IDENTIFIER [ PAREN_OPEN [ <expression> { COMMA <expression> } [ COMMA ] ] PAREN_CLOSE ] }
  ```],
)

