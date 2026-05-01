%grmtools {
    allow_wholeline_comments,
    ignore_whitespace,
}
%%

// Keywords
let                 "LET"

// Operators
\=                  "="
\.                  "."
\+                  "+"
\-                  "-"
\*                  "*"
\/\+                "/+"
\/\-                "/-"
\/\~                "/~"
\/                  "/"

// Punctuation
\(                  "("
\)                  ")"
\{                  "{"
\}                  "}"
\;                  ";"
\,                  ","

// Literals
(\d+)?d\d+          "LIT_DIE"
\d+                 "LIT_NUMBER"
[a-z][a-z_]*        "LIT_IDENTIFIER"
"(\\.|[^"])*"       "LIT_DOUBLE_QUOTED"

// Other
\s+                 ;
.                   "UNMATCHED"
