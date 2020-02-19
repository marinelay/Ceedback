type token =
  | INT of (int)
  | VAR of (string)
  | EXAMPLES
  | INTCOMPS
  | INTVARCOMPS
  | ARRVARCOMPS
  | PARTIALPGM
  | FUN
  | SKIP
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | WHILE
  | PLUS
  | MINUS
  | MUL
  | DIV
  | MOD
  | EQUAL
  | GT
  | LT
  | EQUALEQUAL
  | NOT
  | AND
  | OR
  | HOLE
  | RETURN
  | MAPSTO
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | SEMICOLON
  | NONE
  | EOF

val resource :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Imp.example list * Imp.prog * int list * Imp.var list * Imp.var list
