type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | MATMUL
  | ELMUL
  | ASSIGN
  | FDECL
  | RANGEMAT
  | CONV
  | PLUS
  | MINUS
  | RAISE
  | PLUSREDUCE
  | MULREDUCE
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | IMAT
  | ELMAT
  | TRANSP
  | VLINE
  | SEMIRING
  | CONCAT
  | ZEROMAT
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | LITERAL of (int)
  | ID of (string)
  | FLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
