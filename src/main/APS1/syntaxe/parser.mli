type token =
  | ASTERISK
  | COMMA
  | ARROW
  | COLON
  | LPAR
  | RPAR
  | LCRO
  | RCRO
  | CONST
  | IF
  | FUN
  | REC
  | ECHO
  | TRUE
  | FALSE
  | ADD
  | MUL
  | SUB
  | DIV
  | INTV of (int)
  | BOOL
  | INT
  | IDENT of (string)
  | PLUS
  | MINUS
  | SLASH
  | EQ
  | NOT
  | AND
  | OR
  | LT
  | PC
  | VAR
  | PROC
  | PROCREC
  | VOID
  | SET
  | IFBLOCK
  | WHILE
  | CALL

val ansyn :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ansyn
