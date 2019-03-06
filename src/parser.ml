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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Ast

# 42 "parser.ml"
let yytransl_const = [|
  257 (* ASTERISK *);
  258 (* COMMA *);
  259 (* ARROW *);
  260 (* COLON *);
  261 (* LPAR *);
  262 (* RPAR *);
  263 (* LCRO *);
  264 (* RCRO *);
  265 (* CONST *);
  266 (* IF *);
  267 (* FUN *);
  268 (* REC *);
  269 (* ECHO *);
  270 (* TRUE *);
  271 (* FALSE *);
  272 (* ADD *);
  273 (* MUL *);
  274 (* SUB *);
  275 (* DIV *);
  277 (* BOOL *);
  278 (* INT *);
  280 (* PLUS *);
  281 (* MINUS *);
  282 (* SLASH *);
  283 (* EQ *);
  284 (* NOT *);
  285 (* AND *);
  286 (* OR *);
  287 (* LT *);
  288 (* PC *);
    0|]

let yytransl_block = [|
  276 (* INTV *);
  279 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\004\000\005\000\005\000\
\005\000\007\000\007\000\007\000\009\000\009\000\010\000\008\000\
\008\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\011\000\011\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\004\000\007\000\
\008\000\001\000\001\000\005\000\001\000\003\000\003\000\001\000\
\003\000\001\000\001\000\001\000\001\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\005\000\005\000\004\000\004\000\
\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\036\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\019\000\020\000\021\000\006\000\002\000\000\000\000\000\
\000\000\011\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\004\000\000\000\000\000\
\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\035\000\032\000\015\000\031\000\017\000\014\000\000\000\000\000\
\000\000\000\000\022\000\024\000\023\000\025\000\026\000\029\000\
\030\000\027\000\012\000\000\000\008\000\033\000\009\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\010\000\011\000\062\000\047\000\043\000\
\048\000\044\000\063\000"

let yysindex = "\008\000\
\250\254\000\000\253\254\000\000\000\000\244\254\027\255\254\254\
\008\255\003\255\004\255\012\255\015\255\012\255\055\255\018\255\
\000\000\000\000\000\000\000\000\000\000\000\000\253\254\253\254\
\012\255\000\000\000\000\254\254\012\255\035\255\254\254\254\254\
\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\
\254\254\039\255\047\255\054\255\000\000\000\000\058\255\061\255\
\000\000\059\255\018\255\254\254\254\254\254\254\254\254\254\254\
\254\254\062\255\254\254\254\254\254\254\254\254\070\255\012\255\
\254\254\018\255\012\255\012\255\018\255\053\255\254\254\071\255\
\073\255\074\255\081\255\082\255\000\000\083\255\084\255\085\255\
\000\000\000\000\000\000\000\000\000\000\000\000\087\255\086\255\
\254\254\089\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\254\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\088\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\090\255\000\000\000\000\064\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\091\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\247\255\000\000\000\000\248\255\246\255\227\255\
\032\000\000\000\038\000"

let yytablesize = 100
let yytable = "\021\000\
\003\000\028\000\015\000\030\000\016\000\006\000\041\000\007\000\
\001\000\008\000\012\000\017\000\018\000\045\000\046\000\022\000\
\025\000\019\000\050\000\049\000\020\000\070\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\026\000\027\000\023\000\024\000\085\000\029\000\013\000\088\000\
\042\000\051\000\064\000\071\000\072\000\073\000\074\000\075\000\
\076\000\014\000\078\000\079\000\080\000\083\000\065\000\066\000\
\084\000\087\000\067\000\015\000\089\000\016\000\090\000\068\000\
\031\000\069\000\013\000\077\000\017\000\018\000\032\000\033\000\
\034\000\035\000\019\000\082\000\091\000\020\000\092\000\093\000\
\101\000\036\000\037\000\038\000\039\000\040\000\094\000\095\000\
\096\000\097\000\098\000\103\000\099\000\100\000\102\000\003\000\
\034\000\016\000\086\000\081\000"

let yycheck = "\008\000\
\007\001\012\000\005\001\014\000\007\001\009\001\015\000\011\001\
\001\000\013\001\023\001\014\001\015\001\023\000\024\000\008\001\
\005\001\020\001\029\000\028\000\023\001\051\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\021\001\022\001\032\001\032\001\066\000\023\001\012\001\069\000\
\023\001\007\001\004\001\052\000\053\000\054\000\055\000\056\000\
\057\000\023\001\059\000\060\000\061\000\064\000\008\001\002\001\
\065\000\068\000\001\001\005\001\008\001\007\001\071\000\003\001\
\010\001\007\001\003\001\006\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\006\001\006\001\023\001\006\001\006\001\
\089\000\027\001\028\001\029\001\030\001\031\001\006\001\006\001\
\006\001\006\001\006\001\100\000\006\001\008\001\006\001\008\001\
\006\001\008\001\067\000\062\000"

let yynames_const = "\
  ASTERISK\000\
  COMMA\000\
  ARROW\000\
  COLON\000\
  LPAR\000\
  RPAR\000\
  LCRO\000\
  RCRO\000\
  CONST\000\
  IF\000\
  FUN\000\
  REC\000\
  ECHO\000\
  TRUE\000\
  FALSE\000\
  ADD\000\
  MUL\000\
  SUB\000\
  DIV\000\
  BOOL\000\
  INT\000\
  PLUS\000\
  MINUS\000\
  SLASH\000\
  EQ\000\
  NOT\000\
  AND\000\
  OR\000\
  LT\000\
  PC\000\
  "

let yynames_block = "\
  INTV\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 46 "parser.mly"
        ( ASTprog(_1) )
# 224 "parser.ml"
               : Ast.ansyn))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 48 "parser.mly"
                      ( ASTcmds(_2) )
# 231 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 52 "parser.mly"
      ( ASTstat(_1) )
# 238 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 53 "parser.mly"
               ( ASTdeccmd(_1,_3) )
# 246 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 54 "parser.mly"
                ( ASTstatcmd(_1,_3) )
# 254 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
           ( ASTecho(_2) )
# 261 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                      ( ASTconst(_2, _3, _4) )
# 270 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                     (ASTfun(_2, _3, _5, _7) )
# 280 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                         (ASTrfun (_3, _4, _6, _8) )
# 290 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
     ( Int )
# 296 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
        ( Bool )
# 302 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 70 "parser.mly"
                             ( Fun(_2, _4) )
# 310 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 74 "parser.mly"
     ( Typ(_1) )
# 317 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 75 "parser.mly"
                      ( Couple(_1,_3) )
# 325 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 79 "parser.mly"
                 ( ASTarg(_1, _3) )
# 333 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 83 "parser.mly"
     ( Arg(_1) )
# 340 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 84 "parser.mly"
                  ( ASTargs(_1,_3) )
# 348 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
      ( ASTtrue )
# 354 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
         ( ASTfalse )
# 360 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
        ( ASTintv(_1) )
# 367 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
         ( ASTident(_1) )
# 374 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                            ( ASTprim(Ast.Add, _3, _4 ) )
# 382 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                            ( ASTprim(Ast.Sub, _3, _4 ) )
# 390 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                            ( ASTprim(Ast.Mul, _3, _4 ) )
# 398 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                            ( ASTprim(Ast.Div, _3, _4 ) )
# 406 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                           ( ASTprim(Ast.Eq, _3, _4 ) )
# 414 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                           ( ASTprim(Ast.Lt, _3, _4 ) )
# 422 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                       ( ASTunaryPrim(Ast.Not, _3 ) )
# 429 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                            ( ASTprim(Ast.And, _3, _4 ) )
# 437 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                           ( ASTprim(Ast.Or, _3, _4 ) )
# 445 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                       ( ASTafun(_2, _4))
# 453 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 102 "parser.mly"
                        ( ASTexprl(_2,_3) )
# 461 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                               ( ASTif(_3, _4, _5) )
# 470 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
             (Expr(_1))
# 477 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 107 "parser.mly"
              ( ASTexprs(_1, _2) )
# 485 "parser.ml"
               : 'exprs))
(* Entry ansyn *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let ansyn (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.ansyn)
