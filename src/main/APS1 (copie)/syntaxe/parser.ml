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
  | SET

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Ast

# 44 "parser.ml"
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
  289 (* VAR *);
  290 (* SET *);
    0|]

let yytransl_block = [|
  276 (* INTV *);
  279 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\004\000\004\000\004\000\002\000\006\000\006\000\
\006\000\006\000\005\000\005\000\007\000\007\000\007\000\010\000\
\010\000\011\000\009\000\009\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\003\000\004\000\007\000\
\008\000\003\000\002\000\003\000\001\000\001\000\005\000\001\000\
\003\000\003\000\001\000\003\000\001\000\001\000\001\000\001\000\
\005\000\005\000\005\000\005\000\005\000\005\000\004\000\005\000\
\005\000\004\000\004\000\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\039\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\022\000\023\000\024\000\011\000\000\000\
\000\000\002\000\000\000\000\000\000\000\014\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\012\000\005\000\004\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000\000\000\000\000\000\000\038\000\035\000\
\018\000\034\000\020\000\017\000\000\000\000\000\000\000\000\000\
\025\000\027\000\026\000\028\000\029\000\032\000\033\000\030\000\
\015\000\000\000\008\000\036\000\009\000"

let yydgoto = "\002\000\
\004\000\000\000\005\000\011\000\012\000\013\000\053\000\068\000\
\047\000\054\000\048\000\069\000"

let yysindex = "\007\000\
\005\255\000\000\248\254\000\000\000\000\246\254\251\254\088\255\
\249\254\088\255\007\255\243\254\244\254\040\255\254\254\040\255\
\061\255\015\255\000\000\000\000\000\000\000\000\000\000\040\255\
\088\255\000\000\248\254\248\254\040\255\000\000\000\000\088\255\
\040\255\032\255\088\255\088\255\088\255\088\255\088\255\088\255\
\088\255\088\255\088\255\088\255\088\255\037\255\034\255\041\255\
\000\000\000\000\000\000\000\000\043\255\044\255\000\000\039\255\
\015\255\088\255\088\255\088\255\088\255\088\255\088\255\042\255\
\088\255\088\255\088\255\088\255\050\255\040\255\088\255\015\255\
\040\255\040\255\015\255\057\255\088\255\064\255\066\255\067\255\
\068\255\076\255\000\000\077\255\079\255\080\255\000\000\000\000\
\000\000\000\000\000\000\000\000\090\255\059\255\088\255\091\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\088\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\086\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\092\255\
\000\000\000\000\000\000\000\000\046\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\093\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\239\255\000\000\000\000\246\255\248\255\
\221\255\028\000\000\000\036\000"

let yytablesize = 111
let yytable = "\023\000\
\006\000\025\000\007\000\032\000\008\000\034\000\015\000\001\000\
\045\000\051\000\052\000\003\000\014\000\049\000\026\000\024\000\
\050\000\016\000\027\000\028\000\033\000\076\000\056\000\055\000\
\009\000\010\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\091\000\046\000\057\000\094\000\
\070\000\071\000\072\000\073\000\029\000\075\000\074\000\083\000\
\016\000\077\000\078\000\079\000\080\000\081\000\082\000\088\000\
\084\000\085\000\086\000\089\000\030\000\031\000\090\000\093\000\
\095\000\017\000\106\000\018\000\096\000\097\000\035\000\098\000\
\099\000\100\000\019\000\020\000\036\000\037\000\038\000\039\000\
\021\000\101\000\102\000\022\000\103\000\104\000\107\000\040\000\
\041\000\042\000\043\000\044\000\017\000\003\000\018\000\105\000\
\108\000\109\000\037\000\019\000\092\000\019\000\020\000\087\000\
\000\000\000\000\000\000\021\000\000\000\000\000\022\000"

let yycheck = "\008\000\
\009\001\010\000\011\001\014\000\013\001\016\000\012\001\001\000\
\017\000\027\000\028\000\007\001\023\001\024\000\008\001\023\001\
\025\000\023\001\032\001\032\001\023\001\057\000\033\000\032\000\
\033\001\034\001\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\072\000\023\001\007\001\075\000\
\004\001\008\001\002\001\001\001\005\001\007\001\003\001\006\001\
\003\001\058\000\059\000\060\000\061\000\062\000\063\000\006\001\
\065\000\066\000\067\000\070\000\021\001\022\001\071\000\074\000\
\008\001\005\001\008\001\007\001\077\000\006\001\010\001\006\001\
\006\001\006\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\006\001\006\001\023\001\006\001\006\001\095\000\027\001\
\028\001\029\001\030\001\031\001\005\001\008\001\007\001\006\001\
\006\001\106\000\006\001\008\001\073\000\014\001\015\001\068\000\
\255\255\255\255\255\255\020\001\255\255\255\255\023\001"

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
  VAR\000\
  SET\000\
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
# 49 "parser.mly"
        ( ASTprog(_1) )
# 235 "parser.ml"
               : Ast.ansyn))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 51 "parser.mly"
                      ( ASTcmds(_2) )
# 242 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 54 "parser.mly"
      ( ASTstat(_1) )
# 249 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 55 "parser.mly"
               ( ASTdeccmd(_1,_3) )
# 257 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 56 "parser.mly"
                ( ASTstatcmd(_1,_3) )
# 265 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 60 "parser.mly"
                ( ASTblock(_2))
# 272 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                      ( ASTconst(_2, _3, _4) )
# 281 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                     (ASTfun(_2, _3, _5, _7) )
# 291 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                         (ASTrfun (_3, _4, _6, _8) )
# 301 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 68 "parser.mly"
                 (ASTvar(_2, _3)  )
# 309 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
           ( ASTecho(_2) )
# 316 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                 ( ASTset(_2, _3) )
# 324 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
     ( Int )
# 330 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
        ( Bool )
# 336 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 85 "parser.mly"
                             ( Fun(_2, _4) )
# 344 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 91 "parser.mly"
     ( Typ(_1) )
# 351 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 92 "parser.mly"
                      ( Couple(_1,_3) )
# 359 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 95 "parser.mly"
                 ( ASTarg(_1, _3) )
# 367 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 98 "parser.mly"
     ( Arg(_1) )
# 374 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 99 "parser.mly"
                  ( ASTargs(_1,_3) )
# 382 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
      ( ASTtrue )
# 388 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
         ( ASTfalse )
# 394 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 104 "parser.mly"
        ( ASTnum(_1) )
# 401 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
         ( ASTident(_1) )
# 408 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                            ( ASTprim(Ast.Add, _3, _4 ) )
# 416 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                            ( ASTprim(Ast.Sub, _3, _4 ) )
# 424 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                            ( ASTprim(Ast.Mul, _3, _4 ) )
# 432 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                            ( ASTprim(Ast.Div, _3, _4 ) )
# 440 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                           ( ASTprim(Ast.Eq, _3, _4 ) )
# 448 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                           ( ASTprim(Ast.Lt, _3, _4 ) )
# 456 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                       ( ASTunaryPrim(Ast.Not, _3 ) )
# 463 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                            ( ASTprim(Ast.And, _3, _4 ) )
# 471 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                           ( ASTprim(Ast.Or, _3, _4 ) )
# 479 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                       ( ASTlambda(_2, _4))
# 487 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 116 "parser.mly"
                        ( ASTapply(_2,_3) )
# 495 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                               ( ASTif(_3, _4, _5) )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
             (Expr(_1))
# 511 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 120 "parser.mly"
              ( ASTexprs(_1, _2) )
# 519 "parser.ml"
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
