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
  | SET
  | IFBLOCK
  | WHILE
  | CALL

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Ast

# 49 "parser.ml"
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
  290 (* PROC *);
  291 (* PROCREC *);
  292 (* SET *);
  293 (* IFBLOCK *);
  294 (* WHILE *);
  295 (* CALL *);
    0|]

let yytransl_block = [|
  276 (* INTV *);
  279 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\006\000\005\000\005\000\
\005\000\005\000\005\000\005\000\004\000\004\000\004\000\004\000\
\004\000\007\000\007\000\007\000\010\000\010\000\011\000\009\000\
\009\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\003\000\004\000\007\000\
\008\000\003\000\004\000\004\000\002\000\003\000\004\000\003\000\
\003\000\001\000\001\000\005\000\001\000\003\000\003\000\001\000\
\003\000\001\000\001\000\001\000\001\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\005\000\005\000\004\000\004\000\
\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\044\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\027\000\028\000\029\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\019\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\000\000\000\000\014\000\000\000\000\000\
\016\000\017\000\005\000\004\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\012\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\000\000\
\000\000\000\000\043\000\040\000\023\000\039\000\025\000\006\000\
\022\000\000\000\000\000\000\000\000\000\030\000\032\000\031\000\
\033\000\034\000\037\000\038\000\035\000\020\000\000\000\008\000\
\041\000\009\000"

let yydgoto = "\002\000\
\004\000\005\000\016\000\017\000\018\000\064\000\069\000\084\000\
\057\000\070\000\058\000\085\000"

let yysindex = "\009\000\
\005\255\000\000\019\255\000\000\000\000\253\254\246\254\095\255\
\255\254\000\255\002\255\003\255\095\255\095\255\012\255\023\255\
\004\255\017\255\252\254\036\255\252\254\076\255\037\255\000\000\
\000\000\000\000\000\000\000\000\252\254\037\255\037\255\095\255\
\054\255\054\255\095\255\000\000\019\255\019\255\252\254\000\000\
\000\000\095\255\252\254\055\255\095\255\095\255\095\255\095\255\
\095\255\095\255\095\255\095\255\095\255\095\255\095\255\059\255\
\056\255\075\255\000\000\054\255\054\255\000\000\019\255\054\255\
\000\000\000\000\000\000\000\000\077\255\079\255\000\000\073\255\
\037\255\095\255\095\255\095\255\095\255\095\255\095\255\081\255\
\095\255\095\255\095\255\095\255\082\255\252\254\095\255\037\255\
\000\000\000\000\089\255\000\000\252\254\252\254\037\255\090\255\
\095\255\105\255\106\255\107\255\108\255\110\255\000\000\111\255\
\114\255\115\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\116\255\093\255\095\255\117\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\095\255\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\118\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\121\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\119\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\222\255\000\000\000\000\243\255\246\255\248\255\
\233\255\034\000\000\000\001\000"

let yytablesize = 127
let yytable = "\028\000\
\039\000\020\000\067\000\068\000\033\000\034\000\060\000\061\000\
\042\000\001\000\044\000\003\000\021\000\055\000\024\000\024\000\
\040\000\041\000\059\000\019\000\065\000\029\000\030\000\062\000\
\031\000\032\000\066\000\006\000\091\000\007\000\036\000\008\000\
\072\000\071\000\035\000\037\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\089\000\090\000\
\038\000\096\000\092\000\009\000\010\000\011\000\012\000\013\000\
\014\000\015\000\043\000\056\000\063\000\073\000\086\000\087\000\
\111\000\097\000\098\000\099\000\100\000\101\000\102\000\115\000\
\104\000\105\000\106\000\109\000\088\000\093\000\110\000\095\000\
\022\000\094\000\023\000\114\000\107\000\045\000\103\000\108\000\
\117\000\024\000\025\000\046\000\047\000\048\000\049\000\026\000\
\112\000\116\000\027\000\022\000\127\000\023\000\050\000\051\000\
\052\000\053\000\054\000\128\000\024\000\025\000\118\000\119\000\
\120\000\121\000\026\000\122\000\123\000\027\000\130\000\124\000\
\125\000\126\000\129\000\021\000\042\000\003\000\113\000"

let yycheck = "\008\000\
\005\001\012\001\037\000\038\000\013\000\014\000\030\000\031\000\
\019\000\001\000\021\000\007\001\023\001\022\000\007\001\008\001\
\021\001\022\001\029\000\023\001\034\000\023\001\023\001\032\000\
\023\001\023\001\035\000\009\001\063\000\011\001\008\001\013\001\
\043\000\042\000\023\001\032\001\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\060\000\061\000\
\032\001\073\000\064\000\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\023\001\023\001\007\001\007\001\004\001\008\001\
\088\000\074\000\075\000\076\000\077\000\078\000\079\000\095\000\
\081\000\082\000\083\000\086\000\002\001\001\001\087\000\007\001\
\005\001\003\001\007\001\094\000\084\000\010\001\006\001\006\001\
\097\000\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\008\001\008\001\023\001\005\001\008\001\007\001\027\001\028\001\
\029\001\030\001\031\001\116\000\014\001\015\001\006\001\006\001\
\006\001\006\001\020\001\006\001\006\001\023\001\127\000\006\001\
\006\001\006\001\006\001\003\001\006\001\008\001\093\000"

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
  PROC\000\
  PROCREC\000\
  SET\000\
  IFBLOCK\000\
  WHILE\000\
  CALL\000\
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
# 44 "parser.mly"
        ( ASTprog(_1) )
# 265 "parser.ml"
               : Ast.ansyn))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 46 "parser.mly"
                      ( ASTcmds(_2) )
# 272 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 49 "parser.mly"
      ( ASTstat(_1) )
# 279 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 50 "parser.mly"
               ( ASTdeccmd(_1,_3) )
# 287 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 51 "parser.mly"
                ( ASTstatcmd(_1,_3) )
# 295 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 55 "parser.mly"
                ( ASTblock(_2))
# 302 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                      ( ASTconst(_2, _3, _4) )
# 311 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                                     (ASTfun(_2, _3, _5, _7) )
# 321 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                         (ASTrfun (_3, _4, _6, _8) )
# 331 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 63 "parser.mly"
                 (ASTvar(_2, _3)  )
# 339 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 64 "parser.mly"
                         ( ASTproc(_2, _3, _4))
# 348 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 65 "parser.mly"
                            ( ASTprocrec(_2, _3, _4))
# 357 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
           ( ASTecho(_2) )
# 364 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                  ( ASTset(_2, _3) )
# 372 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 72 "parser.mly"
                            ( ASTifblock(_2, _3, _4) )
# 381 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 73 "parser.mly"
                    ( ASTwhile(_2, _3) )
# 389 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                   ( ASTcall(_2, _3) )
# 397 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
     ( Int )
# 403 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
        ( Bool )
# 409 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 80 "parser.mly"
                             ( Fun(_2, _4) )
# 417 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 83 "parser.mly"
     ( Typ(_1) )
# 424 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 84 "parser.mly"
                      ( Couple(_1,_3) )
# 432 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 87 "parser.mly"
                 ( ASTarg(_1, _3) )
# 440 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 90 "parser.mly"
     ( Arg(_1) )
# 447 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 91 "parser.mly"
                  ( ASTargs(_1,_3) )
# 455 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
      ( ASTtrue )
# 461 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
         ( ASTfalse )
# 467 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 96 "parser.mly"
        ( ASTnum(_1) )
# 474 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
         ( ASTident(_1) )
# 481 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                            ( ASTprim(Ast.Add, _3, _4 ) )
# 489 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                            ( ASTprim(Ast.Sub, _3, _4 ) )
# 497 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                            ( ASTprim(Ast.Mul, _3, _4 ) )
# 505 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                            ( ASTprim(Ast.Div, _3, _4 ) )
# 513 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                           ( ASTprim(Ast.Eq, _3, _4 ) )
# 521 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                           ( ASTprim(Ast.Lt, _3, _4 ) )
# 529 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                       ( ASTunaryPrim(Ast.Not, _3 ) )
# 536 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                            ( ASTprim(Ast.And, _3, _4 ) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                           ( ASTprim(Ast.Or, _3, _4 ) )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                       ( ASTlambda(_2, _4))
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 108 "parser.mly"
                        ( ASTapply(_2,_3) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                               ( ASTif(_3, _4, _5) )
# 577 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
             (Expr(_1))
# 584 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 112 "parser.mly"
              ( ASTexprs(_1, _2) )
# 592 "parser.ml"
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
