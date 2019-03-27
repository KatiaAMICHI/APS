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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Ast

# 50 "parser.ml"
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
  292 (* VOID *);
  293 (* SET *);
  294 (* IFBLOCK *);
  295 (* WHILE *);
  296 (* CALL *);
    0|]

let yytransl_block = [|
  276 (* INTV *);
  279 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\004\000\004\000\004\000\002\000\006\000\006\000\
\006\000\006\000\006\000\006\000\005\000\005\000\005\000\005\000\
\005\000\007\000\007\000\007\000\007\000\010\000\010\000\011\000\
\009\000\009\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\003\000\004\000\007\000\
\008\000\003\000\006\000\007\000\002\000\003\000\004\000\003\000\
\003\000\001\000\001\000\005\000\001\000\001\000\003\000\003\000\
\001\000\003\000\001\000\001\000\001\000\001\000\005\000\005\000\
\005\000\005\000\005\000\005\000\004\000\005\000\005\000\004\000\
\004\000\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\045\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\028\000\
\029\000\030\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\019\000\018\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\000\000\000\000\014\000\000\000\000\000\
\016\000\017\000\005\000\004\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\000\000\000\
\000\000\000\000\044\000\041\000\024\000\040\000\026\000\000\000\
\000\000\006\000\023\000\000\000\000\000\000\000\000\000\031\000\
\033\000\032\000\034\000\035\000\038\000\039\000\036\000\000\000\
\011\000\020\000\000\000\008\000\042\000\012\000\009\000"

let yydgoto = "\002\000\
\004\000\064\000\005\000\015\000\016\000\017\000\069\000\084\000\
\057\000\070\000\058\000\085\000"

let yysindex = "\002\000\
\000\255\000\000\069\255\000\000\000\000\244\254\250\254\078\255\
\245\254\253\254\248\254\078\255\078\255\249\254\011\255\246\254\
\251\254\026\255\254\254\026\255\110\255\005\255\000\000\000\000\
\000\000\000\000\000\000\026\255\012\255\042\255\078\255\043\255\
\043\255\078\255\000\000\069\255\069\255\026\255\000\000\000\000\
\000\000\078\255\026\255\045\255\078\255\078\255\078\255\078\255\
\078\255\078\255\078\255\078\255\078\255\078\255\078\255\049\255\
\046\255\053\255\000\000\050\255\005\255\000\000\069\255\043\255\
\000\000\000\000\000\000\000\000\055\255\056\255\000\000\051\255\
\005\255\078\255\078\255\078\255\078\255\078\255\078\255\054\255\
\078\255\078\255\078\255\078\255\057\255\026\255\078\255\005\255\
\005\255\064\255\079\255\000\000\026\255\026\255\005\255\080\255\
\078\255\058\255\059\255\071\255\084\255\085\255\000\000\088\255\
\089\255\091\255\000\000\000\000\000\000\000\000\000\000\092\255\
\043\255\000\000\000\000\093\255\096\255\078\255\099\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\255\
\000\000\000\000\078\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\103\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\104\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\083\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\107\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\224\255\000\000\244\255\000\000\000\000\246\255\248\255\
\197\255\021\000\000\000\233\255"

let yytablesize = 141
let yytable = "\027\000\
\065\000\090\000\001\000\032\000\033\000\019\000\003\000\042\000\
\029\000\044\000\018\000\028\000\055\000\096\000\031\000\034\000\
\020\000\059\000\035\000\030\000\043\000\036\000\062\000\067\000\
\068\000\066\000\037\000\056\000\111\000\112\000\038\000\092\000\
\072\000\071\000\060\000\117\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\039\000\040\000\
\061\000\063\000\091\000\073\000\086\000\087\000\088\000\093\000\
\089\000\095\000\094\000\103\000\107\000\041\000\108\000\120\000\
\121\000\097\000\098\000\099\000\100\000\101\000\102\000\113\000\
\104\000\105\000\106\000\109\000\122\000\006\000\110\000\007\000\
\129\000\008\000\021\000\116\000\022\000\022\000\114\000\118\000\
\119\000\123\000\124\000\023\000\024\000\125\000\126\000\134\000\
\127\000\025\000\130\000\128\000\026\000\009\000\010\000\131\000\
\133\000\011\000\012\000\013\000\014\000\132\000\003\000\025\000\
\043\000\115\000\021\000\000\000\022\000\000\000\000\000\045\000\
\000\000\000\000\135\000\023\000\024\000\046\000\047\000\048\000\
\049\000\025\000\000\000\000\000\026\000\000\000\000\000\000\000\
\050\000\051\000\052\000\053\000\054\000"

let yycheck = "\008\000\
\033\000\061\000\001\000\012\000\013\000\012\001\007\001\018\000\
\012\001\020\000\023\001\023\001\021\000\073\000\023\001\023\001\
\023\001\028\000\008\001\023\001\023\001\032\001\031\000\036\000\
\037\000\034\000\032\001\023\001\088\000\089\000\005\001\064\000\
\043\000\042\000\023\001\095\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\021\001\022\001\
\007\001\007\001\063\000\007\001\004\001\008\001\002\001\001\001\
\007\001\007\001\003\001\006\001\084\000\036\001\006\001\006\001\
\006\001\074\000\075\000\076\000\077\000\078\000\079\000\008\001\
\081\000\082\000\083\000\086\000\006\001\009\001\087\000\011\001\
\113\000\013\001\005\001\094\000\007\001\003\001\008\001\008\001\
\097\000\006\001\006\001\014\001\015\001\006\001\006\001\128\000\
\006\001\020\001\006\001\008\001\023\001\033\001\034\001\008\001\
\006\001\037\001\038\001\039\001\040\001\118\000\008\001\008\001\
\006\001\093\000\005\001\255\255\007\001\255\255\255\255\010\001\
\255\255\255\255\131\000\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\023\001\255\255\255\255\255\255\
\027\001\028\001\029\001\030\001\031\001"

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
  VOID\000\
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
# 49 "parser.mly"
        ( ASTprog(_1) )
# 272 "parser.ml"
               : Ast.ansyn))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 51 "parser.mly"
                      ( ASTcmds(_2) )
# 279 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 54 "parser.mly"
      ( ASTstat(_1) )
# 286 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 55 "parser.mly"
               ( ASTdeccmd(_1,_3) )
# 294 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 56 "parser.mly"
                ( ASTstatcmd(_1,_3) )
# 302 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 60 "parser.mly"
                ( ASTblock(_2))
# 309 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                      ( ASTconst(_2, _3, _4) )
# 318 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                     (ASTfun(_2, _3, _5, _7) )
# 328 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                         (ASTrfun (_3, _4, _6, _8) )
# 338 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 68 "parser.mly"
                 (ASTvar(_2, _3)  )
# 346 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 69 "parser.mly"
                                   ( ASTproc(_2, _4, _6))
# 355 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 70 "parser.mly"
                                       ( ASTprocrec(_3, _5, _7))
# 364 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
           ( ASTecho(_2) )
# 371 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                  ( ASTset(_2, _3) )
# 379 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 77 "parser.mly"
                            ( ASTifblock(_2, _3, _4) )
# 388 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 78 "parser.mly"
                    ( ASTwhile(_2, _3) )
# 396 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                   ( ASTcall(_2, _3) )
# 404 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
     ( Int )
# 410 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
        ( Bool )
# 416 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 85 "parser.mly"
                             ( Fun(_2, _4) )
# 424 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
         ( Void )
# 430 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 91 "parser.mly"
     ( Typ(_1) )
# 437 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 92 "parser.mly"
                      ( Couple(_1,_3) )
# 445 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 95 "parser.mly"
                 ( ASTarg(_1, _3) )
# 453 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 98 "parser.mly"
     ( Arg(_1) )
# 460 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 99 "parser.mly"
                  ( ASTargs(_1,_3) )
# 468 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
      ( ASTtrue )
# 474 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
         ( ASTfalse )
# 480 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 104 "parser.mly"
        ( ASTnum(_1) )
# 487 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
         ( ASTident(_1) )
# 494 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                            ( ASTprim(Ast.Add, _3, _4 ) )
# 502 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                            ( ASTprim(Ast.Sub, _3, _4 ) )
# 510 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                            ( ASTprim(Ast.Mul, _3, _4 ) )
# 518 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                            ( ASTprim(Ast.Div, _3, _4 ) )
# 526 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                           ( ASTprim(Ast.Eq, _3, _4 ) )
# 534 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                           ( ASTprim(Ast.Lt, _3, _4 ) )
# 542 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                       ( ASTunaryPrim(Ast.Not, _3 ) )
# 549 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                            ( ASTprim(Ast.And, _3, _4 ) )
# 557 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                           ( ASTprim(Ast.Or, _3, _4 ) )
# 565 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                       ( ASTlambda(_2, _4))
# 573 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 116 "parser.mly"
                        ( ASTapply(_2,_3) )
# 581 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                               ( ASTif(_3, _4, _5) )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
             (Expr(_1))
# 597 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 120 "parser.mly"
              ( ASTexprs(_1, _2) )
# 605 "parser.ml"
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
