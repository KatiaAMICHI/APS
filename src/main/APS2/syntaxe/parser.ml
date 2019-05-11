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
  | NUM of (int)
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
  | SET
  | IFBLOCK
  | WHILE
  | CALL
  | VOID
  | LEN
  | NTH
  | ALLOC
  | VEC

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Ast

# 53 "parser.ml"
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
  291 (* SET *);
  292 (* IFBLOCK *);
  293 (* WHILE *);
  294 (* CALL *);
  295 (* VOID *);
  296 (* LEN *);
  297 (* NTH *);
  298 (* ALLOC *);
  299 (* VEC *);
    0|]

let yytransl_block = [|
  276 (* NUM *);
  279 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\005\000\005\000\005\000\002\000\007\000\007\000\
\007\000\007\000\007\000\007\000\003\000\003\000\006\000\006\000\
\006\000\006\000\006\000\008\000\008\000\008\000\008\000\008\000\
\012\000\012\000\013\000\010\000\010\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\011\000\011\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\003\000\004\000\007\000\
\008\000\003\000\006\000\007\000\001\000\005\000\002\000\003\000\
\004\000\003\000\003\000\001\000\001\000\005\000\001\000\004\000\
\001\000\003\000\003\000\001\000\003\000\001\000\001\000\001\000\
\001\000\005\000\005\000\005\000\005\000\005\000\005\000\004\000\
\005\000\005\000\004\000\004\000\006\000\005\000\004\000\004\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\051\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000\031\000\
\032\000\033\000\015\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\021\000\020\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\000\000\000\000\000\000\016\000\000\000\000\000\018\000\000\000\
\019\000\005\000\004\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\048\000\
\000\000\047\000\044\000\027\000\043\000\029\000\000\000\000\000\
\000\000\006\000\024\000\026\000\000\000\000\000\000\000\000\000\
\034\000\036\000\035\000\037\000\038\000\041\000\042\000\039\000\
\046\000\000\000\011\000\014\000\022\000\000\000\008\000\045\000\
\012\000\009\000"

let yydgoto = "\002\000\
\004\000\070\000\033\000\005\000\015\000\016\000\017\000\077\000\
\072\000\062\000\073\000\078\000\063\000"

let yysindex = "\011\000\
\007\255\000\000\112\255\000\000\000\000\249\254\253\254\121\255\
\250\254\003\255\006\255\121\255\121\255\121\255\016\255\252\254\
\254\254\093\255\008\255\093\255\089\255\009\255\000\000\000\000\
\000\000\000\000\000\000\093\255\010\255\027\255\013\255\000\000\
\121\255\050\255\050\255\121\255\000\000\112\255\112\255\048\255\
\000\000\000\000\000\000\121\255\093\255\051\255\121\255\121\255\
\121\255\121\255\121\255\121\255\121\255\121\255\121\255\121\255\
\121\255\121\255\121\255\121\255\055\255\052\255\059\255\000\000\
\056\255\009\255\006\255\000\000\112\255\050\255\000\000\121\255\
\000\000\000\000\000\000\093\255\061\255\064\255\000\000\057\255\
\009\255\121\255\121\255\121\255\121\255\121\255\121\255\062\255\
\121\255\121\255\121\255\065\255\121\255\066\255\067\255\093\255\
\121\255\009\255\009\255\072\255\121\255\076\255\000\000\000\000\
\082\255\093\255\093\255\009\255\084\255\121\255\094\255\095\255\
\104\255\105\255\116\255\000\000\118\255\127\255\128\255\000\000\
\131\255\000\000\000\000\000\000\000\000\000\000\130\255\050\255\
\133\255\000\000\000\000\000\000\134\255\135\255\121\255\145\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\050\255\000\000\000\000\000\000\121\255\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\144\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\146\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\251\254\
\000\000\000\000\000\000\000\000\087\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\223\255\086\000\000\000\239\255\000\000\000\000\246\255\
\248\255\213\255\203\255\049\000\000\000"

let yytablesize = 155
let yytable = "\027\000\
\049\000\071\000\049\000\034\000\035\000\036\000\095\000\044\000\
\019\000\046\000\031\000\001\000\060\000\003\000\029\000\018\000\
\028\000\064\000\104\000\020\000\074\000\075\000\100\000\037\000\
\068\000\030\000\049\000\038\000\032\000\039\000\045\000\061\000\
\065\000\066\000\080\000\079\000\103\000\109\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\102\000\040\000\067\000\126\000\127\000\
\069\000\081\000\096\000\097\000\098\000\106\000\099\000\108\000\
\134\000\105\000\107\000\116\000\041\000\042\000\120\000\122\000\
\123\000\110\000\111\000\112\000\113\000\114\000\115\000\128\000\
\117\000\118\000\119\000\130\000\121\000\124\000\043\000\131\000\
\125\000\025\000\076\000\135\000\129\000\021\000\147\000\022\000\
\133\000\040\000\047\000\137\000\138\000\136\000\023\000\024\000\
\048\000\049\000\050\000\051\000\025\000\139\000\140\000\026\000\
\153\000\041\000\042\000\052\000\053\000\054\000\055\000\056\000\
\006\000\141\000\007\000\142\000\008\000\021\000\151\000\022\000\
\057\000\058\000\059\000\043\000\143\000\144\000\023\000\024\000\
\145\000\146\000\148\000\149\000\025\000\154\000\150\000\026\000\
\009\000\010\000\011\000\012\000\013\000\014\000\152\000\003\000\
\101\000\028\000\132\000"

let yycheck = "\008\000\
\006\001\035\000\008\001\012\000\013\000\014\000\060\000\018\000\
\012\001\020\000\005\001\001\000\021\000\007\001\012\001\023\001\
\023\001\028\000\072\000\023\001\038\000\039\000\066\000\008\001\
\033\000\023\001\032\001\032\001\023\001\032\001\023\001\023\001\
\023\001\007\001\045\000\044\000\070\000\081\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\069\000\005\001\041\001\098\000\099\000\
\007\001\007\001\004\001\008\001\002\001\001\001\007\001\007\001\
\108\000\076\000\003\001\006\001\021\001\022\001\006\001\006\001\
\006\001\082\000\083\000\084\000\085\000\086\000\087\000\008\001\
\089\000\090\000\091\000\008\001\093\000\096\000\039\001\006\001\
\097\000\003\001\043\001\008\001\101\000\005\001\128\000\007\001\
\107\000\005\001\010\001\006\001\006\001\110\000\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\006\001\006\001\023\001\
\146\000\021\001\022\001\027\001\028\001\029\001\030\001\031\001\
\009\001\006\001\011\001\006\001\013\001\005\001\135\000\007\001\
\040\001\041\001\042\001\039\001\006\001\006\001\014\001\015\001\
\006\001\008\001\006\001\006\001\020\001\150\000\008\001\023\001\
\033\001\034\001\035\001\036\001\037\001\038\001\006\001\008\001\
\067\000\008\001\106\000"

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
  SET\000\
  IFBLOCK\000\
  WHILE\000\
  CALL\000\
  VOID\000\
  LEN\000\
  NTH\000\
  ALLOC\000\
  VEC\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 56 "parser.mly"
        ( ASTprog(_1) )
# 296 "parser.ml"
               : Ast.ansyn))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 58 "parser.mly"
                      ( ASTcmds(_2) )
# 303 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 61 "parser.mly"
      ( ASTstat(_1) )
# 310 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 62 "parser.mly"
               ( ASTdeccmd(_1,_3) )
# 318 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 63 "parser.mly"
                ( ASTstatcmd(_1,_3) )
# 326 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 67 "parser.mly"
                ( ASTblock(_2))
# 333 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                      ( ASTconst(_2, _3, _4) )
# 342 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                     (ASTfun(_2, _3, _5, _7) )
# 352 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                                         (ASTrfun (_3, _4, _6, _8) )
# 362 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 75 "parser.mly"
                 (ASTvar(_2, _3)  )
# 370 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 76 "parser.mly"
                                   ( ASTproc(_2, _4, _6))
# 379 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 77 "parser.mly"
                                       ( ASTprocrec(_3, _5, _7))
# 388 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
       ( ASTlid(ASTident(_1)) )
# 395 "parser.ml"
               : Ast.lval))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.lval) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                           ( ASTlnth(_3,_4) )
# 403 "parser.ml"
               : Ast.lval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
           ( ASTecho(_2) )
# 410 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.lval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                 (ASTset(_2,_3))
# 418 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 90 "parser.mly"
                            ( ASTifblock(_2, _3, _4) )
# 427 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 91 "parser.mly"
                    ( ASTwhile(_2, _3) )
# 435 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 92 "parser.mly"
                   ( ASTcall(_2, _3) )
# 443 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
     ( Int )
# 449 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
        ( Bool )
# 455 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 98 "parser.mly"
                             ( Arrow(_2, _4) )
# 463 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
         ( Void )
# 469 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 103 "parser.mly"
                     ( ASTvectype(_3) )
# 476 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 107 "parser.mly"
     ( Typ(_1) )
# 483 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 108 "parser.mly"
                      ( Couple(_1,_3) )
# 491 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 111 "parser.mly"
                 ( ASTarg(_1, _3) )
# 499 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 114 "parser.mly"
     ( Arg(_1) )
# 506 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 115 "parser.mly"
                  ( ASTargs(_1,_3) )
# 514 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
      ( ASTtrue )
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
         ( ASTfalse )
# 526 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "parser.mly"
       ( ASTnum(_1) )
# 533 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
         ( ASTident(_1) )
# 540 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                            ( ASTprim(Ast.Add, _3, _4 ) )
# 548 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                            ( ASTprim(Ast.Sub, _3, _4 ) )
# 556 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                            ( ASTprim(Ast.Mul, _3, _4 ) )
# 564 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                            ( ASTprim(Ast.Div, _3, _4 ) )
# 572 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                           ( ASTprim(Ast.Eq, _3, _4 ) )
# 580 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                           ( ASTprim(Ast.Lt, _3, _4 ) )
# 588 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                       ( ASTunaryPrim(Ast.Not, _3 ) )
# 595 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                            ( ASTprim(Ast.And, _3, _4 ) )
# 603 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                           ( ASTprim(Ast.Or, _3, _4 ) )
# 611 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                       ( ASTlambda(_2, _4))
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 132 "parser.mly"
                        ( ASTapply(_2,_3) )
# 627 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                               ( ASTif(_3, _4, _5) )
# 636 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                           ( ASTenth(_3,_4) )
# 644 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                        ( ASTalloc(_3) )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                      ( ASTlen(_3) )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
      ( Expr(_1) )
# 665 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 142 "parser.mly"
              ( ASTexprs(_1, _2) )
# 673 "parser.ml"
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
