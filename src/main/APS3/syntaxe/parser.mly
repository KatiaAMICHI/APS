%{

open Ast

%}

/* Déclaration des terminaux */
%token ASTERISK COMMA ARROW COLON
%token LPAR RPAR LCRO RCRO
%token CONST IF FUN REC ECHO
%token TRUE FALSE
%token ADD MUL SUB DIV
%token <int> NUM
%token BOOL INT
%token <string> IDENT
%token PLUS MINUS SLASH
%token EQ NOT AND OR LT
%token PC

/*APS1*/
%token VAR PROC SET IFBLOCK WHILE CALL VOID
/*APS1*/

/*APS2*/
%token LEN NTH ALLOC VEC
/*APS2*/

/*APS3*/
%token RETURN
/*APS3*/

/* Précédences (priorité + associativité) des terminaux */
%nonassoc EQ NOT LT
%left COMMA
%left ASTERISK
%nonassoc MUL DIV
%left ADD SUB
%left OR
%left AND
%nonassoc PC

/*Déclaration du non-terminal axiome (analyse syntaxique) et du type de son attribut */
%start ansyn

%type<Ast.ansyn> ansyn

/*APS1*/
%type <Ast.block> block
/*APS1*/

/*aps2*/
%type <Ast.lval> lval
/*aps2*/

/*aps3*/
%type <Ast.ret> ret
/*aps3*/

%%

/* Déclaration de la grammaire avec les actions sémantiques */

ansyn :
	| prog { ASTprog($1) };

prog : LCRO cmds RCRO { ASTcmds($2) };

cmds :
	stat { ASTstat($1) }
	| dec PC cmds { ASTdeccmd($1,$3) }
	| stat PC cmds { ASTstatcmd($1,$3) };
	/* APS3 */
	| ret { ASTcmdRet($1) };
	/* APS3 */


/* APS1 */
block :
	LCRO cmds RCRO { ASTblock($2)}
/* APS1 */

dec :
	CONST IDENT typ expr { ASTconst($2, $3, $4) }
	| FUN IDENT typ LCRO args RCRO expr {ASTfun($2, $3, $5, $7) }
	| FUN REC IDENT typ LCRO args RCRO expr {ASTrfun ($3, $4, $6, $8) };
	/* APS1 */
	| VAR IDENT typ { ASTvar($2, $3) }
	| PROC IDENT LCRO args RCRO block { ASTproc($2, $4, $6)}
	| PROC REC IDENT LCRO args RCRO block { ASTprocrec($3, $5, $7)}
	/* APS1 */
	/* APS3 */
	| FUN IDENT typ LCRO args RCRO block  {ASTfunRet($2,$3,$5,$7)}
	| FUN REC IDENT typ LCRO args RCRO block  {ASTfunrecRet($3,$4,$6,$8)}

	/* APS3 */

/*aps2*/
lval:
	IDENT { ASTlid(ASTident($1)) }
	| LPAR NTH lval expr RPAR { ASTlnth($3,$4) };
/*aps2*/

/*aps2*/
ret:
|RETURN expr {ASTreturn($2)};
/*aps2*/

stat :
	ECHO expr { ASTecho($2) };
	/* APS1 */ /* APS2 new SET*/
	| SET lval expr {ASTset($2,$3)}
	| IFBLOCK expr block block { ASTifblock($2, $3, $4) }
	| WHILE expr block { ASTwhile($2, $3) }
	| CALL expr exprs { ASTcall($2, $3) }
	/* APS1 */

typ :
	INT { Int }
	| BOOL { Bool }
	| LPAR types ARROW typ RPAR { Arrow($2, $4) };
	/* APS1 */
	| VOID  { Void }
	/* APS1 */
	/* APS2 */
	| LPAR VEC typ RPAR { ASTvectype($3) }
	/* APS2 */

types :
	typ { Typ($1) }
	| typ ASTERISK types { Couple($1,$3) };

arg :
	IDENT COLON typ { ASTarg($1, $3) };

args :
	arg { Arg($1) }
	| arg COMMA args { ASTargs($1,$3) };

expr :
	TRUE { ASTtrue }
	| FALSE { ASTfalse }
	| NUM { ASTnum($1) }
	| IDENT { ASTident($1) }
	| LPAR ADD expr expr RPAR  { ASTprim(Ast.Add, $3, $4 ) }
	| LPAR SUB expr expr RPAR  { ASTprim(Ast.Sub, $3, $4 ) }
	| LPAR MUL expr expr RPAR  { ASTprim(Ast.Mul, $3, $4 ) }
	| LPAR DIV expr expr RPAR  { ASTprim(Ast.Div, $3, $4 ) }
	| LPAR EQ expr expr RPAR  { ASTprim(Ast.Eq, $3, $4 ) }
	| LPAR LT expr expr RPAR  { ASTprim(Ast.Lt, $3, $4 ) }
	| LPAR NOT expr RPAR  { ASTunaryPrim(Ast.Not, $3 ) }
	| LPAR AND expr expr RPAR  { ASTprim(Ast.And, $3, $4 ) }
	| LPAR OR expr expr RPAR  { ASTprim(Ast.Or, $3, $4 ) }
	| LCRO args RCRO expr { ASTlambda($2, $4)}
	| LPAR expr exprs RPAR { ASTapply($2,$3) }
	| LPAR IF expr expr expr RPAR { ASTif($3, $4, $5) }
	/* APS2 */
	| LPAR NTH expr expr RPAR { ASTenth($3,$4) }
	| LPAR ALLOC expr RPAR { ASTalloc($3) }
	| LPAR LEN expr RPAR { ASTlen($3) };
	/* APS2 */

exprs :
	expr { Expr($1) }
	| expr exprs { ASTexprs($1, $2) };
