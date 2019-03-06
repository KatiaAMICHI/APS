%{

open Ast

%}

/* Déclaration des terminaux */

%token ASTERISK COMMA ARROW COLON
%token LPAR RPAR LCRO RCRO
%token CONST IF FUN REC ECHO
%token TRUE FALSE 
%token ADD MUL SUB DIV 
%token <int> INTV
%token BOOL INT
%token <string> IDENT
%token PLUS MINUS SLASH
%token EQ NOT AND OR LT
%token PC



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


%%

/* Déclaration de la grammaire avec les actions sémantiques */

ansyn : 
	| prog { ASTprog($1) };

prog : LCRO cmds RCRO { ASTcmds($2) }
;

cmds :
	stat { ASTstat($1) }
	| dec PC cmds { ASTdeccmd($1,$3) }
	| stat PC cmds { ASTstatcmd($1,$3) }
;

stat : 
	ECHO expr { ASTecho($2) }
;

dec : 
	CONST IDENT typ expr { ASTconst($2, $3, $4) }
	| FUN IDENT typ LCRO args RCRO expr {ASTfun($2, $3, $5, $7) }
	| FUN REC IDENT typ LCRO args RCRO expr {ASTrfun ($3, $4, $6, $8) }
;

typ : 
	INT { Int }
	| BOOL { Bool }
	| LPAR types ARROW typ RPAR { Fun($2, $4) }
;

types : 
	typ { Typ($1) }
	| typ ASTERISK types { Couple($1,$3) }
;

arg :
	IDENT COLON typ { ASTarg($1, $3) }
;

args :
	arg { Arg($1) }
	| arg COMMA args { ASTargs($1,$3) }
;

expr :
	TRUE { ASTtrue }
	| FALSE { ASTfalse }
	| INTV { ASTintv($1) }
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
	| LCRO args RCRO expr { ASTafun($2, $4)}
	| LPAR expr exprs RPAR { ASTexprl($2,$3) }
	| LPAR IF expr expr expr RPAR { ASTif($3, $4, $5) }

;
exprs : expr {Expr($1)}
	| expr exprs { ASTexprs($1, $2) }
;
