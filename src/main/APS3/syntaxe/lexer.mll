{
open Parser
exception Eof
}

(*Déclaration du dictionnaire (source -> terminal/token) *)
rule token = parse
  (*Séparateurs*)
  [' ' '\t' '\n' '\r'] { token lexbuf }
  (*constantes numériques*)
  | ('-'?)['0'-'9']+ as i { NUM(int_of_string(i)) }
  (*mots-clefs*)
  | "CONST" { CONST }
	| "if" { IF }
	| "FUN" { FUN }
	| "REC" { REC }
	| "ECHO" { ECHO }
	| "int" { INT }
	| "bool" { BOOL }
	| "true" { TRUE }
	| "false" { FALSE }
	| "not"	{ NOT }
	| "and" { AND }
	| "or"	{ OR }
	| "eq"	{ EQ }
	| "lt"	{ LT }
	| "add"	{ ADD }
	| "mul"	{ MUL }
	| "sub" { SUB }
  | "div" { DIV }
  (*aps1*)
  | "VAR" { VAR }
  | "SET" { SET }
  | "PROC" { PROC }
  | "IF" { IFBLOCK }
  | "WHILE" { WHILE }
  | "CALL" { CALL }
  | "VOID" {VOID}
  (*aps1*)
  (*aps2*)
  | "len"          { LEN }
  | "nth"		       { NTH }
  | "alloc" 		   { ALLOC }
  | "vec" 		       { VEC }
  (*aps2*)
  (*Symboles réservés*)
  | ';'	{ PC }
	| ':'   { COLON }
	| '*'	{ ASTERISK }
	| '('	{ LPAR }
	| ')'	{ RPAR }
	| '['	{ LCRO }
	| ']'	{ RCRO }
	| ','	{ COMMA }
	| "->"	{ ARROW }
	(*identificateurs*)
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT(id) }
	| eof { raise Eof }
 	| _ as lxm {	Printf.eprintf "Unknown caracter '%c' : ignored\n" lxm;
			flush stderr;
			token lexbuf
			}
