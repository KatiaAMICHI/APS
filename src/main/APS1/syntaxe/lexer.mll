{
open Parser
exception Eof
}

(*Déclaration du dictionnaire (source -> terminal/token) *)
rule token = parse
        [' ' '\t' '\n' '\r'] { token lexbuf }
	| ';'	{ PC }
	| ':'   { COLON }
	| '*'	{ ASTERISK }
	| '('	{ LPAR }
	| ')'	{ RPAR }
	| '['	{ LCRO }
	| ']'	{ RCRO }
	| ','	{ COMMA }
	| "->"	{ ARROW }
	| "CONST" { CONST }
	| "if" { IF }
	| "FUN" { FUN }
	| "REC" { REC }
	| "ECHO" { ECHO }
	| "int" { INT }
	| "bool" { BOOL }
	| ('-'?)['0'-'9']+ as i { INTV(int_of_string(i)) }
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
  | "div" { DIV }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT(id) }
  | "VAR" { VAR }
  | "PROC" { PROC }
  | "PROCREC" { PROCREC }
  | "SET" { SET }
  | "IF" { IFBLOCK }
  | "WHILE" { WHILE }
  | "CALL" { CALL }
	| eof { raise Eof }
 	| _ as lxm {	Printf.eprintf "Unknown caracter '%c' : ignored\n" lxm;
			flush stderr;
			token lexbuf
			}
