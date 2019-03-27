open Ast

let rec print_type t =
match t with
	Int -> Printf.printf"int"
	| Bool -> Printf.printf"bool"
	| Fun(ts, t) -> (
		Printf.printf"arrow([";
		print_types ts;
		Printf.printf"],";
		print_type t;
		Printf.printf")")

and print_types ts =
match ts with
	Typ(t) -> print_type t;
	| Couple (t, ts) -> (
		Printf.printf"types(";
		print_type t;
		Printf.printf",";
		print_types ts;
		Printf.printf")")

let print_arg a =
match a with
	ASTarg(a,t) -> (
		Printf.printf"(%s," a;
		print_type t;
		Printf.printf")")

let rec print_args a =
match a with
	Arg a -> print_arg a
	| ASTargs(a,args) -> (
		print_arg a;
		Printf.printf",";
		print_args args)

let rec print_expr e =
match e with
	ASTtrue -> Printf.printf"true"
	| ASTfalse -> Printf.printf"false"
	| ASTnum n -> Printf.printf"int(%d)" n
	| ASTident x -> Printf.printf"ident(%s)" x
	| ASTprim(op, e1, e2) -> (
		Printf.printf"%s" (string_of_op op);
		Printf.printf"(";
		print_expr e1;
		Printf.printf",";
		print_expr e2;
		Printf.printf")"
		)
	| ASTunaryPrim (op, e1) -> (
		Printf.printf"%s" (string_of_op op);
		Printf.printf"(";
		print_expr e1;
		Printf.printf")"
		)
	| ASTlambda(a,e) -> (
		Printf.printf"lambda([";
		print_args a;
		Printf.printf"],";
		print_expr e;
		Printf.printf")")
	| ASTapply(e,es) -> (
		Printf.printf"apply(";
		print_expr e;
		Printf.printf",[";
		print_exprs es;
		Printf.printf"])")
	| ASTif(e1, e2, e3) -> (
		Printf.printf"if(";
		print_expr e1;
		Printf.printf",";
		print_expr e2;
		Printf.printf",";
		print_expr e3;
		Printf.printf")")

and print_exprs es =
match es with
	Expr(e) -> print_expr e
	| ASTexprs(e,es)-> (print_expr e; Printf.printf" "; print_exprs es)


let print_dec dec =
	match dec with
	 ASTconst(x,t,e)  -> (
			Printf.printf"const(%s,"x;
		 	print_type t;
		 	Printf.printf",";
		 	print_expr e;
		 	Printf.printf")")
		| ASTfun(s,t,a,e) -> (
			Printf.printf"fun(%s," s;
			print_type t;
			Printf.printf",[";
			print_args a;
			Printf.printf"],";
			print_expr e;
			Printf.printf")")
		| ASTrfun(s,t,a,e) -> (
			Printf.printf"funRec(%s," s;
			print_type t;
			Printf.printf",[";
			print_args a;
			Printf.printf"],";
			print_expr e;
			Printf.printf")")
		(* APS1 *)
		| ASTvar(s,t) ->
			Printf.printf"var(%s,"x;
			print_type t;
			Printf.printf")")
		| ASTproc of string * args * block
		| ASTprocrec of string * args * block
		(* APS1 *)

let print_stat stat =
	match stat with
	ASTecho e -> (
		Printf.printf"echo(";
		print_expr e;
		Printf.printf")")

let rec print_cmd cmd =
	match cmd with
		 ASTstat(stat) -> (
				Printf.printf "stat(";
				print_stat stat;
				Printf.printf "),epsilon")
		| ASTdeccmd (dec,cmd) -> (
				Printf.printf "dec(";
				print_dec dec;
				Printf.printf"),";
				print_cmd cmd)
		| ASTstatcmd (stat,cmds) ->  (
					Printf.printf "stat(";
					print_stat stat;
					Printf.printf"),";
					print_cmd cmds)


let print_cmds cmds =
	Printf.printf"[";
	print_cmd cmds;
	Printf.printf"]"

let print_prog prog =
	match prog with
		ASTcmds cmds -> (Printf.printf "prog(";
											print_cmds cmds;
											Printf.printf").")

let print_prolog ansync =
	match ansync with
		ASTprog prog -> print_prog prog

let _ =
try
	let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
	let e = Parser.ansyn Lexer.token lexbuf in
		print_prolog e;
		print_char '\n'
with Lexer.Eof -> exit 0
