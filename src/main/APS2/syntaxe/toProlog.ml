open Ast


let rec print_type t =
match t with
	Int -> Printf.printf"int"
	| Bool -> Printf.printf"bool"
	(*APS1*)
	| Void -> Printf.printf"void"
	(*APS1*)
	| Arrow(ts, t) -> (
		Printf.printf"arrow([";
		print_types ts;
		Printf.printf"],";
		print_type t;
		Printf.printf")")
	| ASTvectype(t) -> (
		Printf.printf "vec(";
		print_type t;
		Printf.printf ")"
		)


and print_types ts =
match ts with
	Typ(t) -> print_type t;
	| Couple (t, ts) -> (
		Printf.printf"types(";
		print_type t;
		Printf.printf",";
		print_types ts;
		Printf.printf")"
		)

and print_arg a =
match a with
	ASTarg(a,t) -> (
		Printf.printf"(%s," a;
		print_type t;
		Printf.printf")"
		)

and print_args a =
match a with
	Arg a -> print_arg a
	| ASTargs(a,args) -> (
		print_arg a;
		Printf.printf",";
		print_args args
		)

and print_expr e =
match e with
	ASTtrue -> Printf.printf"true"
	| ASTfalse -> Printf.printf"false"
	| ASTnum n -> Printf.printf"num(%d)" n
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
		Printf.printf"%s" (string_of_op_una op);
		Printf.printf"(";
		print_expr e1;
		Printf.printf")"
		)
	| ASTlambda(a,e) -> (
		Printf.printf"lambda([";
		print_args a;
		Printf.printf"],";
		print_expr e;
		Printf.printf")"
		)
	|ASTapply(e,params) -> (
		Printf.printf "apply(";
		print_expr e;
		Printf.printf ",[";
		print_prolog_params params;
		Printf.printf "])"
		)
	| ASTif(e1, e2, e3) -> (
		Printf.printf"if(";
		print_expr e1;
		Printf.printf",";
		print_expr e2;
		Printf.printf",";
		print_expr e3;
		Printf.printf")"
		)
	(* APS2 *)
	| ASTlen(e) -> (
		Printf.printf "len(";
		print_expr e;
		Printf.printf ")"
		)
	| ASTalloc(e) -> (
		Printf.printf "alloc(";
		print_expr e;
		Printf.printf ")"
		)
	| ASTenth(e1,e2)->  (
		Printf.printf "nth(";
		print_expr e1;
		Printf.printf ",";
		print_expr e2;
		Printf.printf ")"
		)
	(* APS2 *)

and print_exprs es =
match es with
	Expr(e) -> print_expr e
	| ASTexprs(e,es)-> (print_expr e; Printf.printf" "; print_exprs es)

and print_prolog_params params =
match params with
	Expr(e) ->  print_expr e;
	| ASTexprs(e,params) -> (
		print_expr e;
		Printf.printf ",";
		print_prolog_params params;
		)

(*aps1*)
and print_block block =
match block with
	| ASTblock(cmds) ->(
		Printf.printf"block([";
		print_cmd cmds;
		Printf.printf"])"
		)
(*aps1*)

(* APS2 *)
and print_lval lval =
 	match lval with
 	ASTlid(id) -> print_expr id
 	| ASTlnth(lval,e)->  (
		Printf.printf "nth(";
		print_lval lval;
		Printf.printf ",";
		print_expr e;
		Printf.printf ")"
		)
(* APS2 *)

and print_dec dec =
	match dec with
	 ASTconst(x,t,e)  -> (
			Printf.printf"const(%s,"x;
		 	print_type t;
		 	Printf.printf",";
		 	print_expr e;
		 	Printf.printf")"
			)
		| ASTfun(s,t,a,e) -> (
			Printf.printf"fun(%s," s;
			print_type t;
			Printf.printf",[";
			print_args a;
			Printf.printf"],";
			print_expr e;
			Printf.printf")"
			)
		| ASTrfun(s,t,a,e) -> (
			Printf.printf"funRec(%s," s;
			print_type t;
			Printf.printf",[";
			print_args a;
			Printf.printf"],";
			print_expr e;
			Printf.printf")"
			)
		(* APS1 *)
		| ASTvar(s,t) -> (
			Printf.printf"var(%s,"s;
			print_type t;
			Printf.printf")"
			)
		| ASTproc(s,a,b) -> (
			Printf.printf"proc(%s," s;
			Printf.printf"[";
			print_args a;
			Printf.printf"],";
			print_block b;
			Printf.printf")"
			)
		| ASTprocrec(s,a,b) -> (
			Printf.printf"procrec(%s," s;
			Printf.printf"[";
			print_args a;
			Printf.printf"],";
			print_block b;
			Printf.printf")"
			)
		(* APS1 *)

and print_stat stat =
	match stat with
	ASTecho e -> (
		Printf.printf"echo(";
		print_expr e;
		Printf.printf")")
		(* APS1 *) (* APS2 new set *)
	| ASTset(id,e)-> (
		 Printf.printf "set(";
		 print_lval id;
		 Printf.printf ",";
		 print_expr e;
		 Printf.printf ")"
		)
	| ASTifblock(e,b1,b2) -> (
		Printf.printf"ifblock(";
		print_expr e;
		Printf.printf",";
		print_block b1;
		Printf.printf",";
		print_block b2;
		Printf.printf")")
	| ASTwhile(e,b) -> (
		Printf.printf"while(";
		print_expr e;
		Printf.printf",";
		print_block b;
		Printf.printf")")
	| ASTcall(s,e) -> (
		Printf.printf "call(";
    print_expr s;
    Printf.printf ",[";
		print_prolog_params e;
		Printf.printf "])"
	  )
		(* APS1 *)

and print_cmd cmd =
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

let print_prog prog =
	match prog with
		ASTcmds cmds -> (Printf.printf "prog([";
											print_cmd cmds;
											Printf.printf"]).")

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
