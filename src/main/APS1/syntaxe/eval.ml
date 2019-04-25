open Ast

type value_type = (string * value) list
and value =
	InN of int
	| InF of expr * string list * (string * value) list (*(expr, args, ... )*)
	| InFR of string * value (*(ident(f),InF())*)
	(*aps1*)
	| InA of int
	| InP of block * string list * (string * value) list
	| InPR of string * value
	(*aps1*)

(*aps1*)
let cpt = ref 0
let alloc mem =
	let res = (!cpt,(!cpt,ref (InN(-1)))::mem) in
		cpt:=(!cpt+1);
		res

let get_string v =
	match v with
	| InN(e) -> string_of_int e
	|_ -> failwith "[get_string] fail"
(*aps1*)

let int_of_bool x =
	match x with
	| true -> 1
	| false -> 0

let bool_of_int x =
	match x with
	|InN(0) -> false
	|_ -> true

let get_int v =
	match v with
	| InN(e) -> e
	| _ -> failwith "pas encore fait aussi"

let rec parse_arg arg=
	match arg with
	ASTarg(a,_)-> a::[]

and parse_args args =
	match args with
	Arg(arg) -> parse_arg arg
	| ASTargs(arg, args) -> (parse_arg arg)@(parse_args args)

let rec eval_args env mem args =
	match args with
		|Expr(a) ->  (eval_expr env mem a)::[]
		|ASTexprs(a,abis) -> (eval_expr env mem a)::(eval_args env mem abis)

and eval_oprim_bin env mem op e1 e2 =
	match op with
	And -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| (InN b1, InN b2) -> InN(b1 * b2)
		| _ -> failwith "it not And op")
	| Or -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| (InN b1, InN b2) -> InN(if b1 = 1 then 1 else b2)
		| _ -> failwith "it not And op")
	| Eq -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| (InN n1, InN n2) -> InN (int_of_bool(n1 == n2))
		| _ -> failwith "it not Eq op")
	| Lt -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| (InN n1, InN n2) -> InN (int_of_bool(n1 < n2))
		| _ -> failwith "it not Lt op")
	| Add -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| (InN n1, InN n2) -> InN(n1 + n2)
		| _ -> failwith "it not Add op")
	| Mul -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| (InN n1, InN n2) -> InN (n1 * n2)
		| _ -> failwith "it not Mul op")
	| Sub -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| (InN n1, InN n2) -> print_newline(); InN (n1 - n2)
		| _ -> failwith "it not Sub op")
	| Div -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| (InN n1, InN n2) -> InN (n1 / n2)
		| _ -> failwith "it not Div op")

and eval_oprim_una env mem op e =
	match op with
	Not -> (
		match (eval_expr env mem e) with
		InN(b1) ->  InN(1 - b1)
		| _ -> failwith "it not Not op_una")

and eval_expr env mem ast =
	match ast with
		ASTtrue -> InN(1)
	| ASTfalse -> InN(0)
	| ASTnum(n) -> InN(n)
	| ASTident(x) -> (match (List.assoc x env) with
									  |InA(a) -> !(List.assoc a mem)
									  |v -> v)
	| ASTprim(op,e1,e2) -> (eval_oprim_bin env mem op e1 e2)
	| ASTunaryPrim(op,e) -> (eval_oprim_una env mem op e)
	| ASTif(cond,th,el) -> (if(bool_of_int (eval_expr env mem cond))
													then eval_expr env mem th
													else eval_expr env mem el)
	| ASTlambda(args,expr) -> (InF(expr, (parse_args args), env))
	| ASTapply(expr,args) -> let args_list = eval_args env mem args in
		(match eval_expr env mem expr with
		InF(body,params, envf) ->
			let ebis = ((List.map2 (fun x y -> (x,y)) params args_list)@envf) in (print_string "return fun : ";print_int (get_int (eval_expr ebis	mem body)); print_newline());
			let env_bis = ((List.map2 (fun x y -> (x,y)) params args_list)@envf) in
						eval_expr env_bis	mem body
		| InFR(f, InF(body,params, envf)) ->
							let env_bis = ((f,List.assoc f env)::(List.map2 (fun x y -> (x,y)) params args_list)@envf) in
							eval_expr env_bis	mem body
		| _ -> failwith "[eval_expr] ASTapply fail")

and print n =
	match n with
	 InN(n) -> (string_of_int n)
	| _ -> failwith "[print] fail"

and eval_dec env mem ast =
	match ast with
	(* ajouter la const dans notre env <=> [prend] (env [et] exp) [produit] v)*)
	|ASTconst(id,t,e) -> let v = eval_expr env mem e in ((id,v)::env,mem)
	|ASTfun(id,t,args,e) -> ((id,InF(e,parse_args args,env))::env,mem)
	|ASTrfun(id,t,args,e) -> let params = parse_args args in
								 ((id,InFR(id,InF(e,params,env)))::env,mem)
	(* APS1 *)
	| ASTvar(id, t) -> let (a,new_mem) = alloc(mem) in ((id,InA(a))::env,new_mem)
	| ASTproc(id,args,bk) -> ((id,InP(bk,parse_args args,env))::env,mem)
	| ASTprocrec(id,args,b) -> ((id,InPR(id,InP(b,parse_args args,env)))::env,mem)

	(* APS1 *)

(* r : return *)
and eval_stat env mem r ast =
	match ast with
	ASTecho(x) -> let res = eval_expr env mem x in r:=!r^(get_string res)^"\n";(mem,r)

	(* APS1 *)
	|ASTset(id,e) ->(match List.assoc id env with
									InA(a)-> let v = (List.assoc a mem)
													 and affect = eval_expr env mem e in
														 v:= affect;
														 (mem,r)
									|_ -> failwith "Error SET : not a InA")
	| ASTifblock(e, bk1, bk2) -> (print_string "eval_ifblock \n";print_int (get_int (eval_expr env mem e)); print_newline()	;
																if (eval_expr env mem e) = InN(1)
																then (eval_block env mem r bk1)
																else (eval_block env mem r bk2))
	| ASTwhile (e,bk) ->	if (eval_expr env mem e) = InN(0)
												then (mem,r)
												else let (new_mem,new_r) = (eval_block env mem r bk) in
																										eval_stat env new_mem new_r ast
	|ASTcall(p,args) -> let eval_p = eval_expr env mem p
										and args_list = eval_args env mem args in
											(match eval_p with
												|InP(block,params,env1) ->
													 let closure_env = (List.map2 (fun x y -> (x,y)) params args_list)@env1 in
														eval_block closure_env mem r block
												|InPR(p,InP(block,params,env1)) ->
													(print_string "eval_call_rec \n";
													 let closure_env = (p,List.assoc p env)::(List.map2 (fun x y -> (x,y)) params args_list)@env1 in
												 		eval_block closure_env mem r block)
												|_ -> failwith "erreur : impossible d'appliquer une valeur entière")

	(* APS1 *)


and eval_cmds env mem r ast =
	match ast with
	ASTstat(x) -> eval_stat env mem r x
	| ASTdeccmd(x,cms) -> let (new_env, new_mem) = eval_dec env mem x in
														eval_cmds new_env new_mem r cms
	| ASTstatcmd(x,cms) -> let (new_mem,new_r) = eval_stat env mem r x in
														eval_cmds env new_mem new_r cms


(* APS1 *)
and eval_block env mem r ast =
	match ast with
	| ASTblock(cmds) -> (print_string "eval_block \n"; eval_cmds env mem r cmds)
(* APS1 *)

let eval_prog ast =
	match ast with
	|ASTcmds(cmds) -> let sortie = ref "Retour :\n"
					  			  and env = []
										and mem = [] in
					  				let (memory,sortie) = eval_cmds env mem sortie cmds in !sortie


let eval_ansyn prog =
	match prog with
	ASTprog(prog) ->  eval_prog prog

let _ =
	let fic = open_in Sys.argv.(1) in
	let lexbuf = Lexing.from_channel fic in
	let ast = Parser.ansyn Lexer.token lexbuf in
	let result  = eval_ansyn ast in
	print_string result;
	print_newline();;
