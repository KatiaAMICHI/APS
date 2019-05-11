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
	(*aps2*)
	| InB of value * int
	(*aps2*)

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

(*aps2*)
let allocn mem n =
	let capture_cpt = !cpt in
		let rec allocn_b memory nb =
			if nb>0 then(let new_mem = (!cpt,ref (InN(-1)))::memory in
										(cpt:= !cpt+1;
											allocn_b new_mem (nb-1)))
			else((capture_cpt,memory))
			in (allocn_b mem n)
(*aps2*)

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
		| ((InN n1,_), (InN n2,_)) -> InN(if n1 = 0 then 0 else n2)
		| _ -> failwith "it not And op")
	| Or -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| ((InN n1,_), (InN n2,_)) -> InN(if n1 = 1 then 1 else n2)
		| _ -> failwith "it not And op")
	| Eq -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| ((InN n1,_), (InN n2,_)) -> InN (int_of_bool(n1 == n2))
		| _ -> failwith "it not Eq op")
	| Lt -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| ((InN n1,_), (InN n2,_)) -> InN (int_of_bool(n1 < n2))
		| _ -> failwith "it not Lt op")
	| Add -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| ((InN n1,_), (InN n2,_)) -> InN(n1 + n2)
		| _ -> failwith "it not Add op")
	| Mul -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| ((InN n1,_), (InN n2,_)) -> InN (n1 * n2)
		| _ -> failwith "it not Mul op")
	| Sub -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| ((InN n1,_), (InN n2,_)) -> print_newline(); InN (n1 - n2)
		| _ -> failwith "it not Sub op")
	| Div -> (match (eval_expr env mem e1), (eval_expr env mem e2) with
		| ((InN n1,_), (InN n2,_)) -> InN (n1 / n2)
		| _ -> failwith "it not Div op")

and eval_oprim_una env mem op e =
	match op with
	Not -> (
		match (eval_expr env mem e) with
		(InN n1,_) -> match n1 with
		| 0 -> InN(1)
		| 1 -> InN(0)
		| _ -> failwith "it not Not op_una")

and eval_expr env mem ast =
	match ast with
		ASTtrue -> (InN(1), mem) (*aps2*)
	| ASTfalse -> (InN(0), mem) (*aps2*)
	| ASTnum(n) -> (InN(n), mem) (*aps2*)
	| ASTident(x) -> (match (List.assoc x env) with
									  | InA(a) -> (!(List.assoc a mem), mem)
										| InB(a,n) -> (InB(a,n), mem)
									  | v -> (v,mem))
	| ASTprim(op,e1,e2) -> ((eval_oprim_bin env mem op e1 e2), mem) (*aps2*)
	| ASTunaryPrim(op,e) -> ((eval_oprim_una env mem op e), mem) (*aps2*)
	| ASTif(e,e1,e2) -> let (v, o) = eval_expr env mem e in (*aps2*)
													if v = InN(1)
													then (eval_expr env o e1)
													else (eval_expr env o e2)
	| ASTlambda(args,expr) -> (InF(expr, (parse_args args), env), mem) (*aps2*)
	| ASTapply(expr,args) -> let (fer, o) = eval_expr env mem expr in (*aps2*)
													 let args_o_list = eval_args env mem args in
													 let (v_list, o_list) =  List.split args_o_list in
													 let o_end = List.nth o_list ((List.length o_list)-1) in
		(match fer with
		InF(body,params, envf) ->	let env_bis = ((List.map2 (fun x y -> (x,y)) params v_list)@envf) in
																eval_expr env_bis	o_end body
		| InFR(f, InF(body,params, envf)) -> let env_bis = ((f,List.assoc f env)::(List.map2 (fun x y -> (x,y)) params v_list)@envf) in
																					eval_expr env_bis	o_end body
		| _ -> failwith "[eval_expr] ASTapply fail")
	(* aps2 *)
	| ASTlen(e) -> let (inb, o) = eval_expr env mem e in
									(match inb with
									| InB(a,n)-> (InN(n), o)
									| _ -> failwith "[ASTlen] fail - not a InB")
	| ASTalloc(e) -> let (n,o) = eval_expr env mem e in
										let (a, oo) = (allocn o (get_int n)) in
											(InB(InA(a), (get_int n)), oo)
	| ASTenth(e1, e2) -> let (inb, o) = eval_expr env mem e1 in
												let (inn, oo) = eval_expr env mem e2 in
														(match inb with
														| InB(a,n) -> (!(List.assoc ((get_int a + (get_int inn))) oo), oo)
														| _ -> failwith "[ASTenth] fail - not a InB")
	(* aps2 *)


and print n =
	match n with
	 InN(n) -> (string_of_int n)
	| _ -> failwith "[print] fail"

and eval_dec env mem ast =
	match ast with
	(* ajouter la const dans notre env <=> [prend] (env [et] exp) [produit] v)*)
	|ASTconst(id,t,e) -> let v,o = eval_expr env mem e in ((id,v)::env,mem)
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
	ASTecho(x) -> let (res,o) = eval_expr env mem x in r:=!r^(get_string res)^"\n";(o,r)

	(* APS1 *)
	|ASTset(id,e) -> let v, o = eval_expr env mem e in
													let a, oo =  eval_lval env o id  in
													let value = List.assoc a oo in
														value:=v;
														(oo,r)
	| ASTifblock(e, bk1, bk2) -> let eval_e, o =  eval_expr env mem e in
																if eval_e = InN(1)
																then (eval_block env mem r bk1)
																else (eval_block env mem r bk2)
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

(* APS2 *)
and eval_lval lv env memoire =
	match lv  with
	ASTlid(a) -> let inb,new_mem = eval_expr env mem a in
						(match inb with
						InA(a) -> (a,new_mem)
						| InB(a,n) -> ((get_int a),new_mem)
						| _ -> failwith "[ASTlid] fail")
	| ASTlnth(lval, e) -> let a,o = eval_lval env mem lval in
													let i,oo = eval_expr env o e in
														(a+(get_int i),oo)

(* APS2 *)
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
