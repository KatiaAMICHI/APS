open Ast

type value_type = (string * value) list
and value =
	InN of int
	| InF of expr * string list * (string * value) list (*(expr, args, ... )*)
	| InFR of string * value (*(ident(f),InF())*)

(*a vérifier !! InN(truc) = truc*)
let get_value v =
	match v with
	InN(x) -> string_of_int x
	| _ -> failwith "[getvalue] fail"

let int_of_bool x =
	match x with
	|true -> 1
	|false -> 0

let rec ident_exprs exprs =
	match exprs with
	| Expr (e) -> e::[]
	| ASTexprs (e,l)-> e::(ident_exprs l)

let bool_of_int x =
	match x with
	|InN(0) -> false
	|_ -> true

let rec parse_arg arg=
	match arg with
	ASTarg(a,_)-> a::[]
and parse_args args =
	match args with
	Arg(arg) -> parse_arg arg
	| ASTargs(arg, args) -> (parse_arg arg)@(parse_args args)

let rec eval_args env args =
	match args with
		|Expr(a) ->  (eval_expr env a)::[]
		|ASTexprs(a,abis) -> (eval_expr env a)::(eval_args env abis)
and eval_oprim_bin env op e1 e2 =
	match op with
	And -> (match (eval_expr env e1), (eval_expr env e2) with
		| (InN b1, InN b2) -> InN(b1 * b2)
		| _ -> failwith "it not And op")
	| Or -> (match (eval_expr env e1), (eval_expr env e2) with
		| (InN b1, InN b2) -> InN(b1 * b2)
		| _ -> failwith "it not And op")
	| Eq -> (match (eval_expr env e1), (eval_expr env e2) with
		| (InN n1, InN n2) -> InN (int_of_bool(n1 == n2))
		| _ -> failwith "it not Eq op")
	| Lt -> (match (eval_expr env e1), (eval_expr env e2) with
		| (InN n1, InN n2) -> InN (int_of_bool(n1 < n2))
		| _ -> failwith "it not Lt op")
	| Add -> (match (eval_expr env e1), (eval_expr env e2) with
		| (InN n1, InN n2) -> InN(n1 + n2)
		| _ -> failwith "it not Add op")
	| Mul -> (match (eval_expr env e1), (eval_expr env e2) with
		| (InN n1, InN n2) -> InN (n1 * n2)
		| _ -> failwith "it not Mul op")
	| Sub -> (match (eval_expr env e1), (eval_expr env e2) with
		| (InN n1, InN n2) -> InN (n1 - n2)
		| _ -> failwith "it not Sub op")
	| Div -> (match (eval_expr env e1), (eval_expr env e2) with
		| (InN n1, InN n2) -> InN (n1 / n2)
		| _ -> failwith "it not Div op")
	| _ -> failwith "None op"
and eval_oprim_una env op e =
	match op with
	Not -> (
		match (eval_expr env e) with
		InN b1 -> InN(1 - b1)
		| _ -> failwith "it not Not op")
	(*| True -> InN 1
	| False -> InN 0*)
	| _ -> failwith "None op"

and eval_expr env ast =
	match ast with
		ASTtrue -> InN(1)
	| ASTfalse -> InN(0)
	| ASTnum(n) -> InN(n)
	| ASTident(x) -> (List.assoc x env)
	| ASTprim(op,e1,e2) -> (eval_oprim_bin env op e1 e2)
	| ASTunaryPrim(op,e) -> (eval_oprim_una env op e)
	| ASTif(cond,th,el) -> (if(bool_of_int (eval_expr env cond)) then eval_expr env th else eval_expr env el)
	| ASTlambda(args,expr) -> (InF(expr, (parse_args args), env))
	| ASTapply(expr,args) -> let args_list = eval_args env args in
		(match eval_expr env expr with
		(*InF(body,params, envf) ->
						let env_bis = ref ((List.map2 (fun x y -> (x,y)) params args_list)@envf) in
						eval_expr env_bis	body*)
	| _ -> failwith "ASTapply fail")

let print n =
	match n with
	 InN(n) -> (string_of_int n)
	| _ -> failwith "[print] fail"

let eval_dec env de =
	match de with
	(* ajouter la const dans notre env <=> [prend] (env [et] exp) [produit] v) *)
	ASTconst(x,t,expr) -> (x, (eval_expr env expr))::env
	| ASTfun(x,t,args,expr) -> (x, (InF(expr,(parse_args args), env)))::env(* ajouter la fun dans notre env*)
  | ASTrfun(x,t,args,expr) -> (x, (InFR(x, InF(expr,(parse_args args), env))))::env
	| _ -> failwith "[eval_dec] fail"

let eval_stat env st r =
	match st with
	ASTecho(x) -> let res = eval_expr env x in r:=!r^(get_value res)^"\n"


let rec eval_cmds env cm r =
	match cm with
	ASTstat(x) -> eval_stat env x r
	|ASTdeccmd(x,cms) -> let new_env = eval_dec env x in eval_cmds new_env cms r
	| ASTstatcmd(x,cms) -> eval_stat env x r; eval_cmds env cms r

let eval_prog prog =
	match prog with
	ASTcmds(cmds) -> let return = ref "Return : \n" in
					  					let env_prog : (string * value) list = [] in
					  						eval_cmds env_prog cmds return;
												!return

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
