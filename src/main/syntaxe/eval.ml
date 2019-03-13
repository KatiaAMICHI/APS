open Ast

type env_type = (string * value_expr) list
and value_expr = 
	InN of int 
	| InF of expr * string list * env_type (*(expr, args, env_type)*)
	| InFR of string * value_expr (*(ident(f),InF())*)
	| Void
	
(*a vérifier !! InN(truc) = truc*)
let value v =
	match v with
	InN(x) -> string_of_int x
	| _ -> failwith "[value] fail"

let int_of_bool x =
	match x with
	|true -> 1
	|false -> 0
	
let bool_of_int x =
	match x with
	|InN(0) -> false
	|_ -> true
	
let parce_args args = 
	match args with
	Arg(args) -> (ASTarg(a,t)-> a::[])
	| ASTargs(arg, args) -> (ASTarg(a,_) -> a::(parce_args args))
	
let rec eval_oprim_bin env op e1 e2 = 
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
	Not -> (match (eval_expr env e1) with
					| InN b1 -> InN(1 - b1)
					| _ -> failwith "it not Not op")
	| ASTtrue -> InN 1
	| ASTfalse -> InN 0
	| _ -> failwith "None op"

and eval_expr env ast = 
	match ast with
		ASTtrue -> InN(1)
	| ASTfalse -> InN(0)
	| ASTnum(n) -> InN(n)
	(*| ASTident(x) -> x:env ?? *) (* à faire !!!!*)
	| ASTprim(op,e1,e2) -> (eval_oprim_bin env e1 e2)
	| ASTunaryPrim(op,e) -> (eval_oprim_una env e)
	| ASTif(cond,th,el) -> (if(bool_of_int (eval_expr env cond)) then eval_expr env th else eval_expr env els) 
	| ASTlambda(args,expr) -> (InF(expr, (parce_args args), env)) 
	| ASTapply(expr,exprs) ->
		(match eval_expr env expr with
		InF(expr,exprs,env) -> InN(0)
		| INFR(expr, InF(expr,exprs,env))-> InN(0)
		| _ -> failwith "ASTapply fail")

let print n =
	match n with
	 InN(n) -> (string_of_int n)
	| _ -> failwith "[print] fail"

let dec env de =
	match cm with
	ASTconst(x,_,expr) -> (eval_expr env expr)::env(* ajouter la const dans notre env <=> [prend] (env [et] exp) [produit] v) *)
	| ASTfun(x,_,args,expr) -> (x, (InF(expr,(parse_args args), env)))::env(* ajouter la fun dans notre env*) 
  | ASTrfun(x,_,args,expr) -> (x, (InFR(x, InF(expr,(parse_args args)))))::env

let eval_stat  env st r =
	match st with
	(*ASTecho(x) -> print_endline(print(eval_expr env x))*)
	ASTecho(x) -> let res = eval_expr env e in s:=!s^(value res)^"\n"

	
let rec eval_cmds env cm r =
	match cm with
	ASTstat(x) -> eval_stat env x r
	| ASTdeccmd(x,cms) -> eval_dec env x, eval_cmds env cms r
	| ASTstatcmd(x,cms) -> eval_stat env x r, eval_cmds env cms r

let eval_prog prog = 
	match prog with
	ASTProg(cmds) -> let return = ref "return :\n" 
					  and env : (string * valeur) list ref = ref [] in 
					  	eval_cmds env cmds return; !return
					  	
let _ =
try
let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
let e = Parser.prog Lexer.token lexbuf in
eval_prog e;
print_char '\n'
with Lexer.Eof -> exit 0
