type op = Add | Mul | Sub | Div | Eq | Lt | And | Or | Not

let string_of_op op =
	match op with
		  Add -> "add"
		| Mul -> "mul"
		| Sub -> "sub"
		| Div -> "div"
		| Eq -> "eq"
		| Lt -> "lt"
		| And -> "and"
		| Or -> "or"
		| Not -> "not"
let op_of_string op =
	match op with
		  "add" -> Add
		| "mul" -> Mul
		| "sub" -> Sub
		| "div" -> Div
		| "eq" -> Eq
		| "lt" -> Lt
		| "and" -> And
		| "or" -> Or
		| "not" -> Not
		| _ -> failwith "unknown op"

type typ =
	 Int
	| Bool
	(*aps1*)
	| Arrow of types * typ
	| Void
	(*aps1*)

and types =
	Typ of typ
	| Couple of typ * types

type arg =
	ASTarg of string * typ

type args =
	Arg of arg
	 | ASTargs of arg * args

type expr =
	ASTtrue
	| ASTfalse
	| ASTnum of int
	| ASTident of string
	| ASTprim of op * expr * expr
	| ASTunaryPrim of op * expr
	| ASTif of expr * expr * expr
	| ASTlambda of args * expr
	| ASTapply of expr * exprs

and exprs =
	Expr of expr
	| ASTexprs of expr * exprs

type dec =
	ASTconst of string * typ * expr
	| ASTfun of string * typ * args * expr
  | ASTrfun of string * typ * args * expr
	(* APS1 *)
	| ASTvar of string * typ
	| ASTproc of string * args * block
	| ASTprocrec of string * args * block
	(* APS1 *)

and stat =
	ASTecho of expr
	(* APS1 *)
	| ASTset of string * expr
	| ASTifblock of expr * block * block
	| ASTwhile of expr * block
	| ASTcall of string * exprs
	(* APS1 *)

and cmds =
	ASTstat of stat
	| ASTdeccmd of dec * cmds
	| ASTstatcmd of stat * cmds

(* APS1 *)
and block =
	ASTblock of cmds
(* APS1 *)

type prog =
	ASTcmds of cmds

type ansyn =
	| ASTprog of prog
