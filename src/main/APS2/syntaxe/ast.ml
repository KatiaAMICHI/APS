
type op = Add | Mul | Sub | Div | Eq | Lt | And | Or
type op_una = Not

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
		| _ -> failwith "unknown op"

let string_of_op_una op_una =
	match op_una with
		| Not -> "not"
let op_una_of_string op_una =
	match op_una with
		| "not" -> Not
		| _ -> failwith "unknown op_una"

type typ =
	 Int
	| Bool
	(* APS1 *)
	| Arrow of types * typ
	| Void
	(* APS1 *)
	(* APS2 *)
	| ASTvectype of typ
	(* APS2 *)

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
	| ASTunaryPrim of op_una * expr
	| ASTif of expr * expr * expr
	| ASTlambda of args * expr
	| ASTapply of expr * exprs
	(* APS2 *)
	| ASTlen of expr
	| ASTalloc of expr
	| ASTenth of expr * expr
	(* APS2 *)

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
	(* APS1 *) (* APS2 new SET*)
	| ASTset of lval * expr
	| ASTifblock of expr * block * block
	| ASTwhile of expr * block
	| ASTcall of expr * exprs
	(* APS1 *)

and cmds =
	ASTstat of stat
	| ASTdeccmd of dec * cmds
	| ASTstatcmd of stat * cmds

(* APS2 *)
and lval =
		ASTlid of expr
	| ASTlnth of lval * expr
(* APS2 *)

(* APS1 *)
and block =
	ASTblock of cmds
(* APS1 *)

type prog =
	ASTcmds of cmds

type ansyn =
	| ASTprog of prog
