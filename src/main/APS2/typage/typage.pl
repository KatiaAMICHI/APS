assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

/*(PROG)*/
typeProg(C,prog(X),void) :- typeCmds(C,X,void).

/************APS1***********/
/*(BLOCK)*/
typeBlock(C, block(X), void) :- typeCmds(C,X,void).
/************APS1***********/

/*(END)*/
typeCmds(_,[epsilon],void).

/*(STAT)*/
typeCmds(C,[stat(X)|Y],void) :- typeStat(C,X,void),
																typeCmds(C,Y,void).

/*(DEC)*/
typeCmds(C,[dec(X)|Y],void) :-  typeDec(C,X,CBIS),
																typeCmds(CBIS,Y,void).

/*(ECHO)*/
typeStat(C,echo(X),void) :- typeExpr(C,X,int).

/************APS1***********/

/*(SET)*/
/* APS2 new SET */
typeStat(C,set(LV, E),void) :-
	typeExpr(C,LV,T),
	typeExpr(C,E,T).
/* APS2 */

/*(IFBLOCK)*/
typeStat(C,ifblock(COND,B1,B2),void) :-
	typeExpr(C,COND,bool),
	typeBlock(C,B1,void),
	typeBlock(C,B2,void).

/*(WHILE)*/
typeStat(C,while(E,BK),void) :-
	typeExpr(C,E,bool),
	typeBlock(C,BK,void).

/*(CALL)*/
typeStat(C,call(_,_),void) :-
	assoc(_,C,arrow(ARGSTYPE,void)),
	check_types(C,_,ARGSTYPE).

/*APS1*/

/*(CONST)*/
typeDec(C,const(X,TYPE,EXPR),[(X,TYPE)|C]) :-
	typeExpr(C,EXPR,TYPE).

/*APS1*/

/*(VAR)*/
typeDec(C,var(X,_),CN) :-
	CN=[(X,_)|C].

/*(PROC)*/
typeDec(C,proc(_,ARGS,BODY),CN) :-
	append(ARGS,C,CAR),
	typeBlock(CAR,BODY,void),
	get_types_args(ARGS,RES),
	CN=[(_,arrow(RES,void))|C].

/*(PROC REC)*/
typeDec(C,procrec(X,ARGS,BODY),_):-
	get_types_args(ARGS,RES),
	append(ARGS,C,CARGS),
	CT = [(X,arrow(RES,void))|CARGS],
	typeBlock(CT,BODY,void).

/*APS1*/

/*(FUN)*/
typeDec(C,fun(ID,TYPE,ARGS,BODY),[(ID,arrow(TYPESIN,TYPE))|C]):-
	append(C,ARGS,CARGS),
	typeExpr(CARGS,BODY,TYPE),
	get_types_args(ARGS,TYPESIN).

/*(FUN REC)*/
typeDec(C,funRec(ID,TYPE,ARGS,BODY), CBIS):-
	get_types_args(ARGS,TYPESARGS),
	append(C,ARGS,CARGS),
	CTMP = [(ID,arrow(TYPESARGS,TYPE))|CARGS],
	typeExpr(CTMP,BODY,TYPE),
	CBIS=[(ID,arrow(TYPESARGS,TYPE))|C].

/*renvoie la liste de type de tous les arguments dans ARGS
elle prend une liste d'argument et renvoie tous les type des arguments
elle mets tt les type de arg dans RES*/
get_types_args([],[]).
get_types_args([(_,T)|ARGS],[T|RES]) :-
	get_types_args(ARGS,RES).

/*(TRUE)*/
typeExpr(_,true,bool).

/*(FALSE)*/
typeExpr(_,false,bool).

/*(INT)*/
typeExpr(_,int(X),int) :- integer(X).

/*(IDENT)*/
typeExpr(C,ident(X),T) :- assoc(X,C,T).

/*(IF)*/
typeExpr(C,if(COND,E1,E2),T) :-	typeExpr(C,COND,bool),
				typeExpr(C,E1,T),
				typeExpr(C,E2,T).

/*(APP)*/
typeExpr(C,apply(ident(F),ARGS),TYPE) :-
	assoc(F,C,arrow(ARGSTYPE,TYPE)),
	check_types(C,ARGS,ARGSTYPE).

/*(APP lambda)*/
typeExpr(C,apply(X,VARGS),TYPE) :-
	typeExpr(C,X,arrow(TYPEARGS,TYPE)).
	typeExpr(C,VARGS,TYPEARGS).

/*(ABS)*/
typeExpr(C,lambda(ARGS,BODY),arrow(TYPEARGS,TYPEF)) :-
	get_types_args(ARGS,TYPEARGS),
	append(C,ARGS,CBIS),
	typeExpr(CBIS,BODY,TYPEF).


/* APS2 */

/* ALLOC */
typeExpr(C,alloc(E),vec(_)) :-
	typeExpr(C,E,int).

/* NTH */
typeExpr(C,nth(E1,E2),T) :-
	typeExpr(C,E1,vec(T)),
	typeExpr(C,E2,int).

/* LEN */
typeExpr(C,len(E),int) :-
	typeExpr(C,E,vec(_)).

/* APS2 */


/*Opérations mathématiques*/
typeExpr(C,add(X,Y),int) :- typeExpr(C,X,int),
			    typeExpr(C,Y,int).

typeExpr(C,sub(X,Y),int) :- typeExpr(C,X,int),
			    typeExpr(C,Y,int).

typeExpr(C,mul(X,Y),int) :- typeExpr(C,X, int),
			    typeExpr(C,Y, int).

typeExpr(C,div(X,Y),int) :- typeExpr(C,X,int),
			    typeExpr(C,Y,int).

/*Opérations booléennes*/
typeExpr(C,and(X,Y),bool) :- typeExpr(C,X,bool),
			     typeExpr(C,Y,bool).

typeExpr(C,or(X,Y),bool) :- typeExpr(C,X,bool),
				  typeExpr(C,Y,bool).

typeExpr(C,eq(X,Y),bool) :- typeExpr(C,X,int),
			  	  typeExpr(C,Y,int).

typeExpr(C,lt(X,Y),bool) :- typeExpr(C,X,int),
 				  typeExpr(C,Y,int).

/* vérifier que pour tous les arguments dans ARGTYPE sont bien de type ARGSTYPE
Donction chaque type de arg doit correspondre au type dans ARGSTYPE*/
check_types(_,[],[]).
check_types(C,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-	typeExpr(C,ARG,ARGTYPE),
																								check_types(C,ARGS,ARGSTYPE).


main_stdin :-
	read(user_input,T),
	typeProg([], T,R),
	print(R).
