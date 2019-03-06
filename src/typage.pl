assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

/*(PROG)*/
typeProg(C,prog(X),void) :-
	typeCmds(C,X,void).

/*(END)*/
typeCmds(_,[epsilon],void).

/*(STAT)*/
typeCmds(C,[stat(X)|Y],void) :- 
	typeStat(C,X,void),
	typeCmds(C,Y,void).

/*(DEC)*/
typeCmds(C,[dec(X)|Y],void) :-
	typeDec(C,X,CBIS),
	typeCmds(CBIS,Y,void).

/*(ECHO)*/
typeStat(C,echo(X),void) :- 
	typeExpr(C,X,int).


/*(CONST)*/
typeDec(C,const(X,TYPE,EXPR),[(X,TYPE)|C]) :-
	typeExpr(C,EXPR,TYPE).

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

/*(ABS)*/
typeExpr(C,afun(ARGS,BODY),arrow(_,TYPEF)) :-	append(C,ARGS,CBIS),
												typeExpr(CBIS,BODY,TYPEF).		
	
/*(TRUE)*/
typeExpr(_,true,bool).

/*(FALSE)*/
typeExpr(_,false,bool).

/*(INT)*/
typeExpr(_,int(X),int) :- 		integer(X).

/*(IDENT)*/
typeExpr(C,ident(X),T) :- 		assoc(X,C,T).
	
/*(IF)*/
typeExpr(C,if(COND,E1,E2),T) :-	typeExpr(C,COND,bool),
								typeExpr(C,E1,T),
								typeExpr(C,E2,T).	
/*(APP)*/
typeExpr(C,apply(ident(F),ARGS),TYPE) :- assoc(F,C,arrow(ARGSTYPE,TYPE)),
										 check _types(C,ARGS,ARGSTYPE).

/*Opérations mathématiques*/
typeExpr(C,add(X,Y),int) :- typeExpr(C,X,int),
							typeExpr(C,Y,int).

typeExpr(C,sub(X,Y),int) :- typeExpr(C,X,int),
							typeExpr(C,Y,int).

typeExpr(C,mul(X,Y),int) :- typeExpr(C,X, int),
							typeExpr(C,Y, int).

typeExpr(C,div(X,Y),type(int)) :-	typeExpr(C,X,int),
									typeExpr(C,Y,int).

/*Opérations booléennes*/
typeExpr(C,and(X,Y),type(bool)) :- 	typeExpr(C,X,bool),
									typeExpr(C,Y,bool).

typeExpr(C,or(X,Y),type(bool)) :-	typeExpr(C,X,bool),
									typeExpr(C,Y,bool).

typeExpr(C,eq(X,Y),type(bool)) :-	typeExpr(C,X,int),
									typeExpr(C,Y,int).	

typeExpr(C,lt(X,Y),type(bool)) :-	typeExpr(C,X,int),
									typeExpr(C,Y,int).

/*(autre)*/
% vérifier que pour tous les arguments dans ARGTYPE sont bien de type ARGSTYPE  
% Donction chaque type de arg doit correspondre au type dans ARGSTYPE  
check _types(_,[],[]).
check _types(C,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-	typeExpr(C,ARG,ARGTYPE),
												check _types(C,ARGS,ARGSTYPE).
% renvoie la liste de type de tous les arguments dans ARGS
% elle prend une liste d'argument et renvoie tous les type des arguments
% elle mets tt les type de arg dans RES 
get_types_args([],[]).
get_types_args([arg(_,T)|ARGS],[T|RES]) :-
	get_types_args(ARGS,RES).
	
main_stdin :- 
	read(user_input,T),
	typeProg([], T,R),
	print(R).


