%% The Frontend dependent logic is put here
%% Main task is to generate relations input to
%% inference engine

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

:- dynamic malloc/2, addressOf/2, assign/2,
	copy/2, load/2, load/3, store/2, store/3,
	callProc/2, formalArg/3, actualArg/3,
	formalReturn/2, actualReturn/2.

%% : multifile ...

%% --------------------------------
%% Basic input relations encoded
%% for the pointer analysis
%% ----------------------------------
%% Note the frontend already strip the cast and paren expr 

%% malloc
malloc_r(Var, Heap) :-
	(
		rdf(CallExpr, calls, literal('malloc'));
		rdf(CallExpr, calls, literal('alloca'));
		rdf(CallExpr, calls, literal('calloc'))
	),
	(
		%% in declaration float *p = (float *)malloc(...
		rdf(Var, hasInit, CallExpr),
		rdf(Var, isa, literal('Var'));
		%% in definition, p = ... malloc ...
		assignROL(Ref, CallExpr),
		rdf(Ref, isa, literal('DeclRefExpr')),
		rdf(Ref, hasDecl, Var)
	),
	Heap = CallExpr.

%% @tbd : assignment in initialization
%% @tbd refine, first classify the vars?

%% addr
%% p = &x
addressOf_r(Var, Obj) :-
	rdf(AddrOf, hasOperator, literal('&')),
	rdf(AddrOf, hasSubExpr, Ref),
	rdf(Ref, hasDecl, Obj),
	assignROL(To, AddrOf),
	rdf(To, hasDecl, Var),
	dbw(Ref, To).

%% copy
copy_r(To, From) :-
	assignOLR(ToExpr, FromExpr),
	rdf(ToExpr, hasDecl, To),
	rdf(FromExpr, hasDecl, From),
	isPointerType(To), isPointerType(From),
	dbw(FromExpr, ToExpr).

%% load
%% p = *q
load_r(ToVar, DerefVar) :-
	rdf(Uop, hasOperator, literal('*')),
	rdf(Uop, hasSubExpr, DerefVarRef),
	assignROL(ToRef, Uop),
	rdf(DerefVarRef, hasDecl, DerefVar),
	rdf(ToRef, hasDecl, ToVar),
	dbw(DerefVarRef, ToRef).
	
%% p = q.f
load_r(ToVar, BaseVar, Fld) :-
	baseField(Mop, BaseExpr, Fld),
	rdf(BaseExpr, hasDecl, BaseVar),
	assignROL(ToRef, Mop),
	rdf(ToRef, hasDecl, ToVar),
	dbw(BaseExpr, ToRef).

%% store
%% *p = q
store_r(DerefVar, FromVar) :-
	rdf(Uop, hasOperator, literal('*')),
	rdf(Uop, hasSubExpr, DerefVarRef),
	assignLOR(Uop, FromRef),
	rdf(DerefVarRef, hasDecl, DerefVar),
	rdf(FromRef, hasDecl, FromVar),
	dbw(FromRef, DerefVarRef).

%% p.f = q
store_r(BaseVar, Fld, FromVar) :-
	baseField(Mop, BaseExpr, Fld),
	rdf(BaseExpr, hasDecl, BaseVar),
	assignLOR(Mop, FromExpr),
	(
		rdf(FromExpr, hasOperator, literal('&')),
		rdf(FromExpr, hasSubExpr, FromRef);
		isPointerType(FromExpr), FromRef = FromExpr
	),
	rdf(FromRef, hasDecl, FromVar),
	dbw(FromRef, BaseExpr).

%% base[] = x
%% @tbd init 
arrayStore_r(Base, From) :-
	rdf(Sub, isa, literal('ArraySubscriptExpr')),
	assignLOR(Sub, FromRef),
	rdf(Sub, hasBase, BaseRef),
	rdf(BaseRef, hasDecl, Base),
	rdf(FromRef, hasDecl, From),
	dbw(FromRef, BaseRef).


%% x = base[]
arrayLoad_r(To, Base) :-
	rdf(Sub, isa, literal('ArraySubscriptExpr')),
	assignROL(ToRef, Sub),
	rdf(Sub, hasBase, BaseRef),
	rdf(BaseRef, hasDecl, Base),
	rdf(ToRef, hasDecl, To),
	dbw(BaseRef, ToRef).


%% call 
callProc_r(Invoc, Proc) :-
	rdf(Invoc, callsFunc, Proc).

%% +Nth
formalArg_r(Proc, Nth, ParmVar) :-
	rdf(Proc, HasParm, ParmVar),
	atomic_list_concat(['hasParm(', Nth, ')'], HasParm).

formalReturn_r(Proc, RetVar) :-
	rdf(Ret, inProc, Proc),
	rdf(Ret, returns, RetVar).

actualArg_r(Invoc, Nth, ArgVar) :-
	rdf(Invoc, HasArg, ArgExpr),
	atomic_list_concat(['hasArg(', Nth, ')'], HasArg),
	stripCast(ArgExpr, ArgExpr2),
	stripParen(ArgExpr2, ArgVar). %? pass struct ...

actualReturn_r(Invoc, Var) :-
	rdf(Invoc, callsFunc, _), 
	assignROL(VarExpr, Invoc), % stripParen from Invoc?
	rdf(VarExpr, hasDecl, Var).

%% ------------------------
%% Syntax preprocess helper
%% ------------------------

%% +Var
isPointerType(Var) :-
	(rdf(Var, hasDecl, Decl); % Var is a VarRef 
	Decl = Var), % Var is a Decl 
	rdf(Decl, hasTypeClass, literal(Type)),
	term_to_atom(T, Type),
	['PointerType'|_] = T.

%% +CastExpr, -NonCastExpr
stripCast(CastExpr, NonCastExpr) :-
	rdf(CastExpr, isa, literal(CastType)), 
	memberchk(CastType, 
		['ImplicitCastExpr', 'ExplicitCastExpr', 'CStyleCastExpr']),
	rdf(CastExpr, hasSubExpr, SubExpr),
	stripCast(SubExpr, NonCastExpr), !.
stripCast(NonCastExpr, NonCastExpr).

stripParen(ParenExpr, NonParenExpr) :-
	rdf(ParenExpr, isa, literal('ParenExpr')),
	rdf(ParenExpr, hasSubExpr, SubExpr),
	stripParen(SubExpr, NonParenExpr), !.
stripParen(NonParenExpr, NonParenExpr).

%% assignment
assignOLR(To, From) :-
	rdf(Bop, hasOperator, literal('=')),
	rdf(Bop, hasLHS, To),
	rdf(Bop, hasRHS, From).

assignROL(To, From) :-
	rdf(Bop, hasRHS, From),
	rdf(Bop, hasOperator, literal('=')),
	rdf(Bop, hasLHS, To).

assignLOR(To, From) :-
	rdf(Bop, hasLHS, To),
	rdf(Bop, hasOperator, literal('=')),
	rdf(Bop, hasRHS, From).

%% decomp a s.p or s->p expression
baseField(Mop, BaseExpr, FldDecl) :-
	rdf(Mop, isa, literal('MemberExpr')),
	rdf(Mop, hasBase, BaseExpr),
	rdf(Mop, hasMemberDecl, FldDecl).

%% -----------------------------------------
%% Driver predicates to generate relations 
%% -----------------------------------------

%% scan all the assignment (initialization, assignment)
gen_all :-
	nl.

gen_malloc :-
	forall(malloc_r(Var, Heap), assert(malloc(Var, Heap))).

del_malloc :-
	forall(malloc(Var, Heap), retract(malloc(Var, Heap))).

%% 
%% Iteractive helper
%% 

%% given the line number and the var name, return the 
%% id in the database
grabVar(Line, Name, Var) :-
	rdf(Var, hasName, literal(Name)),
	(
		atomic_concat('Var(', Line, Prx);
		atomic_concat('DeclRefExpr(', Line, Prx)
	),	
	atom_prefix(Var, Prx), !.

dbw(From, To) :-
	write(From), write('->'), write(To), nl.