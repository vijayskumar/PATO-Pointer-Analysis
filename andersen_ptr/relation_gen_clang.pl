%% module must be the first directive, and must appear just once.
%% :- module(relation_gen_clang, [build/1, relationGen/1]).

:- use_module(library(semweb/rdf_db)).

:- dynamic alloc/2, address/2, copy/2, 
	load/2, fieldLoad/3, arrayLoad/2, 
	store/2, fieldStore/3, arrayStore/2,
	callProc/2, formalArg/3, actualArg/3,
	formalReturn/2, actualReturn/2.

%% ------------------------------
%% relation build helper
%% ------------------------------

%% List is a list of [Relation, Arg1, Arg2, ...]
%% e.g., [copy, toVar, FromVar]
insertFact(List) :- 
	Fact =.. List, 
	%% write(Fact), nl,
	assertz(Fact).

%% The all in one step predicate that build all the 
%% relations
build(all) :-
	forall(relationGen(X), (write(X), nl)),
	write('relations built successfully'), nl.

%% ------------------------------
%% Some syntax helper
%% ------------------------------

%% +Expr
alloc(Expr, Heap) :-
	(
		rdf(Expr, calls, literal('malloc'));
		rdf(Expr, calls, literal('alloca'));
		rdf(Expr, calls, literal('calloc'))
	), atom_concat(heap, Expr, Heap). %% Heap is abstracted(named) as callsite

%% [x]
assign(LHS, RHS) :-
	rdf(Bop, hasOperator, literal('=')),
	rdf(Bop, hasLHS, LHS),
	rdf(Bop, hasRHS, RHS).

%% +Proc
formalArg(Proc) :-
	rdf(Proc, HasParm, ParmVar),
	rdf(ParmVar, isa, literal('ParmVar')),
	atom_concat('hasParm(', X, HasParm),
	atom_concat(Nth, ')', X),
	insertFact(['formalArg', Proc, Nth, ParmVar]).

%% +Proc
formalReturn(Proc) :-
	rdf(Return, isa, literal('ReturnStmt')),
	rdf(Return, inProc, Proc),
	rdf(Return, returns, RetExpr),
	parseRHS(RetExpr, RHSINode),
	insertFact(['formalReturn', Proc, RHSINode]). 

%% +Invoc
callGraph(Invoc) :-
	rdf(Invoc, callsFunc, Proc),
	insertFact([callProc, Invoc, Proc]),
	rdf(Invoc, HasArg, ArgExpr),
	atom_concat('hasArg(', X, HasArg),
	atom_concat(Nth, ')', X),
	%% parse ArgExpr 
	%% func(ArgExpr) => 
	%% (INode = ArgExpr), func(INode)
	parseRHS(ArgExpr, RHSINode),
	insertFact(['actualArg', Invoc, Nth, RHSINode]). 

%% ------------------------------
%% Main predicates to generate relations
%% ------------------------------

%% $
%% assignment
relationGen(assignment) :-
	assign(LHS, RHS), %% scan assignments
	%% write(LHS), write('---'), nl, %% only for debug
	parseLHS(LHS, ToRef, Relation), 
	parseRHS(RHS, RHSINode),
	append([Relation|ToRef], [RHSINode], Constrain),
	insertFact(Constrain).

%% $
%% initialization
%% @tbd parse init list 
relationGen(initialization) :-
	rdf(Decl, hasInit, Initializer),
	parseRHS(Initializer, RHSINode),
	insertFact([copy, Decl, RHSINode]).

%% $
%% An aggregate type is an array or a class type (struct, union, or class)
%% treat struct as object, so its declaration as malloc
%% treat array declaration as malloc 
relationGen(aggregateDecl) :-
	rdf(Decl, hasTypeClass, literal('AggregateType')),
	insertFact([alloc, Decl, heap(Decl)]).

%% $
%% function declaration 
relationGen(functionDecl) :-
	rdf(Proc, isa, literal('Function')),
	(formalArg(Proc); formalReturn(Proc)).

%% $
relationGen(functionCall) :-
	rdf(Invoc, isa, literal('CallExpr')),
	callGraph(Invoc).

%% ------------------------------
%% ParseLHS 
%% ------------------------------
assignKind(reference, copy). 
assignKind(dereference, store).
assignKind(member, fieldStore).
assignKind(subscript, arrayStore).

%% determine the relation based on LHS format
relationKind(LHS, Relation) :-
	rdf(LHS, isa, literal('DeclRefExpr')),
	assignKind(reference, Relation).
relationKind(LHS, Relation) :-
	rdf(LHS, isa, literal('UnaryOperator')),
	rdf(LHS, hasOperator, literal('*')),
	assignKind(dereference, Relation).
relationKind(LHS, Relation) :-
	rdf(LHS, isa, literal('MemberExpr')),
	assignKind(member, Relation).
relationKind(LHS, Relation) :-
	rdf(LHS, isa, literal('ArraySubscriptExpr')),
	assignKind(subscript, Relation).

%% +LHS, -ToRef, -Relation
%% @tbd add type check 
parseLHS(LHS, ToRef, Relation) :-
	relationKind(LHS, Relation), 
	(
		Relation == copy 
		-> rdf(LHS, hasDecl, Decl),	ToRef = [Decl]
		; Relation == store
		-> rdf(LHS, hasSubExpr, DerefVarRef), rdf(DerefVarRef, hasDecl, DerefVar), ToRef = [DerefVar]
		; Relation == fieldStore
		-> rdf(LHS, hasBase, BaseExpr), rdf(LHS, hasMemberDecl, FldDecl), rdf(BaseExpr, hasDecl, BaseVar), ToRef = [BaseVar, FldDecl]
		; Relation == arrayStore
		-> rdf(LHS, hasBase, BaseExpr), rdf(BaseExpr, hasDecl, BaseVar), ToRef = [BaseVar]
	).

%% ------------------------------
%% ParseRHS 
%% ------------------------------
%% ``LHS = RHS'' is first split to
%% LHS = tmp; tmp = RHS
%% tmp is regarded as normal reference (keep transition) 
%% e.g., *p = *q or s.f = t.q ...

%% +RHS -RHSINode
%% = alloc()
parseRHS(RHS, RHSINode) :-
	alloc(RHS, Heap),
	atom_concat(var, RHS, RHSINode),
	insertFact([alloc, RHSINode, Heap]), !.

%% = &X
parseRHS(RHS, RHSINode) :-
	rdf(RHS, hasOperator, literal('&')),
	rdf(RHS, hasSubExpr, LocRef),
	rdf(LocRef, hasDecl, Var),
	atom_concat(var, RHS, RHSINode),
	insertFact([address, RHSINode, Var]), !.

%% = ref 
parseRHS(RHS, RHSINode) :-
	rdf(RHS, isa, literal('DeclRefExpr')),
	rdf(RHS, hasDecl, Var),
	RHSINode = Var, !.

%% = *ref 
parseRHS(RHS, RHSINode) :-
	rdf(RHS, hasOperator, literal('*')),
	rdf(RHS, hasSubExpr, DerefVarRef),
	atom_concat(var, RHS, RHSINode),
	rdf(DerefVarRef, hasDecl, DerefVar),
	insertFact([load, RHSINode, DerefVar]), !.

%% = ref.fld, ref->fld
parseRHS(RHS, RHSINode) :-
	rdf(RHS, isa, literal('MemberExpr')),
	rdf(RHS, hasBase, BaseRef),
	rdf(BaseRef, hasDecl, Base),
	rdf(RHS, hasMemberDecl, FldDecl),
	atom_concat(var, RHS, RHSINode),
	insertFact([fieldLoad, RHSINode, Base, FldDecl]), !.

%% = ref[]
parseRHS(RHS, RHSINode) :-
	rdf(RHS, isa, literal('ArraySubscriptExpr')),
	rdf(RHS, hasBase, BaseRef),
	rdf(BaseRef, hasDecl, Base),
	atom_concat(var, RHS, RHSINode),
	insertFact([arrayLoad, RHSINode, Base]), !.

%% CallExpr
%% = tmp; tmp = invoc(); 
%% tmp is abstracted by callsite/invocation
parseRHS(RHS, RHSINode) :-
	rdf(RHS, isa, literal('CallExpr')),
	atom_concat(var, RHS, RHSINode), %% only a tmp node
	insertFact(['actualReturn', RHS, RHSINode]). 


%% ------------------------------
%% Unused, as backup for improvement
%% ------------------------------

%% @tdb 
%% check type of expression
%% checkType(LHS, Type) :- nl.

%% A more complicated alternative
%% RHS ::= refExpr | &(refExpr) | *refExpr | refExpr.f | refExpr[]
%% refExpr ::= 'DeclRefExpr' | alloc | callExpr | RHS
recursiveParseRHS(RHS, INode) :-
	rdf(RHS, isa, literal('DeclRefExpr')),
	rdf(RHS, hasDecl, Var),
	INode = Var, !.

%% alloc
recursiveParseRHS(RHS, INode) :-
	alloc(RHS, Heap),
	atom_concat(var, RHS, INode),
	insertFact([alloc, INode, Heap]), !.

%% call 
recursiveParseRHS(RHS, INode) :-
	rdf(RHS, isa, literal('CallExpr')),
	atom_concat(var, RHS, INode),
	insertFact(['actualReturn', RHS, INode]), !.

%% = &Expr => = INode; address(INode, INode2);
%% (INode2 = Expr)
recursiveParseRHS(RHS, INode) :-
	rdf(RHS, hasOperator, literal('&')),
	rdf(RHS, hasSubExpr, Sub),
	atom_concat(var, RHS, INode), 
	recursiveParseRHS(Sub, INode2),
	insertFact([address, INode, INode2]).

%% = *Expr => (INode2 = Expr); load(INode, INode2)
recursiveParseRHS(RHS, INode) :-
	rdf(RHS, hasOperator, literal('*')),
	rdf(RHS, hasSubExpr, Sub),
	atom_concat(var, RHS, INode),
	recursiveParseRHS(Sub, INode2),
	insertFact([load, INode, INode2]).

%% = refExpr.fld => (INode2 = refExpr);
%% fieldLoad(INode, INode2, fld)
recursiveParseRHS(RHS, INode) :-
	rdf(RHS, isa, literal('MemberExpr')),
	rdf(RHS, hasBase, BaseRef),
	rdf(RHS, hasMemberDecl, FldDecl),
	atom_concat(var, RHS, INode),
	recursiveParseRHS(BaseRef, INode2),
	insertFact([fieldLoad, INode, INode2, FldDecl]).

%% = refExpr[] => (INode2 = refExpr);
%% arrayLoad(INode, INode2)
recursiveParseRHS(RHS, INode) :-
	rdf(RHS, isa, literal('ArraySubscriptExpr')),
	rdf(RHS, hasBase, BaseRef),
	atom_concat(var, RHS, INode),
	recursiveParseRHS(BaseRef, INode2),
	insertFact([arrayLoad, INode, INode2]).
