%%
%% Pointer analysis: computation of the set of objects
%% (points-to set) that a program var may point to
%%
%% This module defines the general inference rule independent of
%% language and compiler, the detailed module should implement
%% the following input relations

:- module(andersen, [varPointsTo/2,
	fldPointsTo/3,
	interProcAssign/2
	]).

:- dynamic alloc/2, address/2, copy/2, 
	load/2, fieldLoad/3, arrayLoad/2, 
	store/2, fieldStore/3, arrayStore/2,
	callProc/2, formalArg/3, actualArg/3,
	formalReturn/2, actualReturn/2.

:- discontiguous varPointsTo/2.


%% --------------------
%% Computed relations  
%% ------------------- 

%% basic
%% --------------------
varPointsTo(Var, Loc) :-
	alloc(Var, Loc).

varPointsTo(Var, Loc) :-
	address(Var, Loc).

%% copy |- \include 
%% p \include q |-
%% pointsTo(q, var) => pointsTo(p, var)
varPointsTo(To, Obj) :-
	copy(To, From), varPointsTo(From, Obj).

%% complex
%% -------------------
%% p = *q
varPointsTo(Var, Obj) :-
	load(Var, DerefVar),
	varPointsTo(DerefVar, DerefVarPtee),
	varPointsTo(DerefVarPtee, Obj).

%% *p = q
varPointsTo(Var, Obj) :-
	%% varPointsTo(DerefVar, Var), %reorder for speed
	store(DerefVar, From),
	varPointsTo(DerefVar, Var),
	varPointsTo(From, Obj).

%% field
%% --------------------
%% To = Base.Fld
varPointsTo(To, Obj) :-
	fieldLoad(To, BaseP, Fld),
	varPointsTo(BaseP, Base),
	fieldPointsTo(Base, Fld, Obj).

%% BaseP.Fld = From |-
%% BaseP.Fld \include From |-
%% pointsTo(BaseP, Base) => Base.Fld \include From
fieldPointsTo(Base, Fld, Obj) :-
	fieldStore(BaseP, Fld, From),
	varPointsTo(BaseP, Base),
	%% fieldStore(BaseP, Fld, From),
	varPointsTo(From, Obj).

%% inter procedure
%% ----------------------------
%% 
interProcAssign(To, From) :-
	callProc(Invoc, Proc),
	formalArg(Proc, Nth, To),
	actualArg(Invoc, Nth, From).

interProcAssign(To, From) :-
	callProc(Invoc, Proc),
	formalReturn(Proc, From),
	actualReturn(Invoc, To).

varPointsTo(To, Obj) :-
	interProcAssign(To, From),
	varPointsTo(From, Obj).

%% array
%% -------------------
%% base[...] = from
arrayContentsPointsTo(Base, Obj) :-
	arrayStore(BaseP, From),
	varPointsTo(BaseP, Base),
	%% arrayStore(BaseP, From),
	varPointsTo(From, Obj).

%% To = base[...]
varPointsTo(To, Obj) :-
	arrayLoad(To, BaseP),
	varPointsTo(BaseP, Base),
	arrayContentsPointsTo(Base, Obj).
