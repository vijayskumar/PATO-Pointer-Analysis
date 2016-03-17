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

%% :- use_module('andersen_c.pl').
:- include('andersen_c_preprocess.pl').
:- include('andersen_csem.pl').


:- discontiguous varPointsTo/2.

%%
%% Input relations %%
%%

%% ADDR ::=
	%% alloc(p, heap) e.g., p = alloc()
	%% | p = &x
%% addr(Var, Loc).

%% COPY ::=
	%% to = from
%% copy(ToVar, FromVar).

%% LOAD ::=
	%% field read (to = base.fld)
	%% | (C) deref p = *q
%% load(ToVar, BaseVar, Fld).
%% load(ToVar, DerefVar).

%% STORE ::=
	%% filed assign (p.f = q)
	%% | (C) assign *p = q
%% store(BaseVar, Fld, FromVar).
%% store(DerefVar, FromVar).

%% CALL
%% callProc(Invoc, Proc).

%% VCALL for object-orient language, base.sig()
%% vcallMethod(BaseVar, Sig, Invoc).

%% FORMALARG
%% formalArg(Method, Nth, ParmVar).

%% ACTUALARG
%% actualArg(Invoc, Nth, ArgVar).

%% FORMALRETURN
%% formalReturn(Proc, RetVar).

%% ACTUALRETURN
%% actualReturn(Invoc, Var).


%%
%% Computed relations %%
%%

%% basic %%
%%
varPointsTo(Var, Obj) :-
	addr(Var, Obj).

%% p \include q |-
%% pointsTo(q, var) => pointsTo(p, var)
varPointsTo(To, Obj) :-
	copy(To, From), varPointsTo(From, Obj).

%% complex
%% multilevel pointer 
%% p = *q
varPointsTo(Var, Obj) :-
	load(Var, DerefVar),
	varPointsTo(DerefVar, DerefVarCtx),
	varPointsTo(DerefVarCtx, Obj).

%% *p = q
varPointsTo(Var, Obj) :-
	varPointsTo(DerefVar, Var), %reorder for speed
	store(DerefVar, From),
	%% varPointsTo(DerefVar, Var),
	varPointsTo(From, Obj).

%% field %%
%%
%% To = Base.Fld
varPointsTo(To, Obj) :-
	load(To, BaseP, Fld),
	varPointsTo(BaseP, Base),
	fldPointsTo(Base, Fld, Obj).

%% BaseP.Fld = From |-
%% BaseP.Fld \include From |-
%% pointsTo(BaseP, Base) => Base.Fld \include From
fldPointsTo(Base, Fld, Obj) :-
	%% store(BaseP, Fld, From),
	varPointsTo(BaseP, Base),
	store(BaseP, Fld, From),
	varPointsTo(From, Obj).
%% ? p.f = &x case ?

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
%%
%% base[...] = from
arrayContentPointsTo(Base, Obj) :-
	%% arrayStore(BaseP, From),
	varPointsTo(BaseP, Base),
	arrayStore(BaseP, From),
	varPointsTo(From, Obj).

%% To = base[...]
varPointsTo(To, Obj) :-
	arrayLoad(To, BaseP),
	varPointsTo(BaseP, Base),
	arrayContentPointsTo(Base, Obj).
