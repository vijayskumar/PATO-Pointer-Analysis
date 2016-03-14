#!/usr/bin/env swipl

%% input relations
%% var = malloc ..., var = new ...
alloc(Var, Heap, InMeth) :- nl.
%% to = from
move(ToV, FromV) :- nl.
%% to = base.field
load(ToV, BaseV, Field) :- nl.
%% base.fld = from
store(BaseV, Field, FromV).

vcall(BaseV, Sig, InvocI, InMeth).
%% arg is in nth index
formalArg(Meth, Nth, ArgV).

actualArg(InvocI, Nth, ArgV).

formalReturn(Meth, RetV).
%% given InvocI, RetV is the local var at the call siet that receive the method's return value
actualReturn(InvocI, RetV).

thisVar(Meth, ThisV).
%% matches an obj to its type
heapType(Heap, Type).
%% match a method signature to the actual method def inside a type
lookup(Type, Sig, Meth).

varType(Var, Type).

inMethod(Instr, Meth).

subType(Type, SuperType).

%% output
varPointsTo(V, Heap).
callGraph(InvocI, Meth).
fieldPointsTo(BaseH, Field, Heap).
interProcAssign(ToV, FromV).
reachable(Meth).




%% Andersen-style points-to-analysis


varPointsTo(Var, Heap) :- 
    reachable(Meth), alloc(Var, Heap, Meth).

varPointsTo(ToV, Heap) :-
    move(ToV, FromV), varPointsTo(FromV, Heap).

%% basev = &baseh, basev.fld = from, from = new heap
fieldPointsTo(BaseH, Field, Heap) :-
    store(BaseV, Field, FromV), varPointsTo(FromV, Heap),
    varPointsTo(BaseV, BaseH).
%% to = basev.fld, basev->baseh, baseh.fld->heap
varPointsTo(ToV, Heap) :-
    load(ToV, BaseV, Field), varPointsTo(BaseV, BaseH),
    fieldPointsTo(BaseH, Field, Heap).

reachable(ToMesh) :-
    vrvhlt(_, _, _, ToMesh).

varPointsTo(This, Heap) :-
    vrvhlt(_, This, Heap, _).

callGraph(InvocI, ToMesh) :-
    vrvhlt(InvocI, _, _, ToMesh).

%% inmeth::invoc::base.sig(), base->heap, 
vrvhlt(InvocI, This, Heap, ToMesh) :-
    vcall(Base, Sig, InvocI, InMeth), reachable(InMeth),
    varPointsTo(Base, Heap),
    heapType(Heap, HeapT), lookup(HeapT, Sig, ToMesh),
    thisVar(ToMesh, This).

%% formal:meth(...to...); invoc:meth(...from...), 
interProcAssign(ToV, FromV) :-
    callGraph(InvocI, Meth),
    formalArg(Meth, Nth, ToV), actualArg(InvocI, Nth, FromV).
%% invoc:tov = meth(); meth() { return from }
interProcAssign(ToV, FromV) :-
    callGraph(InvocI, Meth),
    formalReturn(Meth, From), actualReturn(InvocI, ToV).

varPointsTo(To, Heap) :-
    interProcAssign(To, From),
    varPointsTo(From, Heap).



