%% :- module(andersen_wl, [andersenPtr/1, constraintInit/0, run/1]).

% The worklist algorithm

:- dynamic heapLoc/2, stackLoc/2, copy/2,
	load/2, fieldLoad/3, arrayLoad/2,
	store/2, fieldStore/3, arrayStore/2,
	callProc/2, formalArg/3, actualArg/3,
	formalReturn/2, actualReturn/2.

:- dynamic wl/1, incedge/2, pts/2.
% incedge : inclusion edge 
% incedge(A, B), A -> B, for B = A

% ----------------------------------
% APIs
% ----------------------------------

%% \+ Input is the file name .pl storing
%% input relations
run(Input) :-
	[Input], constraintInit, andersenPtr(0).

% #
andersenPtr(IterNumber) :-
	wl(N), retract(wl(N)), % select N from W
	write(IterNumber), nl,

	%% solve complex constraint
	propagateLoad(N), !,
	propagateStore(N), !,
	propagateFieldLoad(N), !,
	propagateFieldStore(N), !,
	propagateArrayLoad(N), !,
	propagateArrayStore(N), !,
	%% propagate along the inclusion edge 
	propagateEdge(N), !,

	IterNumber2 is IterNumber + 1,
	andersenPtr(IterNumber2).

% ----------------------------------
% Internal implementation
% ----------------------------------

% init points-to constraints from assignment
% init order is important
% pts(N, V) :- heapLoc(N, V); stackLoc(N, V).
constraintInit :-
	forall(stackLoc(N, V), (\+ pts(N, V) -> assertz(pts(N, V)); true)),
	forall(heapLoc(N, V), (\+ pts(N, V) -> assertz(pts(N, V)); true)),
	% init worklist for each N with non-empty points-to set
	forall(pts(N, _), (\+ wl(N) -> assertz(wl(N)); true)),
	% init flow edge for each p = q
	forall(copy(To, From), (\+ incedge(From, To) -> assertz(incedge(From, To)); true)),
	forall(interProcAssign(To, From), (\+ incedge(From, To) -> assertz(incedge(From, To)); true)).


interProcAssign(To, From) :-
	callProc(Invoc, Proc),
	formalArg(Proc, Nth, To),
	actualArg(Invoc, Nth, From).
interProcAssign(To, From) :-
	callProc(Invoc, Proc),
	formalReturn(Proc, From),
	actualReturn(Invoc, To).

%% +N 
% for any node with outgoing edge, add to worklist
% for each input constraint A = *N
propagateLoad(N) :-
	load(A, N),
	pts(N, V),  % for each V \in pts(N)
	(\+ incedge(V, A) -> assertz(incedge(V, A)), unionWL(V)
		), fail. % fail is important to repeat search in current step 
propagateLoad(_) :- true.

% for each constraint *N = B
propagateStore(N) :-
	store(N, B),
	pts(N, V),
	(\+ incedge(B, V) -> assertz(incedge(B, V)), unionWL(B)
		), fail.
propagateStore(_) :- true.

% A = N.f
propagateFieldLoad(N) :-
	fieldLoad(A, N, Fld),
	pts(N, V),
	(\+ incedge(record(V, Fld), A)
		-> assertz(incedge(record(V, Fld), A)),
		unionWL(record(V, Fld))
	), fail.
propagateFieldLoad(_) :- true.

% N.f = B
propagateFieldStore(N) :-
	fieldStore(N, Fld, B),
	pts(N, V),
	(\+ incedge(B, record(V, Fld))
		-> assertz(incedge(B, record(V, Fld))),
		unionWL(B)
	), fail.
propagateFieldStore(_) :- true.

% A = N[i]
propagateArrayLoad(N) :-
	arrayLoad(A, N),
	pts(N, V),
	(\+ incedge(V, A) -> assertz(incedge(V, A)), unionWL(V)
		), fail.
propagateArrayLoad(_) :- true.

% N[i] = B
propagateArrayStore(N) :-
	arrayStore(N, B),
	pts(N, V),
	(\+ incedge(B, V) -> assertz(incedge(B, V)), unionWL(B)
		), fail.
propagateArrayStore(_) :- true.

% points-to propagation
propagateEdge(N) :-
	incedge(N, Z), % for each N -> Z
	unionPts(Z, N, Changed),
	(Changed == true -> unionWL(Z)), fail.
propagateEdge(_) :- true.

% ----------------------------------
% Helper
% ----------------------------------
% pts(Y) = pts(Y) \cup pts(X)
% +Y, +X, -Changed
% Changed: if Y is changed
unionPts(Y, X, Changed) :-
	pts(X, E), (\+ pts(Y, E) -> assertz(pts(Y, E)), Changed = true
	).

% if X is not in worklist, insert it
unionWL(X) :-
	(\+ wl(X) -> assertz(wl(X))).

% ----------------------------------
% For debug only
% ----------------------------------
testPts(X, Y) :-
	pts(X, Y),
	(atom(X) -> \+ atom_prefix(X, 'tmp');
		true).

