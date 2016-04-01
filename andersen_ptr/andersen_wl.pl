% The worklist algorithm

:- dynamic heapLoc/2, stackLoc/2, copy/2,
	load/2, fieldLoad/3, arrayLoad/2,
	store/2, fieldStore/3, arrayStore/2,
	callProc/2, formalArg/3, actualArg/3,
	formalReturn/2, actualReturn/2.

:- dynamic wl/1, pedge/2, pts/2.
% pedge(A, B), A -> B, for B = A

% ----------------------------------
% APIs
% ----------------------------------
% #

%% \+ Input is the file name .pl storing 
%% input relations
run(Input) :-
	[Input], init, andersenPtr(0).
 
% #
andersenPtr(IterNumber) :-
	wl(N), retract(wl(N)), % select N from W
	write(IterNumber), nl,

	solveComplex(N),
	propagateEdge(N),

	IterNumber2 is IterNumber + 1, !,
	andersenPtr(IterNumber2).

% ----------------------------------
% Internal implementation
% ----------------------------------

%% init order is important
% init points-to set from ground cases
% pts(N, V) :- heapLoc(N, V); stackLoc(N, V).
init :-
	forall(stackLoc(N, V), (\+ pts(N, V) -> assertz(pts(N, V)); true)),
	forall(heapLoc(N, V), (\+ pts(N, V) -> assertz(pts(N, V)); true)).
% init worklist for each N with non-empty points-to set
init :-
	forall(pts(N, _), (\+ wl(N) -> assertz(wl(N)); true)).
% init flow edge for each p = q
init :-
	forall(copy(To, From), (\+ pedge(From, To) -> assertz(pedge(From, To)); true)).

% Complex constraint
% +N
solveComplex(N) :-
	propagateLoad(N);
	propagateStore(N);
	propagateFieldLoad(N);
	propagateFieldStore(N)
	.
solveComplex(_) :-
	true.

% for any node with outgoing edge, add to worklist
% +N, +V
% for each input constraint A = *N
propagateLoad(N) :-
	load(A, N), 
	pts(N, V),  % for each V \in pts(N)
	(\+ pedge(V, A) -> assertz(pedge(V, A)), unionWL(V)
	).
% for each constraint *N = B
propagateStore(N) :-
	store(N, B), 
	pts(N, V),
	(\+ pedge(B, V) -> assertz(pedge(B, V)), unionWL(B)
	).
% A = N.f
propagateFieldLoad(N) :-
	fieldLoad(A, N, Fld), 
	pts(N, V),
	(\+ pedge(record(V, Fld), A) 
		-> assertz(pedge(record(V, Fld), A)), 
		unionWL(record(V, Fld))
		).
% N.f = B
propagateFieldStore(N) :-
	fieldStore(N, Fld, B),
	pts(N, V),
	(\+ pedge(B, record(V, Fld))
		-> assertz(pedge(B, record(V, Fld))),
		unionWL(B)
		).

% points-to propagation
% +N
propagateEdge(N) :-
	pedge(N, Z), % for each N -> Z
	unionPts(Z, N, Changed),
	(Changed == true -> unionWL(Z)).
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
	\+ wl(X) -> assertz(wl(X)).

% ----------------------------------
% For debug only
% ----------------------------------
testPts(X, Y) :-
	pts(X, Y), \+ atom_prefix(X, 'tmp').


