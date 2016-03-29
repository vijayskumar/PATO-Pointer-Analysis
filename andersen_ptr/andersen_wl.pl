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
init :-
	init(pts), init(w), init(e).

% #
andersenPtr :-
  wl(N), retract(wl(N)), % select N from W
	write('N '), write(N), nl,
	% +N
	(
		solveComplex(N);
		propagateCopy(N);
		andersenPtr
	).

% ----------------------------------
% Internal implementation
% ----------------------------------

% V is in N's points-to set
% pts(N, V) :- heapLoc(N, V); stackLoc(N, V).
init(pts) :-
	forall(stackLoc(N, V), (\+ pts(N, V) -> assertz(pts(N, V)); true)),
	forall(heapLoc(N, V), (\+ pts(N, V) -> assertz(pts(N, V)); true)).
% init worklist for each N with non-empty points-to set
% must after init(pts)
init(w) :-
	forall(pts(N, _), (\+ wl(N) -> assertz(wl(N)); true)).
% init pedge for each p = q
init(e) :-
	forall(copy(To, From), (\+ pedge(From, To) -> assertz(pedge(From, To)); true)).

% Complex constraint
% +N
solveComplex(N) :-
	pts(N, V), % for each V \in pts(N)
	write('V '), write(V), nl,
	(
		propagateLoad(N, V);
		propagateStore(N, V)
	).

% for any node with outgoing edge, add to worklist
% +N, +V
propagateLoad(N, V) :-
	load(A, N), % for each input constraint a = *n
	write('A '), write(A), nl,
	(\+ pedge(V, A) -> assertz(pedge(V, A)), unionWL(V)
	).
% +N, +V
propagateStore(N, V) :-
	store(N, B), % for each constraint *n = b
	write('B '), write(B), nl,
	(\+ pedge(B, V) -> assertz(pedge(B, V)), unionWL(B)
	).

% points-to propagation
% +N
propagateCopy(N) :-
	pedge(N, Z), % for each N -> Z
	write('Z '), write(Z), nl,
	unionPts(Z, N, Changed),
	(Changed == true -> unionWL(Z)).

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
