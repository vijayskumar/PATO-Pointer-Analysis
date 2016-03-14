%% The Frontend dependent logic is put here

allocationSite(X) :-
	rdf(X, isa, literal('CallExpr')), 
	rdf(X, calls, literal('malloc()')).

