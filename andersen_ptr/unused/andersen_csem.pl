%% The language dependent but frontend independent
%% logic is here

%% malloc create heap pointers, 
%% & create stack pointers
%% p = &x
addr(Ptr, Heap) :-
	malloc(Ptr, Heap). % heap is abstracted by allocation site

addr(Ptr, Obj) :-
	addressOf(Ptr, Obj).

%% to = base[...]
%% arrayLoad(ToVar, BaseVar).

%% base[...] = from
%% arrayStore(BaseVar, FromVar).

%% Pointers
% int *p, i; p = &i;
%% Call-by-reference
% void (m Obj a, Obj b)
% m(x, x); m(x, y);
%% Array indexing
% int i, j, a[100]; i = j; // a[i], a[j] alias
%% treat array as an whole object and element as a single field

%% pass by reference in C++, not allowed in C or Java

