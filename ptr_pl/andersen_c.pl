%% The language dependent logic is here


%% alloc()




%% Alias analysis: when do two pointer expressions refer to the same storage location

%% Pointers
% int *p, i; p = &i;
%% Call-by-reference
% void (m Obj a, Obj b)
% m(x, x); m(x, y);
%% Array indexing
% int i, j, a[100]; i = j; // a[i], a[j] alias

%% operators

%% A = B
assign(A, B) :- nl.

%% include allocation statement
addressOf(ADDR) :- nl.

%% malloc create heap pointers, & create stack pointers

derefer :- nl.

referTo(PointerExp, MemLoc) :- 
    true.

%% pass by reference in C++, not allowed in C or Java

