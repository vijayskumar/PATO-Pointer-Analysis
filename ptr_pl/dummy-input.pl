type(cp, [pointer]).
type(fpp, [pointer, pointer]).
type(st, [struct]).
type(stp,[pointer, struct]).

type(parr, [pointer, array]).
type(pl, float).

%% init(parr, addressOf(f)).
addr(parr, f). %% array as single 

addr(pl, malloc28).

addr(cp, c).
move(cp2, cp).

addr(stp, st).
addr(stpp, f). %%
addr(fpp, stpp).

