#!/usr/bin/env swipl
%% swipl --nosignals --quiet main.pl ../test/test.trp

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

:- set_prolog_flag(verbose, silent).

% file loading path is relative to this module, not the invoking directory
:- ['preprocessing.pl']. 
:- ['andersen_wl.pl'].

:- initialization main.


eval :-
	current_prolog_flag(argv, Arguments),
	[Inputfile, Outputfile|_] = Arguments,
	rdf_load(Inputfile, [format(ntriples)]),
	build(_), writeln('build done'), 
	constraintInit, writeln('init done'), !,
	(andersenPtr(0); true),	writeln('analysis done'),
	nl.

main :-
	catch(eval, E, (print_message(error, E), fail)),
	%% halt(0).
	nl.

main :-
	%% halt(0).
	nl.


%%
%% Iteractive helper
%%

%% +Line, +Name, -Var
%% given the line number and the var name, return the
%% id of the var/var reference in the database
getVarId(Line, Name, Var) :-
	rdf(Var, hasName, literal(Name)),
	(
		atomic_concat('Var(', Line, Prfx);
		atomic_concat('DeclRefExpr(', Line, Prfx)
	),
	atom_prefix(Var, Prfx), !.
