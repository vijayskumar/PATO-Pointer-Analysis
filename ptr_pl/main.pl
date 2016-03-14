#!/usr/bin/env swipl
%% swipl --nosignals --quiet main.pl input.trp 

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

%% :- use_module('./andersen.pl').

:- initialization main.


eval :-
	current_prolog_flag(argv, Arguments),
	[Inputfile|_] = Arguments,
	rdf_load(Inputfile, [format(ntriples)]).

main :- 
	catch(eval, E, (print_message(error, E), fail)),
	%% halt(0).
	nl.

main :-
	%% halt(0).
	nl.