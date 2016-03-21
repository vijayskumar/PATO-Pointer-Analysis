#!/usr/bin/env swipl
%% swipl --nosignals --quiet main.pl input.trp 

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

%% :- use_module('andersen.pl').
:- ['RelationGenClang.pl'].

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
	
	
