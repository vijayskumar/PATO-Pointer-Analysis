:- module(andersen, []);
%% 
%% Pointer analysis: computation of the set of objects 
%% (points-to set) that a program var may point to 
%% 
%% This module defines the general inference rule independent of 
%% language and compiler, the detailed module should implement
%% the following input relations
%% 
%% Input relations %%
%% 

%% ALLOC var = malloc() ...
%% abstract repr of objects using allocation site
%% alloc(Var, Heap).

%% MOVE/ASSIGN to = from

%% LOAD to = base.fld

%% STORE

%% CALL



%% base, a = &b; 
%% a = b 
%% a = *b
%% *a = b

