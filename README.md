# Pointer-Analysis

[TOC]

A basic Andersen pointer analysis based on Clang and Prolog

+ context sensitive(?)
+ field sensitive 
+ flow insensitive 

This will be improved.

## Code structure

	src/ - the source code for the C program parser tool built on Clang
	
	bin/ - the compiled parser tool and the data format processing script
	
	andersen_ptr/ - the pointer analysis program in Prolog
	
	test/ - some test input
	
	document/ - some documents
	

## Install

### Install the Clang/LLVM 

The C parser is built on top of Clang-3.7, so install the Clang-3.7 
(other version may also work). 
A recommended method under Debian/Ubuntu is to use the apt repository

http://llvm.org/apt/

### Install the Prolog

The program is written in SWI-Prolog. 


## Usage

1. Extract information from AST and build the knowledge base using the
Clang tooling frontend:

	1. Extract raw relations from AST

	```
	ast2db input.c -- -Wall -I/usr/lib/llvm-3.7/lib/clang/3.7.1/include > out.csv
	```
	
	Note the double dash and the `-I` flag (otherwise, it raises error of can't 
	finding system headers.
	
	2. Translate from csv to ntriples format so it can be loaded by Prolog
	
	```
	cat out.csv | csv2ntriples.py > out.trp
	```

2. (Iteractively) Run the pointer analysis inference program

	1. `main.pl` is the main entry to the program
	
    	```
    	swipl main.pl out.trp
    	```	
	
	2. query the point-to relation
	
		1. Run `build(_).` to generate the basic relations of pointer assignment.
	Optionally can run `build('out.pl')` to save the basic relations to file.
	
		2. run `init.` to initialize the worklist-based algorithm.
		
		3. run `andersenPtr.` to start the pointer analysis. After it is done, use
		`testPts(X, Y)` to check the result.
	
	


## Implementation

see document/

## Reference

1. Andersen, L.O., 1994. Program analysis and specialization for the C programming language (Doctoral dissertation, University of Cophenhagen).







