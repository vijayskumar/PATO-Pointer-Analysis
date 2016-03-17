# Pointer-Analysis

A basic Andersen pointer analysis based on Clang and Prolog

+ context sensitive(?)
+ field sensitive 
+ flow insensitive 

This will be improved.

## Install

### Install the Clang/LLVM 

The C parser is built on top of Clang-3.7, so install the Clang-3.7 
(other version may also work). 
A recommended method under Debian/Ubuntu is to use the apt repository

http://llvm.org/apt/

## Usage

1. Extract information from AST and build the knowledge base using the
Clang tooling frontend:

	```
	ast2db input.c > out.csv
	# translate from csv to ntriples format so it can be loaded by Prolog
	cat out.csv | csv2ntriples.py > out.trp
	```

2. (Iteractively) Run the pointer analysis inference program

	1. main.pl is the main entry to the program
	
    	```    
    	swipl main.pl out.trp
    	```	
	
	2. query the point-to relation
	3. 
	
## Note on the implementation

...


## Reference

1. Andersen, L.O., 1994. Program analysis and specialization for the C programming language (Doctoral dissertation, University of Cophenhagen).






