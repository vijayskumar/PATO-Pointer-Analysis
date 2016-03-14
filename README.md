# Pointer-Analysis

A basic Andersen pointer analysis based on Clang and Prolog

+ context insensitive 
+ field sensitive 
+ flow insensitive 

will be improved ...

## Install

### Install the Clang/LLVM 

The C parser is built on top of Clang-3.7, so install the Clang-3.7 
(other version may also work).

A recommended method under Debian/Ubuntu is to use the apt repository

http://llvm.org/apt/


## Usage

1. Extract information from AST and build the knowledge base using the
Clang tooling frontend:

    ast2db input.c > out.csv
    # translate from csv to ntiples format so it 
    # can be loaded by Prolog
    cat out.csv | csv2ntriples.py > out.trp


2. Run the pointer analysis using the Prolog based inference engine

The detailed explaination of how it works is comming ...


## Reference

...







