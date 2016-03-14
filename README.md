# PATO---Pointer-Analysis

A context insensitive field sensitive Andersen pointer analysis using Clang and Prolog

## Install

### Install the Clang/LLVM 

Built on top of Clang-3.7


## Usage

### Extract information from AST 

A Clang tooling frontend

    ast2db input.c > out.csv
    # translate from csv to ntiples format so it 
    # can be loaded by Prolog
    cat out.csv | csv2ntriples.py > out.trp


### Run the pointer analysis 



