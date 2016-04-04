# PATO - Pointer-Analysis 

This program is a part of the project PATO (Program Analysis Through Ontology). 

In the PATO, we represent the information of program as knowledge base in the form of
[Ontology](https://en.wikipedia.org/wiki/Ontology_(information_science)). Ontology
captures the concepts like Structures (Declaration, Variable, etc)  
and their relationships in `(subject predicate/relation object)` triples. 
For example, 

```
0 // s.c
1 int a = 0;
2 int foo () {
3 for (int i = 0; i < 10; i++) {
4 a = a + i;
5 }
6 return 0;
7 }
```
may generate
```
1 ( '3:1:5:1' , rdf:type, 'ForStatement')
2 ( '3:6:3:14' , rdf:type, 'VariableDecl')
3 ( '3:6:3:10' , rdf:type, Variable')
4 ( '3:1:5:1' , 'hasForInit', '3:6:3:14')
5 ( '3:1:5:1' , 'hasForTest', '3:17:3:22')
6 ( '3:1:5:1' , 'hasForIncr', '3:25:3:27')
7 ( '3:1:5:1' , 'hasBody', '3:30:5:1')
```
where the entities are named with their source locations (may in other minor variant form).
The store file format for ontology can be RDF, 
[OWL](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/), 
[ntriples](https://www.w3.org/TR/n-triples/), 
[turtle](https://www.w3.org/TR/turtle/), etc.

We represent ontology because ontology is a standard format with logic semantics. We can leverage the
existing tool sets to process the knowledge base. In our projects, we utilize the 
[Prolog](http://www.swi-prolog.org/) and the 
[SWI-Prolog Semantic Web Library](www.swi-prolog.org/pldoc/package/semweb.html)
to parse and load the generated ontology knowledge base. Then we build different inference engines for different
program analysis.

This sub-project implements a basic Andersen-style pointer analysis in Prolog. The initial 
[PATO project](https://github.com/yzhao30/PATO-ROSE)
build the program parser based on [ROSE compiler](http://rosecompiler.org/) while this project 
shifts to the Clang compiler.

## Getting Started

### Overview

The PATO project usually has two main parts:
```
           +-------+    +----------+    +---------+
.c  +------>Parser +--->+ ontology +----> Inference----> result
           |       |    +----------+    | Engine  |
           +-------+    +----------+    +---------+
```
The Parser is usually built on Frontend compilers like ROSE or Clang. The inference engines
do logic inference on the generated ontology. You can think ontology as an Intermediate representation (IR) 
which looks like linked data or graph database while it also has other features.

### Structure

In this particular project, the structure is:

- src/ - the source code for the C parser built on Clang. It reads in the c program
	and extract information by visiting the AST tree.

	The `ast2db.cpp` uses the [LibTooling](http://clang.llvm.org/docs/LibTooling.html).
	The `visitor.h` is the core code which implements the custom code for visiting the AST tree. 
	Currently, it only gets enough information for the pointer analysis. 
	If you want to get more complete information, start with extending this	file.
	
- bin/ - the parser is compiled here. There is alos a data format processing script. It is 
	because the output of the parser is in CSV format (subject, predicate, object) and we can
	translate it into different format (remember we mentioned the RDF, OWL, ntriples, etc). 
	We don't want to bind to a fixed format. A script `csv2ntripls.py` is used to generate 
	ntriples format (.trp).

- andersen_ptr/ - the pointer analysis program in Prolog. `preprocessing.pl` handles all the 
	syntax details of the raw output ontology and generate the basic constraint relations for
	the pointer analysis. The generated relation can be in-memory or dumped as `.pl` format as
	Prolog database. `andersen_wl.pl` is the worklist algorithm for Andersen's inclusion based
	pointer analysis. It starts with the basic constraints relations and computes the full points-to
	relations. So these two files are the only essential codes. The `andersen.pl` is a logic correct
	inference program for the same pointer analysis but very inefficient for Prolog.
	Finally, the `main.pl` is just a utility to automatically load and run the predicates, and it
	is optional. 
	
- test/ - some test codes. For example, `test0.c` tests the basic pointer copy, `tes1.c` tests the structure
	cases, etc. See the following usage. In each test file, there are comments listing the correct output
	of the Andersen's pointer analysis. The `test#.log` is the output of the program with manual comments.
	You can verify the output with the comments in the program.

- document/ - some unofficial documents.

### Install

**Install the Clang and Prolog**

The C parser is built on top of Clang. The particular version used is Clang-3.7 and all the
experiments are done under Ubuntu 16.04 (other versions of Clang or Ubuntu may also work). 

The Clang can be installed following the official instruction but a easy method under Debian/Ubuntu 
is to use the apt repository http://llvm.org/apt/

The particular Prolog used is the [SWI-Prolog](http://www.swi-prolog.org/). 

### Test and Run the PATO pointer analysis

The following section demonstrates the usage with the `test/test0.c`. Assume it is in the root directory.

1. First, extract information from AST and build the knowledge base with te parser:
	```
	bin/ast2db test0.c  -- -I/usr/lib/llvm-3.7/lib/clang/3.7.1/include | ../bin/csv2ntriples.py > test0.c.trp
	```
	For the `--` and the `-I` part, check the `document/LibToolingNote.md`.

2. Run the pointer analysis. Suppose do not use the `main.pl` script.

	1. Generate the basic constraint database
	
    	```
    	swipl andersen_ptr/main.pl test/test0.c.trp
    	```	
	It will generate a file `test0.c.trp.pl`.
	
	2. Run the analysis
	
		1. Run `swipl ../andersen_ptr/andersen_wl.pl` to load the required module and get into the REPL mode.
	
		2. run `constraintInit.` to initialize the worklist algorithm with basic constraints.
		
		3. run `andersenPtr(0).` to start the pointer analysis. The iteration number is shown during the execution. 		After it is done, use `testPts(X, Y)` to check the result (enter `;` when pause for more results).
	
### Extension

See the Structure section. If you want to extract more complete information from the AST tree of C program or even want to cover the C++ case, start with `src/visitor.h`. 

If you want to handle more complicated syntax cases like some ticky `&(*p[i])` or multi-dimensional array, etc, you can check the `andersen_ptr/preprocessing.pl`. The code is commented. Since predicates for different syntax structure is organized in a module way, it is kind of easy to extend. Finally, if you want to improve the worklist algorithm of
Andersen's pointer analysis, check `andersen_wl.pl`. Otherwise, you can write new program to implement new pointer analysis.

## Publication

The related paper is under reveiw by ECOOP 2016 and it will be released once decision is made.


## Contact

The project is tested but far from perfect. If you have any comments, please feel free to contact

yzhao30@ncsu.edu

## LICENSE

MIT License. See [LICENSE](LICENSE).



