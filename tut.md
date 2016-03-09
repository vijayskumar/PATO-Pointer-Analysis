# standalone tool based on LibTooling

## Environment

Ubuntu 15.10
Clang/LLVM 3.7 - installed via the apt approach

## How to build and run the tool

compilation options:

	the order of options is important!
		easy to have "undefined reference error" in linking
	the order of -l libraries seems important

which libs to link against? 

(Guess) the included header as `clang/XXX/yyy.h` under path `$(LLVM_INSTALL)/include/clang/XXX` folder seems associated with the library file named libclangXXX.a under `$(LLVM_INSTALL)/lib`.

If compiling any clang tool or example, make sure you check the Makefiles under clang/tools folder :)

**`/usr/bin/ld: cannot find -ledit` ERRORS**

	sudo apt-get install libedit-dev

**`/usr/bin/ld: cannot find -lz**

	sudo apt-get install zlib1g-dev

or libz-dev


## Invoke the tool

libTooling has the concept of a compilation database, which tells tools about the compiler options used to build the sources under consideration, but in the meantime it helps knowing that a compilation database isn't strictly required to run tools.

The libTooling command-line parser (CommonOptionsParser) supports providing compiler flags on the command line, following the special flag --. Think of it as a simplified, ad-hoc version of a compilation database.

	./sample test.c --
 
You can run on a file by specifying all the needed parameters after a “--” separator.
Note that anything before the double dash “--” on is an input to your LibTooling program, argv in main(), while anything after the double dash is an input to Clang itself 
 
## Code the tool

### include clang or llvm libraries

the include path is relative to the `llvm-config --includedir`

### Clang APIs

	ASTContext *Context;

	SourceManager &SM = Context->getSourceManager();
	
	RecursiveASTVisitor implemented using the CRTP pattern
	
	
	
	TranslationUnitDecl::getASTContext() -> ASTContext&
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

