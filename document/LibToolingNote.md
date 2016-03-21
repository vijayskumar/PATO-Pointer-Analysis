# Some random note on building compiler tools based on Clang LibTooling

Standalone tool

## Environment

* Ubuntu 15.10

* Clang/LLVM 3.7 
installed via the apt approach, command is `clang-3.7`, `clang-config-3.7`, etc

## Building the tool

What libraries to link against? 

The included headers  `clang/XXX/yyy.h` are under path `$(LLVM_INSTALL)/include/clang/XXX` and the associated library files named libclangXXX.a are under `$(LLVM_INSTALL)/lib`.

If compiling any clang tool or example, make sure you check the Makefiles under clang/tools folder :)

For a list of libraries to link, look at one of the tools’ Makefiles (for example clang-check/Makefile).

See [troubleshooting](#trouble)

### Invoking the tool

libTooling has the concept of a compilation database, which tells tools about the compiler options used to build the sources under consideration, but in the meantime it helps knowing that a compilation database isn't strictly required to run tools.

The libTooling command-line parser (CommonOptionsParser) supports providing compiler flags on the command line, following the special flag --. Think of it as a simplified, ad-hoc version of a compilation database.

	./sample test.c --
 
You can run on a file by specifying all the needed parameters after a “--” separator.
Note that anything before the double dash “--” on is an input to your LibTooling program, argv in main(), while anything after the double dash is an input to Clang itself 
 
## Coding the tool

### include Clang or LLVM API headers

the include path is relative to the `llvm-config --includedir`

### Clang APIs

#### `ASTContext` and `SourceManager`

	ASTContext *Context;

	SourceManager &SM = Context->getSourceManager();
	
	TranslationUnitDecl::getASTContext() -> ASTContext&

	Context->getSourceManager().isInSystemHeader(Loc)
	
+ DynTypedNodeList clang::ASTContext::getParents 	( 	const NodeT &  	Node	) 	
	inline

	Returns the parents of the given node.

	Note that this will lazily compute the parents of all nodes and store them for later retrieval. Thus, the first call is O(n) in the number of AST nodes.
	
	http://clang.llvm.org/doxygen/classclang_1_1ASTContext.html#a32d11844fdb82310b9059784fd4ceb6b
	
#### RecursiveASTVisitor
	
`RecursiveASTVisitor` is implemented using the CRTP pattern
	
**Control how to traverse the AST tree**
	
Implementing the member methods `TraverseDecl(Decl *x)`, `TraverseStmt(Stmt *x)` and `TraverseType(QualType x)` for your `RecursiveASTVisitor`-derived class
	
		class MyClass : public RecursiveASTVisitor<MyClass> {
	public:
	    bool TraverseDecl(Decl *D) {
	        // your logic here
	        // for example, check if from header or main
	        // if (Context->getSourceManager().isInSystemHeader(D->getLocation()))
	        // if (Context->getSourceManager().isFromMainFile(D->getLocation()))
	        RecursiveASTVisitor<MyClass>::TraverseDecl(D); // **Forward to base class**
	        return true; // Return false to stop the AST analyzing
	    }
	    bool TraverseStmt(Stmt *x) {
	        // your logic here
	        RecursiveASTVisitor<MyClass>::TraverseStmt(x);
	        return true;
	    }
	    bool TraverseType(QualType x) {
	        // your logic here
	        RecursiveASTVisitor<MyClass>::TraverseType(x);
	        return true;
	    }
	};


#### Misc

llvm::StringRef::str() -> string

## Debugging

If the tool fails to find `stddef.h` or similar headers, call the tool with `-v` (after the `--` separator) and look at the search paths

## <a name="trouble"></a>Troubleshooting

### Compiling/Linking the tool

#### "undefined reference error" 

The "undefined reference error" in linking may be caused by the order of linked libraries (-lxxx)

#### "undefined references to`clang::QualType::getAsString...`"

	undefined reference to `clang::QualType::getAsString(clang::Type const*, clang::Qualifiers)'

Use `g++` as the compiler instead of using `clang++`, no idea why :(

#### System lib error

	`/usr/bin/ld: cannot find -ledit` ERRORS

solution:

	sudo apt-get install libedit-dev

similarly,

	/usr/bin/ld: cannot find -lz

	sudo apt-get install zlib1g-dev

or `libz-dev`

### Using the tool to parse input

#### Error with system headers 

	Error while processing ...

or 

	fatal error: 'stddef.h' file not found

It is the builtin include problem, one solution is to put the tool in the clang bin dir, another is to add include path with `-I`

	./pg test/type.c -- -Wall -I/usr/lib/llvm-3.7/lib/clang/3.7.1/include

see (http://clang.llvm.org/docs/LibTooling.html#libtooling-builtin-includes)

> Clang tools need their builtin headers and search for them the same way Clang does. Thus, the default location to look for builtin headers is in a path $(dirname /path/to/tool)/../lib/clang/3.3/include relative to the tool binary.

and (http://clang.llvm.org/docs/FAQ.html)

> Some header files (stddef.h, stdarg.h, and others) are shipped with Clang — these are called builtin includes. Clang searches for them in a directory relative to the location of the clang binary. If you moved the clang binary, you need to move the builtin headers, too.
	
	
	
	
	
	
	
	
	
## Some links


http://stackoverflow.com/questions/30805595/how-to-traverse-clang-ast-manually

http://stackoverflow.com/questions/28048486/how-to-exclude-headers-from-ast-in-clang

	
	
	
[TOC]	
	
	
	
	
	
	
	

