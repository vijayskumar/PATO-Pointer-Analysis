#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "llvm/Support/raw_ostream.h"

using namespace clang::tooling;
using namespace clang;

// RecursiveASTVisitor provides hooks of the form bool VisitNodeType(Node *)
// for most AST nodes, we only need to implement the methods for the relevant
// node types
class TestVisitor : public clang::RecursiveASTVisitor<TestVisitor> {
public:
	explicit TestVisitor(ASTContext *Context) : Context(Context) {}
	bool VisitFunctionDecl(FunctionDecl *f) {
		f->dump();
		clang::FullSourceLoc FL = Context->getFullLoc(f->getLocStart());
		if (FL.isValid()) {
			llvm::errs() << "Found declaration at " 
				<< FL.getSpellingLineNumber() << ":"
				<< FL.getSpellingColumnNumber() << "\n";
		}
		return true; // return false to stop the traversal
	}
private:
	ASTContext *Context;
};

// interface to write generic actions on an AST
class TestConsumer : public clang::ASTConsumer {
public:
	explicit TestConsumer(ASTContext *Context) : Visitor(Context) {}
	virtual void HandleTranslationUnit(clang::ASTContext &Context) {
		// traversing the translation unit decl via a RecursiveASTVisitor
		Visitor.TraverseDecl(Context.getTranslationUnitDecl());
	}
private:
	TestVisitor Visitor;
};

// tool based on LibTooling, the common entry point is the FrontendAction
class TestAction : public clang::ASTFrontendAction {
public:
	virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
	clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
		return std::unique_ptr<clang::ASTConsumer>(
			new TestConsumer(&Compiler.getASTContext())
		);
	}
};

static llvm::cl::OptionCategory SampleToolCategory("sample-tool options");

int main(int argc, const char **argv) {
	// CommonOptionsParser constructor will parse arguments and create
	// a CompilationDatabase. 
	CommonOptionsParser OptionsParser(argc, argv, SampleToolCategory);
	// retrive CompilationDatabase and the list of input file paths, hand
	// to the tool constructor -> create a ClangTool and run FrontendAction
	// over the code
	ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());
	// ClangTool needs a new FrontendAction for each translation unit
	int result = Tool.run(newFrontendActionFactory<TestAction>().get());
}
