/*
 * ptr-constr-gen.cpp
 * The pointer analysis constraint (fact) generator
 * Copyright 2016 yzhao30 <yzhao30@yzhao30-csc>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 * 
 * 
 */
#include <string>
#include <sstream>

#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/DeclarationName.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Casting.h"
		
//#if defined(ANDERSEN)
//#include "visitor_andersen.h"
//#elif defined(CFG)
//#include "visitor_cfg.h"
//#else
#include "visitor.h"
//#endif


// interface to write generic actions on an AST
class PGConsumer : public clang::ASTConsumer {
public:
	explicit PGConsumer(clang::ASTContext *Context) : Visitor(Context) {}
	virtual void HandleTranslationUnit(clang::ASTContext &Context) {
		// traversing the translation unit decl via a RecursiveASTVisitor
		Visitor.TraverseDecl(Context.getTranslationUnitDecl());
	}
private:
	PGVisitor Visitor;
};

// tool based on LibTooling, the common entry point is the FrontendAction
class PGAction : public clang::ASTFrontendAction {
public:
	virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
	clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
		return std::unique_ptr<clang::ASTConsumer>(
			new PGConsumer(&Compiler.getASTContext())
		);
	}
};

static llvm::cl::OptionCategory SampleToolCategory("sample-tool options");

int main(int argc, const char **argv) {
	// CommonOptionsParser constructor will parse arguments and create
	// a CompilationDatabase. 
	clang::tooling::CommonOptionsParser OptionsParser(argc, argv, SampleToolCategory);
	// retrive CompilationDatabase and the list of input file paths, hand
	// to the tool constructor -> create a ClangTool and run FrontendAction
	// over the code
	clang::tooling::ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());
	// ClangTool needs a new FrontendAction for each translation unit
	int result = Tool.run(clang::tooling::newFrontendActionFactory<PGAction>().get());
}
