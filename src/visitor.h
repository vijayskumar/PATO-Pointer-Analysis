
//#define GETLOC(s) clang::FullSourceLoc FL = Context->getFullLoc((s)->getLocStart()); \
		//if (FL.isValid()) {llvm::outs() << FL.getSpellingLineNumber() << ":" << FL.getSpellingColumnNumber() << "\n";}

// using namespace clang::tooling;
using namespace clang;

std::string currentId;

// @TODO
// function pointer

// RecursiveASTVisitor provides hooks of the form bool VisitNodeType(Node *)
// for most AST nodes, we only need to implement the methods for the relevant
// node types
class PGVisitor : public clang::RecursiveASTVisitor<PGVisitor> {
public:
	explicit PGVisitor(ASTContext *Context) : Context(Context) {
	}

	// Skip the system headers
	bool TraverseDecl(Decl *D) {
		SourceLocation Loc = D->getLocation();
		if (Context->getSourceManager().isInSystemHeader(Loc)) {
			// skip
		} else {
			// forward to base class
			RecursiveASTVisitor<PGVisitor>::TraverseDecl(D);
		}

		return true; // return false to stop the AST analyzing
	}

	// Decl - the base class
	bool VisitDecl(Decl *d) {
		currentId = genId(d);
		llvm::outs() << currentId << ", isa, " << d->getDeclKindName() << "\n";
		return true;
	}
	// ->
	bool VisitNamedDecl(NamedDecl *n) {
		llvm::outs() << currentId << ", hasName, " << n->getNameAsString() << "\n";
		// n->getTypeForDecl() -> const Type *
		return true;
	}
	// --> TypeDecl
	// ---> TagDecl
	// ----> 
    bool VisitRecordDecl(RecordDecl *RD) {
    	QualType qt = Context->getRecordType(RD);
    	RecordDecl *RDF = RD->getDefinition();
    	llvm::outs() << currentId << ", hasDefinition, " << genId(RDF) << "\n";
    	for (auto *I : RD->fields()) {
    		llvm::outs() << currentId << ", hasField, " << genId(I) << "\n";
    	}
    	return true;
    }
    // ---> TypedefNameDecl
    // ---->
    // Represents the declaration of a typedef-name via the 'typedef' type specifier
    bool VisitTypedefDecl(TypedefDecl *TD) {
		QualType qt = TD->getUnderlyingType();
		const Type *ty = qt.getTypePtrOrNull();
		if (ty) 
			// llvm::outs() << currentId << ", hasUnderlyingTypeClass, " << ty->getTypeClassName() << "\n";
			llvm::outs() << currentId << ", hasUnderlyingType, " << qt.getAsString() << "\n";
		if (!qt.isCanonical()) {
			QualType cqt = qt.getCanonicalType();
			// FIXME
			// llvm::outs() << currentId << ", hasCanonicalTypeClass, " << cqt.getTypePtr()->getTypeClassName() << "\n";
			llvm::outs() << currentId << ", hasCanonicalType, " << cqt.getAsString() << "\n";
		}
		TypedefNameDecl *tdnd = TD->getCanonicalDecl();
		if (tdnd)
			llvm::outs() << currentId << ", hasCanonicalDecl, " << genId(tdnd) << "\n";
		return true;
	}
	// -->
    bool VisitValueDecl(ValueDecl *vd) {
    	QualType qt = vd->getType();
    	const Type *ty = qt.getTypePtrOrNull();
    	// llvm::outs() << "<debug> \n";
    	// qt.dump();
    	// llvm::outs() << "</debug> \n";
    	llvm::outs() << currentId << ", hasTypeClass, ";
    	dumpNestPointerType(ty, false);
    	llvm::outs() << "\n";

    	if (ty->isArrayType()) {
    		const ArrayType *at = ty->getAsArrayTypeUnsafe();
    		QualType eleqt = at->getElementType();
    		llvm::outs() << currentId << ", hasElementTypeClass, ";
    		dumpNestPointerType(eleqt.getTypePtrOrNull(), false);
    		llvm::outs() << "\n";
    	}
		
    	llvm::outs() << currentId << ", hasType, " << qt.getAsString() << "\n";

    	return true;
    }
    // ---> DeclaratorDecl
    // ---->
    bool VisitFieldDecl(FieldDecl *fd) {
    	// getParent - Returns the parent of this field declaration, which is the struct in which this method is defined.
    	RecordDecl *rd = fd->getParent();
    	llvm::outs() << currentId << ", hasParent, " << genId(rd) << "\n";
    	// getFieldIndex -> unsigned
    	// getCanonicalDecl -> FieldDecl *
    	return true;
    }
    // ---->
	bool VisitFunctionDecl(FunctionDecl *f) {
		//f->dump();
		if (f->isMain()) {
			llvm::outs() << currentId << ", isa, " << "MainFunction\n";
		}
		// isDefined -> bool
		if (f->hasBody()) {
			Stmt *body = f->getBody();
			if (body) {
				llvm::outs() << currentId << ", hasBody, " << genId(body) << "\n";
			}
		}
		// iterate params
		for (int i = 0; i < f->getNumParams(); i++) {
			ParmVarDecl *p = f->getParamDecl(i);
			llvm::outs() << currentId << ", hasParm(" << i << "), " << genId(p) << "\n";
		}

		QualType qt = f->getReturnType();
		// llvm::outs() << currentId << ", hasReturnType, " << qt.getAsString() << "\n";
		// getCanonicalDecl -> FunctionDecl *
		return true; 
	}
	// ---->
	bool VisitVarDecl(VarDecl *VDecl) {
		// isStaticLocal() -> bool
		if (VDecl->isLocalVarDecl()) { // Returns true for local variable declarations other than parameters
			llvm::outs() << currentId << ", isa, " << "LocalVarDecl\n";
		}

		VarDecl *CV = VDecl->getCanonicalDecl();
		if (CV) {
			llvm::outs() << currentId << ", hasCanonicalDecl, " << genId(CV) << "\n";
		}
		// hasLocalStorage()
		// isThisDeclarationADefinition()
		VarDecl *VDef = VDecl->getDefinition();
		if (VDef) {
			llvm::outs() << currentId << ", hasDefinition, " << genId(VDef) << "\n";
		}
		// isFileVarDecl() -> bool // file scoped variable declaration
		Expr *Init = VDecl->getInit();
		if (Init) {
			llvm::outs() << currentId << ", hasInit, " << genId(Init) << "\n";
		}
		
		return true;
	}
	// ---->
	// bool VisitParmVarDecl(ParmVarDecl *pd) {
	// 	llvm::outs() << currentId << ", hasType, " << QualType::getAsString(pd->getType().split()) << "\n";
	// 	return true;
	// }

	/// Stmt
	bool VisitStmt(Stmt *s) {
		currentId = genId(s);
		//s->dump();
		llvm::outs() << currentId << ", isa, " << s->getStmtClassName() << "\n";
				
		return true;
	}

	bool VisitBreakStmt(BreakStmt *b) {
		return true;
	}
	// This represents a group of statements like { stmt stmt }.
	bool VisitCompoundStmt(CompoundStmt *c) {

		for (auto *B : c->body()) {
			// B is Stmt *
		}
		return true;
	}
	// ContinueStmt, DoStmt, ForStmt, GotoStmt, IfStmt, LabelStmt,
	// NullStmt, SwitchCase, SwitchStmt, WhileStmt

	bool VisitDeclStmt(DeclStmt *DS) {

		return true;
	}

	bool VisitReturnStmt(ReturnStmt *r) {
		return true;
	}

	/// Expr

	// The ?: ternary operator
	bool VisitConditionalOperator(ConditionalOperator *co) {
		Expr *cond = co->getCond(),
			*texp = co->getTrueExpr(),
			*fexp = co->getFalseExpr();
		llvm::outs() << currentId << ", hasCond, " << genId(cond) << "\n";
		llvm::outs() << currentId << ", hasTrueExpr, " << genId(texp) << "\n";
		llvm::outs() << currentId << ", hasFalseExpr, " << genId(fexp) << "\n";

		return true;
	}  

	// [C99 6.5.2.1] Array Subscripting.
	bool VisitArraySubscriptExpr(ArraySubscriptExpr *sub) {
		Expr *base = sub->getBase(),
			*idx = sub->getIdx();
		if (base && idx) {
			llvm::outs() << currentId << ", hasBase, " << genId(base) << "\n";
			llvm::outs() << currentId << ", hasIndex, " << genId(idx) << "\n";
		}
		
		return true;
	}

	bool VisitBinaryOperator(BinaryOperator *bop) {
		
		llvm::outs() << currentId << ", hasOperator, " << bop->getOpcodeStr() << "\n";
		Expr *lhs = bop->getLHS();
		Expr *rhs = bop->getRHS();
		llvm::outs() << currentId << ", hasLHS, " << genId(lhs) << "\n";
		llvm::outs() << currentId << ", hasRHS, " << genId(rhs) << "\n";

		if (bop->isAssignmentOp()) {
			llvm::outs() << currentId << ", isa, " << "AssignmentOp\n";
		}
		// if (bop->isCompoundAssignmentOp()) {}
		// if (llvm::isa<CompoundAssignOperator>(bop)) {
			// e.g., += 
			// CompoundAssignOperator *caop = llvm::cast<CompoundAssignOperator>(bop);
			// ... 
		// }
		return true;
	}
	// Represents a function call (C99 6.5.2.2, C++ [expr.call]).
	bool VisitCallExpr(CallExpr *CE) {
		// Expr *callee = CE->getCallee(); // foo(bar), callee is foo
		// llvm::outs() << currentId << ", hasCallee, " << genId(callee) << "\n";

		// Decl *decl = CE->getCalleeDecl(); // decl of foo
		// llvm::outs() << currentId << ", calls, " << genId(decl) << "\n";
		
		FunctionDecl *fdecl = CE->getDirectCallee(); // return dyn_cast_or_null<FunctionDecl>(getCalleeDecl());
		if (fdecl) {
			llvm::outs() << currentId << ", callsFunc, " << genId(fdecl) << "\n";
			DeclarationNameInfo dn_info = fdecl->getNameInfo();
			if (dn_info.getAsString() == "malloc") {
				llvm::outs() << currentId << ", calls, malloc()\n";
			}
		}

		int i = 0;
		for (CallExpr::arg_iterator ai = CE->arg_begin(), 
			ae = CE->arg_end(); ai != ae; ++ai) {
			Expr *arg = *ai;
			llvm::outs() << currentId << ", hasArg(" << i << "), " << genId(arg) << "\n";
			i++;
		}

		return true;
	}

	bool VisitCastExpr(CastExpr *C) {
		llvm::outs() << currentId << ", hasCastKind, " << C->getCastKindName() << "\n";
		Expr *sub = C->getSubExpr();
		llvm::outs() << currentId << ", hasSubExpr, " << genId(sub) << "\n";
		return true;
	}

	// CompoundLiteralExpr C99 6.5.2.5

	// A reference to a declared variable, function, enum, etc. [C99 6.5.1p2]
	// This encodes all the information about how a declaration is referenced within an expression.
	bool VisitDeclRefExpr(DeclRefExpr *DRE) {
		ValueDecl *vd = DRE->getDecl();
		if (vd)	llvm::outs() << currentId << ", hasDecl, " << genId(vd) << "\n";
		return true;
	}

	// DesignatedInitExpr C99 designated initializer expr
	// http://clang.llvm.org/doxygen/classclang_1_1DesignatedInitExpr.html#details
	// struct point {
	// 	double x, y;
	// };
	// struct point ptarr[10] = { [2].y = 1.0, [2].x = 2.0, [0].x = 1.0 }
	// The {...} is the InitListExpr, it contains three DesignatedInitExpr
	// bool VisitDesignatedInitExpr(DesignatedInitExpr *DIE) {
	// designators() -> designators_range
	// 	return true;
	// }

	// bool VisitFloatingLiteral(FloatingLiteral *fl) {
	// 	llvm::APFloat f = fl->getValue();
	// 	return true;
	// }

	// Describes an C or C++ initializer list, which can be used to initialize 
	// objects of different types, including struct/class/union types, arrays, and vectors. For example:
	// struct foo x = { 1, { 2, 3 } };
	// http://clang.llvm.org/doxygen/classclang_1_1InitListExpr.html#details
	bool VisitInitListExpr(InitListExpr *ILE) {
		InitListExpr *SF = nullptr;
		if (!ILE->isSemanticForm()) {
			ILE = ILE->getSemanticForm();
		}

		if(ILE->isStringLiteralInit()) {
			llvm::outs() << currentId << ", isInitialized, StringLiteral\n";
		}

		Expr **Inits = ILE->getInits();
		for (int i = 0; i < ILE->getNumInits(); ++i)
		{
			llvm::outs() << currentId << ", hasSubInit(" << i << "), " << genId(Inits[i]) << "\n";
		}
		return true;
	}

	// IntegerLiteral

	// C99 6.5.2.3 structure/union members 
	// x->f and x.f
	bool VisitMemberExpr(MemberExpr *ME) {
		Expr *base = ME->getBase();
		llvm::outs() << currentId << ", hasBase, " << genId(base) << "\n";
		ValueDecl *vdecl = ME->getMemberDecl(); // return a FieldDecl
		llvm::outs() << currentId << ", hasMemberDecl, " << genId(vdecl) << "\n";
		return true;
	}

	// OffsetOfExpr
	// http://clang.llvm.org/doxygen/classclang_1_1OffsetOfExpr.html#details

	// a parenthesized expression, e.g. "(1)"
	bool VisitParenExpr(ParenExpr *PE) {
		Expr * expr = PE->getSubExpr();
		llvm::outs() << currentId << ", hasSubExpr, " << genId(expr) << "\n";
		return true;
	}

	// https://en.wikipedia.org/wiki/Comma_operator ?
	bool VisitParenListExpr(ParenListExpr *PLE) {
		Expr **exprs = PLE->getExprs();
		for (int i = 0; i < PLE->getNumExprs(); ++i)
		{
			Expr *expr = exprs[i];
			llvm::outs() << currentId << ", hasSubExpr(" << i << "), " << genId(expr) << "\n";
		}
		return true;
	}

	// [C99 6.4.2.2] - A predefined identifier such as func.
	// VisitPredefinedExpr(PredefinedExpr *PE) {}

	// StmtExpr - This is the GNU Statement Expression extension: ({int X=4; X;}).
	// The StmtExpr contains a single CompoundStmt node, which it evaluates and takes the value of the last subexpression.
	// A StmtExpr is always an r-value; values "returned" out of a StmtExpr will be copied.
	// StmtExpr

	// VisitStringLiteral(StringLiteral *SL) {
		// SL->getBytes() -> StringRef
	// }

	// UnaryOperator - This represents the unary-expression
	// (except sizeof and alignof), the postinc/postdec operators 
	// from postfix-expression, and various extensions.
	bool VisitUnaryOperator(UnaryOperator *UOP) {
		// isPrefix()
		// isPostfix()
		// isIncrementOp()
		// isDecrementOp()
		llvm::outs() << currentId << ", hasOperator, " << UnaryOperator::getOpcodeStr(UOP->getOpcode()) << "\n";
		Expr *sub = UOP->getSubExpr();
		llvm::outs() << currentId << ", hasSubExpr, " << genId(sub) << "\n";

		return true;
	}

	/// Type
	// VisitType(Type *T) 
	// VisitTypeLoc(TypeLoc TL)
private:
	ASTContext *Context;

	// generate a id based on the location <start:end>
	template<typename NodeType>
	std::string getLoc(NodeType *s) {
		std::ostringstream idss;
		clang::FullSourceLoc FL = Context->getFullLoc(s->getLocStart()),
			FLE = Context->getFullLoc(s->getLocEnd());
		if (FL.isValid() && FLE.isValid()) {
			idss << FL.getSpellingLineNumber() << ":" << FL.getSpellingColumnNumber()
				<< ":" << FLE.getSpellingLineNumber() << ":" << FLE.getSpellingColumnNumber();
		}
		return idss.str();
	}

	std::string genId(Decl *d) {
		//@to-do: only print filename, without full path
		// std::string fn;
		// SourceManager &SM = Context->getSourceManager();
		// PresumedLoc PLoc = SM.getPresumedLoc(SM.getSpellingLoc(d->getLocation()));
		// if (PLoc.isInvalid()) {
		// 	fn = "invalid";
		// } else {
		// 	fn = PLoc.getFilename();
		// }
		// ---------------------------------------------v---
		return std::string(d->getDeclKindName()) + "(" + getLoc<Decl>(d) + ")";
	}

	std::string genId(Stmt *s) {
		return std::string(s->getStmtClassName()) + "(" + getLoc<Stmt>(s) + ")";
	}

	void dumpNestPointerType(const Type *T, bool comma) {
		if (!comma) {
			llvm::outs() << "[";
		}
		if (!T) {
			llvm::outs() << "]";
			return;
		}
		if (comma) {
			llvm::outs() << ",";
		}
		if (T->isPointerType()) {
			llvm::outs() << "'PointerType'";
			QualType ptee = T->getPointeeType();
			const Type *ptty = ptee.getTypePtrOrNull();
			dumpNestPointerType(ptty, true);
		} else {
			llvm::outs() << "'" << T->getTypeClassName() << "'" <<"]";
		}
	}
};
