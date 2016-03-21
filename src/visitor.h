
//#define GETLOC(s) clang::FullSourceLoc FL = Context->getFullLoc((s)->getLocStart()); \
		//if (FL.isValid()) {llvm::outs() << FL.getSpellingLineNumber() << ":" << FL.getSpellingColumnNumber() << "\n";}

using namespace clang;

// RecursiveASTVisitor provides hooks of the form bool VisitNodeType(Node *)
// for most AST nodes, we only need to implement the methods for the relevant
// node types
class PGVisitor : public clang::RecursiveASTVisitor<PGVisitor> {
public:
	explicit PGVisitor(ASTContext *Context) : Context(Context),
	currentFunction(nullptr) {}

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
    	QualType QT = Context->getRecordType(RD);
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
		QualType QT = TD->getUnderlyingType();
		const Type *Ty = QT.getTypePtrOrNull();
		if (Ty) {
			// llvm::outs() << currentId << ", hasUnderlyingTypeClass, " << Ty->getTypeClassName() << "\n";
			llvm::outs() << currentId << ", hasUnderlyingType, " << QT.getAsString() << "\n";
		}
		if (!QT.isCanonical()) {
			QualType CQT = QT.getCanonicalType();
			// FIXME
			// llvm::outs() << currentId << ", hasCanonicalTypeClass, " << CQT.getTypePtr()->getTypeClassName() << "\n";
			llvm::outs() << currentId << ", hasCanonicalType, " << CQT.getAsString() << "\n";
		}
		TypedefNameDecl *CDecl = TD->getCanonicalDecl();
		if (CDecl)
			llvm::outs() << currentId << ", hasCanonicalDecl, " << genId(CDecl) << "\n";
		return true;
	}
	// -->
    bool VisitValueDecl(ValueDecl *ValD) {
    	QualType QT = ValD->getType();
    	const Type *Ty = QT.getTypePtrOrNull();

    	// QT.dump();
    	llvm::outs() << currentId << ", hasTypeClass, ";
    	dumpNestPointerType(Ty, false);
    	llvm::outs() << "\n";

    	if (Ty->isAggregateType()) {
    		llvm::outs() << currentId << ", hasTypeClass, AggregateType\n"; 
    	}

    	// QualType CQT = QT.getCanonicalType();
    	// llvm::outs() << currentId << ", hasCanonicalType, " << CQT.getTypePtr()->getTypeClassName() << "\n";

    	llvm::outs() << currentId << ", hasType, " << QT.getAsString() << "\n";

    	if (Ty->isArrayType()) {
    		const ArrayType *ArrTy = Ty->getAsArrayTypeUnsafe();
    		QualType EleQT = ArrTy->getElementType();
    		llvm::outs() << currentId << ", hasElementTypeClass, ";
    		dumpNestPointerType(EleQT.getTypePtrOrNull(), false);
    		llvm::outs() << "\n";
    		llvm::outs() << currentId << ", hasElementType, " << EleQT.getAsString() << "\n";
    	}
		
    	return true;
    }
    // ---> DeclaratorDecl
    // ---->
    bool VisitFieldDecl(FieldDecl *FD) {
    	// getParent - Returns the parent of this field declaration, 
    	// which is the struct in which this method is defined.
    	//RecordDecl *rd = FD->getParent();
    	//llvm::outs() << currentId << ", hasParent, " << genId(rd) << "\n";
    	int index = FD->getFieldIndex();
    	llvm::outs() << currentId << ", hasIndex, " << index << "\n";
    	//FieldDecl *CD = FD->getCanonicalDecl();
    	return true;
    }
    // ---->
    // @todo child relation
	bool VisitFunctionDecl(FunctionDecl *FD) {
		//FD->dump();
		currentFunction = FD;
		if (FD->isMain()) {
			llvm::outs() << currentId << ", isa, " << "MainFunction\n";
		}
		// isDefined -> bool
		if (FD->hasBody()) {
			Stmt *body = FD->getBody();
			if (body) {
				llvm::outs() << currentId << ", hasBody, " << genId(body) << "\n";
			}
		}
		// iterate params
		for (int i = 0; i < FD->getNumParams(); i++) {
			ParmVarDecl *p = FD->getParamDecl(i);
			llvm::outs() << currentId << ", hasParm(" << i << "), " << genId(p) << "\n";
		}

		QualType QT = FD->getReturnType();
		// llvm::outs() << currentId << ", hasReturnType, " << QT.getAsString() << "\n";
		// FunctionDecl *CFD = FD->getCanonicalDecl();
		return true; 
	}
	// ---->
	bool VisitVarDecl(VarDecl *VDecl) {
		// isStaticLocal() -> bool
		if (VDecl->isLocalVarDecl()) { // Returns true for local variable declarations other than parameters
			llvm::outs() << currentId << ", isa, " << "LocalVarDecl\n";
		}

		VarDecl *CVDecl = VDecl->getCanonicalDecl();
		if (CVDecl) {
			llvm::outs() << currentId << ", hasCanonicalDecl, " << genId(CVDecl) << "\n";
		}
		// hasLocalStorage()
		// isThisDeclarationADefinition()
		VarDecl *VDef = VDecl->getDefinition();
		if (VDef) {
			llvm::outs() << currentId << ", hasDefinition, " << genId(VDef) << "\n";
		}
		// isFileVarDecl() // file scoped variable declaration
		Expr *Init = VDecl->getInit();
		if (Init) {
			llvm::outs() << currentId << ", hasInit, " << genId(Init->IgnoreParenCasts()) << "\n";
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

		// for (auto *B : c->body()) {
		// 	// B is Stmt *
		// }
		return true;
	}
	// ContinueStmt, DoStmt, ForStmt, GotoStmt, IfStmt, LabelStmt,
	// NullStmt, SwitchCase, SwitchStmt, WhileStmt

	bool VisitDeclStmt(DeclStmt *DS) {

		return true;
	}

	bool VisitReturnStmt(ReturnStmt *R) {
		Expr *Ret = R->getRetValue();
		if (Ret) {
			llvm::outs() << currentId << ", returns, " << genId(Ret->IgnoreParenCasts()) << "\n";
		}
		if (currentFunction) {
			llvm::outs() << currentId << ", inProc, " << genId(currentFunction) << "\n";
		}
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
			llvm::outs() << currentId << ", hasBase, " << genId(base->IgnoreParenCasts()) << "\n";
			llvm::outs() << currentId << ", hasIndex, " << genId(idx->IgnoreParenCasts()) << "\n";
		}
		
		return true;
	}

	bool VisitBinaryOperator(BinaryOperator *bop) {
		
		llvm::outs() << currentId << ", hasOperator, " << bop->getOpcodeStr() << "\n";
		Expr *lhs = bop->getLHS()->IgnoreParenCasts();
		Expr *rhs = bop->getRHS()->IgnoreParenCasts();
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
		
		// call graph build
		if (currentFunction) {
			llvm::outs() << currentId << ", inProc, " << genId(currentFunction) << "\n";
		}
		
		FunctionDecl *fdecl = CE->getDirectCallee(); // return dyn_cast_or_null<FunctionDecl>(getCalleeDecl());
		if (fdecl) {
			llvm::outs() << currentId << ", callsFunc, " << genId(fdecl) << "\n";
			DeclarationNameInfo dn_info = fdecl->getNameInfo();
			llvm::outs() << currentId << ", calls, " << dn_info.getAsString() << "\n";
			// if (dn_info.getAsString() == "malloc") {
			// 	llvm::outs() << currentId << ", calls, malloc()\n";
			// }
		}

		int i = 0;
		for (CallExpr::arg_iterator ai = CE->arg_begin(), 
			ae = CE->arg_end(); ai != ae; ++ai) {
			Expr *arg = *ai;
			llvm::outs() << currentId << ", hasArg(" << i << "), " << genId(arg->IgnoreParenCasts()) << "\n";
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
		DeclarationNameInfo DNI = DRE->getNameInfo();
		llvm::outs() << currentId << ", hasName, " << DNI.getAsString() << "\n";
		return true;
	}

	// DesignatedInitExpr C99 designated initializer expr
	// http://clang.llvm.org/doxygen/classclang_1_1DesignatedInitExpr.html#details
	// struct point {
	// 	double x, y;
	// };
	// struct point ptarr[10] = { [2].y = 1.0, [2].x = 2.0, [0].x = 1.0 }
	// The {...} is the InitListExpr, it contains three DesignatedInitExpr
	bool VisitDesignatedInitExpr(DesignatedInitExpr *DIE) {
	// designators() -> designators_range
		return true;
	}

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
			Expr *Init = Inits[i];
			llvm::outs() << currentId << ", hasSubInit(" << i << "), " << genId(Init->IgnoreParenCasts()) << "\n";
		}
		return true;
	}

	// IntegerLiteral

	// C99 6.5.2.3 structure/union members 
	// x->f and x.f
	bool VisitMemberExpr(MemberExpr *ME) {
		Expr *base = ME->getBase()->IgnoreParenCasts();
		llvm::outs() << currentId << ", hasBase, " << genId(base) << "\n";
		ValueDecl *vdecl = ME->getMemberDecl(); // return a FieldDecl
		llvm::outs() << currentId << ", hasMemberDecl, " << genId(vdecl) << "\n";
		return true;
	}

	// OffsetOfExpr
	// http://clang.llvm.org/doxygen/classclang_1_1OffsetOfExpr.html#details

	// a parenthesized expression, e.g. "(1)"
	// bool VisitParenExpr(ParenExpr *PE) {
	// 	Expr * expr = PE->getSubExpr();
	// 	llvm::outs() << currentId << ", hasSubExpr, " << genId(expr) << "\n";
	// 	return true;
	// }

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

	// VisitStringLiteral(StringLiteral *SL) {
		// SL->getBytes() -> StringRef
	// }

	// UnaryOperator - This represents the unary-expression
	// (except sizeof and alignof), the postinc/postdec operators 
	// from postfix-expression, and various extensions.
	bool VisitUnaryOperator(UnaryOperator *UOP) {
		// isPrefix(),isPostfix(),isIncrementOp(),isDecrementOp()
		llvm::outs() << currentId << ", hasOperator, " << UnaryOperator::getOpcodeStr(UOP->getOpcode()) << "\n";
		Expr *sub = UOP->getSubExpr()->IgnoreParenCasts();
		llvm::outs() << currentId << ", hasSubExpr, " << genId(sub) << "\n";

		return true;
	}

	/// Type
	// VisitType(Type *T) 
	// VisitTypeLoc(TypeLoc TL)
private:
	ASTContext *Context;
	std::string currentId;
	FunctionDecl *currentFunction;

	// generate an id based on the location <start:end>
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


// @todo 
// [] add scope info: when traversing, use a global var
// for current scope (function, block, compound stmt, etc)
// build the "inScope" relation
// [] add stmt order info? (next)
