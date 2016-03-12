
//#define GETLOC(s) clang::FullSourceLoc FL = Context->getFullLoc((s)->getLocStart()); \
		//if (FL.isValid()) {llvm::errs() << FL.getSpellingLineNumber() << ":" << FL.getSpellingColumnNumber() << "\n";}

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
		llvm::errs() << currentId << " isa " << d->getDeclKindName() << "\n";
		return true;
	}

	bool VisitNamedDecl(NamedDecl *n) {
		llvm::errs() << currentId << " hasName " << n->getName() << "\n";
		return true;
	}

    // bool VisitRecordDecl(RecordDecl *r);
    bool VisitTypedefDecl(TypedefDecl *t) {
		// getUnderlyingType()
		return true;
	}

    bool VisitValueDecl(ValueDecl *vd) {
    	QualType qt = vd->getType();
    	const Type *ty = qt.getTypePtrOrNull();
    	// llvm::errs() << "<debug> \n";
    	// qt.dump();
    	
    	int nestptr = 0;
    	llvm::errs() << currentId << " hasType ";
    	dumpNestedType(ty, false);
    	llvm::errs() << "\n";
		// llvm::errs() << "</debug> \n";

    	// llvm::errs() << currentId << " hasType " << qt.getAsString() << "\n";

    	return true;
    }

    bool VisitFieldDecl(FieldDecl *fd) {
    	RecordDecl *rd = fd->getParent();

    	llvm::errs() << currentId << " hasBase " << genId(rd) << "\n";
    	return true;
    }

	bool VisitFunctionDecl(FunctionDecl *f) {
		//f->dump();
		if (f->isMain()) {
			llvm::errs() << currentId << " is " << "MainFunction\n";
		}

		if (f->hasBody()) {
			Stmt *body = f->getBody();
			if (body) {
				llvm::errs() << currentId << " hasBody " << genId(body) << "\n";
			}
		}
		// iterate params
		for (int i = 0; i < f->getNumParams(); i++) {
			ParmVarDecl *p = f->getParamDecl(i);
			llvm::errs() << currentId << " hasParm(" << i << ") " << genId(p) << "\n";
		}

		QualType qt = f->getReturnType();
		// llvm::errs() << currentId << " hasReturnType " << qt.getAsString() << "\n";
		return true; 
	}

	bool VisitVarDecl(VarDecl *v) {
		if (v->isLocalVarDecl()) {
			llvm::errs() << currentId << " isa " << "LocalVarDecl\n";
		}

		VarDecl *cv = v->getCanonicalDecl();
		if (cv) {
			llvm::errs() << currentId << " hasCanonicalDecl " << genId(cv) << "\n";
		}
		// hasLocalStorage()
		// isStaticLocal()
		// isThisDeclarationADefinition()
		// getDefinition()
		// isFileVarDecl()
		// getInit()
		return true;
	}

	// bool VisitParmVarDecl(ParmVarDecl *pd) {
	// 	llvm::errs() << currentId << " hasType " << QualType::getAsString(pd->getType().split()) << "\n";
	// 	return true;
	// }

	/// Stmt
	bool VisitStmt(Stmt *s) {
		currentId = genId(s);
		//s->dump();
		llvm::errs() << currentId << " isa " << s->getStmtClassName() << "\n";
				
		return true;
	}

	bool VisitBreakStmt(BreakStmt *b) {
		return true;
	}

	bool VisitCompoundStmt(CompoundStmt *c) {
		return true;
	}
	// ContinueStmt, DeclStmt, DoStmt, ForStmt, GotoStmt, IfStmt, LabelStmt,
	// NullStmt, SwitchCase, SwitchStmt, WhileStmt

	bool VisitReturnStmt(ReturnStmt *r) {
		return true;
	}

	/// Expr
	bool VisitArraySubscriptExpr(ArraySubscriptExpr *sub) {
		return true;
	}

	bool VisitBinaryOperator(BinaryOperator *bop) {
		Expr *lhs = bop->getLHS();
		Expr *rhs = bop->getRHS();
		llvm::errs() << currentId << "hasOperator '" << BinaryOperator::getOpcodeStr(bop->getOpcode()) << "'\n";
		llvm::errs() << currentId << " hasLHS " << genId(lhs) << "\n";
		llvm::errs() << currentId << " hasRHS " << genId(rhs) << "\n";

		if (llvm::isa<CompoundAssignOperator>(bop)) {
			// e.g., += 
			CompoundAssignOperator *caop = llvm::cast<CompoundAssignOperator>(bop);
			// ... skip
		}
		return true;
	}
	// ConditionalOperator ?: 

	bool VisitCallExpr(CallExpr *callexp) {
		Expr *callee = callexp->getCallee();
		llvm::errs() << currentId << " call " << genId(callee) << "\n";

		Decl *decl = callexp->getCalleeDecl(); // ?
		

		FunctionDecl *fdecl = callexp->getDirectCallee();
		if (fdecl) {
			llvm::errs() << currentId << " callFunc " << genId(fdecl) << "\n";
		}

		int i = 0;
		for (CallExpr::arg_iterator ai = callexp->arg_begin(), 
			ae = callexp->arg_end(); ai != ae; ++ai) {
			Expr *arg = *ai;
			llvm::errs() << currentId << " hasArg(" << i << ") " << genId(arg) << "\n";
			i++;
		}

		return true;
	}

	bool VisitCastExpr(CastExpr *cast) {
		Expr *sub = cast->getSubExpr();
		llvm::errs() << currentId << " hasSubExpr " << genId(sub) << "\n";
		return true;
	}

	// CompoundLiteralExpr C99 6.5.2.5

	bool VisitDeclRefExpr(DeclRefExpr *dr) {
		ValueDecl *vd = dr->getDecl();
		if (vd)	llvm::errs() << currentId << " referTo " << genId(vd) << "\n";
		return true;
	}

	// DesignatedInitExpr C99 designated initializer expr
	// http://clang.llvm.org/doxygen/classclang_1_1DesignatedInitExpr.html#details

	// bool VisitFloatingLiteral(FloatingLiteral *fl) {
	// 	llvm::APFloat f = fl->getValue();
	// 	return true;
	// }

	bool VisitInitListExpr(InitListExpr *init) {
		return true;
	}

	// IntegerLiteral

	// C99 6.5.2.3 structure/union members x->f and x.f
	bool VisitMemberExpr(MemberExpr *mb) {
		Expr *base = mb->getBase();
		llvm::errs() << currentId << " hasBase " << genId(base) << "\n";
		ValueDecl *vdecl = mb->getMemberDecl(); // return a FieldDecl
		llvm::errs() << currentId << " referTo " << genId(vdecl) << "\n";
		return true;
	}

	// OffsetOfExpr
	// http://clang.llvm.org/doxygen/classclang_1_1OffsetOfExpr.html#details

	// a parenthesized expression, e.g. "(1)"
	bool VisitParenExpr(ParenExpr *paren) {
		// Expr * exp = paren->getSubExpr();
		return true;
	}

	// ParenListExpr ?

	// StringLiteral
		// getBytes() -> StringRef

	bool VisitUnaryOperator(UnaryOperator *uop) {
		// isPrefix()
		// isPostfix()
		// isIncrementOp()
		// isDecrementOp()
		llvm::errs() << currentId << " hasOperator '" << UnaryOperator::getOpcodeStr(uop->getOpcode()) << "'\n";
		Expr *sub = uop->getSubExpr();
		llvm::errs() << currentId << " hasSubExpr " << genId(sub) << "\n";

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
		return std::string(d->getDeclKindName()) + "<" + getLoc<Decl>(d) + ">";
	}

	std::string genId(Stmt *s) {
		return std::string(s->getStmtClassName()) + "<" + getLoc<Stmt>(s) + ">";
	}

	void dumpNestedType(const Type *T, bool comma) {
		if (!comma) {
			llvm::errs() << "[";
		}
		if (!T) {
			llvm::errs() << "]";
			return;
		}
		if (comma) {
			llvm::errs() << ",";
		}
		if (T->isPointerType()) {
			llvm::errs() << "pointerType";
			QualType ptee = T->getPointeeType();
			const Type *ptty = ptee.getTypePtrOrNull();
			dumpNestedType(ptty, true);
		} else {
			llvm::errs() << T->getTypeClassName() <<"]";
		}
	}
};
