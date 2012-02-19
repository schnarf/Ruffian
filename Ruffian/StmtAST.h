#pragma once

#include "ExprAST.h"
#include "TypeAST.h"

class CodegenContext; class CodegenScope;

//! Statement AST node base class. Statements may have an effect,
//! but do not return any value.
class StmtAST {
public:
  StmtAST( const SourceRange& range ) : m_range(range) {}
	virtual ~StmtAST() {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const= 0;

	//! Returns whether this generates code that has a return in the outermost basic block
	bool HasReturn() const;

  //! Returns our source range
  SourceRange GetSourceRange() const { return m_range; }
private:
  const SourceRange m_range;
}; // end class StmtAST


//! Primary statement AST node base class. This is either a variable declaration or expression
class PrimaryStmtAST : public StmtAST {
public:
  PrimaryStmtAST( const SourceRange& range ) : StmtAST(range) {}
}; // end class PrimaryStmtAST


class ReturnAST : public StmtAST {
public:
	//! Initialize with return expression
	ReturnAST( const SourceRange& range, const shared_ptr<ExprAST>& pExpr ) : StmtAST(range), m_pExpr(pExpr) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<ExprAST> m_pExpr;
}; // end class ReturnAST


class DeclarationAST : public PrimaryStmtAST {
public:
	//! Initialize with variable name, type, and optional initializer expression
	DeclarationAST( const SourceRange& range, const string& strName, const shared_ptr<const TypeAST>& pType, const shared_ptr<ExprAST>& pInitializer= NULL ) :
		PrimaryStmtAST(range), m_strName(strName), m_pType(pType), m_pInitializer(pInitializer) {
	} // end DeclarationAST()

	//! Returns our name
	const string& GetName() const { return m_strName; }
	//! Returns our type
	const shared_ptr<const TypeAST>& GetType() const { ASSERT( m_pType ); return m_pType; }
  //! Returns whether we have an initializer
  bool HasInitializer() const { return m_pInitializer; }

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	string m_strName;
	shared_ptr<const TypeAST> m_pType;
	shared_ptr<ExprAST> m_pInitializer;
}; // end class DeclarationAST


//! Block AST node, a list of statements
class BlockAST : public StmtAST {
public:
	//! Initialize with our list of statements
	BlockAST( const SourceRange& range, const vector<shared_ptr<StmtAST>>& pStmts ) : StmtAST(range), m_pStmts(pStmts) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	friend class StmtAST;
	vector<shared_ptr<StmtAST>> m_pStmts;
}; // end class BlockAST


//! Conditional "if" AST node
class ConditionalAST : public StmtAST {
public:
	//! Initialize with condition, true statement, and optional else statement
	ConditionalAST( const SourceRange& range, const shared_ptr<ExprAST>& pCondExpr, const shared_ptr<StmtAST>& pIfStmt, const shared_ptr<StmtAST>& pElseStmt ) :
		StmtAST(range), m_pCondExpr(pCondExpr), m_pIfStmt(pIfStmt), m_pElseStmt(pElseStmt) { ASSERT( m_pIfStmt != NULL ); }

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<ExprAST> m_pCondExpr;
	shared_ptr<StmtAST> m_pIfStmt;
	shared_ptr<StmtAST> m_pElseStmt;		// not null
}; // end class ConditionalAST


//! For loop AST node
class ForAST : public StmtAST {
public:
	//! Initialize with initializer statement, condition, and update expressions, plus the body statement
	ForAST( const SourceRange& range, const shared_ptr<PrimaryStmtAST>& pInitializer, const shared_ptr<ExprAST>& pCondition, const shared_ptr<ExprAST>& pUpdate, const shared_ptr<StmtAST>& pBody ) :
		StmtAST(range), m_pInitializer(pInitializer), m_pCondition(pCondition), m_pUpdate(pUpdate), m_pBody(pBody) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<PrimaryStmtAST> m_pInitializer;
	shared_ptr<ExprAST> m_pCondition,
						m_pUpdate;
	shared_ptr<StmtAST> m_pBody;
}; // end class ForAST


//! Range for loop AST node
class ForRangeAST : public StmtAST {
public:
  //! Initialize with declaration, variable, begin, and end, plus the body statement
  ForRangeAST( const SourceRange& range, const shared_ptr<DeclarationAST>& pDeclaration, const shared_ptr<VariableAST>& pVariable,
               const shared_ptr<ExprAST>& pBegin, const shared_ptr<ExprAST>& pEnd, const shared_ptr<StmtAST>& pBody ) :
    StmtAST(range),
    m_pDeclaration(pDeclaration), m_pVariable(pVariable),
    m_pBegin(pBegin), m_pEnd(pEnd), m_pBody(pBody) {}
  //! Initialize with variable, begin, and end, plus the body statement
  ForRangeAST( const SourceRange& range, const shared_ptr<VariableAST>& pVariable, const shared_ptr<ExprAST>& pBegin,
               const shared_ptr<ExprAST>& pEnd, const shared_ptr<StmtAST>& pBody ) :
    StmtAST(range), m_pDeclaration(), m_pVariable(pVariable), m_pBegin(pBegin), m_pEnd(pEnd), m_pBody(pBody) {}
  
  virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
  shared_ptr<DeclarationAST> m_pDeclaration;    //!< Variable declaration. NULL if no variable declared
  shared_ptr<VariableAST> m_pVariable;          //!< Our loop variable
  shared_ptr<ExprAST> m_pBegin,
                      m_pEnd;
  shared_ptr<StmtAST> m_pBody;
}; // end class ForRangeAST

//! While loop AST node
class WhileAST : public StmtAST {
public:
	//! Initialize with condition and body
	WhileAST( const SourceRange& range, const shared_ptr<ExprAST>& pCondition, const shared_ptr<StmtAST>& pBody ) :
    StmtAST(range), m_pCondition(pCondition), m_pBody(pBody) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<ExprAST> m_pCondition;
	shared_ptr<StmtAST> m_pBody;
}; // end class WhileAST


//! Expression statement AST Node
//! Just an expression with a semicolon after it
class ExprStmtAST : public PrimaryStmtAST {
public:
	//! Initialize with expression
	ExprStmtAST( const SourceRange& range, const shared_ptr<ExprAST>& pExpr ) : PrimaryStmtAST(range), m_pExpr(pExpr) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
  shared_ptr<ExprAST> GetExpr() { return m_pExpr; }
private:
	shared_ptr<ExprAST> m_pExpr;
}; // end class ExprStmtAST


//! Returns whether this generates code that has a return in the outermost basic block
inline bool StmtAST::HasReturn() const {
	if( dynamic_cast<const ReturnAST*>(this) ) return true;
	const BlockAST* pBlock= dynamic_cast<const BlockAST*>(this);
	if( pBlock ) {
		for( uint i=0; i<pBlock->m_pStmts.size(); ++i ) {
			if( dynamic_cast<const ReturnAST*>(pBlock->m_pStmts[i].get()) ) return true;
		} // end for statement
	} // end if block

	return false;
} // end StmtAST::HasReturn()
