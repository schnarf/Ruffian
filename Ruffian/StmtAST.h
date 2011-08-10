#pragma once

#include "ExprAST.h"
#include "TypeAST.h"

class CodegenContext; class CodegenScope;

//! Statement AST node base class. Statements may have an effect,
//! but do not return any value.
class StmtAST {
public:
	virtual ~StmtAST() {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const= 0;
}; // end class StmtAST


class ReturnAST : public StmtAST {
public:
	//! Initialize, taking ownership of return expression
	ReturnAST( ExprAST* pExpr ) : m_pExpr(pExpr) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	unique_ptr<ExprAST> m_pExpr;
}; // end class ReturnAST


class DeclarationAST : public StmtAST {
public:
	//! Initialize with variable name, type, and optional initializer expression
	//! Takes ownership of type and initializer expression
	DeclarationAST( const string& strName, TypeAST* pType, ExprAST* pInitializer= NULL ) :
		m_strName(strName), m_pType(pType), m_pInitializer(pInitializer) {
	} // end DeclarationAST()

	//! Returns our name
	const string& GetName() const { return m_strName; }
	//! Returns our type
	const TypeAST& GetType() const { ASSERT( m_pType ); return *m_pType; }

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	string m_strName;
	unique_ptr<TypeAST> m_pType;
	unique_ptr<ExprAST> m_pInitializer;
}; // end class DeclarationAST


class AssignmentAST : public StmtAST {
public:
	//! Initialize, taking ownership of lhs and rhs
	AssignmentAST( VariableAST* pLeft, ExprAST* pRight ) : m_pLeft(pLeft), m_pRight(pRight) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	unique_ptr<VariableAST> m_pLeft;
	unique_ptr<ExprAST> m_pRight;
}; // end class AssignmentAST


//! Block AST node, a list of statements
class BlockAST : public StmtAST {
public:
	//! Initialize, taking ownership of each statement
	BlockAST( const vector<StmtAST*>& pStmts ) : m_pStmts(pStmts.size()) {
		for( uint iStmt=0; iStmt<pStmts.size(); ++iStmt ) m_pStmts[iStmt].reset( pStmts[iStmt] );
	} // end BlockAST()

	//! Returns true if any of the statements in this block are a return statement
	bool HasReturn() const;

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	vector<shared_ptr<StmtAST>> m_pStmts;
}; // end class BlockAST


//! Conditional "if" AST Node
class ConditionalAST : public StmtAST {
public:
	//! Initialize with condition, true block, and optional else block
	// Takes ownership of all
	ConditionalAST( ExprAST* pCondExpr, BlockAST* pIfStmt, BlockAST* pElseStmt ) :
		m_pCondExpr(pCondExpr), m_pIfStmt(pIfStmt), m_pElseStmt(pElseStmt) { ASSERT( m_pIfStmt != NULL ); }

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	unique_ptr<ExprAST> m_pCondExpr;
	unique_ptr<BlockAST> m_pIfStmt;
	unique_ptr<BlockAST> m_pElseStmt;		// not null
}; // end class ConditionalAST