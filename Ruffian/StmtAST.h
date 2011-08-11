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
	//! Initialize with return expression
	ReturnAST( const shared_ptr<ExprAST>& pExpr ) : m_pExpr(pExpr) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<ExprAST> m_pExpr;
}; // end class ReturnAST


class DeclarationAST : public StmtAST {
public:
	//! Initialize with variable name, type, and optional initializer expression
	DeclarationAST( const string& strName, const shared_ptr<TypeAST>& pType, const shared_ptr<ExprAST>& pInitializer= NULL ) :
		m_strName(strName), m_pType(pType), m_pInitializer(pInitializer) {
	} // end DeclarationAST()

	//! Returns our name
	const string& GetName() const { return m_strName; }
	//! Returns our type
	const TypeAST& GetType() const { ASSERT( m_pType ); return *m_pType; }

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	string m_strName;
	shared_ptr<TypeAST> m_pType;
	shared_ptr<ExprAST> m_pInitializer;
}; // end class DeclarationAST


class AssignmentAST : public StmtAST {
public:
	//! Initialize with lhs and rhs
	AssignmentAST( const shared_ptr<VariableAST>& pLeft, const shared_ptr<ExprAST> pRight ) : m_pLeft(pLeft), m_pRight(pRight) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<VariableAST> m_pLeft;
	shared_ptr<ExprAST> m_pRight;
}; // end class AssignmentAST


//! Block AST node, a list of statements
class BlockAST : public StmtAST {
public:
	//! Initialize with our list of statements
	BlockAST( const vector<shared_ptr<StmtAST>>& pStmts ) : m_pStmts(pStmts) {}

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
	ConditionalAST( const shared_ptr<ExprAST>& pCondExpr, const shared_ptr<BlockAST>& pIfStmt, const shared_ptr<BlockAST>& pElseStmt ) :
		m_pCondExpr(pCondExpr), m_pIfStmt(pIfStmt), m_pElseStmt(pElseStmt) { ASSERT( m_pIfStmt != NULL ); }

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<ExprAST> m_pCondExpr;
	shared_ptr<BlockAST> m_pIfStmt;
	shared_ptr<BlockAST> m_pElseStmt;		// not null
}; // end class ConditionalAST


//! Call statement AST node (same as expression, discarding return value)
class CallStmtAST : public StmtAST {
public:
	//! Initialize with call expression
	CallStmtAST( const shared_ptr<CallAST>& pCall ) : m_pCall(pCall) {}

	virtual void Codegen( CodegenContext& context, CodegenScope& scope ) const { return (void)m_pCall->Codegen( context, scope ); }
private:
	shared_ptr<CallAST> m_pCall;
}; // end CallStmtAST