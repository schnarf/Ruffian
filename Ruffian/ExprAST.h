#pragma once

#include "Lexer.h"

class ExprAST {
public:
	virtual ~ExprAST() {}
}; // end class ExprAST

//! Primary expression AST node, a single full statement that has some effect
class PrimaryExprAST : public ExprAST {
public:
}; // end class PrimaryExprAST

//! Literal value AST node
class LiteralAST : public ExprAST { };

class ReturnAST : public PrimaryExprAST {
public:
	//! Initialize with return expression
	ReturnAST( ExprAST* pExpr ) : m_pExpr(pExpr) {}
private:
	ExprAST* m_pExpr;
}; // end class ReturnAST

//! AST node for variables, like "a"
class VariableAST : public ExprAST {
public:
	//! Initialize with variable name
	VariableAST( const string& strName ) : m_strName(strName) {}
private:
	string m_strName;
}; // end VariableAST

//! AST node for types, like "int" or a user-defined type
class TypeAST : public ExprAST {
public:
	//! Initialize with type name
	TypeAST( const string& strType ) : m_strType(strType) {}
private:
	string m_strType;
}; // end class TypeAST

class DeclarationAST : public PrimaryExprAST {
public:
	//! Initialize with variable name, type, and optional initializer expression
	DeclarationAST( VariableAST* pVariable, TypeAST* pType, ExprAST* pInitializer= NULL ) :
	  m_pVariable(pVariable), m_pType(pType), m_pInitializer(pInitializer) {}
private:
	VariableAST* m_pVariable;
	TypeAST* m_pType;
	ExprAST* m_pInitializer;
}; // end class DeclarationAST

class AssignmentAST : public PrimaryExprAST {
public:
	//! Initialize with lhs and rhs
	AssignmentAST( VariableAST* pLeft, ExprAST* pRight ) : m_pLeft(pLeft), m_pRight(pRight) {}
private:
	VariableAST* m_pLeft;
	ExprAST* m_pRight;
}; // end class AssignmentAST

//! Block AST node, a list of expressions
class BlockAST : public PrimaryExprAST {
public:
	BlockAST( const vector<PrimaryExprAST*>& pExprs ) : m_pExprs(pExprs) {}
private:
	vector<PrimaryExprAST*> m_pExprs;
}; // end class BlockAST

//! Empty expression AST node
class EmptyAST : public ExprAST { };

//! Binary operator AST Node
class BinopAST : public ExprAST {
public:
	//! Initialize with operator, LHS, and RHS
	BinopAST( Token binop, ExprAST* pLeft, ExprAST* pRight ) : m_binop(binop), m_pLeft(pLeft), m_pRight(pRight) {}
private:
	Token m_binop;
	ExprAST* m_pLeft,
	       * m_pRight;
}; // end class BinopAST

//! Integer literal AST node
class IntegerAST : public LiteralAST {
public:
	//! Initialize with value
	IntegerAST( int64 iValue ) : m_iValue(iValue) {}
private:
	int64 m_iValue;
}; // end class IntegerAST

//! Float literal AST node
class FloatAST : public LiteralAST {
public:
	//! Initialize with value
	FloatAST( double fValue ) : m_fValue(fValue) {}
private:
	double m_fValue;
}; // end class FloatAST

//! Conditional "if" AST Node
class ConditionalAST : public PrimaryExprAST {
public:
	//! Initialize with condition, true block, and optional else block
	ConditionalAST( ExprAST* pCondExpr, PrimaryExprAST* pIfExpr, PrimaryExprAST* pElseExpr ) :
		m_pCondExpr(pCondExpr), m_pIfExpr(pIfExpr), m_pElseExpr(pElseExpr) { ASSERT( m_pIfExpr != NULL ); }
private:
	ExprAST* m_pCondExpr;
	PrimaryExprAST* m_pIfExpr;
	PrimaryExprAST* m_pElseExpr;		// not null
}; // end class ConditionalAST