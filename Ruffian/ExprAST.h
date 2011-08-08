#pragma once

#include "Lexer.h"

class DeclarationAST;
class FunctionAST;
class PrototypeAST;
class Scope;
class TypeAST;

//! AST node abstract base for expressions.
//! Expressions evaluate to some value that has a type
class ExprAST {
public:
	virtual ~ExprAST() {}

	//! Returns the type of this expression
	virtual const TypeAST& GetType() const= 0;
	//! Generates code to evaluate the value of this expression
	virtual Value* Codegen( Scope& scope ) const= 0;
}; // end class ExprAST


//! Literal value AST node abstract base
class LiteralAST : public ExprAST {
}; // end class LiteralAST


//! AST node for variables, like "a"
class VariableAST : public ExprAST {
public:
	//! Initialize with declaration
	VariableAST( DeclarationAST* pDeclaration ) : m_pDeclaration(pDeclaration) {}

	//! Returns our variable name
	const string& GetName() const;
	//! Returns our declaration statement AST node
	const DeclarationAST* GetDeclaration() { ASSERT( m_pDeclaration ); return m_pDeclaration; }
	
	virtual const TypeAST& GetType() const;
	virtual Value* Codegen( Scope& scope ) const;
private:
	DeclarationAST* m_pDeclaration;			//!< Points to the declaration statement that initialized us, must be set
}; // end VariableAST


//! Binary operator AST Node
class BinopAST : public ExprAST {
public:
	//! Initialize with operator, LHS, and RHS
	BinopAST( Token binop, ExprAST* pLeft, ExprAST* pRight ) : m_binop(binop), m_pLeft(pLeft), m_pRight(pRight) {}

	virtual const TypeAST& GetType() const;
	virtual Value* Codegen( Scope& scope ) const;
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

	virtual const TypeAST& GetType() const;
	virtual Value* Codegen( Scope& scope ) const;
private:
	int64 m_iValue;
}; // end class IntegerAST


//! Float literal AST node
class FloatAST : public LiteralAST {
public:
	//! Initialize with value
	FloatAST( double fValue ) : m_fValue(fValue) {}

	virtual const TypeAST& GetType() const;
	virtual Value* Codegen( Scope& scope ) const;
private:
	double m_fValue;
}; // end class FloatAST


//! Bool literal AST node
class BoolAST : public LiteralAST {
public:
	//! Initialize with value
	BoolAST( bool bValue ) : m_bValue(bValue) {}

	virtual const TypeAST& GetType() const;
	virtual Value* Codegen( Scope& scope ) const;
private:
	bool m_bValue;
}; // end class BoolAST


//! Function call
class CallAST : public ExprAST {
public:
	//! Initialize with function prototype and argument list
	CallAST( PrototypeAST* pPrototype, const vector<ExprAST*>& pArgs ) : m_pPrototype(pPrototype), m_pArgs(pArgs) {}
	
	virtual const TypeAST& GetType() const;
	virtual Value* Codegen( Scope& scope ) const;
private:
	PrototypeAST* m_pPrototype;
	vector<ExprAST*> m_pArgs;
}; // end class CallAST