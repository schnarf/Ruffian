#pragma once

#include "Lexer.h"
#include "SourceLocation.h"

class CodegenContext;
class CodegenScope;
class DeclarationAST;
class PrototypeAST;
class TypeAST;
class IntegerAST;
class DoubleAST;
class LValueAST;

//! AST node abstract base for expressions.
//! Expressions evaluate to some value that has a type
class ExprAST {
public:
  ExprAST( const SourceRange& range ) : m_range(range) {}
	virtual ~ExprAST() {}

	//! Returns the type of this expression
	virtual const shared_ptr<const TypeAST>& GetType() const= 0;
	//! Generates code to evaluate the value of this expression
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const= 0;

	//! Returns whether this is a constant value. We can't prove whether
	//! an arbitrary expression is constant, so we consider an expression
	//! to be constant if it's made up entirely of literals and operators.
	virtual bool IsConstant() const { return false; }

	//! Returns whether this is an lvalue
	bool IsLValue() const;
	//! Tries to cast this to an lvalue. If it is not an lvalue, returns NULL.
	const LValueAST* ToLValue() const;

  //! Returns our source range
  SourceRange GetSourceRange() const { return m_range; }
private:
  const SourceRange m_range;
}; // end class ExprAST


//! Literal value AST node abstract base
class LiteralAST : public ExprAST {
public:
  LiteralAST( const SourceRange& range ) : ExprAST(range) {}
	virtual bool IsConstant() const { return true; }
}; // end class LiteralAST


//! LValue AST node base class. This represents an expression
//! whose address can be taken, so this provides a function
//! to generate code to compute this address
class LValueAST : public ExprAST {
public:
  LValueAST( const SourceRange& range ) : ExprAST(range) {}
	//! Generates code for the address of this expression
	virtual Value* CodegenAddress( CodegenContext& context, CodegenScope& scope ) const= 0;
}; // end class LValueAST

//! Returns whether this is an lvalue
inline bool ExprAST::IsLValue() const { return ToLValue() != NULL; }
//! Tries to cast this to an lvalue. If it is not an lvalue, returns NULL.
inline const LValueAST* ExprAST::ToLValue() const { return dynamic_cast<const LValueAST*>(this); }

//! AST node for variables, like "a"
class VariableAST : public LValueAST {
public:
	//! Initialize with declaration
	VariableAST( const SourceRange& range, const shared_ptr<DeclarationAST>& pDeclaration ) :
      LValueAST(range), m_pDeclaration(pDeclaration) { ASSERT( m_pDeclaration ); }

	//! Returns our variable name
	const string& GetName() const;
	//! Returns our declaration statement AST node
	shared_ptr<const DeclarationAST> GetDeclaration() const { ASSERT( m_pDeclaration ); return m_pDeclaration; }
	
	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;
	virtual Value* CodegenAddress( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<DeclarationAST> m_pDeclaration;			//!< Points to the declaration statement that initialized us, must be set.
}; // end VariableAST


//! Array reference AST node
class ArrayRefAST : public VariableAST {
public:
	//! Initialize with reference to array declaration and index expression
	ArrayRefAST( const SourceRange& range, const shared_ptr<DeclarationAST>& pDeclaration, const shared_ptr<ExprAST>& pIndex ) :
      VariableAST(range, pDeclaration), m_pIndex(pIndex) {}

	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;
	virtual Value* CodegenAddress( CodegenContext& context, CodegenScope& scope ) const;
	virtual bool IsConstant() const { return false; }
private:
	shared_ptr<ExprAST> m_pIndex;
}; // end class ArrayRefAST


//! Binary operator AST node
class BinopAST : public ExprAST {
public:
	//! Initialize with operator, LHS, and RHS
	BinopAST( const SourceRange& range, Token binop, const shared_ptr<ExprAST>& pLeft,
            const shared_ptr<ExprAST>& pRight ) :
    ExprAST(range), m_binop(binop), m_pLeft(pLeft), m_pRight(pRight) {}

	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;
	virtual bool IsConstant() const { return (m_binop == TOKEN_ASSIGN || m_pLeft->IsConstant()) && m_pRight->IsConstant(); }
private:
	Token m_binop;
	shared_ptr<ExprAST> m_pLeft,
	                    m_pRight;
}; // end class BinopAST


//! Prefix unary operator AST node
class PrefixUnaryAST : public ExprAST {
public:
	//! Initialize with operator and operator
	PrefixUnaryAST( const SourceRange& range, Token op, const shared_ptr<ExprAST>& pExpr ) :
      ExprAST(range), m_op(op), m_pExpr(pExpr) {}

	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;
	virtual bool IsConstant() const { return m_pExpr->IsConstant(); }
private:
	Token m_op;
	shared_ptr<ExprAST> m_pExpr;
}; // end class PrefixUnaryAST


//! Postfix unary operator AST node given an expression and token for the operator
class PostfixUnaryAST : public ExprAST {
public:
	//! Initialize with operator and operator
	PostfixUnaryAST( const SourceRange& range, Token op, const shared_ptr<ExprAST>& pExpr ) : ExprAST(range), m_op(op), m_pExpr(pExpr) {}

	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;
	virtual bool IsConstant() const { return m_pExpr->IsConstant(); }
private:
	Token m_op;
	shared_ptr<ExprAST> m_pExpr;
}; // end class PostfixUnaryAST


//! Integer literal AST node
class IntegerAST : public LiteralAST {
public:
	//! Initialize with string representation of value
	IntegerAST( const SourceRange& range, const string& strValue ) : LiteralAST(range), m_apValue(64, strValue, 10) {}
	//! Initialize with signed integer value
	IntegerAST( const SourceRange& range, int64 iValue ) : LiteralAST(range), m_apValue(64, iValue, true) {}
	//! Initialize with llvm arbitrary precision value
	IntegerAST( const SourceRange& range, const llvm::APInt& apValue ) : LiteralAST(range), m_apValue(apValue) {}

	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;

	//! Returns our value
	const llvm::APInt& GetValue() const { return m_apValue; }
private:
	llvm::APInt m_apValue;
}; // end class IntegerAST


//! Float literal AST node
class DoubleAST : public LiteralAST {
public:
	//! Initialize with string representation of value
	DoubleAST( const SourceRange& range, const string& strValue ) : LiteralAST(range), m_apValue(llvm::APFloat::IEEEdouble, strValue) {}
	//! Initialize with value
	DoubleAST( const SourceRange& range, double fValue ) : LiteralAST(range), m_apValue(fValue) {}
	//! Initialize with llvm arbitrary precision value
	DoubleAST( const SourceRange& range, const llvm::APFloat& apValue ) : LiteralAST(range), m_apValue(apValue) {}

	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;

	//! Returns our value
	const llvm::APFloat& GetValue() const { return m_apValue; }
private:
	llvm::APFloat m_apValue;
}; // end class DoubleAST


//! Bool literal AST node
class BoolAST : public LiteralAST {
public:
	//! Initialize with string representation of value
	BoolAST( const SourceRange& range, const string& strValue ) :
    LiteralAST(range) {
		if( strValue == "true" ) m_bValue= true;
		else if( strValue == "false" ) m_bValue= false;
		else { ASSERT( false ); m_bValue= false;}
	} // end BoolAST()
	//! Initialize with value
	BoolAST( const SourceRange& range, bool bValue ) : LiteralAST(range), m_bValue(bValue) {}

	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	bool m_bValue;
}; // end class BoolAST


//! Function call
class CallAST : public ExprAST {
public:
	//! Initialize with function prototype and argument list
	CallAST( const SourceRange& range, const shared_ptr<PrototypeAST>& pPrototype, const vector<shared_ptr<ExprAST>>& pArgs ) :
      ExprAST(range), m_pPrototype(pPrototype), m_pArgs(pArgs) {}
	
	virtual const shared_ptr<const TypeAST>& GetType() const;
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;
private:
	shared_ptr<PrototypeAST> m_pPrototype;				//!< Non-owning pointer
	vector<shared_ptr<ExprAST>> m_pArgs;
}; // end class CallAST


//! Casts an expression to another type
class CastAST : public ExprAST {
public:
	//! Initialize with the expression and target type
	CastAST( const SourceRange& range, const shared_ptr<ExprAST>& pExpr, const shared_ptr<const TypeAST>& pType ) :
      ExprAST(range), m_pExpr(pExpr), m_pType(pType) {}

	virtual const shared_ptr<const TypeAST>& GetType() const { return m_pType; }
	virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const;
	virtual bool IsConstant() const { return m_pExpr->IsConstant(); }
private:
	shared_ptr<ExprAST> m_pExpr;
	shared_ptr<const TypeAST> m_pType;
}; // end class CastAST


//! Array size AST node
class ArraysizeAST : public ExprAST {
public:
  //! Initialize with the source range and expression
  ArraysizeAST( const SourceRange& range, const shared_ptr<ExprAST>& pExpr ) : ExprAST(range), m_pExpr(pExpr) {}

  virtual const shared_ptr<const TypeAST>& GetType() const { return m_pExpr->GetType(); }
  virtual Value* Codegen( CodegenContext& context, CodegenScope& scope ) const { return m_pExpr->Codegen( context, scope ); }
	virtual bool IsConstant() const { return m_pExpr->IsConstant(); }
private:
  shared_ptr<ExprAST> m_pExpr;
}; // end class ArraysizeAST