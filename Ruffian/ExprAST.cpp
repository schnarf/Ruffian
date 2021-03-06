#include "common.h"
#include "ExprAST.h"
#include "FunctionAST.h"
#include "SemBinop.h"
#include "SemUnaryOp.h"
#include "StmtAST.h"
#include "TypeAST.h"

const string& VariableAST::GetName() const { return m_pDeclaration->GetName(); }
const shared_ptr<const TypeAST>& VariableAST::GetType() const { return m_pDeclaration->GetType(); }

const shared_ptr<const TypeAST>& ArrayRefAST::GetType() const {
	// Get our declaration's type. This must be an array type
	if( shared_ptr<const ArrayTypeAST> pType= dynamic_pointer_cast<const ArrayTypeAST>(GetDeclaration()->GetType()) ) {
		return pType->GetElementType();
	} else {
		ASSERT( false );
		return BuiltinTypeAST::GetError();
	}
} // end ArrayRefAST::GetType()

const shared_ptr<const TypeAST>& BinopAST::GetType() const { return GetBinopType(m_binop, m_pLeft->GetType(), m_pRight->GetType()); }
const shared_ptr<const TypeAST>& PrefixUnaryAST::GetType() const { return GetPrefixUnaryOpType(m_op, m_pExpr->GetType()); }
const shared_ptr<const TypeAST>& PostfixUnaryAST::GetType() const { return GetPostfixUnaryOpType(m_op, m_pExpr->GetType()); }

const shared_ptr<const TypeAST>& IntegerAST::GetType() const { return BuiltinTypeAST::GetLong(); }
const shared_ptr<const TypeAST>& DoubleAST::GetType() const { return BuiltinTypeAST::GetDouble(); }
const shared_ptr<const TypeAST>& BoolAST::GetType() const { return BuiltinTypeAST::GetBool(); }

const shared_ptr<const TypeAST>& CallAST::GetType() const { return m_pPrototype->GetReturnType(); }
