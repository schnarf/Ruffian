#include "common.h"
#include "ExprAST.h"
#include "FunctionAST.h"
#include "SemBinop.h"
#include "StmtAST.h"
#include "TypeAST.h"

const string& VariableAST::GetName() const { return m_pDeclaration->GetName(); }
const TypeAST& VariableAST::GetType() const { return m_pDeclaration->GetType(); }

const TypeAST& BinopAST::GetType() const { return GetBinopType(m_binop, m_pLeft->GetType(), m_pRight->GetType()); }

const TypeAST& IntegerAST::GetType() const { return TypeAST::GetInt(); }
const TypeAST& FloatAST::GetType() const { return TypeAST::GetFloat(); }
const TypeAST& BoolAST::GetType() const { return TypeAST::GetBool(); }

const TypeAST& CallAST::GetType() const { return m_pPrototype->GetReturnType(); }