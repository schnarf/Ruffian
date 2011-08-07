#pragma once

#include "ExprAST.h"

//! Function declaration
class FunctionAST : public ExprAST {
public:
	//! Initialize with function name, return type, argument list
	FunctionAST( const string& strName, TypeAST* pReturnType, const vector< pair<TypeAST*, VariableAST*> >& pArgs, ExprAST* pBody ) : m_strName(strName), m_pReturnType(pReturnType), m_pArgs(pArgs), m_pBody(pBody) {}

	virtual Value* Codegen( Scope& scope );
private:
	string m_strName;
	TypeAST* m_pReturnType;
	vector< pair<TypeAST*, VariableAST*> > m_pArgs;
	ExprAST* m_pBody;
}; // end class FunctionAST


//! Function call
class CallAST : public ExprAST {
public:
	//! Initialize with function name and argument list
	CallAST( const string& strName, const vector<ExprAST*>& pArgs ) : m_strName(strName), m_pArgs(pArgs) {}

	virtual Value* Codegen( Scope& scope );
private:
	string m_strName;
	vector<ExprAST*> m_pArgs;
}; // end class CallAST