#pragma once

#include "FunctionAST.h"

//! Module root AST node
class ModuleAST {
public:
	//! Initialize with a list of function prototypes and function definitions
	ModuleAST( const vector<shared_ptr<PrototypeAST>>& pPrototypes, const vector<shared_ptr<FunctionAST>>& pFunctions ) : m_pPrototypes(pPrototypes), m_pFunctions(pFunctions) {}

	//! Runs code generation for this module, returning TRUE on success and FALSE on failure
	bool Codegen( CodegenContext& context, CodegenScope& scope );

private:
	vector<shared_ptr<PrototypeAST>> m_pPrototypes;
	vector<shared_ptr<FunctionAST>> m_pFunctions;
}; // end class ModuleAST