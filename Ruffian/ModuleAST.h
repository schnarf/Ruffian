#pragma once

class PrototypeAST; class FunctionAST;

//! Module root AST node
class ModuleAST {
public:
	//! Initialize with a list of function prototypes and function definitions
	ModuleAST( const vector<PrototypeAST*>& pPrototypes, const vector<FunctionAST*>& pFunctions ) : m_pPrototypes(pPrototypes), m_pFunctions(pFunctions) {}

	//! Runs code generation for this module, returning TRUE on success and FALSE on failure
	bool Codegen( CodegenContext& context, CodegenScope& scope );

private:
	vector<PrototypeAST*> m_pPrototypes;
	vector<FunctionAST*> m_pFunctions;
}; // end class ModuleAST