#pragma once

class CodegenContext;
class ModuleAST;

class Codegen {
public:
	//! Initialize
	Codegen();
	//! Non-inline destructor
	~Codegen();

	//! Generates code for the given module, returning TRUE on success or FALSE on failure
	bool Run( ModuleAST* pModule );
private:
	unique_ptr<CodegenContext> m_pContext;
}; // end class Codegen