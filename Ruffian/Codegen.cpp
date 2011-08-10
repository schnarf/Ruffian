#include "common.h"
#include "Codegen.h"
#include "CodegenContext.h"
#include "CodegenScope.h"
#include "ModuleAST.h"

//! Initialize
Codegen::Codegen() :
	m_pContext(new CodegenContext) {
} // end Codegen::Codegen()


//! Generates code for the given module, returning TRUE on success or FALSE on failure
bool Codegen::Run( ModuleAST* pModule ) {
	// Create a new scope
	CodegenScope scope;

	// Codegen the module
	return pModule->Codegen( *m_pContext, scope );
} // end Codegen::Run()


//! Non-inline destructor
Codegen::~Codegen() { }