#include "common.h"
#include "CodegenScope.h"

using namespace llvm;

//! Initialize
CodegenScope::CodegenScope() :
	m_scopes(1) {
} // end CodegenScope::CodegenScope()


//! Looks up a variable
AllocaInst* CodegenScope::LookupVariable( const string& strName ) {
	map<string, AllocaInst*>& curScope= m_scopes.back().variables;
	map<string, AllocaInst*>::iterator itVar= curScope.find( strName );
	if( itVar == curScope.end() ) return NULL;
	else return itVar->second;
} // end LookupVariable()


//! Registers a variable, returns false for a failure because it already exists
bool CodegenScope::RegisterVariable( const string& strName, AllocaInst* pAllocaInst ) {
	map<string, AllocaInst*>& curScope= m_scopes.back().variables;
	if( curScope.find(strName) != curScope.end() ) return false;

	curScope[strName]= pAllocaInst;
	return true;
} // end RegisterVariable()


//! Non-inline destructor
CodegenScope::~CodegenScope() { }