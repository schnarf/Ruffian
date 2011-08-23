#include "common.h"
#include "CodegenScope.h"
#include "CodegenContext.h"
#include "StmtAST.h"

using namespace llvm;

//! Initialize
CodegenScope::CodegenScope() :
	m_scopes(),
	m_freelists() {
} // end CodegenScope::CodegenScope()


//! Looks up a variable
Value* CodegenScope::LookupVariable( const string& strName ) {
	map<string, Value*>& curScope= m_scopes.back().variables;
	map<string, Value*>::iterator itVar= curScope.find( strName );
	if( itVar == curScope.end() ) return NULL;
	else return itVar->second;
} // end CodegenScope::LookupVariable()


//! Registers a variable, returns false for a failure because it already exists
bool CodegenScope::RegisterVariable( const string& strName, Value* pValue ) {
	map<string, Value*>& curScope= m_scopes.back().variables;
	if( curScope.find(strName) != curScope.end() ) return false;

	curScope[strName]= pValue;
	return true;
} // end CodegenScope::RegisterVariable()


//! Adds a pointer to the free list
void CodegenScope::AddToFreeList( llvm::Value* pPtr ) {
	if( m_freelists.empty() ) { ASSERT(false); return; }

	ASSERT( pPtr->getType()->isPointerTy() );
	m_freelists.back().pFrees.push_back( pPtr );
} // end CodegenScope::AddToFreeList()


//! Non-inline destructor
CodegenScope::~CodegenScope() { }

//! Exit the scope
CodegenScope::ScopeEnterSentry::~ScopeEnterSentry() {
	// Lookup free()
	Function* pFreeFunction= m_context.GetModule()->getFunction( "free" );

	// Generate free instructions
	vector<Value*>& freelist= m_scope.m_freelists.back().pFrees;
	for( ; !freelist.empty(); freelist.pop_back() ) {
		Value* pPtr= freelist.back();

		// Cast to i8*
		pPtr= m_context.GetBuilder().CreatePointerCast( pPtr, llvm::Type::getInt8PtrTy(getGlobalContext()), "free_cast" );

		// Generate the call
		m_context.GetBuilder().CreateCall( pFreeFunction, pPtr );
	} // end while not empty

	// If we have a return statement, emit it now
	if( m_scope.m_pReturn ) m_scope.m_pReturn->Codegen( m_context, m_scope );
	m_scope.m_pReturn.reset();

	m_scope.m_freelists.pop_back();
	m_scope.m_scopes.pop_back();
} // end ScopeEnterSentry::~ScopeEnterSentry()