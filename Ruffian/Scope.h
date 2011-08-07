#pragma once

class llvm::AllocaInst;

class Scope {
public:
	//! Initialize our empty scope
	Scope() : m_scopes(1), m_pModule(new llvm::Module("module", llvm::getGlobalContext())), m_builder(llvm::getGlobalContext()) {}

	//! Enters a new scope
	void Enter() {
		m_scopes.push_back( m_scopes.back() );
	} // end Enter()
	//! Exits the current scope
	void Exit() {
		if( m_scopes.size() <= 1 ) {
			ASSERT( false );
			return;
		}
		m_scopes.pop_back();
	} // end Exit()

	//! Looks up a variable
	llvm::AllocaInst* LookupVariable( const string& strName ) {
		map<string, llvm::AllocaInst*>& curScope= m_scopes.back().variables;
		map<string, llvm::AllocaInst*>::iterator itVar= curScope.find( strName );
		if( itVar == curScope.end() ) return NULL;
		else return itVar->second;
	} // end LookupVariable()

	//! Registers a variable, returns false for a failure because it already exists
	bool RegisterVariable( const string& strName, llvm::AllocaInst* pAllocaInst ) {
		map<string, llvm::AllocaInst*>& curScope= m_scopes.back().variables;
		if( curScope.find(strName) != curScope.end() ) return false;

		curScope[strName]= pAllocaInst;
		return true;
	} // end RegisterVariable()
	
	/*
	//! Looks up a function
	llvm::Function* LookupFunction( const string& strName ) {
		map<string, llvm::Function*> curScope= m_scopes.back().functions;
		map<string, llvm::Function*>::iterator itFunc= curScope.find( strName );
		if( itFunc == curScope.end() ) return NULL;
		else return itFunc->second;
	} // end LookupFunction()

	//! Registers a function, returns false for a failure because it already exists
	bool RegisterFunction( const string& strName, llvm::Function* pFunction ) {
		map<string, llvm::Function*>& curScope= m_scopes.back().functions;
		if( curScope.find(strName) != curScope.end() ) return false;

		curScope[strName]= pFunction;
		return true;
	} // end RegisterFunction()*/

	//! Gets our module
	llvm::Module* GetModule() { return m_pModule.get(); }
	//! Gets our IR builder
	llvm::IRBuilder<>& GetBuilder() { return m_builder; }
private:
	struct Frame {
		map<string, llvm::AllocaInst*> variables;
		//map<string, llvm::Function*> functions;
	}; // end struct Frame

	vector<Frame> m_scopes;		//!< Our existing scopes

	shared_ptr<llvm::Module> m_pModule;
	llvm::IRBuilder<> m_builder;
}; // end class Scope

//! Sentry for entering/exiting a scope
class ScopeEnterSentry {
public:
	//! Initialize ith scope to enter
	ScopeEnterSentry( Scope& scope ) : m_scope(scope) { m_scope.Enter(); }
	//! Exit the scope
	~ScopeEnterSentry() { m_scope.Exit(); }
private:
	Scope& m_scope;
}; // end class ScopeEnterSentry