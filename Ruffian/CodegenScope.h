#pragma once

namespace llvm { class AllocaInst; }

//! Handles scoped declarations at codegen time
class CodegenScope {
public:
	//! Initialize
	CodegenScope();
	//! Non-inline destructor
	~CodegenScope();

	//! Looks up a variable, returning NULL if it doesn't exist
	llvm::Value* LookupVariable( const string& strName );

	//! Registers a variable, returns FALSE for a failure if it already exists
	bool RegisterVariable( const string& strName, llvm::Value* pValue );

	//! Sentry for entering/exiting a scope
	class ScopeEnterSentry {
	public:
		//! Initialize ith scope to enter
		ScopeEnterSentry( CodegenScope& scope ) : m_scope(scope) { m_scope.m_scopes.push_back( m_scope.m_scopes.back() ); }
		//! Exit the scope
		~ScopeEnterSentry() { m_scope.m_scopes.pop_back(); ASSERT( !m_scope.m_scopes.empty() ); }
	private:
		CodegenScope& m_scope;
	}; // end class ScopeEnterSentry

	friend class ScopeEnterSentry;
private:
	struct Frame {
		map<string, llvm::Value*> variables;
	}; // end struct Frame

	vector<Frame> m_scopes;		//!< Our existing scopes
}; // end class CodegenScope