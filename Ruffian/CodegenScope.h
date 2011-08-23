#pragma once

namespace llvm { class Value; }
class CodegenContext;
class ReturnAST;
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

	//! Adds a pointer to the free list
	void AddToFreeList( llvm::Value* pPtr );
	
	//! Sets our return statement, if we have one to emit
	void SetReturn( const shared_ptr<ReturnAST>& pReturn ) { m_pReturn= pReturn; }

	//! Sentry for entering/exiting a scope
	class ScopeEnterSentry {
	public:
		//! Initialize ith scope to enter
		ScopeEnterSentry( CodegenContext& context, CodegenScope& scope ) : m_context(context), m_scope(scope) {
			if( m_scope.m_scopes.empty() ) m_scope.m_scopes.push_back( CodegenScope::Frame() );
			else m_scope.m_scopes.push_back( m_scope.m_scopes.back() );
			m_scope.m_freelists.push_back( CodegenScope::FreeList() );
		}
		
		//! Exit the scope
		~ScopeEnterSentry();
	private:
		CodegenContext& m_context;
		CodegenScope& m_scope;
	}; // end class ScopeEnterSentry

	friend class ScopeEnterSentry;
private:
	struct Frame {
		map<string, llvm::Value*> variables;
	}; // end struct Frame

	vector<Frame> m_scopes;		//!< Our existing scopes

	struct FreeList {
		vector<llvm::Value*> pFrees;
	}; // end struct FreeList

	vector<FreeList> m_freelists;

	shared_ptr<ReturnAST> m_pReturn;
}; // end class CodegenScope