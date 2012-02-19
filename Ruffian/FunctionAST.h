#pragma once

#include "StmtAST.h"
#include "TypeAST.h"

class CodegenContext; class CodegenScope;

class BlockAST;
class DeclarationAST;
class VariableAST;

//! Function prototype
class PrototypeAST {
public:
	//! Initialize with function name, return type, and argument list
	PrototypeAST( const SourceRange& range, const string& strName, const shared_ptr<const TypeAST>& pReturnType, const vector<shared_ptr<DeclarationAST>>& pArgs ) : m_range(range), m_strName(strName), m_pReturnType(pReturnType), m_pArgs(pArgs) {}

	//! Returns our name
	const string& GetName() const { return m_strName; }
	//! Returns our arguments
	const vector<shared_ptr<DeclarationAST>>& GetArgs() const { return m_pArgs; }
	//! Returns our return type
	const shared_ptr<const TypeAST>& GetReturnType() const { ASSERT( m_pReturnType ); return m_pReturnType; }

	llvm::Function* Codegen( CodegenContext& context, CodegenScope& scope ) const;

  SourceRange GetSourceRange() const { return m_range; }

	//! Compares two function prototypes to see if they have the same signature
	bool operator==( const PrototypeAST& rhs ) const;
	//! Compares two function prototypes to see if they do not have the same signature
	bool operator!=( const PrototypeAST& rhs ) const { return !(*this == rhs); }
private:
  const SourceRange m_range;
	string m_strName;
	shared_ptr<const TypeAST> m_pReturnType;
	vector<shared_ptr<DeclarationAST>> m_pArgs;
}; // end class PrototypeAST

//! Function definition, includes the body
class FunctionAST {
public:
	//! Initialize with prototype and body
	FunctionAST( const SourceRange& range, const shared_ptr<PrototypeAST>& pPrototype, const shared_ptr<BlockAST>& pBody ) : m_range(range), m_pPrototype(pPrototype), m_pBody(pBody) {}

	//! Returns our name
	const string& GetName() const { return m_pPrototype->GetName(); }
	//! Returns our arguments
	const vector<shared_ptr<DeclarationAST>>& GetArgs() const { return m_pPrototype->GetArgs(); }
	//! Returns our return type
	const shared_ptr<const TypeAST>& GetReturnType() const { return m_pPrototype->GetReturnType(); }

	llvm::Function* Codegen( CodegenContext& context, CodegenScope& scope ) const;
  SourceRange GetSourceRange() const { return m_range; }
private:
  const SourceRange m_range;
	shared_ptr<PrototypeAST> m_pPrototype;
	shared_ptr<BlockAST> m_pBody;
}; // end class FunctionAST