#pragma once

class BlockAST;
class DeclarationAST;
class Scope;
class TypeAST;
class VariableAST;

//! Function prototype
class PrototypeAST {
public:
	//! Initialize with function name, return type, and argument list
	PrototypeAST( const string& strName, TypeAST* pReturnType, const vector<DeclarationAST*>& pArgs ) : m_strName(strName), m_pReturnType(pReturnType), m_pArgs(pArgs) {}

	//! Returns our name
	const string& GetName() const { return m_strName; }
	//! Returns our arguments
	const vector<DeclarationAST*>& GetArgs() const { return m_pArgs; }
	//! Returns our return type
	const TypeAST& GetReturnType() const { ASSERT( m_pReturnType ); return *m_pReturnType; }

	llvm::Function* Codegen( Scope& scope ) const;

	//! Compares two function prototypes to see if they have the same signature
	bool operator==( const PrototypeAST& rhs ) const;
	//! Compares two function prototypes to see if they do not have the same signature
	bool operator!=( const PrototypeAST& rhs ) const { return !(*this == rhs); }
private:
	string m_strName;
	TypeAST* m_pReturnType;
	vector<DeclarationAST*> m_pArgs;
}; // end class PrototypeAST

//! Function definition, includes the body
class FunctionAST {
public:
	//! Initialize with prototype and body
	FunctionAST( PrototypeAST* pPrototype, BlockAST* pBody ) : m_pPrototype(pPrototype), m_pBody(pBody) {}

	//! Returns our name
	const string& GetName() const { return m_pPrototype->GetName(); }
	//! Returns our arguments
	const vector<DeclarationAST*>& GetArgs() const { return m_pPrototype->GetArgs(); }
	//! Returns our return type
	const TypeAST& GetReturnType() const { return m_pPrototype->GetReturnType(); }

	llvm::Function* Codegen( Scope& scope ) const;
private:
	PrototypeAST* m_pPrototype;
	BlockAST* m_pBody;
}; // end class FunctionAST