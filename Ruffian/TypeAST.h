#pragma once

class CodegenContext; class CodegenScope; class Scope;

//! AST node for types, like "int" or a user-defined type
class TypeAST {
public:
	//! Initialize with type name
	TypeAST( const string& strType ) : m_strType(strType) {}
	//! Returns our type name
	const string& GetName() const { return m_strType; }

	virtual const llvm::Type* Codegen( CodegenContext& context, CodegenScope& scope ) const;

	//! Returns the static instance of the "int" type
	static const TypeAST& GetInt() { return *m_pTypeInt; }
	//! Returns the static instance of the "float" type
	static const TypeAST& GetFloat() { return *m_pTypeFloat; }
	//! Returns the static instance of the "bool" type
	static const TypeAST& GetBool() { return *m_pTypeBool; }
	//! Returns the static instance of the "void" type
	static const TypeAST& GetVoid() { return *m_pTypeVoid; }
	//! Returns the static instance of the "error" type, not a valid type,
	//! but one used to report that an expression has no valid type
	static const TypeAST& GetError() { return *m_pTypeError; }

	//! Gets a vector of built-in types
	static const vector<shared_ptr<const TypeAST>>& GetBuiltinTypes() {
		static vector<shared_ptr<const TypeAST>> pTypes;
		if( pTypes.empty() ) {
			pTypes.push_back( m_pTypeInt );
			pTypes.push_back( m_pTypeFloat );
			pTypes.push_back( m_pTypeBool );
			pTypes.push_back( m_pTypeVoid );
			pTypes.push_back( m_pTypeError );
		} // end if empty

		return pTypes;
	} // end GetBuiltinTypes()

	//! Returns whether the named type is a builtin type
	static bool IsBuiltinTypeName( const string& strName ) {
		if( strName == "int" ) return true;
		if( strName == "float" ) return true;
		if( strName == "bool" ) return true;
		if( strName == "void" ) return true;
		return false;
	} // end IsBuiltinTypeName()

	//! Returns whether this type is builtin
	bool IsBuiltin() const { return IsBuiltinTypeName(m_strType); }
	//! Returns whether this type is integral
	bool IsIntegral() const { return m_strType == "int"; }
	//! Returns whether this type is floating-point
	bool IsFloatingPoint() const { return m_strType == "float"; }
	//! Returns whether this type is arithmetic
	bool IsArithmetic() const { return IsIntegral() || IsFloatingPoint(); }
	//! Returns whether this type is signed integral
	bool IsSigned() const { return m_strType == "int"; }
	//! Returns whether this type is unsigned integral
	bool IsUnsigned() const { return false; }

	//! Compares two types
	bool operator==( const TypeAST& rhs ) const { return m_strType == rhs.m_strType; }
	//! Compare for non-equality
	bool operator!=( const TypeAST& rhs ) const { return !(*this == rhs); }
private:
	string m_strType;

	//! Built-in types
	static shared_ptr<TypeAST> m_pTypeInt,
	                           m_pTypeFloat,
				               m_pTypeBool,
				               m_pTypeVoid,
				               m_pTypeError;
}; // end class TypeAST
