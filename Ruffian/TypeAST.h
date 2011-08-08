#pragma once

class Scope;

//! AST node for types, like "int" or a user-defined type
class TypeAST {
public:
	//! Initialize with type name
	TypeAST( const string& strType ) : m_strType(strType) {}
	//! Returns our type name
	const string& GetName() const { return m_strType; }

	virtual const llvm::Type* Codegen( Scope& scope ) const;

	//! Returns the static instance of the "int" type
	static const TypeAST& GetInt() { static TypeAST typeInt("int"); return typeInt; }
	//! Returns the static instance of the "float" type
	static const TypeAST& GetFloat() { static TypeAST typeFloat("float"); return typeFloat; }
	//! Returns the static instance of the "bool" type
	static const TypeAST& GetBool() { static TypeAST typeBool("bool"); return typeBool; }
	//! Returns the static instance of the "error" type, not a valid type,
	//! but one used to report that an expression has no valid type
	static const TypeAST& GetError() { static TypeAST typeError("error-type"); return typeError; }

	//! Returns whether the named type is a builtin type
	static bool IsBuiltinTypeName( const string& strName ) {
		if( strName == "int" ) return true;
		if( strName == "float" ) return true;
		if( strName == "bool" ) return true;
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
}; // end class TypeAST
