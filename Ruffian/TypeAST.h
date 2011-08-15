#pragma once

class CodegenContext; class CodegenScope; class Scope; class BuiltinTypeAST;

//! AST node for types, like "int" or a user-defined type
class TypeAST {
public:
	//! Initialize with type name
	TypeAST( const string& strType ) : m_strType(strType) {}

	//! Returns our type name
	const string& GetName() const { return m_strType; }

	virtual const llvm::Type* Codegen( CodegenContext& context, CodegenScope& scope ) const= 0;

	//! Returns whether this type is builtin
	bool IsBuiltin() const;
	//! Casts this type to a builtin, returning NULL if it's not a builtin
	const BuiltinTypeAST* ToBuiltin() const;

	//! Returns our size in bytes
	virtual uint GetSizeBytes() const= 0;
	
	//! Compares two types
	bool operator==( const TypeAST& rhs ) const { return isEqual( rhs ); }
	//! Compare for non-equality
	bool operator!=( const TypeAST& rhs ) const { return !(*this == rhs); }
protected:
	virtual bool isEqual( const TypeAST& rhs ) const= 0;
private:
	string m_strType;
}; // end class TypeAST


//! Basic built-in types
class BuiltinTypeAST : public TypeAST {
public:
	//! Initialize with type name
	BuiltinTypeAST( const string& strType ) : TypeAST(strType) {}

	//! Returns the static instance of the "char" type
	static const shared_ptr<const TypeAST>& GetChar() { return m_pTypeChar; }
	//! Returns the static instance of the "short" type
	static const shared_ptr<const TypeAST>& GetShort() { return m_pTypeShort; }
	//! Returns the static instance of the "int" type
	static const shared_ptr<const TypeAST>& GetInt() { return m_pTypeInt; }
	//! Returns the static instance of the "long" type
	static const shared_ptr<const TypeAST>& GetLong() { return m_pTypeLong; }
	//! Returns the static instance of the "uchar" type
	static const shared_ptr<const TypeAST>& GetUChar() { return m_pTypeUChar; }
	//! Returns the static instance of the "ushort" type
	static const shared_ptr<const TypeAST>& GetUShort() { return m_pTypeUShort; }
	//! Returns the static instance of the "uint" type
	static const shared_ptr<const TypeAST>& GetUInt() { return m_pTypeUInt; }
	//! Returns the static instance of the "ulong" type
	static const shared_ptr<const TypeAST>& GetULong() { return m_pTypeULong; }
	//! Returns the static instance of the "float" type
	static const shared_ptr<const TypeAST>& GetFloat() { return m_pTypeFloat; }
	//! Returns the static instance of the "double" type
	static const shared_ptr<const TypeAST>& GetDouble() { return m_pTypeDouble; }
	//! Returns the static instance of the "bool" type
	static const shared_ptr<const TypeAST>& GetBool() { return m_pTypeBool; }
	//! Returns the static instance of the "void" type
	static const shared_ptr<const TypeAST>& GetVoid() { return m_pTypeVoid; }
	//! Returns the static instance of the "error" type, not a valid type,
	//! but one used to report that an expression has no valid type
	static const shared_ptr<const TypeAST>& GetError() { return m_pTypeError; }

	//! Gets a vector of built-in types
	static const vector<shared_ptr<const TypeAST>>& GetBuiltinTypes() {
		static vector<shared_ptr<const TypeAST>> pTypes;
		if( pTypes.empty() ) {
			pTypes.push_back( m_pTypeChar );
			pTypes.push_back( m_pTypeShort );
			pTypes.push_back( m_pTypeInt );
			pTypes.push_back( m_pTypeLong );
			pTypes.push_back( m_pTypeUChar );
			pTypes.push_back( m_pTypeUShort );
			pTypes.push_back( m_pTypeUInt );
			pTypes.push_back( m_pTypeULong );
			pTypes.push_back( m_pTypeFloat );
			pTypes.push_back( m_pTypeDouble );
			pTypes.push_back( m_pTypeBool );
			pTypes.push_back( m_pTypeVoid );
			pTypes.push_back( m_pTypeError );
		} // end if empty

		return pTypes;
	} // end GetBuiltinTypes()

	//! Returns whether the named type is a builtin type
	static bool IsBuiltinTypeName( const string& strName ) {
		if( strName == "char" ) return true;
		if( strName == "short" ) return true;
		if( strName == "int" ) return true;
		if( strName == "long" ) return true;
		if( strName == "uchar" ) return true;
		if( strName == "ushort" ) return true;
		if( strName == "uint" ) return true;
		if( strName == "ulong" ) return true;
		if( strName == "float" ) return true;
		if( strName == "double" ) return true;
		if( strName == "bool" ) return true;
		if( strName == "void" ) return true;
		return false;
	} // end IsBuiltinTypeName()

	//! Returns whether this type is integral
	bool IsIntegral() const { return IsSigned() || IsUnsigned(); }
	//! Returns whether this type is floating-point
	bool IsFloatingPoint() const { return GetName() == "float" || GetName() == "double"; }
	//! Returns whether this type is arithmetic
	bool IsArithmetic() const { return IsIntegral() || IsFloatingPoint(); }
	//! Returns whether this type is signed integral
	bool IsSigned() const { return GetName() == "char" || GetName() == "short" || GetName() == "int" || GetName() == "long"; }
	//! Returns whether this type is unsigned integral
	bool IsUnsigned() const { return GetName() == "uchar" || GetName() == "ushort" || GetName() == "uint" || GetName() == "ulong"; }

	virtual const llvm::Type* Codegen( CodegenContext& context, CodegenScope& scope ) const;

	//! Returns the size in bytes
	virtual uint GetSizeBytes() const {
		if( *this == *GetChar() || *this == *GetUChar() ) return 1;
		if( *this == *GetShort() || *this == *GetUShort() ) return 2;
		if( *this == *GetInt() || *this == *GetUInt() ) return 4;
		if( *this == *GetLong() || *this == *GetULong() ) return 8;
		if( *this == *GetFloat() ) return 4;
		if( *this == *GetDouble() ) return 8;
		if( *this == *GetBool() ) return 1;		// TODO: sizeof(bool)?
		if( *this == *GetVoid() ) return 0;

		ASSERT( false );
		return 0;
	} // end GetSizeBytes()
protected:
	virtual bool isEqual( const TypeAST& rhs ) const {
		if( dynamic_cast<const BuiltinTypeAST*>(&rhs) ) {
			return GetName() == rhs.GetName();
		} else {
			return false;
		}
	} // end isEqual()
private:
	//! Built-in type static instances
	static shared_ptr<const TypeAST> m_pTypeChar,
	                                 m_pTypeShort,
									 m_pTypeInt,
									 m_pTypeLong,
									 m_pTypeUChar,
	                                 m_pTypeUShort,
									 m_pTypeUInt,
									 m_pTypeULong,
	                                 m_pTypeFloat,
							         m_pTypeDouble,
				                     m_pTypeBool,
				                     m_pTypeVoid,
				                     m_pTypeError;
}; // end class BuiltinTypeAST

//! Returns whether this type is builtin
inline bool TypeAST::IsBuiltin() const { return ToBuiltin() != NULL; }
//! Casts this type to a builtin, returning NULL if it's not a builtin
inline const BuiltinTypeAST* TypeAST::ToBuiltin() const { return dynamic_cast<const BuiltinTypeAST*>(this); }