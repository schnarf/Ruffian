#include "common.h"
#include "TypeAST.h"

shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeChar= shared_ptr<const TypeAST>( new BuiltinTypeAST("char") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeShort= shared_ptr<const TypeAST>( new BuiltinTypeAST("short") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeInt= shared_ptr<const TypeAST>( new BuiltinTypeAST("int") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeLong= shared_ptr<const TypeAST>( new BuiltinTypeAST("long") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeUChar= shared_ptr<const TypeAST>( new BuiltinTypeAST("uchar") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeUShort= shared_ptr<const TypeAST>( new BuiltinTypeAST("ushort") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeUInt= shared_ptr<const TypeAST>( new BuiltinTypeAST("uint") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeULong= shared_ptr<const TypeAST>( new BuiltinTypeAST("ulong") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeFloat= shared_ptr<const TypeAST>( new BuiltinTypeAST("float") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeDouble= shared_ptr<const TypeAST>( new BuiltinTypeAST("double") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeBool= shared_ptr<const TypeAST>( new BuiltinTypeAST("bool") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeVoid= shared_ptr<const TypeAST>( new BuiltinTypeAST("void") );
shared_ptr<const TypeAST> BuiltinTypeAST::m_pTypeError= shared_ptr<const TypeAST>( new BuiltinTypeAST("error-type") );