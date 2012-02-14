#pragma once

#include "Lexer.h"
#include "TypeAST.h"

inline const shared_ptr<const TypeAST>& GetPrefixUnaryOpType( Token op, const shared_ptr<const TypeAST>& type ) {
	return type;
} // end GetPrefixUnaryOpType()

inline const shared_ptr<const TypeAST>& GetPostfixUnaryOpType( Token op, const shared_ptr<const TypeAST>& type ) {
	return type;
} // end GetPostfixUnaryOpType()