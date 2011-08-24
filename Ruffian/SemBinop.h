#pragma once

#include "Lexer.h"
#include "TypeAST.h"

//! Returns the resulting type of a binary operation
inline const shared_ptr<const TypeAST>& GetBinopType( Token binop, const shared_ptr<const TypeAST>& l, const shared_ptr<const TypeAST>& r ) {
	if( !Lexer::IsBinopToken(binop) ) { ASSERT( false ); return BuiltinTypeAST::GetError(); }

	// For now, only allow builtin types
	if( !l->ToBuiltin() || !r->ToBuiltin() ) return BuiltinTypeAST::GetError();

	switch( binop ) {
	case TOKEN_PLUS:
	case TOKEN_MINUS:
	case TOKEN_SLASH:
	case TOKEN_STAR:
		// If both arguments are the same type, return that type
		if( l == r ) return l;
		// If one is signed integral, and one is floating point, return the floating-point type
		if( l->ToBuiltin()->IsSigned() && r->ToBuiltin()->IsFloatingPoint() ) return r;
		if( l->ToBuiltin()->IsFloatingPoint() && r->ToBuiltin()->IsSigned() ) return l;
		// Otherwise, this is an invalid type
		return BuiltinTypeAST::GetError();
	case TOKEN_ASSIGN:
		// Even when casts are added, this will return the type of the LHS
		return l;
	case TOKEN_LT:
	case TOKEN_GT:
	case TOKEN_LE:
	case TOKEN_GE:
	case TOKEN_NEQ:
	case TOKEN_EQ:
	case TOKEN_AND:
	case TOKEN_OR:
		// These comparison operators all give a bool
		return BuiltinTypeAST::GetBool();
	default:
		ASSERT( false );
		return BuiltinTypeAST::GetError();
	}
} // end GetBinopType()

//! Returns whether the given binary operation is a comparison
inline bool IsComparisonBinop( Token binop ) {
	ASSERT( Lexer::IsBinopToken(binop) );

	switch( binop ) {
	case TOKEN_LT:
	case TOKEN_GT:
	case TOKEN_LE:
	case TOKEN_GE:
	case TOKEN_EQ:
	case TOKEN_NEQ:
		return true;
	default:
		return false;
	} // end switch binop
} // end IsComparisonBinop()

//! Returns a binary operator's precedence
//! If it's not a binary operator, returns 0
inline int GetBinopPrecedence( Token binop ) {
	if( !Lexer::IsBinopToken(binop) ) return 0;

	// Install our binary operator precedences
	static map<Token, int> precedence;
	if( precedence.empty() ) {
		precedence[TOKEN_ASSIGN]= 2;
		precedence[TOKEN_AND]= precedence[TOKEN_OR]= 4;
		precedence[TOKEN_EQ]= precedence[TOKEN_NEQ]= 5;
		precedence[TOKEN_LT]= precedence[TOKEN_GT]= precedence[TOKEN_LE]= precedence[TOKEN_GE]= 10;
		precedence[TOKEN_PLUS]= precedence[TOKEN_MINUS]= 20;
		precedence[TOKEN_STAR]= precedence[TOKEN_SLASH]= 30;
	} // end if empty

	ASSERT( precedence.find(binop) != precedence.end() );
	return precedence[binop];
} // end CompareBinopPrecedence()