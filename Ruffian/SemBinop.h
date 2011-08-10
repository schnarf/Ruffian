#pragma once

#include "Lexer.h"
#include "TypeAST.h"

//! Returns the resulting type of a binary operation
inline const TypeAST& GetBinopType( Token binop, const TypeAST& l, const TypeAST& r ) {
	if( !Lexer::IsBinopToken(binop) ) { ASSERT( false ); return TypeAST::GetError(); }

	// For now, only allow builtin arithmetic types
	if( !l.IsArithmetic() || !r.IsArithmetic() ) return TypeAST::GetError();

	switch( binop ) {
	case TOKEN_PLUS:
	case TOKEN_MINUS:
	case TOKEN_SLASH:
	case TOKEN_STAR:
		// If both arguments are the same type, return that type
		if( l == r ) return l;
		// If one is signed integral, and one is floating point, return the floating-point type
		if( l.IsSigned() && r.IsFloatingPoint() ) return r;
		if( l.IsFloatingPoint() && r.IsSigned() ) return l;
		// Otherwise, this is an invalid type
		return TypeAST::GetError();
	case TOKEN_LT:
	case TOKEN_GT:
	case TOKEN_LE:
	case TOKEN_GE:
	case TOKEN_COMPARE:
		// These comparison operators all give a bool
		return TypeAST::GetBool();
	default:
		ASSERT( false );
		return TypeAST::GetError();
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
	case TOKEN_COMPARE:
		return true;
	default:
		return false;
	} // end switch binop
} // end IsComparisonBinop()