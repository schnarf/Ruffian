#pragma once

#include "ExprAST.h"
#include "TypeAST.h"

//! Returns whether an implicit cast from the type of \a pA to the type of \a pB exists
bool IsImplicitCastAllowed( const shared_ptr<const ExprAST>& pA, const shared_ptr<const TypeAST>& pU ) {
	// Grab the type
	shared_ptr<const TypeAST> pT= pA->GetType();

	// If the types are already the same, no conversion is necessary
	if( *pT == *pU ) return true;
	
	// Handle builtin types
	if( pT->IsBuiltin() && pU->IsBuiltin() ) {
		// Allow casts from float to double
		if( *pT == *BuiltinTypeAST::GetFloat() && *pU == *BuiltinTypeAST::GetDouble() ) return true;

		// Handle integral types
		if( pT->ToBuiltin()->IsIntegral() && pU->ToBuiltin()->IsIntegral() ) {
			// Allow casts between integer types of the same signedness,
			// to an size that's at least as large
			if( pT->ToBuiltin()->IsSigned() == pU->ToBuiltin()->IsSigned() && pU->GetSizeBytes() >= pT->GetSizeBytes() ) return true;
		} // end if integral
	} // end if builtin

	// Handle array types
	if( pT->IsArray() && pU->IsArray() ) {
		// Allow an implicit cast from a sized array type to an array type with unknown length
		const ArrayTypeAST* pArrayT= pT->ToArray(),
		                  * pArrayU= pU->ToArray();
		if( *pArrayT->GetElementType() == *pArrayU->GetElementType()
			&& pArrayT->GetLengthExpression() && !pArrayU->GetLengthExpression() ) {

			return true;
		} // end if converting sized array to unsized array
	} // end if array

	// Now the rules for constants come. The rules are relaxed for constant casts.
	// - We allow casting of an integer constant to any other integer type, as long as there is no overflow.
	// - We allow casting between floating point types, as long as there is no overflow. Note rounding error
	//   is fine.
	// - We allow casting from integer constants to floating point types, as long as there is no overflow
	if( pA->IsConstant() && pT->IsBuiltin() && pU->IsBuiltin() ) {
		// TODO: Check for overflow
		if( pT->ToBuiltin()->IsIntegral() && pU->ToBuiltin()->IsIntegral() ) return true;
		if( pT->ToBuiltin()->IsFloatingPoint() && pU->ToBuiltin()->IsFloatingPoint() ) return true;
		if( pT->ToBuiltin()->IsIntegral() && pU->ToBuiltin()->IsFloatingPoint() ) return true;
	} // end if constant cast

	// If we get here, implicit casts are not allowed
	return false;
} // end IsImplicitCastAllowed()

//! Returns the type to implicitly cast the two binary operands to
//! If no implicit conversion is possible, returns NULL
shared_ptr<const TypeAST> GetBinaryOpsImplicitCastType( const shared_ptr<const ExprAST>& pA, const shared_ptr<const ExprAST>& pB ) {
	// Grab types
	shared_ptr<const TypeAST> pT= pA->GetType(),
	                          pU= pB->GetType();

	// If the types are already the same, no action is necessary
	if( *pT == *pU ) return pT;

	// Handle rules for constants before the other rules
	if( pA->IsConstant() && IsImplicitCastAllowed(pA, pB->GetType()) ) return pB->GetType();
	if( pB->IsConstant() && IsImplicitCastAllowed(pB, pA->GetType()) ) return pA->GetType();

	// If we have two distinct floating point types, cast them to double
	if( pT->ToBuiltin()->IsFloatingPoint() && pU->ToBuiltin()->IsFloatingPoint() ) {
		if( *pT == *BuiltinTypeAST::GetDouble() ) { return pT; }
		else { ASSERT( *pU == *BuiltinTypeAST::GetDouble() ); return pU; }
	} // end if floating point

	// Integer/integer conversions
	if( pT->ToBuiltin()->IsIntegral() && pU->ToBuiltin()->IsIntegral() ) {
		// If the integers have the same signedness, convert to the larger type
		if( pT->ToBuiltin()->IsSigned() == pU->ToBuiltin()->IsSigned() ) {
			return pT->GetSizeBytes() >= pU->GetSizeBytes()
				? pT
				: pU;
		} // end if same signedness
	} // end if both integral

	// If we get here, no cast is possible, so return the error type
	return NULL;
} // end GetBinaryOpsImplicitCastType()

//! Returns whether an explicit cast from \a pA to \a pU exists
bool IsCastAllowed( const shared_ptr<const TypeAST>& pT, const shared_ptr<const TypeAST>& pU ) {
	// Allow casts between all arithmetic types
	if( pT->ToBuiltin()->IsArithmetic() && pU->ToBuiltin()->IsArithmetic() ) return true;
	// Allow bool -> int
	if( *pT == *BuiltinTypeAST::GetBool() && pU->ToBuiltin()->IsIntegral() ) return true;

	// If we get here, explicit casts are not allowed
	return false;
} // end IsCastAllowed()