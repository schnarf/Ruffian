#include "common.h"
#include "FunctionAST.h"
#include "StmtAST.h"
#include "TypeAST.h"

//! Compares two function prototypes to see if they have the same signature
bool PrototypeAST::operator==( const PrototypeAST& rhs ) const {
	if( GetName() != rhs.GetName() ) return false;
	if( m_pReturnType != rhs.m_pReturnType ) return false;
	if( m_pArgs.size() != rhs.m_pArgs.size() ) return false;
	for( uint iArg=0; iArg<m_pArgs.size(); ++iArg ) {
		if( m_pArgs[iArg]->GetType() != rhs.m_pArgs[iArg]->GetType() ) return false;
	} // end for arg

	return true;
} // end PrototypeAST::operator==()