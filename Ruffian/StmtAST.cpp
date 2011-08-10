#include "common.h"
#include "StmtAST.h"

//! Returns true if any of the statements in this block are a return statement
bool BlockAST::HasReturn() const {
	for( uint iStmt=0; iStmt<m_pStmts.size(); ++iStmt ) {
		if( dynamic_cast<ReturnAST*>(m_pStmts[iStmt].get()) ) return true;
	} // end for statement

	return false;
} // end BlockAST::HasReturn()
