#include "common.h"
#include "Scope.h"

using namespace llvm;

namespace {
	string strError;
}

//! Initialize our empty scope
Scope::Scope() :
	m_scopes(1),
	m_pModule(new llvm::Module("module", llvm::getGlobalContext())),
	m_builder(llvm::getGlobalContext()),
	m_pExecutionEngine(EngineBuilder(m_pModule).setErrorStr(&strError).create()),
	m_fpm(m_pModule) {

	if( !m_pExecutionEngine ) { /*ASSERT( false );*/ cerr << strError << endl; return; }

	// Set up the optimizer pipeline.  Start with registering info about how the
	// target lays out data structures.
	//m_fpm.add(new TargetData(*m_pExecutionEngine->getTargetData()));
	// Provide basic AliasAnalysis support for GVN.
	m_fpm.add(createBasicAliasAnalysisPass());
	// Do simple "peephole" optimizations and bit-twiddling optzns.
	m_fpm.add(createInstructionCombiningPass());
	// Reassociate expressions.
	m_fpm.add(createReassociatePass());
	// Eliminate Common SubExpressions.
	m_fpm.add(createGVNPass());
	// Simplify the control flow graph (deleting unreachable blocks, etc).
	m_fpm.add(createCFGSimplificationPass());

	m_fpm.doInitialization();
} // end Scope::Scope()