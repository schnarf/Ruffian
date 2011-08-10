#include "common.h"
#include "CodegenContext.h"

using namespace llvm;

namespace {
	//! Error string passed to the EngineBuilder
	string strError; 
} // end file-scope

//! Initialize
CodegenContext::CodegenContext() :
	m_pModule(new llvm::Module("module", getGlobalContext())),
	m_builder(getGlobalContext()),
	m_pExecutionEngine(EngineBuilder(m_pModule).setErrorStr(&strError).create()),
	m_fpm(m_pModule) {

	if( !m_pExecutionEngine ) { ASSERT( false ); cerr << strError << endl; return; }

	// Set up the optimizer pipeline.  Start with registering info about how the
	// target lays out data structures.
	if( m_pExecutionEngine ) m_fpm.add(new TargetData(*m_pExecutionEngine->getTargetData()));
	// Provide basic AliasAnalysis support for GVN.
	m_fpm.add(createBasicAliasAnalysisPass());
	// Promote allocas to registers.
    m_fpm.add(createPromoteMemoryToRegisterPass());
	// Do simple "peephole" optimizations and bit-twiddling optzns.
	m_fpm.add(createInstructionCombiningPass());
	// Reassociate expressions.
	m_fpm.add(createReassociatePass());
	// Eliminate Common SubExpressions.
	m_fpm.add(createGVNPass());
	// Simplify the control flow graph (deleting unreachable blocks, etc).
	m_fpm.add(createCFGSimplificationPass());

	m_fpm.doInitialization();
} // end CodegenContext::CodegenContext()

//! Non-inline destructor
CodegenContext::~CodegenContext() { }