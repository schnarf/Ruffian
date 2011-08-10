#pragma once

//! Holds references to instances of objects that
//! code-emitting AST nodes need to know about
class CodegenContext {
public:
	//! Initialize
	CodegenContext();
	//! Non-inline destructor
	~CodegenContext();

	//! Gets our module
	llvm::Module* GetModule() { return m_pModule; }
	//! Gets our IR builder
	llvm::IRBuilder<>& GetBuilder() { return m_builder; }
	//! Gets our execution engine
	llvm::ExecutionEngine* GetExecutionEngine() { return m_pExecutionEngine; }
	//! Gets our function pass manager
	llvm::FunctionPassManager& GetFunctionPassManager() { return m_fpm; }
private:
	llvm::Module* m_pModule;
	llvm::IRBuilder<> m_builder;
	llvm::ExecutionEngine* m_pExecutionEngine;
	llvm::FunctionPassManager m_fpm;
}; // end class CodegenContext