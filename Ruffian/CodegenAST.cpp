#include "common.h"
#include "AST.h"
#include "Scope.h"

using namespace llvm;

//! Codegen error utility function
Value* ErrorCodegen( const string& strError ) {
	cerr << "Codegen error: " << strError << endl;
	return NULL;
} // end ErrorCodegen()

//! Create an alloca instruction in the entry block of the function
AllocaInst* CreateEntryBlockAlloca( Scope& scope, Function* pFunction, const string& strName, const Type* pType ) {
  IRBuilder<> tempBuilder( &pFunction->getEntryBlock(), pFunction->getEntryBlock().begin() );
  return tempBuilder.CreateAlloca( pType, 0, strName.c_str() );
} // end CreateEntryBlockAlloca()


Value* IntegerAST::Codegen( Scope& scope ) const {
	// TODO: rework int stuff
	return ConstantInt::get( getGlobalContext(), APInt(64, uint64_t(m_iValue), true) );
	//return ConstantFP::get( getGlobalContext(), APFloat(double(m_iValue)) );
} // end IntegerAST::Codegen()


Value* FloatAST::Codegen( Scope& scope ) const {
	return ConstantFP::get( getGlobalContext(), APFloat(m_fValue) );
} // end FloatAST::Codegen()


Value* BoolAST::Codegen( Scope& scope ) const {
	// Convert to a 1-bit integer
	return ConstantInt::get( getGlobalContext(), APInt(1, m_bValue ? 1 : 0) );
} // end BoolAST::Codegen()


Value* VariableAST::Codegen( Scope& scope ) const {
	// Look up the variable in the scope and load it
	const string& strName= m_pDeclaration->GetName();
	AllocaInst* pAlloca= scope.LookupVariable( strName );
	if( !pAlloca ) return ErrorCodegen( string("Variable \"") + strName + "\" was not defined in the current scope" );
	else return scope.GetBuilder().CreateLoad( pAlloca, strName );
} // end VariableAST::Codegen()


Value* BinopAST::Codegen( Scope& scope ) const {
	Value* pLeft= m_pLeft->Codegen( scope );
	Value* pRight= m_pRight->Codegen( scope );
	if( !pLeft || !pRight ) return ErrorCodegen( "Could not generate lhs and rhs for binary expression" );

	// TODO: everything is being done floating-point. Should take into account types of operands
	// and come up with simple rules for conversion
	switch( m_binop ) {
	case TOKEN_PLUS: return scope.GetBuilder().CreateAdd( pLeft, pRight, "addtmp" );
	case TOKEN_MINUS: return scope.GetBuilder().CreateSub( pLeft, pRight, "subtmp" );
	case TOKEN_STAR: return scope.GetBuilder().CreateMul( pLeft, pRight, "multmp" );
	case TOKEN_SLASH: return scope.GetBuilder().CreateFDiv( pLeft, pRight, "divtmp" );
	
		// For comparisons, convert bool 0/1 to double 0.0 or 1.0 for now
	case TOKEN_COMPARE:
		//pLeft= scope.GetBuilder().CreateFCmpUEQ( pLeft, pRight, "cmptmp" );
		pLeft= scope.GetBuilder().CreateICmpEQ( pLeft, pRight, "cmptmp" );
		return pLeft;
		//return scope.GetBuilder().CreateUIToFP( pLeft, Type::getDoubleTy(getGlobalContext()), "booltmp" );
	case TOKEN_LT:
		pLeft= scope.GetBuilder().CreateFCmpULT( pLeft, pRight, "cmptmp" );
		return pLeft;
		//return scope.GetBuilder().CreateUIToFP( pLeft, Type::getDoubleTy(getGlobalContext()), "booltmp" );
	case TOKEN_GT:
		pLeft= scope.GetBuilder().CreateFCmpUGT( pLeft, pRight, "cmptmp" );
		return pLeft;
		//return scope.GetBuilder().CreateUIToFP( pLeft, Type::getDoubleTy(getGlobalContext()), "booltmp" );
	case TOKEN_LE:
		pLeft= scope.GetBuilder().CreateFCmpULE( pLeft, pRight, "cmptmp" );
		return pLeft;
		//return scope.GetBuilder().CreateUIToFP( pLeft, Type::getDoubleTy(getGlobalContext()), "booltmp" );
	case TOKEN_GE:
		pLeft= scope.GetBuilder().CreateFCmpUGE( pLeft, pRight, "cmptmp" );
		return pLeft;
		//return scope.GetBuilder().CreateUIToFP( pLeft, Type::getDoubleTy(getGlobalContext()), "booltmp" );
	default:
		return ErrorCodegen( string("Unhandled binary operator '") + Lexer::StringifyToken(m_binop) + "' for binary expression" );
	} // end switch binop
} // end BinopAST::Codegen()


Value* CallAST::Codegen( Scope& scope ) const {
	// Look up the name in the global module table
	Function* pFunction= scope.GetModule()->getFunction( m_pPrototype->GetName() );
	if( !pFunction ) return ErrorCodegen( string("Unknown function \"") + m_pPrototype->GetName() + "\" referenced in call expression" );

	// Check for argument mismatch
	if( pFunction->arg_size() != m_pArgs.size() ) return ErrorCodegen( string("Wrong number of arguments for call expression for function \"") + m_pPrototype->GetName() + "\"" );

	// Codegen function arguments
	vector<Value*> pArgs;
	for( uint i=0; i<m_pArgs.size(); ++i ) {
		pArgs.push_back( m_pArgs[i]->Codegen(scope) );
		if( !pArgs.back() ) return ErrorCodegen( "Error generating function argument in call expression\n" );
	} // end for argument

	return scope.GetBuilder().CreateCall( pFunction, pArgs.begin(), pArgs.end(), "calltmp" );
} // end CallAST::Codegen()


Function* PrototypeAST::Codegen( Scope& scope ) const {
	// Make the function type
	vector<const Type*> pArgTypes( m_pArgs.size() );
	for( uint i=0; i<m_pArgs.size(); ++i ) pArgTypes[i]= m_pArgs[i]->GetType().Codegen( scope );
	
	FunctionType* pFunctionType= FunctionType::get( m_pReturnType->Codegen(scope), pArgTypes, false );

	// Create the function
	Function* pFunction= Function::Create( pFunctionType, Function::ExternalLinkage, m_strName, scope.GetModule() );

	// If there was a conflict, erase the function and return the existing one
	if( pFunction->getName() != m_strName ) {
		// Delete the function first
		pFunction->eraseFromParent();
		pFunction= scope.GetModule()->getFunction( GetName() );
		ASSERT( pFunction );
	} // end if conflict

	return pFunction;
} // end PrototypeAST::Codegen()


Function* FunctionAST::Codegen( Scope& scope ) const {
	// Emit code for the prototype. If the prototype has already been emitted,
	// that's fine, and it will be returned
	Function* pFunction= m_pPrototype->Codegen( scope );

	// Begin a new scope
	ScopeEnterSentry s_scope( scope );

	// Create a new basic block for the function's body
	BasicBlock* pBlock= BasicBlock::Create( getGlobalContext(), "entry", pFunction );
	scope.GetBuilder().SetInsertPoint( pBlock );

	// Add all arguments to the symbol table and create their allocas
	Function::arg_iterator itArg= pFunction->arg_begin();
	for( uint iArg=0; iArg<GetArgs().size(); ++iArg, ++itArg ) {
		// Create the alloca for this variable
		AllocaInst* pAlloca= CreateEntryBlockAlloca( scope, pFunction, GetArgs()[iArg]->GetName(), GetArgs()[iArg]->GetType().Codegen(scope) );
		// Store the initial value into the alloca
		// TODO: is this necessary/correct?
		scope.GetBuilder().CreateStore( itArg, pAlloca );
		// Add arguments to the symbol table
		scope.RegisterVariable( GetArgs()[iArg]->GetName(), pAlloca );
	} // end for argument

#if 0
	if( Value* pRetVal= m_pBody->Codegen(scope) ) {
		// Finish off the function
		scope.GetBuilder().CreateRet( pRetVal );
		// Validate the generated code, checking for consistency
		verifyFunction( *pFunction );
		return pFunction;
	} // end if success
#else
	// Codegen the body
	m_pBody->Codegen( scope );

	// Validate the generated code, checking for consistency
	verifyFunction( *pFunction );

	// Optimize
	scope.GetFunctionPassManager().run( *pFunction );

	return pFunction;
#endif

	// If we had an error generating the body, remove the function and give an error
	pFunction->eraseFromParent();
	return static_cast<Function*>(ErrorCodegen(string("Could not create the body for function \"") + GetName() + "\""));
} // end FunctionAST::Codegen()


void ReturnAST::Codegen( Scope& scope ) const {
	// If our expression is NULL, return void
	if( !m_pExpr ) {
		scope.GetBuilder().CreateRetVoid();
		return;
	}

	// Just emit code for the return expression
	Value* pRet= m_pExpr->Codegen( scope );
	if( !pRet ) { ErrorCodegen( "Could not emit code for return expression" ); return; }
	scope.GetBuilder().CreateRet( pRet );
} // end ReturnAST::Codegen()


void DeclarationAST::Codegen( Scope& scope ) const {
	Function* pFunction= scope.GetBuilder().GetInsertBlock()->getParent();

	// Emit the initializer before adding the variable to scope, to prevent the
	// initializer from referencing the variable itself
	Value* pInitVal;
	if( m_pInitializer ) {
		pInitVal= m_pInitializer->Codegen( scope );
		if( !pInitVal ) { ErrorCodegen( string("Error emitting code for initializer for the declaration of variable \"") + GetName() + "\"" ); return; }
	}

	AllocaInst* pAlloca= CreateEntryBlockAlloca( scope, pFunction, GetName(), m_pType->Codegen(scope) );
	if( pInitVal ) scope.GetBuilder().CreateStore( pInitVal, pAlloca );

	// Register the binding
	bool bSuccess= scope.RegisterVariable( GetName(), pAlloca );
	if( !bSuccess ) { ErrorCodegen( string("Variable \"") + GetName() + "\" already exists in variable declaration"  ); return; }
} // end DeclarationAST::Codegen()


void BlockAST::Codegen( Scope& scope ) const {
	// Enter a new scope
	ScopeEnterSentry s_scope( scope );

	// We return a value only if it's a return instruction
	Value* pRet= NULL;

	// Emit code for each of our statements
	for( uint iStmt=0; iStmt<m_pStmts.size(); ++iStmt ) {
		m_pStmts[iStmt]->Codegen( scope );

		// Check for a return
		if( dynamic_cast<ReturnAST*>(m_pStmts[iStmt]) ) {
			if( iStmt+1<m_pStmts.size() ) { ErrorCodegen( string("Return expression in a block was not the last expression") ); return; }
		} // end if return
	} // end for expression
} // end BlockAST::Codegen()


void AssignmentAST::Codegen( Scope& scope ) const {
	// Lookup the lhs
	AllocaInst* pAlloca= scope.LookupVariable( m_pLeft->GetName() );
	if( !pAlloca ) { ErrorCodegen( string("Variable \"") + m_pLeft->GetName() + "\" does not exist while creating assignment expression" ); return; }

	// Codegen the RHS
	Value* pValue= m_pRight->Codegen( scope );
	if( !pValue ) { ErrorCodegen( "Couldn't generate code for rhs in assignment expression" ); return; }

	// Create the store instruction
	scope.GetBuilder().CreateStore( pValue, pAlloca );
} // end AssignmentAST::Codegen()


void ConditionalAST::Codegen( Scope& scope ) const {
	// Generate code for evaluating the conditional
	Value* pCond= m_pCondExpr->Codegen( scope );
	if( !pCond ) { ErrorCodegen( "Could not emit code for condition in conditional statement" ); return; }
	// Convert to bool by comparing equal to 0.0
	//pCond= scope.GetBuilder().CreateFCmpONE( pCond, ConstantFP::get(getGlobalContext(), APFloat(0.0), "ifcond") );

	// Grab the function
	Function* pFunction= scope.GetBuilder().GetInsertBlock()->getParent();

	// Create blocks for then/else. Insert "then" block at the end of the function
	BasicBlock* pThenBlock= BasicBlock::Create( getGlobalContext(), "then", pFunction );
	BasicBlock* pElseBlock= BasicBlock::Create( getGlobalContext(), "else" );
	BasicBlock* pMergeBlock= BasicBlock::Create( getGlobalContext(), "ifcont" );

	scope.GetBuilder().CreateCondBr( pCond, pThenBlock, pElseBlock );
	
	// Emit "then"
	scope.GetBuilder().SetInsertPoint( pThenBlock );
	m_pIfStmt->Codegen( scope );
	// Only create the branch to the merge block if there's no return statement
	if( !m_pIfStmt->HasReturn() ) scope.GetBuilder().CreateBr( pMergeBlock );

	// Codegen of "then" can change current block, so update the then block for the PHI
	pThenBlock= scope.GetBuilder().GetInsertBlock();

	// Emit else block. If we have no else block, just emit a jump to the merge block
	pFunction->getBasicBlockList().push_back( pElseBlock );
	scope.GetBuilder().SetInsertPoint( pElseBlock );
	if( m_pElseStmt ) m_pElseStmt->Codegen( scope );

	// Only create the branch to the merge block if there's no return statement
	if( !m_pElseStmt || !m_pElseStmt->HasReturn() ) scope.GetBuilder().CreateBr( pMergeBlock );
	pElseBlock= scope.GetBuilder().GetInsertBlock();

	// Emit merge block
	pFunction->getBasicBlockList().push_back( pMergeBlock );
	scope.GetBuilder().SetInsertPoint( pMergeBlock );
} // end ConditionalAST::Codegen()


const Type* TypeAST::Codegen( Scope& scope ) const {
	if( *this == GetInt() ) return Type::getInt64Ty(getGlobalContext());
	else if( *this == GetFloat() ) return Type::getDoubleTy(getGlobalContext());
	else if( *this == GetBool() ) return Type::getInt1Ty(getGlobalContext());
	else {
		cerr << "Unknown type \"" << m_strType << "\"";
		return NULL;
	}
} // end TypeAST::Codegen()