#include "common.h"
#include "AST.h"
#include "CodegenContext.h"
#include "CodegenScope.h"
#include "SemBinop.h"

using namespace llvm;

namespace {
	//! Codegen error utility function
	Value* ErrorCodegen( const string& strError ) {
		cerr << "Codegen error: " << strError << endl;
		return NULL;
	} // end ErrorCodegen()

	//! Create an alloca instruction in the entry block of the function
	AllocaInst* CreateEntryBlockAlloca( CodegenContext& context, CodegenScope& scope, Function* pFunction, const string& strName, const Type* pType ) {
	  IRBuilder<> tempBuilder( &pFunction->getEntryBlock(), pFunction->getEntryBlock().begin() );
	  return tempBuilder.CreateAlloca( pType, 0, strName.c_str() );
	} // end CreateEntryBlockAlloca()
} // end file-scope

Value* IntegerAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// TODO: rework int stuff
	return ConstantInt::get( getGlobalContext(), m_apValue );
} // end IntegerAST::Codegen()


Value* DoubleAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	return ConstantFP::get( getGlobalContext(), m_apValue );
} // end DoubleAST::Codegen()


Value* BoolAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Convert to a 1-bit integer
	return ConstantInt::get( getGlobalContext(), APInt(1, m_bValue ? 1 : 0) );
} // end BoolAST::Codegen()


Value* VariableAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Look up the variable in the scope and load it
	const string& strName= m_pDeclaration->GetName();
	AllocaInst* pAlloca= scope.LookupVariable( strName );
	if( !pAlloca ) return ErrorCodegen( string("Variable \"") + strName + "\" was not defined in the current scope" );
	else return context.GetBuilder().CreateLoad( pAlloca, strName );
} // end VariableAST::Codegen()


Value* BinopAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Special-case assignments
	if( m_binop == TOKEN_ASSIGN ) {
		// Cast our LHS to a variable
		shared_ptr<VariableAST> pVariable= dynamic_pointer_cast<VariableAST>( m_pLeft );
		if( !pVariable ) return ErrorCodegen( "Left-hand side of assignment expression must be a variable" );

		// Lookup the lhs
		AllocaInst* pAlloca= scope.LookupVariable( pVariable->GetName() );
		if( !pAlloca ) return ErrorCodegen( string("Variable \"") + pVariable->GetName() + "\" does not exist while creating assignment expression" );

		// Codegen the RHS
		Value* pValue= m_pRight->Codegen( context, scope );
		if( !pValue ) return ErrorCodegen( "Couldn't generate code for rhs in assignment expression" );

		// Create the store instruction
		context.GetBuilder().CreateStore( pValue, pAlloca );

		// We return the variable as our value
		return pVariable->Codegen( context, scope );
	} // end if assignment

	Value* pLeft= m_pLeft->Codegen( context, scope );
	Value* pRight= m_pRight->Codegen( context, scope );
	if( !pLeft || !pRight ) return ErrorCodegen( "Could not generate lhs and rhs for binary expression" );

	// If this is a comparison binop, check that the two operands do not have the same type
	// TODO: Check this at semantic analysis time
	if( IsComparisonBinop(m_binop) && m_pLeft->GetType() != m_pRight->GetType() ) {
		return ErrorCodegen( "Lhs and rhs do not have the same type for comparison binary expression" );
	} // end if error

	// At this point we expect casts to have been done. We just have to see if the operands
	// are floating point or integer
	if( !m_pLeft->GetType()->IsArithmetic() && !m_pRight->GetType()->IsArithmetic() )
		return ErrorCodegen( "Cannot generate binary operation code for non-arithmetic types" );
	if( m_pLeft->GetType()->IsFloatingPoint() != m_pRight->GetType()->IsFloatingPoint() )
		return ErrorCodegen( "Lhs and rhs are not both floating point or integer types" );
	bool bIntegral= m_pLeft->GetType()->IsIntegral();
	switch( m_binop ) {
	case TOKEN_PLUS: return bIntegral
						 ? context.GetBuilder().CreateAdd( pLeft, pRight, "addtmp" )
						 : context.GetBuilder().CreateFAdd( pLeft, pRight, "addtmp" );
	case TOKEN_MINUS: return bIntegral
						  ? context.GetBuilder().CreateSub( pLeft, pRight, "subtmp" )
						  : context.GetBuilder().CreateFSub( pLeft, pRight, "subtmp" );
	case TOKEN_STAR: return bIntegral
						 ? context.GetBuilder().CreateMul( pLeft, pRight, "multmp" )
						 : context.GetBuilder().CreateFMul( pLeft, pRight, "multmp" );
	case TOKEN_SLASH: 
		if( m_pLeft->GetType()->IsSigned() && m_pRight->GetType()->IsSigned() )
			return context.GetBuilder().CreateSDiv( pLeft, pRight, "divtmp" );
		else if( m_pLeft->GetType()->IsUnsigned() && m_pRight->GetType()->IsUnsigned() )
			return context.GetBuilder().CreateUDiv( pLeft, pRight, "divtmp" );
		else if( m_pLeft->GetType()->IsFloatingPoint() && m_pRight->GetType()->IsFloatingPoint() )
			return context.GetBuilder().CreateFDiv( pLeft, pRight, "divtmp" );
	
		// For comparisons, convert bool 0/1 to double 0.0 or 1.0 for now
	case TOKEN_EQ:
		return m_pLeft->GetType()->IsIntegral()
			? context.GetBuilder().CreateICmpEQ( pLeft, pRight, "cmptmp" )
			: context.GetBuilder().CreateFCmpUEQ( pLeft, pRight, "cmptmp" );
	case TOKEN_NEQ:
		return m_pLeft->GetType()->IsIntegral()
			? context.GetBuilder().CreateICmpNE( pLeft, pRight, "cmptmp" )
			: context.GetBuilder().CreateFCmpUNE( pLeft, pRight, "cmptmp" );
	case TOKEN_LT:
		return m_pLeft->GetType()->IsIntegral()
			? context.GetBuilder().CreateICmpULT( pLeft, pRight, "cmptmp" )
			: context.GetBuilder().CreateFCmpULT( pLeft, pRight, "cmptmp" );
	case TOKEN_GT:
		return m_pLeft->GetType()->IsIntegral()
			? context.GetBuilder().CreateICmpUGT( pLeft, pRight, "cmptmp" )
			: context.GetBuilder().CreateFCmpUGT( pLeft, pRight, "cmptmp" );
	case TOKEN_LE:
		return m_pLeft->GetType()->IsIntegral()
			? context.GetBuilder().CreateICmpULE( pLeft, pRight, "cmptmp" )
			: context.GetBuilder().CreateFCmpULE( pLeft, pRight, "cmptmp" );
	case TOKEN_GE:
		return m_pLeft->GetType()->IsIntegral()
			? context.GetBuilder().CreateICmpUGE( pLeft, pRight, "cmptmp" )
			: context.GetBuilder().CreateFCmpUGE( pLeft, pRight, "cmptmp" );
	default:
		return ErrorCodegen( string("Unhandled binary operator '") + Lexer::StringifyToken(m_binop) + "' for binary expression" );
	} // end switch binop
} // end BinopAST::Codegen()


Value* PrefixUnaryAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Emit code for the operand
	Value* pOp= m_pExpr->Codegen( context, scope );
	if( !pOp ) return ErrorCodegen( "Could not emit code for unary prefix operand" );

	switch( m_op ) {
	case TOKEN_NOT:
		// We require a bool type
		if( m_pExpr->GetType() != TypeAST::GetBool() ) return ErrorCodegen( "Unary ! requires a boolean operand" );
		return context.GetBuilder().CreateNot( pOp, "nottmp" );

	case TOKEN_INCREMENT: {
		// We require an integer type
		if( !m_pExpr->GetType()->IsIntegral() ) return ErrorCodegen( "Postfix increment requires an integral operand" );

		// Cast our operand to a variable
		shared_ptr<VariableAST> pVariable= dynamic_pointer_cast<VariableAST>( m_pExpr );
		if( !pVariable ) return ErrorCodegen( "Operand of postfix increment must be a variable" );

		// Lookup the lhs
		AllocaInst* pAlloca= scope.LookupVariable( pVariable->GetName() );
		if( !pAlloca ) return ErrorCodegen( string("Variable \"") + pVariable->GetName() + "\" does not exist while creating postfix increment" );
		Value* pLoad= context.GetBuilder().CreateLoad( pAlloca );

		// Create the assignment
		Value* pOne= ConstantInt::get( getGlobalContext(), APInt(pLoad->getType()->getPrimitiveSizeInBits(), 1, m_pExpr->GetType()->IsSigned()) );
		Value* pValue= context.GetBuilder().CreateAdd( pLoad, pOne, "addtmp" );

		// Emit the store
		context.GetBuilder().CreateStore( pValue, pAlloca );
		// Return the value
		return pValue;
	}

	case TOKEN_DECREMENT: {
		// We require a signed integer type
		if( !m_pExpr->GetType()->IsSigned() ) return ErrorCodegen( "Postfix decrement requires an integral operand" );
		
		// Cast our operand to a variable
		shared_ptr<VariableAST> pVariable= dynamic_pointer_cast<VariableAST>( m_pExpr );
		if( !pVariable ) return ErrorCodegen( "Operand of postfix decrement must be a variable" );

		// Lookup the lhs
		AllocaInst* pAlloca= scope.LookupVariable( pVariable->GetName() );
		if( !pAlloca ) return ErrorCodegen( string("Variable \"") + pVariable->GetName() + "\" does not exist while creating postfix decrement" );
		Value* pLoad= context.GetBuilder().CreateLoad( pAlloca );

		// Create the assignment
		Value* pOne= ConstantInt::get(getGlobalContext(), APInt(pLoad->getType()->getPrimitiveSizeInBits(), 1, m_pExpr->GetType()->IsSigned()));
		Value* pValue= context.GetBuilder().CreateSub( context.GetBuilder().CreateLoad(pAlloca), pOne, "subtmp" );

		// Emit the store
		context.GetBuilder().CreateStore( pValue, pAlloca );
		// Return the value
		return pValue;
	}
	
	default:
		return ErrorCodegen( string("Unhandled prefix unary operator '") + Lexer::StringifyToken(m_op) + "' for unary expression" );
	}
} // end PrefixUnaryAST::Codegen()


Value* PostfixUnaryAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	switch( m_op ) {
	case TOKEN_INCREMENT: {
		// We require an integer type
		if( !m_pExpr->GetType()->IsIntegral() ) return ErrorCodegen( "Postfix increment requires an integral operand" );

		// Cast our operand to a variable
		shared_ptr<VariableAST> pVariable= dynamic_pointer_cast<VariableAST>( m_pExpr );
		if( !pVariable ) return ErrorCodegen( "Operand of postfix increment must be a variable" );

		// Lookup the lhs
		AllocaInst* pAlloca= scope.LookupVariable( pVariable->GetName() );
		if( !pAlloca ) return ErrorCodegen( string("Variable \"") + pVariable->GetName() + "\" does not exist while creating postfix increment" );
		Value* pLoad= context.GetBuilder().CreateLoad( pAlloca );

		// Create the assignment
		Value* pOne= ConstantInt::get( getGlobalContext(), APInt(pLoad->getType()->getPrimitiveSizeInBits(), 1, m_pExpr->GetType()->IsSigned()) );
		Value* pValue= context.GetBuilder().CreateAdd( pLoad, pOne, "addtmp" );

		// Emit the store
		context.GetBuilder().CreateStore( pValue, pAlloca );
		// Return the original value
		return pLoad;
	}

	case TOKEN_DECREMENT: {
		// We require a signed integer type
		if( !m_pExpr->GetType()->IsSigned() ) return ErrorCodegen( "Postfix decrement requires an integral operand" );
		
		// Cast our operand to a variable
		shared_ptr<VariableAST> pVariable= dynamic_pointer_cast<VariableAST>( m_pExpr );
		if( !pVariable ) return ErrorCodegen( "Operand of postfix decrement must be a variable" );

		// Lookup the lhs
		AllocaInst* pAlloca= scope.LookupVariable( pVariable->GetName() );
		if( !pAlloca ) return ErrorCodegen( string("Variable \"") + pVariable->GetName() + "\" does not exist while creating postfix decrement" );
		Value* pLoad= context.GetBuilder().CreateLoad( pAlloca );

		// Create the assignment
		Value* pOne= ConstantInt::get(getGlobalContext(), APInt(pLoad->getType()->getPrimitiveSizeInBits(), 1, m_pExpr->GetType()->IsSigned()));
		Value* pValue= context.GetBuilder().CreateSub( context.GetBuilder().CreateLoad(pAlloca), pOne, "subtmp" );

		// Emit the store
		context.GetBuilder().CreateStore( pValue, pAlloca );
		// Return the original value
		return pLoad;
	}

	default:
		return ErrorCodegen( string("Unhandled prefix unary operator '") + Lexer::StringifyToken(m_op) + "' for unary expression" );
	}
} // end PostfixUnaryAST::Codegen()


Value* CallAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Look up the name in the global module table
	Function* pFunction= context.GetModule()->getFunction( m_pPrototype->GetName() );
	if( !pFunction ) return ErrorCodegen( string("Unknown function \"") + m_pPrototype->GetName() + "\" referenced in call expression" );

	// Check for argument mismatch
	if( pFunction->arg_size() != m_pArgs.size() ) return ErrorCodegen( string("Wrong number of arguments for call expression for function \"") + m_pPrototype->GetName() + "\"" );

	// Codegen function arguments
	vector<Value*> pArgs;
	for( uint i=0; i<m_pArgs.size(); ++i ) {
		pArgs.push_back( m_pArgs[i]->Codegen(context, scope) );
		if( !pArgs.back() ) return ErrorCodegen( "Error generating function argument in call expression\n" );
	} // end for argument

	// We can't assign this temporary a name if it's a void, or LLVM will complain,
	// so only give it the name "calltmp" if it's non-void. For now we still return
	// the Value, but maybe consider returning NULL.
	return context.GetBuilder().CreateCall( pFunction, pArgs.begin(), pArgs.end(), m_pPrototype->GetReturnType() == TypeAST::GetVoid() ? "" : "calltmp" );
} // end CallAST::Codegen()


Value* CastAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Emit code for our expression
	Value* pValue= m_pExpr->Codegen( context, scope );
	if( !pValue ) return ErrorCodegen( "Could not generate code for cast operand" );

	// If this is an empty cast, just return the expression's code
	if( *m_pExpr->GetType() == *m_pType ) return pValue;

	// Floating-point casts
	if( m_pExpr->GetType()->IsFloatingPoint() && m_pType->IsFloatingPoint() )
		return context.GetBuilder().CreateFPCast( pValue, m_pType->Codegen(context, scope), "casttmp" );

	// Unhandled case
	return ErrorCodegen( "Unhandled cast from \"" + m_pExpr->GetType()->GetName() + "\" to \"" + m_pType->GetName() );
} // end CastAST::Codegen()


Function* PrototypeAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Make the function type
	vector<const Type*> pArgTypes( m_pArgs.size() );
	for( uint i=0; i<m_pArgs.size(); ++i ) pArgTypes[i]= m_pArgs[i]->GetType()->Codegen( context, scope );
	
	FunctionType* pFunctionType= FunctionType::get( m_pReturnType->Codegen(context, scope), pArgTypes, false );

	// Create the function
	Function* pFunction= Function::Create( pFunctionType, Function::ExternalLinkage, m_strName, context.GetModule() );

	// If there was a conflict, erase the function and return the existing one
	if( pFunction->getName() != m_strName ) {
		// Delete the function first
		pFunction->eraseFromParent();
		pFunction= context.GetModule()->getFunction( GetName() );
		ASSERT( pFunction );
	} // end if conflict

	return pFunction;
} // end PrototypeAST::Codegen()


Function* FunctionAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Emit code for the prototype. If the prototype has already been emitted,
	// that's fine, and it will be returned
	Function* pFunction= m_pPrototype->Codegen( context, scope );

	// Begin a new scope
	CodegenScope::ScopeEnterSentry s_scope( scope );

	// Create a new basic block for the function's body
	BasicBlock* pBlock= BasicBlock::Create( getGlobalContext(), "entry", pFunction );
	context.GetBuilder().SetInsertPoint( pBlock );

	// Add all arguments to the symbol table and create their allocas
	Function::arg_iterator itArg= pFunction->arg_begin();
	for( uint iArg=0; iArg<GetArgs().size(); ++iArg, ++itArg ) {
		// Create the alloca for this variable
		AllocaInst* pAlloca= CreateEntryBlockAlloca( context, scope, pFunction, GetArgs()[iArg]->GetName(), GetArgs()[iArg]->GetType()->Codegen(context, scope) );
		// Store the initial value into the alloca
		// TODO: is this necessary/correct?
		context.GetBuilder().CreateStore( itArg, pAlloca );
		// Add arguments to the symbol table
		scope.RegisterVariable( GetArgs()[iArg]->GetName(), pAlloca );
	} // end for argument

	// Codegen the body
	m_pBody->Codegen( context, scope );

	// Validate the generated code, checking for consistency
	verifyFunction( *pFunction );

	// Optimize
	context.GetFunctionPassManager().run( *pFunction );

	return pFunction;

	// If we had an error generating the body, remove the function and give an error
	pFunction->eraseFromParent();
	return static_cast<Function*>(ErrorCodegen(string("Could not create the body for function \"") + GetName() + "\""));
} // end FunctionAST::Codegen()


void ReturnAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// If our expression is NULL, return void
	if( !m_pExpr ) {
		context.GetBuilder().CreateRetVoid();
		return;
	}

	// Just emit code for the return expression
	Value* pRet= m_pExpr->Codegen( context, scope );
	if( !pRet ) { ErrorCodegen( "Could not emit code for return expression" ); return; }
	context.GetBuilder().CreateRet( pRet );
} // end ReturnAST::Codegen()


void DeclarationAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	Function* pFunction= context.GetBuilder().GetInsertBlock()->getParent();

	// Emit the initializer before adding the variable to scope, to prevent the
	// initializer from referencing the variable itself
	Value* pInitVal;
	if( m_pInitializer ) {
		pInitVal= m_pInitializer->Codegen( context, scope );
		if( !pInitVal ) { ErrorCodegen( string("Error emitting code for initializer for the declaration of variable \"") + GetName() + "\"" ); return; }
	}

	AllocaInst* pAlloca= CreateEntryBlockAlloca( context, scope, pFunction, GetName(), m_pType->Codegen(context, scope) );
	if( m_pInitializer ) context.GetBuilder().CreateStore( pInitVal, pAlloca );

	// Register the binding
	bool bSuccess= scope.RegisterVariable( GetName(), pAlloca );
	if( !bSuccess ) { ErrorCodegen( string("Variable \"") + GetName() + "\" already exists in variable declaration"  ); return; }
} // end DeclarationAST::Codegen()


void BlockAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Enter a new scope
	CodegenScope::ScopeEnterSentry s_scope( scope );

	// We return a value only if it's a return instruction
	Value* pRet= NULL;

	// Emit code for each of our statements
	for( uint iStmt=0; iStmt<m_pStmts.size(); ++iStmt ) {
		m_pStmts[iStmt]->Codegen( context, scope );

		// Check for a return
		if( dynamic_cast<ReturnAST*>(m_pStmts[iStmt].get()) ) {
			if( iStmt+1<m_pStmts.size() ) { ErrorCodegen( string("Return expression in a block was not the last expression") ); return; }
		} // end if return
	} // end for expression
} // end BlockAST::Codegen()


void ConditionalAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	// Generate code for evaluating the conditional
	Value* pCond= m_pCondExpr->Codegen( context, scope );
	if( !pCond ) { ErrorCodegen( "Could not emit code for condition in conditional statement" ); return; }
	// Convert to bool by comparing equal to 0.0
	//pCond= context.GetBuilder().CreateFCmpONE( pCond, ConstantFP::get(getGlobalContext(), APFloat(0.0), "ifcond") );

	// Grab the function
	Function* pFunction= context.GetBuilder().GetInsertBlock()->getParent();

	// Create blocks for then/else. Insert "then" block at the end of the function
	BasicBlock* pThenBlock= BasicBlock::Create( getGlobalContext(), "then", pFunction );
	BasicBlock* pElseBlock= BasicBlock::Create( getGlobalContext(), "else" );
	BasicBlock* pMergeBlock= BasicBlock::Create( getGlobalContext(), "ifcont" );

	context.GetBuilder().CreateCondBr( pCond, pThenBlock, pElseBlock );
	
	// Emit "then"
	context.GetBuilder().SetInsertPoint( pThenBlock );
	m_pIfStmt->Codegen( context, scope );
	// Only create the branch to the merge block if there's no return statement
	if( !m_pIfStmt->HasReturn() ) context.GetBuilder().CreateBr( pMergeBlock );

	// Codegen of "then" can change current block, so update the then block for the PHI
	pThenBlock= context.GetBuilder().GetInsertBlock();

	// Emit else block. If we have no else block, just emit a jump to the merge block
	pFunction->getBasicBlockList().push_back( pElseBlock );
	context.GetBuilder().SetInsertPoint( pElseBlock );
	if( m_pElseStmt ) m_pElseStmt->Codegen( context, scope );

	// Only create the branch to the merge block if there's no return statement
	if( !m_pElseStmt || !m_pElseStmt->HasReturn() ) context.GetBuilder().CreateBr( pMergeBlock );
	pElseBlock= context.GetBuilder().GetInsertBlock();

	// Emit merge block
	pFunction->getBasicBlockList().push_back( pMergeBlock );
	context.GetBuilder().SetInsertPoint( pMergeBlock );
} // end ConditionalAST::Codegen()


void ForAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	/* Output this as:

		initializer_statement
		goto loop
	loop:
		body_statement
		update_expression
		endcond = condexpr
		br endcond, loop, endloop
	endloop:
	*/

	// Enter a new scope
	CodegenScope::ScopeEnterSentry s_scope( scope );

	// Emit the initializer expression
	m_pInitializer->Codegen( context, scope );

	// Make the new basic block for the loop header, inserting after current block
	Function* pFunction= context.GetBuilder().GetInsertBlock()->getParent();
	BasicBlock* pLoopBlock= BasicBlock::Create( getGlobalContext(), "loop", pFunction );

	// Create an explicit fallthrough from the current block to the loop block
	context.GetBuilder().CreateBr( pLoopBlock );

	// Start insertion in the loop block
	context.GetBuilder().SetInsertPoint( pLoopBlock );
	
	// Emit the body statement followed by the update expression
	m_pBody->Codegen( context, scope );
	Value* pUpdateValue= m_pUpdate->Codegen( context, scope );
	if( !pUpdateValue ) return (void)ErrorCodegen( "Could not generate code in for loop update expression" );

	// Compute the end condition
	Value* pCondition= m_pCondition->Codegen( context, scope );
	if( !pCondition ) return (void)ErrorCodegen( "could not generate code in for loop condition expression" );

	// Create the end loop block and add the conditional branch to it
	BasicBlock* pLoopEndBlock= BasicBlock::Create( getGlobalContext(), "endloop", pFunction );
	context.GetBuilder().CreateCondBr( pCondition, pLoopBlock, pLoopEndBlock );

	// Set the insert point for any additional code to the loop end block
	context.GetBuilder().SetInsertPoint( pLoopEndBlock );
} // end ForAST::Codegen()


void ExprStmtAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	Value* pValue= m_pExpr->Codegen( context, scope );
	if( !pValue ) (void)ErrorCodegen( "Could not generate code for expression statement" );

	return;
} // end ExprStmtAST::Codegen()


const Type* TypeAST::Codegen( CodegenContext& context, CodegenScope& scope ) const {
	if( *this == *GetInt() ) return Type::getInt64Ty(getGlobalContext());
	else if( *this == *GetFloat() ) return Type::getFloatTy(getGlobalContext());
	else if( *this == *GetDouble() ) return Type::getDoubleTy(getGlobalContext());
	else if( *this == *GetBool() ) return Type::getInt1Ty(getGlobalContext());
	else if( *this == *GetVoid() ) return Type::getVoidTy(getGlobalContext());
	else {
		cerr << "Unknown type \"" << m_strType << "\"";
		return NULL;
	}
} // end TypeAST::Codegen()


//! Runs code generation for this module, returning TRUE on success and FALSE on failure
bool ModuleAST::Codegen( CodegenContext& context, CodegenScope& scope ) {
	// Generate prototypes before functions
	for( uint iPrototype=0; iPrototype<m_pPrototypes.size(); ++iPrototype ) {
		Function* pPrototype= m_pPrototypes[iPrototype]->Codegen( context, scope );
		if( !pPrototype ) return false;
	} // end for prototype

	// Now generate functions
	for( uint iFunction=0; iFunction<m_pFunctions.size(); ++iFunction ) {
		Function* pFunction= m_pFunctions[iFunction]->Codegen( context, scope );
		if( !pFunction ) return false;
	} // end for function

	return true;
} // end ModuleAST::Codegen()