#include "common.h"
#include "Parser.h"
#include "AST.h"
#include "Lexer.h"
#include "SemBinop.h"
#include "SemCast.h"
#include "DiagContext.h"
#include "DiagStream.h"

//! Initialize
Parser::Parser() :
	m_pLexer(NULL),
  m_pDiagContext(NULL) {

	// Initialize our scope with our built-in types
	for( vector<shared_ptr<const TypeAST>>::const_iterator itType=BuiltinTypeAST::GetBuiltinTypes().begin(); itType!=BuiltinTypeAST::GetBuiltinTypes().end(); ++itType ) {
		// Don't add the error type*
		if( **itType == *BuiltinTypeAST::GetError() ) continue;

		// Add the type to our map
		m_parseScope.types[(*itType)->GetName()]= *itType;
	} // end for type

} // end Parser::Parser()


//! Runs the main parsing loop with the given lexer
//! Returns TRUE on success or FALSE on failure
bool Parser::Run( Lexer& lexer, DiagContext& diagContext ) {
  m_pLexer= &lexer;
  m_pDiagContext= &diagContext;

	// Parse until we hit the end of the file or an error is encountered
	bool bEOF= false, bError= false;
	while( !bEOF && !bError ) {
		switch( m_pLexer->GetCurrentToken() ) {
		case TOKEN_EOF:
			bEOF= true;
			break;
		case TOKEN_DEF: {
			// Attempt to parse the declaration or definition; only one pointer will be non-NULL
      const SourceLocation locBegin= m_pLexer->GetSourceRange().begin;
			pair<shared_ptr<PrototypeAST>, shared_ptr<FunctionAST>> functionRet= parseFunctionDeclarationOrDefinition();
			// Add the prototype or definition to the appropriate list
			if( functionRet.first ) m_pPrototypes.push_back( functionRet.first );
			else if( functionRet.second ) m_pFunctions.push_back( functionRet.second );
			else {
				m_pDiagContext->Error( error_parsing_function, SourceRange(locBegin, m_pLexer->GetSourceRange().begin) );
				bError= true;
			} // end if error
			break;
		} // end case function declaration/definition
		default:
      m_pDiagContext->Error( error_unexpected_token, m_pLexer->GetSourceRange() )
                             << Lexer::StringifyToken(m_pLexer->GetCurrentToken());
			bError= true;
			break;
		} // end switch current token
	} // end while not eof or error

  m_pLexer= NULL;
  m_pDiagContext= NULL;
	return !bError;
} // end Parser::Run()


//! Creates a module AST node from everything we've parsed so far
ModuleAST* Parser::CreateModule() {
  return new ModuleAST( m_pPrototypes, m_pFunctions );
} // end Parser::CreateModule()


//! Parses a primary expression.
shared_ptr<ExprAST> Parser::parsePrimaryExpression() {
	/*
	primary_expression
		::= identifier_expr
		::= literal_expr
		::= paren_expr
		::= arraysize_expr
	*/

	// We expect an identifier, literal, or lparen
	shared_ptr<ExprAST> pExpr;
  const SourceLocation locBegin= m_pLexer->GetSourceRange().begin;
	if( m_pLexer->GetCurrentToken() == TOKEN_IDENTIFIER ) {
		const string strName= m_pLexer->GetIdentifier();
		pExpr= parseIdentifierExpression();
		if( !pExpr ) m_pDiagContext->Error( error_variable_not_in_scope, SourceRange(locBegin, m_pLexer->GetSourceRange().begin) ) << strName;
	} else if( Lexer::IsLiteralToken(m_pLexer->GetCurrentToken()) ) {
		pExpr= parseLiteralExpression();
    if( !pExpr ) m_pDiagContext->Error( error_could_not_parse_literal, SourceRange(locBegin, m_pLexer->GetSourceRange().begin) ) << m_pLexer->GetLiteral();
	} else if( m_pLexer->GetCurrentToken() == TOKEN_LPAREN ) {
		pExpr= parseParenExpression();
		if( !pExpr ) m_pDiagContext->Error( error_could_not_parse_paren_expr, SourceRange(locBegin, m_pLexer->GetSourceRange().begin) );
	} else if( m_pLexer->GetCurrentToken() == TOKEN_ARRAYSIZE ) {
		pExpr= parseArraySizeExpression();
    if( !pExpr ) m_pDiagContext->Error( error_could_not_parse_arraysize, SourceRange(locBegin, m_pLexer->GetSourceRange().begin) );
	} else {
		m_pDiagContext->Error( error_primary_expr_expected_found, SourceRange(locBegin, m_pLexer->GetSourceRange().begin) ) << Lexer::StringifyToken(m_pLexer->GetCurrentToken());
		return NULL;
	}

	return pExpr;
} // end Parser::parsePrimaryExpression()


//! Parses an identifier expression (variable or function call)
shared_ptr<ExprAST> Parser::parseIdentifierExpression() {
	/*
	identifier_expr
		::= identifier
		::= call_expr
		::= cast_expr
		::= array_ref_expr
	*/

	// This is a variable, or a cast/function call. Determine this by whether the next token is a lparen.
  if( !expectToken(TOKEN_IDENTIFIER) ) return NULL;
  const SourceRange rangeName= m_pLexer->GetSourceRange();
  const string& strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();

	// If this is a function call, parse the call expression and return it
	// If the identifier is a type, then treat this as a cast, otherwise, treat it as a function call
	if( m_pLexer->GetCurrentToken() == TOKEN_LPAREN ) {
		if( findTypeInScope(strName) ) return parseCastExpression( strName, rangeName );
		else return parseCallExpression( strName, rangeName );
	} // end if lparen

	// Check for '[', which indicates an array reference
	if( m_pLexer->GetCurrentToken() == TOKEN_LBRACKET ) {
		// Look up the array declaration
		shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strName );
		if( !pDeclaration ) {
      m_pDiagContext->Error( error_variable_not_in_scope, rangeName ) << strName;
			return NULL;
		} // end if not declared

		// Eat the left bracket, and parse the index expression
		m_pLexer->GetNextToken();

		shared_ptr<ExprAST> pIndex= parseExpression();
		if( !pIndex ) {
			m_pDiagContext->Error( error_could_not_parse_array_index, pIndex->GetSourceRange() );
			return NULL;
		} // end if error

		// Eat the right bracket
    SourceLocation locEnd= m_pLexer->GetSourceRange().end;
    if( !expectEatToken(TOKEN_RBRACKET) ) return NULL;

		// Create the array reference expression
		return shared_ptr<ExprAST>( new ArrayRefAST(SourceRange(rangeName.begin, locEnd), pDeclaration, pIndex) );		
	} // end if array reference

	// If we're here, this is a variable reference
	// Look up the declaration, and give an error if it hasn't been declared
	shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strName );
	// Only create an AST node if the declaration exists
	if( pDeclaration ) return shared_ptr<ExprAST>( new VariableAST(rangeName, pDeclaration) );
	else return NULL;
} // end Parser::parseIdentifierExpression()


//! Parses a literal expression
shared_ptr<LiteralAST> Parser::parseLiteralExpression() {
	// See makeLiteral() for the definition of a literal_expr

	// Create the literal expression and do error handling. Eat the token in any case.
	shared_ptr<LiteralAST> pLiteral= makeLiteral( m_pLexer->GetCurrentToken(),
                                                m_pLexer->GetLiteral(),
                                                m_pLexer->GetSourceRange() );
	m_pLexer->GetNextToken();
	
	return pLiteral;
} // end Parser::ParseLiteralExpression()


//! Parses a parenthesized expression
shared_ptr<ExprAST> Parser::parseParenExpression() {
	/*
	paren_expr
		::= '(' expression ')'
	*/

	// Eat the left paren
	if( !expectEatToken(TOKEN_LPAREN) ) return NULL;
	// Parse an expression up until the next unmatched rparen
	shared_ptr<ExprAST> pExpr= parseExpression();
	// Eat the rparen
	if( !expectEatToken(TOKEN_RPAREN) ) return NULL;

	return pExpr;
} // end Parser::parseParenExpression()


//! Parses an arraysize expression
shared_ptr<ArraysizeAST> Parser::parseArraySizeExpression() {
	/*
	arraysize_expr
		::= 'arraysize' '(' identifier ')'
	*/

	// Eat 'arraysize'
  const SourceLocation locArraysize= m_pLexer->GetSourceLocation();
  if( !expectEatToken(TOKEN_ARRAYSIZE) ) return NULL;

	// Eat the lparen
  if( !expectEatToken(TOKEN_LPAREN) ) return NULL;

	// Parse the identifier
  if( !expectToken(TOKEN_IDENTIFIER) ) return NULL;
  const SourceRange rangeIdentifier= m_pLexer->GetSourceRange();
	const string strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();

	// Eat the rparen
	if( !expectEatToken(TOKEN_RPAREN) ) return NULL;
  const SourceRange rangeExpr( locArraysize, m_pLexer->GetSourceLocation() );

	// Look up the variable in scope, and verify that it's an array with a length specified
	shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strName );
	if( !pDeclaration ) {
    m_pDiagContext->Error( error_variable_not_in_scope, rangeIdentifier ) << strName;
		return NULL;
	} // end if no declaration

	shared_ptr<const ArrayTypeAST> pArrayType= dynamic_pointer_cast<const ArrayTypeAST>(pDeclaration->GetType());
	if( !pArrayType ) {
    m_pDiagContext->Error( error_arraysize_not_array, rangeIdentifier ) << strName;
		return NULL;
	} // end if not array

	shared_ptr<ExprAST> pSize= pArrayType->GetLengthExpression();
	if( !pSize ) {
    m_pDiagContext->Error( error_arraysize_no_size, rangeIdentifier ) << strName;
		return NULL;
	} // end if no size

	return make_shared<ArraysizeAST>( rangeExpr, pSize );
} // end Parser::parseArraySizeExpression()


//! Parses a unary operator expression.
shared_ptr<ExprAST> Parser::parseUnaryOpExpression() {
	/*
	unary_op_expr
		::= primary_expression
		::= pre_unary_op unary_op_expr
		::= unary_op_expr post_unary_op
		::= unary_op_expr '[' expression ']'
	*/

	// Check for a prefix unary operator
	if( Lexer::IsPreUnaryOpToken(m_pLexer->GetCurrentToken()) ) {
		// Store and eat the token
		Token op= m_pLexer->GetCurrentToken();
    const SourceLocation locOp= m_pLexer->GetSourceLocation();
		m_pLexer->GetNextToken();

		// Parse the following unary op expression
    const SourceLocation locOperand= m_pLexer->GetSourceLocation();
		shared_ptr<ExprAST> pExpr= parseUnaryOpExpression();
		if( !pExpr ) {
      m_pDiagContext->Error( error_expected_expr, SourceRange(locOperand, m_pLexer->GetSourceLocation()) ) << "unary-op-expression";
			return NULL;
		} // end if parse error

		// Increment and decrement require a variable operand for now
		if( (op == TOKEN_INCREMENT || op == TOKEN_DECREMENT) && !dynamic_pointer_cast<VariableAST>(pExpr) ) {
			m_pDiagContext->Error( error_prefix_inc_dec_require_variable, pExpr->GetSourceRange() );
			return NULL;
		} // end if not variable

		// Create and return the AST node
		return shared_ptr<ExprAST>( new PrefixUnaryAST(SourceRange(locOp, pExpr->GetSourceRange().end), op, pExpr) );
	} // end if prefix operator

	// If we don't have a prefix operator, then we have either a primary expression,
	// or a unary op expr that starts with a primary expression. First parse the
	// primary expression
  const SourceLocation locPrimaryExpr= m_pLexer->GetSourceLocation();
	shared_ptr<ExprAST> pPrimaryExpr= parsePrimaryExpression();
  const SourceRange rangePrimaryExpr= pPrimaryExpr ? pPrimaryExpr->GetSourceRange() : SourceRange(locPrimaryExpr, m_pLexer->GetSourceLocation());
	if( !pPrimaryExpr ) {
    m_pDiagContext->Error( error_expected_expr, rangePrimaryExpr ) << "primary-expression";
		return NULL;
	} // end if parse error

	// Now, if we have a postfix unary operator token, create the AST node for it
	if( Lexer::IsPostUnaryOpToken(m_pLexer->GetCurrentToken()) ) {
		// Store and eat the token
		Token op= m_pLexer->GetCurrentToken();
		m_pLexer->GetNextToken();
    const SourceLocation locOpEnd= m_pLexer->GetSourceLocation();

		// Increment and decrement require a variable operand for now
		if( (op == TOKEN_INCREMENT || op == TOKEN_DECREMENT) && !dynamic_pointer_cast<VariableAST>(pPrimaryExpr) ) {
			m_pDiagContext->Error( error_prefix_inc_dec_require_variable, rangePrimaryExpr );
			return NULL;
		} // end if not variable

		return shared_ptr<ExprAST>( new PostfixUnaryAST(SourceRange(pPrimaryExpr->GetSourceRange().begin, locOpEnd), op, pPrimaryExpr) );
	} // end if postfix unary operator

	// If we got here, it's just a primary expression
	return pPrimaryExpr;
} // end Parser::parseUnaryOpExpression()


//! Parses the right-hand side of a binary expression, given the left-hand side and its highest-precedence operator
shared_ptr<ExprAST> Parser::parseBinopRHS( int precedence, shared_ptr<ExprAST> pLeft ) {
	while( 1 ) {
		// We expect a binop here. If it does not bind as tightly as our current binop,
		// then just return the LHS and we are done
		if( !Lexer::IsBinopToken(m_pLexer->GetCurrentToken()) || GetBinopPrecedence(m_pLexer->GetCurrentToken()) < precedence ) {
			return pLeft;
		} // end if done

		// Save and eat the binop
		Token binop= m_pLexer->GetCurrentToken();
		m_pLexer->GetNextToken();

		// Parse the unary op expression after the binop
    const SourceLocation locUnaryOp= m_pLexer->GetSourceLocation();
		shared_ptr<ExprAST> pRight= parseUnaryOpExpression();
    const SourceRange rangeRight= pRight ? SourceRange(locUnaryOp, m_pLexer->GetSourceLocation()) : pRight->GetSourceRange();
		if( !pRight ) {
      m_pDiagContext->Error( error_expected_expr, rangeRight ) << "binary operator right-hand primary-expression";
			return NULL;
		} // end if error

		// Now check for another binop
		// If the binop binds less tightly with the RHS than the operator after
		// the RHS, then let the pending operator take the RHS as its LHS
		if( Lexer::IsBinopToken(m_pLexer->GetCurrentToken()) && GetBinopPrecedence(binop) < GetBinopPrecedence(m_pLexer->GetCurrentToken()) ) {
			pRight= parseBinopRHS( GetBinopPrecedence(binop), pRight );
			if( !pRight ) {
				return NULL;
			} // end if error
		} // end if parsing rhs

		// Before we construct the binop AST node, check for an assignment expression
		// These are special-cased: the LHS must be a variable
		if( binop == TOKEN_ASSIGN && !dynamic_pointer_cast<VariableAST>(pLeft) ) {
      m_pDiagContext->Error( error_left_of_assignment_must_be_variable, pLeft->GetSourceRange() );
			return NULL;
		} // end if assignment

    const SourceRange rangeExpr( pLeft->GetSourceRange().begin, pRight->GetSourceRange().end );

		// If the lhs and rhs don't have the same type, check for an implicit conversion
		if( pLeft->GetType() != pRight->GetType() ) {
			shared_ptr<const TypeAST> pType= GetBinaryOpsImplicitCastType( pLeft, pRight );
			if( !pType ) {
        m_pDiagContext->Error( error_no_binop_implicit_conversion, rangeExpr ) << pLeft->GetType()->GetName() << pRight->GetType()->GetName();
				return NULL;
			} // end if no implicit conversion

			// Cast the LHS and RHS as necessary
			if( *pLeft->GetType() != *pType ) pLeft.reset( new CastAST(pLeft->GetSourceRange(), pLeft, pType) );
			if( *pRight->GetType() != *pType ) pRight.reset( new CastAST(pRight->GetSourceRange(), pRight, pType) );
		} // end if not same type

		// Merge LHS and RHS
		pLeft.reset( new BinopAST(rangeExpr, binop, pLeft, pRight) );
	} // end while parsing

	// We should never get here
	ASSERT( false );
	return NULL;
} // end parseBinopRHS()


//! Parses a function declaration or definition.
//! Returns either a prototype or full function
pair<shared_ptr<PrototypeAST>, shared_ptr<FunctionAST>> Parser::parseFunctionDeclarationOrDefinition() {
	/*
		argument_list ::=
			''
			identifier identifier ',' argument_list
		
		function_declaration
			::= 'def' identifier '(' argument_list ')' -> identifier ';'

		function_definition
			::= 'def' identifier '(' argument_list ')' -> identifier '{' expression_list '}'
		
	*/

	typedef pair<shared_ptr<PrototypeAST>, shared_ptr<FunctionAST>> ret_pair_type;
	const ret_pair_type null_ret( nullptr, nullptr );

	// Check for "def" and eat it
  const SourceRange rangeDef= m_pLexer->GetSourceRange();
  if( !expectEatToken(TOKEN_DEF) ) return null_ret;

	// Parse the identifier for the function's name
  if( !expectToken(TOKEN_IDENTIFIER) ) return null_ret;

	// Store the identifier and eat it
	string strName= m_pLexer->GetIdentifier();
  const SourceRange rangeName= m_pLexer->GetSourceRange();
  m_pLexer->GetNextToken();

	// Parse the argument list
	// Eat the lparen
  if( !expectEatToken(TOKEN_LPAREN) ) return null_ret;

	vector< shared_ptr<DeclarationAST> > pArgs;
	while( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		// typename identifier ','
    if( !expectToken(TOKEN_IDENTIFIER) ) return null_ret;
    const SourceLocation locTypeBegin= m_pLexer->GetSourceLocation();
		shared_ptr<const TypeAST> pType( parseType() );
    const SourceLocation locTypeEnd= m_pLexer->GetSourceLocation();

		// Prevent void arguments
		if( *pType == *BuiltinTypeAST::GetVoid() ) {
      m_pDiagContext->Error( error_void_argument, SourceRange(locTypeBegin, locTypeEnd) );
			return null_ret;
		} // end if void type

    if( !expectToken(TOKEN_IDENTIFIER) ) return null_ret;

		string strVariable= m_pLexer->GetIdentifier();
    const SourceRange rangeVariable= m_pLexer->GetSourceRange();
		m_pLexer->GetNextToken();

		// Create a declaration for this argument
		// Don't add it to scope until later, if this is a full function definition
    const SourceRange rangeDeclaration( locTypeBegin, rangeVariable.end );
		shared_ptr<DeclarationAST> pDeclaration( new DeclarationAST(rangeDeclaration, strVariable, pType) );

		// Now we can have a comma or rparen
		if( m_pLexer->GetCurrentToken() == TOKEN_COMMA ) {
			// Eat the comma before parsing the next argument
			m_pLexer->GetNextToken();
		} else if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
      m_pDiagContext->Error( error_expected_in_function_arg_list, m_pLexer->GetSourceRange() ) << Lexer::StringifyToken(m_pLexer->GetCurrentToken());
			return null_ret;
		}

		pArgs.push_back( pDeclaration );
	} // end while parsing argument list

	// We should have only gotten here by hitting a rparen
	// Eat the rparen
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RPAREN );
	m_pLexer->GetNextToken();

	// We allow functions to indicate that they return no type by simply
	// omitting the arrow and type. Therefore, we can have either an arrow,
	// semicolon, or brace
	shared_ptr<const TypeAST> pReturnType= BuiltinTypeAST::GetVoid();

	// If we have an arrow, parse the return type
	if( m_pLexer->GetCurrentToken() == TOKEN_ARROW ) {
		// Eat the arrow
		m_pLexer->GetNextToken();

		// Parse the return type
		if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
      m_pDiagContext->Error( error_expected_typename, m_pLexer->GetSourceRange() );
			return null_ret;
		} // end if no identifier

		pReturnType= parseType();
	} // end if return type specified

	// Now lookup the prototype, and create it if it does not exist
	shared_ptr<PrototypeAST> pPrototype= findPrototypeInScope( strName );
	if( !pPrototype ) pPrototype.reset( new PrototypeAST(rangeName, strName, pReturnType, pArgs) );
	bool bPrototypeDoesNotExist= addPrototypeToScope( pPrototype ); ASSERT( bPrototypeDoesNotExist );

	// Now we expect either an opening brace for a function definition, or a semicolon for a function prototype
	if( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON ) {
		// Eat the semicolon and return the prototype
		m_pLexer->GetNextToken();
		return ret_pair_type( pPrototype, nullptr );
	} // end if function prototype

	// Now look for the opening brace
  if( !expectToken(TOKEN_LBRACE) ) return null_ret;

	// See if this function is already defined in scope. If it is, give an error and return,
	// since a function can have multiple prototypes but only one body.
	if( isFunctionDefinedInScope(strName) ) {
		m_pDiagContext->Error( error_function_already_defined, rangeName ) << strName;
		return null_ret;
	} // end if already defined

	// If this is a full function definition, start a new scope, and add the variable declarations so the function body can reference them
	ScopeSentry s_scope( this );
	for( uint iArg=0; iArg<pArgs.size(); ++iArg ) if( !addVariableToScope(pArgs[iArg]) ) return null_ret;

	// Set the expected return type for this function, so the expressions for return statements
	// can be implicitly casted to this type
	setExpectedReturnType( pReturnType );

	// Parse the function's body
  const SourceLocation locBodyBegin= m_pLexer->GetSourceLocation();
	shared_ptr<BlockAST> pBody( parseBlock() );
  const SourceRange rangeBody= pBody ? pBody->GetSourceRange() : SourceRange(locBodyBegin, m_pLexer->GetSourceLocation());
	if( !pBody ) {
		m_pDiagContext->Error( error_could_not_parse_function_body, rangeBody );
		return null_ret;
	} // end if couldn't parse body

	// Add the function to scope
  shared_ptr<FunctionAST> pFunction( new FunctionAST(SourceRange(rangeDef.begin, pBody->GetSourceRange().end), pPrototype, pBody) );
	setFunctionDefinedInScope( strName );

	return ret_pair_type( nullptr, pFunction );
} // end Parser::parseFunctionDeclarationOrDefinition()


//! Parses a block
shared_ptr<BlockAST> Parser::parseBlock() {
	/* block
		::= '{' statements* '}'
	*/

	// Enter a new scope
	ScopeSentry s_scope( this );

  const SourceLocation locLbrace= m_pLexer->GetSourceLocation();
  if( !expectEatToken(TOKEN_LBRACE) ) return NULL;

	vector<shared_ptr<StmtAST>> pExprs;
	while( m_pLexer->GetCurrentToken() != TOKEN_RBRACE ) {
    const SourceLocation locExprBegin= m_pLexer->GetSourceLocation();
		shared_ptr<StmtAST> pExpr( parseStatement() );
    const SourceRange rangeExpr= pExpr ? pExpr->GetSourceRange() : SourceRange(locExprBegin, m_pLexer->GetSourceLocation());
		if( !pExpr ) {
      m_pDiagContext->Error( error_expected_stmt, rangeExpr );
			return NULL;
		} // end if parse error

		pExprs.push_back( pExpr );
	} // end while looking for a right brace

	// Eat the right brace
	if( !expectEatToken(TOKEN_RBRACE) ) return NULL;
  const SourceRange rangeBlock( locLbrace, m_pLexer->GetSourceLocation() );

	return shared_ptr<BlockAST>( new BlockAST(rangeBlock, pExprs) );
} // end Parser::parseBlock()


//! Parses a statement
shared_ptr<StmtAST> Parser::parseStatement() {
	/*
	statement
		::= primary_statement
		::= return_statement
		::= block
		::= conditional_statement
		::= for_statement
    ::= for_range_statement
		::= while_statement
	*/

	switch( m_pLexer->GetCurrentToken() ) {
	case TOKEN_RETURN: return parseReturnStatement();
	case TOKEN_LBRACE: return parseBlock();
	case TOKEN_IF: return parseConditionalStatement();
	case TOKEN_FOR: return parseForStatement();
  case TOKEN_FOREACH: return parseForRangeStatement();
	case TOKEN_WHILE: return parseWhileStatement();
	// Parse everything else as a primary statement, since they can start
	// with different kinds of tokens
	default: return parsePrimaryStatement();
	} // end switch current token
} // end Parser::parseStatement()


//! Parses a primary statement.
shared_ptr<PrimaryStmtAST> Parser::parsePrimaryStatement() {
	// For now, it might be helpful to think of a primary statement as one that starts with an identifer

	/*
	primary_statement
		::= expression ';'
		::= variable_declaration
	*/

	// This is either a variable declaration statement or expression statement
	// If we have an identifier aand the identifier is a type we recognize, then
	// parse it as a variable declaration, otherwise try to parse an expression
	if( m_pLexer->GetCurrentToken() == TOKEN_IDENTIFIER && findTypeInScope(m_pLexer->GetIdentifier()) ) {
		// Parse this as a variable declaration
		return parseVariableDeclaration();
	} else {
		// Parse this as an expression statement
    const SourceLocation locExprBegin= m_pLexer->GetSourceLocation();
		shared_ptr<ExprAST> pExpr= parseExpression();
    const SourceRange rangeExpr( locExprBegin, m_pLexer->GetSourceLocation() );
		if( !pExpr ) {
      m_pDiagContext->Error( error_expected_expr, rangeExpr ) << "expression in expression statement";
			return NULL;
		} // end if could not parse

		// We expect to eat a semicolon
	  if( !expectEatToken(TOKEN_SEMICOLON) ) return NULL;
    const SourceRange rangeStmt( pExpr->GetSourceRange().begin, m_pLexer->GetSourceLocation() );

		// Return the statement expression
		return shared_ptr<PrimaryStmtAST>( new ExprStmtAST(rangeStmt, pExpr) );
	}
} // end Parser::parsePrimaryStatement()


//! Parses an expression.
shared_ptr<ExprAST> Parser::parseExpression() {
	/*
	expression
		::= unary_op_expr binoprhs
	*/

	// Parse the LHS
  const SourceLocation locBegin= m_pLexer->GetSourceLocation();
	shared_ptr<ExprAST> pLeft= parseUnaryOpExpression();
  const SourceRange rangeLeft( locBegin, m_pLexer->GetSourceLocation() );
	if( !pLeft ) {
    m_pDiagContext->Error( error_expected_expr, rangeLeft ) << "unary-op-expression";
		return NULL;
	} // end if error
	
	// Parse the RHS
	return parseBinopRHS( 0, pLeft );
} // end Parser:parseExpression()


//! Parses a return statment
shared_ptr<ReturnAST> Parser::parseReturnStatement() {
	/* return_stmt
		::= 'return' ';'
		::= 'return' expression ';'
	*/

	// Eat "return"
  const SourceLocation locReturnBegin= m_pLexer->GetSourceLocation();
  if( !expectEatToken(TOKEN_RETURN) ) return NULL;

	// Parse the expression, if we have one
	shared_ptr<ExprAST> pExpr;
	if( m_pLexer->GetCurrentToken() != TOKEN_SEMICOLON ) {
		pExpr= parseExpression();
		if( !pExpr ) return NULL;
	} // end if parsing return expression

	// Now eat the semicolon
	if( !expectEatToken(TOKEN_SEMICOLON) ) return NULL;
  const SourceRange rangeReturn( locReturnBegin, m_pLexer->GetSourceLocation() );

	// Check for the expected return type. If it doesn't exist, that means this
	// statement is not inside a function body, and that's an error.
	if( !getExpectedReturnType() ) {
		m_pDiagContext->Error( error_return_outside_function_body, rangeReturn );
		return NULL;
	} // end if no return type

	// Implicitly cast to the expected return type, if we have a different type
	if( pExpr && *pExpr->GetType() != *getExpectedReturnType() ) {
		if( IsImplicitCastAllowed(pExpr, getExpectedReturnType()) ) {
			pExpr.reset( new CastAST(pExpr->GetSourceRange(), pExpr, getExpectedReturnType()) );
		} else {
      m_pDiagContext->Error( error_no_implicit_cast, pExpr->GetSourceRange() )
        << pExpr->GetType()->GetName() << getExpectedReturnType()->GetName();
			return NULL;
		}
	} // end if not type type

	return shared_ptr<ReturnAST>( new ReturnAST(rangeReturn, pExpr) );
} // end Parser::parseReturnStatement()


//! Parses a variable declaration.
shared_ptr<DeclarationAST> Parser::parseVariableDeclaration() {
	/* variable_declaration
		::= type identifier ';'
		::= type identifier '=' expression ';'
	*/

	// Parse the type
  const SourceLocation locTypeBegin= m_pLexer->GetSourceLocation();
	shared_ptr<const TypeAST> pType= parseType();
  const SourceRange rangeType( locTypeBegin, m_pLexer->GetSourceLocation() );
	if( !pType ) {
    m_pDiagContext->Error( error_expected_typename, rangeType );
		return NULL;
	} // end if could not parse type

	// Disallow variables of type "void"
	if( *pType == *BuiltinTypeAST::GetVoid() ) {
    m_pDiagContext->Error( error_void_variable, rangeType );
		return NULL;
	} // end if void

	// Now parse the variable identifier
  if( !expectToken(TOKEN_IDENTIFIER) ) return NULL;
	const string strName= m_pLexer->GetIdentifier();
  const SourceRange rangeName= m_pLexer->GetSourceRange();
	m_pLexer->GetNextToken();

	// We expect either a semicolon, in which case
	// we are done, or an equals sign, in which case we will parse another expression
	shared_ptr<ExprAST> pInitializer;
	if( m_pLexer->GetCurrentToken() == TOKEN_ASSIGN ) {
		// Eat the '='
		m_pLexer->GetNextToken();

		// Parse the right-hand side
		pInitializer= parseExpression();
		if( !pInitializer ) return NULL;
	} else if( m_pLexer->GetCurrentToken() != TOKEN_SEMICOLON ) {
    m_pDiagContext->Error( error_expected_semicolon_or_eq_in_variable_declaration, m_pLexer->GetSourceRange() )
      << Lexer::StringifyToken(m_pLexer->GetCurrentToken());
		return NULL;
	}

	// Now eat the current token, which we expect to be a semicolon
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON );
  const SourceRange rangeVariableDecl( locTypeBegin, m_pLexer->GetSourceRange().end );
	m_pLexer->GetNextToken();

	// Check for implicit casts if we have an initializer
	if( pInitializer ) {
		if( *pType != *pInitializer->GetType() ) {
			if( !IsImplicitCastAllowed(pInitializer, pType) ) {
        m_pDiagContext->Error( error_no_implicit_cast, pInitializer->GetSourceRange() )
          << pInitializer->GetType()->GetName() << pType->GetName();
				return NULL;
			} // end if no implicit cast

			// If we got here, construct the implicit cast
			pInitializer.reset( new CastAST(pInitializer->GetSourceRange(), pInitializer, pType) );
		} // end if not same type
	} // end if checking for implicit cast

	// At this point, if the initializer exists, it should have the same type as the declaration
	ASSERT( !pInitializer || *pInitializer->GetType() == *pType );

	// Add the variable to the current scope and then return the declaration
	shared_ptr<DeclarationAST> pDeclaration( new DeclarationAST(rangeVariableDecl, strName, pType, pInitializer) );
	bool bSuccess= addVariableToScope( pDeclaration );

  // If we didn't succeed, that means the variable was already in scope.
  // Don't output an error message since addVariableToScope() does.
  if( !bSuccess ) return NULL;

	return pDeclaration;
} // end Parser::parseVariableDeclaration()


//! Parses a variable identifier
shared_ptr<VariableAST> Parser::parseVariable() {
	/* variable ::= identifier */
	
  if( !expectToken(TOKEN_IDENTIFIER) ) return NULL;
	string strName= m_pLexer->GetIdentifier();
  const SourceRange rangeName= m_pLexer->GetSourceRange();
	m_pLexer->GetNextToken();

	// Find the declaration
	shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strName );
	if( !pDeclaration ) {
    m_pDiagContext->Error( error_variable_not_in_scope, rangeName ) << strName;
		return NULL;
	} // end if not declared

	return shared_ptr<VariableAST>( new VariableAST(rangeName, pDeclaration) );
} // end Parser::parseVariable()


//! Parses a type identifier.
shared_ptr<const TypeAST> Parser::parseType() {
	/* type ::= identifier */
	
  if( !expectToken(TOKEN_IDENTIFIER) ) return NULL;
	const string strName= m_pLexer->GetIdentifier();
  const SourceRange rangeName= m_pLexer->GetSourceRange();
	m_pLexer->GetNextToken();

	// Look up the type
	shared_ptr<const TypeAST> pType= findTypeInScope( strName );
	if( !pType ) {
		m_pDiagContext->Error( error_type_not_in_scope, rangeName ) << strName;
		return NULL;
	} // end if not found

	// Check for a '[', which would indicate an array type
	if( m_pLexer->GetCurrentToken() == TOKEN_LBRACKET ) {
		// Eat the left bracket
		m_pLexer->GetNextToken();

		// If we have a right bracket, skip parsing the array length
		shared_ptr<ExprAST> pArrayLen;
		if( m_pLexer->GetCurrentToken() != TOKEN_RBRACKET ) {
			// Parse an expression for the array length
      const SourceLocation locArrayLenBegin= m_pLexer->GetSourceLocation();
			pArrayLen= parseExpression();
      const SourceRange rangeArrayLen= pArrayLen ? m_pLexer->GetSourceRange() : SourceRange(locArrayLenBegin, m_pLexer->GetSourceLocation());
			if( !pArrayLen ) {
        m_pDiagContext->Error( error_expected_expr, rangeArrayLen ) << "array length";
				return NULL;
			} // end if parse error
		} // end if parsing length

		// We expect a right bracket
    if( !expectEatToken(TOKEN_RBRACKET) ) return NULL;

		// Build the array type
		pType.reset( new ArrayTypeAST(pType, pArrayLen) );
	} // end if array type

	return pType;
} // end Parser::parseType()


//! Parses a function call expression, having already parsed the function name
shared_ptr<CallAST> Parser::parseCallExpression( const string& strName, const SourceRange& rangeName ) {
	/* expression_list
		::= expression ',' expression_list

	   call_expr
	    ::= identifier '(' ')'
	    ::= identifier '(' expression_list ')'
	*/

	// Look up the function prototype and check if we succeeded
	shared_ptr<PrototypeAST> pPrototype= findPrototypeInScope( strName );
	if( !pPrototype ) {
    m_pDiagContext->Error( error_function_not_in_scope, rangeName ) << strName;
		return NULL;
	} // end if lookup failed

	// Eat the left paren
  const SourceLocation locLparen= m_pLexer->GetSourceLocation();
  if( !expectEatToken(TOKEN_LPAREN) ) return NULL;

	// Now parse all the arguments
	vector< shared_ptr<ExprAST> > pArgs;
	while( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
    const SourceLocation locExprBegin= m_pLexer->GetSourceLocation();
		shared_ptr<ExprAST> pExpr( parseExpression() );
    const SourceRange rangeExpr= pExpr ? pExpr->GetSourceRange() : SourceRange(locExprBegin, m_pLexer->GetSourceLocation());
		if( !pExpr ) {
      m_pDiagContext->Error( error_expected_expr, rangeExpr ) << "function argument";
			return NULL;
		} // end if parse error

		pArgs.push_back( pExpr );

		// We expect a right paren or a comma; eat the comma if there's one
		if( m_pLexer->GetCurrentToken() == TOKEN_COMMA ) {
			m_pLexer->GetNextToken();
		} else if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
      m_pDiagContext->Error( error_expected_comma_or_rparen_in_call, m_pLexer->GetSourceRange() );
			return NULL;
		}
	} // end while parsing arguments

	// Eat the right paren
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RPAREN );
  const SourceRange rangeRparen= m_pLexer->GetSourceRange();
	m_pLexer->GetNextToken();
  const SourceRange rangeArgs( locLparen, rangeRparen.end );

	// Verify the function's signature
	if( pArgs.size() != pPrototype->GetArgs().size() ) {
    m_pDiagContext->Error( error_wrong_number_of_args_in_call, rangeArgs )
      << pArgs.size() << pPrototype->GetArgs().size();
		return NULL;
	} // end if wrong number of arguments

	// Check for implicit casts for each of the arguments
	for( uint iArg=0; iArg<pArgs.size(); ++iArg ) {
		// Skip this argument if it already has the right type
		if( *pArgs[iArg]->GetType() == *pPrototype->GetArgs()[iArg]->GetType() ) continue;

		// Check for an implicit conversion
		if( IsImplicitCastAllowed(pArgs[iArg], pPrototype->GetArgs()[iArg]->GetType()) ) {
			pArgs[iArg].reset( new CastAST(pArgs[iArg]->GetSourceRange(), pArgs[iArg], pPrototype->GetArgs()[iArg]->GetType()) );
		} else {
      m_pDiagContext->Error( error_no_implicit_cast, pArgs[iArg]->GetSourceRange() )
        << pArgs[iArg]->GetType()->GetName() << pPrototype->GetArgs()[iArg]->GetType()->GetName();
			return NULL;
		}
	} // end for argument

  const SourceRange rangeCall( rangeName.begin, rangeRparen.end );
	return shared_ptr<CallAST>( new CallAST(rangeCall, pPrototype, pArgs) );
} // end Parser::parseCallExpression()


//! Parses a cast expression having already parsed the type name
shared_ptr<CastAST> Parser::parseCastExpression( const string& strType, const SourceRange& rangeType ) {
	/*
	cast_expr
		::= identifier paren_expr
	*/

	// Look up the type
	shared_ptr<const TypeAST> pType= findTypeInScope( strType );
	if( !pType ) {
    m_pDiagContext->Error( error_type_not_in_scope, rangeType ) << strType;
		return NULL;
	} // end if type not found

	// Now parse the paren expression
  const SourceLocation locParenBegin= m_pLexer->GetSourceLocation();
	shared_ptr<ExprAST> pExpr= parseParenExpression();
  const SourceRange rangeParenExpr= pExpr ? pExpr->GetSourceRange() : SourceRange(locParenBegin, m_pLexer->GetSourceLocation());
	if( !pExpr ) {
    m_pDiagContext->Error( error_expected_expr, rangeParenExpr ) << "parenthesized expression";
		return NULL;
	} // end if parse error

	// TODO: Check if the cast is valid
	if( !IsCastAllowed(pExpr->GetType(), pType) ) {
    m_pDiagContext->Error( error_cannot_cast, pExpr->GetSourceRange() )
      << pExpr->GetType()->GetName() << pType->GetName();
		return NULL;
	} // end if void

	return shared_ptr<CastAST>( new CastAST(pExpr->GetSourceRange(), pExpr, pType) );
} // end Parser::parseCastExpression()


//! Parses a numeric literal
shared_ptr<LiteralAST> Parser::parseLiteral() {
	/*
	literal
		::= int_literal
		::= float_literal
		::= bool_literal
	*/

	if( !Lexer::IsLiteralToken(m_pLexer->GetCurrentToken()) ) {
    m_pDiagContext->Error( error_expected_literal, m_pLexer->GetSourceRange() )
      << Lexer::StringifyToken(m_pLexer->GetCurrentToken());
		return NULL;
	} // end if not a literal

	// Make the AST node
	shared_ptr<LiteralAST> pRet= makeLiteral( m_pLexer->GetCurrentToken(),
                                            m_pLexer->GetLiteral(),
                                            m_pLexer->GetSourceRange() );

	// Now eat the token
	m_pLexer->GetNextToken();

	return pRet;
} // end Parser::parseLiteral()


//! Parses a conditional statements
shared_ptr<ConditionalAST> Parser::parseConditionalStatement() {
	/*
	conditional_statement
		::= if '(' expression ')' statement
		::= conditional_stmt else conditional_stmt
	*/

	// Eat the "if"
  const SourceRange rangeIf= m_pLexer->GetSourceRange();
  if( !expectEatToken(TOKEN_IF) ) return NULL;

	// Find the left paren and eat it
  if( !expectEatToken(TOKEN_LPAREN) ) return NULL;

	// Parse the expression up to the right paren
  const SourceLocation locCondBegin= m_pLexer->GetSourceLocation();
	shared_ptr<ExprAST> pCondExpr( parseExpression() );
  const SourceRange rangeCond= pCondExpr ? pCondExpr->GetSourceRange() : SourceRange(locCondBegin, m_pLexer->GetSourceLocation());
	if( !pCondExpr ) {
    m_pDiagContext->Error( error_expected_expr, rangeCond ) << "condition";
		return NULL;
	} // end if no expression

	// Now find and eat the right paren
  if( !expectEatToken(TOKEN_RPAREN) ) return NULL;

	// Parse the statement in its own scope
  const SourceLocation locStmtBegin= m_pLexer->GetSourceLocation();
	shared_ptr<StmtAST> pStatement;
	{
		ScopeSentry s_scope( this );
		pStatement= parseStatement();
	}
  const SourceRange rangeStmt= pStatement ? pStatement->GetSourceRange() : SourceRange(locStmtBegin, m_pLexer->GetSourceLocation());
	if( !pStatement ) {
    m_pDiagContext->Error( error_expected_stmt, rangeStmt );
		return NULL;
	} // end if no block

	// Check for "else". If we find an else, keep parsing recursively
	shared_ptr<StmtAST> pElseStmt;
	if( m_pLexer->GetCurrentToken() == TOKEN_ELSE ) {
		// Eat the else
		m_pLexer->GetNextToken();

		// Now we either expect a left brace or "if"
		if( m_pLexer->GetCurrentToken() == TOKEN_LBRACE ) {
      const SourceLocation locElseStmtBegin= m_pLexer->GetSourceLocation();
			{
				// Parse the statement in its own scope
				ScopeSentry s_scope( this );
				pElseStmt= parseStatement();
			}
      const SourceRange rangeElseStmt= pElseStmt ? pElseStmt->GetSourceRange() : SourceRange(locElseStmtBegin, m_pLexer->GetSourceLocation());
			if( !pElseStmt ) {
        m_pDiagContext->Error( error_expected_stmt, rangeElseStmt );
				return NULL;
			} // end if no else block
		} else if( m_pLexer->GetCurrentToken() == TOKEN_IF ) {
			// Parse the next "if" block
			shared_ptr<ConditionalAST> pNextConditional;
      const SourceLocation locNextCondBegin= m_pLexer->GetSourceLocation();
			{
				ScopeSentry s_scope( this );
				pNextConditional= parseConditionalStatement();
			}
      const SourceRange rangeNextCond= pNextConditional ? pNextConditional->GetSourceRange() : SourceRange(locNextCondBegin, m_pLexer->GetSourceLocation());
			if( !pNextConditional ) {
        m_pDiagContext->Error( error_expected_else_if, rangeNextCond );
				return NULL;
			} // end if no else block

			// Build a block containing the next conditional
			vector<shared_ptr<StmtAST>> pExprs;
			pExprs.push_back( pNextConditional );
			pElseStmt.reset( new BlockAST(pNextConditional->GetSourceRange(), pExprs) );
		} else {
      m_pDiagContext->Error( error_expected_lbrace_if_after_else, m_pLexer->GetSourceRange() );
			return NULL;
		}
	}

	// Semantic checking: Make sure the condition expression is implicitly convertable to bool
	if( !IsImplicitCastAllowed(pCondExpr, BuiltinTypeAST::GetBool()) ) {
		m_pDiagContext->Error( error_conditional_must_be_implicitly_convertible_to_bool, pCondExpr->GetSourceRange() );
		return NULL;
	} // end if cannot cast to bool

  const SourceRange rangeConditional( rangeIf.begin, pElseStmt ? pElseStmt->GetSourceRange().end : pStatement->GetSourceRange().end );
  return shared_ptr<ConditionalAST>( new ConditionalAST(rangeConditional, pCondExpr, pStatement, pElseStmt) );
} // end Parser::parseConditionalStatement()


//! Parses a for statement
shared_ptr<StmtAST> Parser::parseForStatement() {
	/*
	for_statement
		::= 'for' '(' primary_statement expression ';' expression ')' statement
	*/

	// Start a new scope for any variable that might be declared in the initializer statement
	ScopeSentry s_scope( this );
	
	// We expect "for"
  const SourceRange rangeFor= m_pLexer->GetSourceRange();
  if( !expectEatToken(TOKEN_FOR) ) return NULL;

	// Now we expect a lparen
  const SourceRange rangeLparen= m_pLexer->GetSourceRange();
  if( !expectEatToken(TOKEN_LPAREN) ) return NULL;

	// Parse the initializer statement
  const SourceLocation locInitializerBegin= m_pLexer->GetSourceLocation();
	shared_ptr<PrimaryStmtAST> pInitializer= parsePrimaryStatement();
  const SourceRange rangeInitializer= pInitializer ? pInitializer->GetSourceRange() : SourceRange(locInitializerBegin, m_pLexer->GetSourceLocation());
	if( !pInitializer ) {
    m_pDiagContext->Error( error_expected_stmt, rangeInitializer );
		return NULL;
	} // end if error

	// Do not eat any semicolon, since it was parsed as part of the statement

	// Parse the condition expression
  const SourceLocation locConditionBegin= m_pLexer->GetSourceLocation();
	shared_ptr<ExprAST> pCondition= parseExpression();
  const SourceRange rangeCondition= pCondition ? pCondition->GetSourceRange() : SourceRange(locConditionBegin, m_pLexer->GetSourceLocation());
	if( !pCondition ) {
		m_pDiagContext->Error( error_expected_expr, rangeCondition ) << "for-loop condition";
		return NULL;
	} // end if error

	// Eat the semicolon
  if( !expectEatToken(TOKEN_SEMICOLON) ) return NULL;

	// Parse the update expression
  const SourceLocation locUpdateBegin= m_pLexer->GetSourceLocation();
	shared_ptr<ExprAST> pUpdate= parseExpression();
  const SourceRange rangeUpdate= pUpdate ? pUpdate->GetSourceRange() : SourceRange(locUpdateBegin, m_pLexer->GetSourceLocation());
	if( !pUpdate ) {
		m_pDiagContext->Error( error_expected_expr, rangeUpdate ) << "for-loop update";
		return NULL;
	} // end if error

	// Eat the rparen
	if( !expectEatToken(TOKEN_RPAREN) ) return NULL;

	// Parse the body statement in its own scope
	shared_ptr<StmtAST> pBody;
  const SourceLocation locBodyBegin= m_pLexer->GetSourceLocation();
	{
		ScopeSentry s_scopeInner( this );
		pBody= parseStatement();
	}
  const SourceRange rangeBody= pBody ? pBody->GetSourceRange() : SourceRange(locBodyBegin, m_pLexer->GetSourceLocation());
	if( !pBody ) {
		m_pDiagContext->Error( error_expected_stmt, rangeBody );
		return NULL;
	} // end if error

	// Semantic checking: make sure the condition is implicitly convertable to bool
	if( !IsImplicitCastAllowed(pCondition, BuiltinTypeAST::GetBool()) ) {
    m_pDiagContext->Error( error_conditional_must_be_implicitly_convertible_to_bool, pCondition->GetSourceRange() );
		return NULL;
	} // end if cannot cast to bool

  const SourceRange rangeForStmt( rangeFor.begin, rangeBody.end );
	return shared_ptr<ForAST>( new ForAST(rangeForStmt, pInitializer, pCondition, pUpdate, pBody) );
} // end Parser::parseForStatement()

namespace {
  //! If \a pExpr is not of type \a pType, returns a cast expression of \a pExpr
  //! to \a pType. Otherwise returns \a pExpr
  shared_ptr<ExprAST> castIfNecessary( shared_ptr<ExprAST> pExpr, const shared_ptr<const TypeAST>& pType ) {
    if( *pExpr->GetType() == *pType ) return pExpr;
    return shared_ptr<ExprAST>( new CastAST(pExpr->GetSourceRange(), pExpr, pType) );
  } // end castIfNecessary()
}

//! Parses a for-range statement
shared_ptr<ForRangeAST> Parser::parseForRangeStatement() {
  /*
	for_range_statement
		::= 'foreach' '(' for_range_index expression ':' expression ')' statement

  for_range_index
    ::= primary_statement
    Where if primary statment is a variable declaration, it can't have an initializer.
	*/

  // Start a new scope for any variable that might be declared in the initializer statement
	ScopeSentry s_scope( this );
  
  // Eat the "foreach"
  const SourceRange rangeForeach= m_pLexer->GetSourceRange();
  if( !expectEatToken(TOKEN_FOREACH) ) return NULL;

  // Eat the paren
  const SourceRange rangeLparen= m_pLexer->GetSourceRange();
  if( !expectEatToken(TOKEN_LPAREN) ) return NULL;

  // Parse the index. We expect a primary statement that's either a variable declaration
  // without an initializer, or a variable.
  const SourceLocation locPrimaryStmtBegin= m_pLexer->GetSourceLocation();
  shared_ptr<PrimaryStmtAST> pPrimaryStmt= parsePrimaryStatement();
  const SourceRange rangePrimaryStmt= pPrimaryStmt ? pPrimaryStmt->GetSourceRange() : SourceRange(locPrimaryStmtBegin, m_pLexer->GetSourceLocation());
  if( !pPrimaryStmt ) {
    m_pDiagContext->Error( error_expected_stmt, rangePrimaryStmt );
    return NULL;
  }

  // Figure out what kind of primary statement we got, and make sure it's right.
  // First see if it's a declaration, then otherwise fall back on a variable
  shared_ptr<DeclarationAST> pDeclaration= dynamic_pointer_cast<DeclarationAST>(pPrimaryStmt);
  shared_ptr<VariableAST> pVariable;
  if( pDeclaration ) {
    // Make sure there's no initializer
    if( pDeclaration->HasInitializer() ) {
      m_pDiagContext->Error( error_for_range_variable_cannot_have_initializer, pDeclaration->GetSourceRange() );
      return NULL;
    }

    // Store the variable
    pVariable.reset( new VariableAST(pDeclaration->GetSourceRange(), pDeclaration) );
  } else {
    // This should be an ExprStmt wrapping a variable.
    shared_ptr<ExprStmtAST> pExprStmt= dynamic_pointer_cast<ExprStmtAST>( pPrimaryStmt );
    if( !pExprStmt ) {
      ASSERT( false && "Unexpected! For-range variable declaration should be an expr stmt wrapping a variable" );
      return NULL;
    }

    // The expression should be a variable
    shared_ptr<ExprAST> pExpr= pExprStmt->GetExpr();
    pVariable= dynamic_pointer_cast<VariableAST>(pExpr);
    if( !pVariable ) {
      ASSERT( false && "Unexpected! For-range variable declaration was neither a declaration or variable" );
      return NULL;
    }
  }
  
  // If we got here, we must have a variable at the very least
  ASSERT( pVariable );
  
  // Parse the begin and end of the range
  const SourceLocation locBeginBegin= m_pLexer->GetSourceLocation();
  shared_ptr<ExprAST> pBegin= parseExpression();
  const SourceRange rangeBegin= pBegin ? pBegin->GetSourceRange() : SourceRange(locBeginBegin, m_pLexer->GetSourceLocation());
  if( !pBegin ) {
    m_pDiagContext->Error( error_expected_expr, rangeBegin ) << "for-range begin";
    return NULL;
  } // end if no begin
  
  // Eat the colon
  if( !expectEatToken(TOKEN_COLON) ) return NULL;

  // Parse the end of the range
  const SourceLocation locEndBegin= m_pLexer->GetSourceLocation();
  shared_ptr<ExprAST> pEnd= parseExpression();
  const SourceRange rangeEnd= pEnd ? pEnd->GetSourceRange() : SourceRange(locEndBegin, m_pLexer->GetSourceLocation());
  if( !pEnd ) {
    m_pDiagContext->Error( error_expected_expr, rangeEnd ) << "for-range end";
    return NULL;
  } // end if no end

  // Eat the rparen
  if( !expectEatToken(TOKEN_RPAREN) ) return NULL;
  
  // Parse the body statement in its own scope
	shared_ptr<StmtAST> pBody;
  const SourceLocation locBodyBegin= m_pLexer->GetSourceLocation();
	{
		ScopeSentry s_scopeInner( this );
		pBody= parseStatement();
	}
  const SourceRange rangeBody= pBody ? pBody->GetSourceRange() : SourceRange(locBodyBegin, m_pLexer->GetSourceLocation());
	if( !pBody ) {
		m_pDiagContext->Error( error_expected_stmt, rangeBody );
		return NULL;
	} // end if error
  
  // Implicitly cast begin and end to the same type as the declaration/variable
  shared_ptr<const TypeAST> pType= pDeclaration
    ? pDeclaration->GetType()
    : pVariable->GetType();

  if( !IsImplicitCastAllowed(pBegin, pType) ) {
    m_pDiagContext->Error( error_no_implicit_cast, pBegin->GetSourceRange() )
      << pBegin->GetType()->GetName() << pType->GetName();
    return NULL;
  } else {
    pBegin= castIfNecessary( pBegin, pType );
  }

  if( !IsImplicitCastAllowed(pEnd, pType) ) {
    m_pDiagContext->Error( error_no_implicit_cast, pEnd->GetSourceRange() )
      << pEnd->GetType()->GetName() << pType->GetName();
    return NULL;
  } else {
    pEnd= castIfNecessary( pEnd, pType );
  }
  
  // We expect to have a variable here.
  if( !pVariable ) {
    ASSERT( false && "Internal error: Expected a variable to begin for-range statement\n" );
    return NULL;
  }

  // Create the expression
  if( pDeclaration ) {
    const SourceRange rangeForRange( pDeclaration->GetSourceRange().begin, pBody->GetSourceRange().end );
    return std::make_shared<ForRangeAST>( rangeForRange, pDeclaration, pVariable, pBegin, pEnd, pBody );
  } else {
    const SourceRange rangeForRange( pVariable->GetSourceRange().begin, pBody->GetSourceRange().end );
    return std::make_shared<ForRangeAST>( rangeForRange, pVariable, pBegin, pEnd, pBody );
  }
} // end Parser::parseForRangeStatement()

//! Parses a while statement
shared_ptr<WhileAST> Parser::parseWhileStatement() {
	/*
	while_statement
		::= while '(' expression ')' statement
	*/

	// We expect "while"
  const SourceRange rangeWhile= m_pLexer->GetSourceRange();
  if( !expectEatToken(TOKEN_WHILE) ) return NULL;

	// We expect '('
	if( !expectEatToken(TOKEN_LPAREN) ) return NULL;

	// Parse the condition
  const SourceLocation locCondBegin= m_pLexer->GetSourceLocation();
	shared_ptr<ExprAST> pCondition= parseExpression();
  const SourceRange rangeCond= pCondition ? pCondition->GetSourceRange() : SourceRange(locCondBegin, m_pLexer->GetSourceLocation());
	if( !pCondition ) {
    m_pDiagContext->Error( error_expected_expr, rangeCond ) << "while-loop condition";
		return NULL;
	} // end if parse error

	// We expect ')'
	if( !expectEatToken(TOKEN_RPAREN) ) return NULL;

	// Parse the statement
  const SourceLocation locBodyBegin= m_pLexer->GetSourceLocation();
	shared_ptr<StmtAST> pBody= parseStatement();
  const SourceRange rangeBody= pBody ? pBody->GetSourceRange() : SourceRange(locBodyBegin, m_pLexer->GetSourceLocation());
	if( !pBody ) {
		m_pDiagContext->Error( error_expected_stmt, rangeBody );
		return NULL;
	} // end if parse error

	// Semantic checking: make sure the condition is implicitly convertable to bool
	if( !IsImplicitCastAllowed(pCondition, BuiltinTypeAST::GetBool()) ) {
    m_pDiagContext->Error( error_no_implicit_cast, pCondition->GetSourceRange() );
		return NULL;
	} // end if cannot cast to bool

	// Create the while expression
  const SourceRange rangeWhileStmt( rangeWhile.begin, pBody->GetSourceRange().end );
  return shared_ptr<WhileAST>( new WhileAST(rangeWhileStmt, pCondition, pBody) );
} // end Parser::parseWhileStatement()


//! Creates a numeric literal, given the type and its string representation
shared_ptr<LiteralAST> Parser::makeLiteral( Token token, const string& strLiteral, const SourceRange& range ) {
	if( !Lexer::IsLiteralToken(token) ) {
		ASSERT( false );
		return NULL;
	} // end if not literal token

	LiteralAST* pRet= NULL;
	switch( token ) {
	case TOKEN_LITERAL_INT:
		return shared_ptr<LiteralAST>( new IntegerAST(range, strLiteral) );
	case TOKEN_LITERAL_FLOAT:
		return shared_ptr<DoubleAST>( new DoubleAST(range, strLiteral) );
	case TOKEN_LITERAL_BOOL:
		return shared_ptr<BoolAST>( new BoolAST(range, strLiteral) );
	default:
		// Unhandled case, fix me!
		ASSERT( false );
		return NULL;
	} // end switch token
} // end Parser::makeLiteral()


//! Adds a variable declaration to the current scope. Returns false
//! if the variable already exists in scope
bool Parser::addVariableToScope( const shared_ptr<DeclarationAST>& pDeclaration ) {
	// Lookup the declaration by name and fail if it exists
	const string& strName= pDeclaration->GetName();
	if( m_parseScope.variables.find(strName) != m_parseScope.variables.end() ) {
    m_pDiagContext->Error( error_variable_already_declared, pDeclaration->GetSourceRange() ) << strName;
		return false;
	} // end if variable exists in scope

	// Insert the declaration
	m_parseScope.variables[strName]= pDeclaration;
	return true;
} // end Parser::addVariableToScope()


//! Adds a function prototype to the current scope. Returns false if the
//! prototype already exists in scope, but with a different signature
bool Parser::addPrototypeToScope( const shared_ptr<PrototypeAST>& pPrototype ) {
	//! Lookup the function by name and fail if it exists with a different signature
	const string& strName= pPrototype->GetName();
	map<string, shared_ptr<PrototypeAST>>::iterator itProto= m_parseScope.prototypes.find( strName );
	if( itProto != m_parseScope.prototypes.end() && *itProto->second != *pPrototype ) {
    m_pDiagContext->Error( error_function_already_declared_different_signature, pPrototype->GetSourceRange() ) << strName;
		return false;
	} // end if function exists in scope

	// Insert the function
	m_parseScope.prototypes[strName]= pPrototype;
	return true;
} // end Parser::addPrototypeToScope()


//! Adds a type to the current scope. Returns false if the type
//! already exists in scope
bool Parser::addTypeToScope( const shared_ptr<const TypeAST>& pType, const SourceRange& range ) {
	// Lookup the type by name and fail if it exists
	const string& strName= pType->GetName();
	map<string, shared_ptr<const TypeAST>>::iterator itType= m_parseScope.types.find( strName );
	if( itType != m_parseScope.types.end() ) {
    m_pDiagContext->Error( error_type_already_declared, range ) << strName;
		return false;
	} // end if type exists in scope

	// Insert the type
	m_parseScope.types[strName]= pType;
	return true;
} // end Parser::addTypeToScope()


//! Sets that the function has been defined in scope. This should only
//! ever be called once per function.
void Parser::setFunctionDefinedInScope( const string& strName ) {
	// This should never be called for a function that's already defined
	ASSERT( m_parseScope.definedFunctions.find(strName) == m_parseScope.definedFunctions.end() );
	m_parseScope.definedFunctions.insert( strName );
} // end Parser::setFunctionDefinedInScope()


//! Looks for a variable declaration in scope. Returns NULL if it does not exist,
//! but does not give any error messages
shared_ptr<DeclarationAST> Parser::findVariableInScope( const string& strName ) {
	map<string, shared_ptr<DeclarationAST>>::iterator itVar= m_parseScope.variables.find( strName );
	if( itVar == m_parseScope.variables.end() ) return NULL;
	else return itVar->second;
} // end Parser::findVariableInScope()


//! Looks up a function prototype in scope. Returns NULL if it does not exist,
//! but does not give any error messages
shared_ptr<PrototypeAST> Parser::findPrototypeInScope( const string& strName ) {
	map<string, shared_ptr<PrototypeAST>>::iterator itProto= m_parseScope.prototypes.find( strName );
	if( itProto == m_parseScope.prototypes.end() ) return NULL;
	else return itProto->second;
} // end Parser::findPrototypeInScope()


//! Looks up a type in scope. Returns NULL if it does not exist,
//! but does not give any error messages
shared_ptr<const TypeAST> Parser::findTypeInScope( const string& strName ) {
	map<string, shared_ptr<const TypeAST>>::iterator itType= m_parseScope.types.find( strName );
	if( itType == m_parseScope.types.end() ) return NULL;
	else return itType->second;
} // end Parser::findTypeInScope()


//! Returns whether the specified function is defined in scope. If this is called
//! for a function that hasn't even been prototyped, also returns FALSE.
bool Parser::isFunctionDefinedInScope( const string& strName ) {
	return m_parseScope.definedFunctions.find( strName ) != m_parseScope.definedFunctions.end();
} // end Parser::isFunctionDefinedInScope()


//! Called when we expect to find \a token.
//! If \a token is not found, emits an error message and returns FALSE,
//! otherwise returns TRUE
bool Parser::expectToken( Token token ) {
  if( m_pLexer->GetCurrentToken() != token ) {
    m_pDiagContext->Error( error_expected_token_found_token, m_pLexer->GetSourceRange() )
      << Lexer::StringifyToken(token)
      << Lexer::StringifyToken(m_pLexer->GetCurrentToken());
    return false;
  } else {
    return true;
  }
}


//! Called when we expect to find and consume \a token
//! If \a token is not found, emits an error message and return FALSE,
//! otherwise eats the token and returns TRUE
bool Parser::expectEatToken( Token token ) {
  if( !expectToken(token) ) return false;

  m_pLexer->GetNextToken();
  return true;
}

//! Non-inline destructor
Parser::~Parser() { }