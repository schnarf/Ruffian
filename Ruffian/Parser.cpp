#include "common.h"
#include "Parser.h"
#include "AST.h"
#include "Lexer.h"
#include "SemBinop.h"
#include "SemCast.h"

//! Initialize with parser
Parser::Parser( const shared_ptr<Lexer>& pLexer ) :
	m_pLexer(pLexer) {

	// Initialize our scope with our built-in types
	for( vector<shared_ptr<const TypeAST>>::const_iterator itType=BuiltinTypeAST::GetBuiltinTypes().begin(); itType!=BuiltinTypeAST::GetBuiltinTypes().end(); ++itType ) {
		// Don't add the error type*
		if( **itType == *BuiltinTypeAST::GetError() ) continue;

		// Add the type to our map
		m_parseScope.types[(*itType)->GetName()]= *itType;
	} // end for type

} // end Parser::Parser()


//! Runs the main parsing loop, returning the parsed module at the
//! root of the AST on success, or NULL on failure.
ModuleAST* Parser::Run() {
	// Build a list of function prototypes and definitions encountered
	vector<shared_ptr<PrototypeAST>> pPrototypes;
	vector<shared_ptr<FunctionAST>> pFunctions;

	// Parse until we hit the end of the file or an error is encountered
	bool bEOF= false, bError= false;
	while( !bEOF && !bError ) {
		switch( m_pLexer->GetCurrentToken() ) {
		case TOKEN_EOF:
			bEOF= true;
			break;
		case TOKEN_DEF: {
			// Attempt to parse the declaration or definition; only one pointer will be non-NULL
			pair<shared_ptr<PrototypeAST>, shared_ptr<FunctionAST>> functionRet= parseFunctionDeclarationOrDefinition();
			// Add the prototype or definition to the appropriate list
			if( functionRet.first ) pPrototypes.push_back( functionRet.first );
			else if( functionRet.second ) pFunctions.push_back( functionRet.second );
			else { 
				cerr << "Encountered an error parsing a function declaration/definition\n";
				bError= true;
			} // end if error
			break;
		} // end case function declaration/definition
		default:
			cerr << "Unexpected token \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\".\n";
			bError= true;
			break;
		} // end switch current token
	} // end while not eof or error

	// If we hit an error, just exit out here. We already printed an error message,
	// and should not try to assemble any module.
	if( bError ) return NULL;

	// If we are here, create and return the module
	return new ModuleAST( pPrototypes, pFunctions );
} // end Parser::Run()


//! Parses a primary expression
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
	if( m_pLexer->GetCurrentToken() == TOKEN_IDENTIFIER ) {
		const string strName= m_pLexer->GetIdentifier();
		pExpr= parseIdentifierExpression();
		if( !pExpr ) cerr << "Variable \"" + strName + "\" was not declared in the current scope while parsing primary expression\n";
	} else if( Lexer::IsLiteralToken(m_pLexer->GetCurrentToken()) ) {
		pExpr= parseLiteralExpression();
		if( !pExpr ) cerr << "Could not parse literal for primary expression\n";
	} else if( m_pLexer->GetCurrentToken() == TOKEN_LPAREN ) {
		pExpr= parseParenExpression();
		if( !pExpr ) cerr << "Could not parse parenthesized expression for primary expression\n";
	} else if( m_pLexer->GetCurrentToken() == TOKEN_ARRAYSIZE ) {
		pExpr= parseArraySizeExpression();
		if( !pExpr ) cerr << "Could not parse arraysize expression for primary expression\n";
	} else {
		cerr << "Expected identifier, literal, '(', or \"arraysize\" while parsing primary expression, found " + Lexer::StringifyToken(m_pLexer->GetCurrentToken()) + "\n";
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
	const string& strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();

	// If this is a function call, parse the call expression and return it
	// If the identifier is a type, then treat this as a cast, otherwise, treat it as a function call
	if( m_pLexer->GetCurrentToken() == TOKEN_LPAREN ) {
		if( findTypeInScope(strName) ) return parseCastExpression( strName );
		else return parseCallExpression( strName );
	} // end if lparen

	// Check for '[', which indicates an array reference
	if( m_pLexer->GetCurrentToken() == TOKEN_LBRACKET ) {
		// Look up the array declaration
		shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strName );
		if( !pDeclaration ) {
			cerr << "Could not find array " + strName + " in scope\n";
			return NULL;
		} // end if not declared

		// Eat the left bracket, and parse the index expression
		m_pLexer->GetNextToken();

		shared_ptr<ExprAST> pIndex= parseExpression();
		if( !pIndex ) {
			cerr << "Could not parse array index expression\n";
			return NULL;
		} // end if error

		// Eat the right brackets
		if( m_pLexer->GetCurrentToken() != TOKEN_RBRACKET ) {
			cerr << "Expected ']' after array index expression\n";
			return NULL;
		} // end if no right bracket
		m_pLexer->GetNextToken();

		// Create the array reference expression
		return shared_ptr<ExprAST>( new ArrayRefAST(pDeclaration, pIndex) );		
	} // end if array reference

	// If we're here, this is a variable reference
	// Look up the declaration, and give an error if it hasn't been declared
	shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strName );
	// Only create an AST node if the declaration exists
	if( pDeclaration ) return shared_ptr<ExprAST>( new VariableAST(pDeclaration) );
	else return NULL;
} // end Parser::parseIdentifierExpression()


//! Parses a literal expression
shared_ptr<LiteralAST> Parser::parseLiteralExpression() {
	// See makeLiteral() for the definition of a literal_expr

	// Create the literal expression and do error handling. Eat the token in any case.
	shared_ptr<LiteralAST> pLiteral= makeLiteral( m_pLexer->GetCurrentToken(), m_pLexer->GetLiteral() );
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
	m_pLexer->GetNextToken();
	// Parse an expression up until the next unmatched rparen
	shared_ptr<ExprAST> pExpr= parseExpression();
	
	// Eat the rparen
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RPAREN );
	m_pLexer->GetNextToken();

	return pExpr;
} // end Parser::parseParenExpression()


//! Parses an arraysize expression
shared_ptr<ExprAST> Parser::parseArraySizeExpression() {
	/*
	arraysize_expr
		::= 'arraysize' '(' identifier ')'
	*/

	// Eat 'arraysize'
	m_pLexer->GetNextToken();

	// Eat the lparen
	if( m_pLexer->GetCurrentToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' after 'arraysize'\n";
		return NULL;
	} // end if no lparen
	m_pLexer->GetNextToken();

	// Parse the identifier
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected identifier in arraysize expression\n";
		return NULL;
	} // end if no identifier
	const string strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();

	// Eat the rparen
	if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		cerr << "Expected ')' at end of arraysize expression\n";
		return NULL;
	} // end if no rparen
	m_pLexer->GetNextToken();

	// Look up the variable in scope, and verify that it's an array with a length specified
	shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strName );
	if( !pDeclaration ) {
		cerr << "Cannot find the arraysize of \"" + strName + "\" because it was not declared in scope\n";
		return NULL;
	} // end if no declaration

	shared_ptr<const ArrayTypeAST> pArrayType= dynamic_pointer_cast<const ArrayTypeAST>(pDeclaration->GetType());
	if( !pArrayType ) {
		cerr << "Cannot find the arraysize of \"" + strName + "\" because it is not an array\n";
		return NULL;
	} // end if not array

	shared_ptr<ExprAST> pSize= pArrayType->GetLengthExpression();
	if( !pSize ) {
		cerr << "Cannot find the arraysize of \"" + strName + "\" because it has no size\n";
		return NULL;
	} // end if no size

	return pSize;
} // end Parser::parseArraySizeExpression()


//! Parses a unary operator expression
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
		m_pLexer->GetNextToken();

		// Parse the following unary op expression
		shared_ptr<ExprAST> pExpr= parseUnaryOpExpression();
		if( !pExpr ) {
			cerr << "Could not parse unary op expression after prefix unary operator\n";
			return NULL;
		} // end if parse error

		// Increment and decrement require a variable operand for now
		if( (op == TOKEN_INCREMENT || op == TOKEN_DECREMENT) && !dynamic_pointer_cast<VariableAST>(pExpr) ) {
			cerr << "Prefix increment and decrement require a variable operand\n";
			return NULL;
		} // end if not variable

		// Create and return the AST node
		return shared_ptr<ExprAST>( new PrefixUnaryAST(op, pExpr) );
	} // end if prefix operator

	// If we don't have a prefix operator, then we have either a primary expression,
	// or a unary op expr that starts with a primary expression. First parse the
	// primary expression
	shared_ptr<ExprAST> pPrimaryExpr= parsePrimaryExpression();
	if( !pPrimaryExpr ) {
		cerr << "Expected a primary expression while parsing a unary op expression\n";
		return NULL;
	} // end if parse error

	// Now, if we have a postfix unary operator token, create the AST node for it
	if( Lexer::IsPostUnaryOpToken(m_pLexer->GetCurrentToken()) ) {
		// Store and eat the token
		Token op= m_pLexer->GetCurrentToken();
		m_pLexer->GetNextToken();

		// Increment and decrement require a variable operand for now
		if( (op == TOKEN_INCREMENT || op == TOKEN_DECREMENT) && !dynamic_pointer_cast<VariableAST>(pPrimaryExpr) ) {
			cerr << "Postfix increment and decrement require a variable operand\n";
			return NULL;
		} // end if not variable

		return shared_ptr<ExprAST>( new PostfixUnaryAST(op, pPrimaryExpr) );
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
		shared_ptr<ExprAST> pRight= parseUnaryOpExpression();
		if( !pRight ) {
			cerr << "Could not parse primary expression for binop rhs\n";
			return NULL;
		} // end if error

		// Now check for another binop
		// If the binop binds less tightly with the RHS than the operator after
		// the RHS, then let the pending operator take the RHS as its LHS
		if( Lexer::IsBinopToken(m_pLexer->GetCurrentToken()) && GetBinopPrecedence(binop) < GetBinopPrecedence(m_pLexer->GetCurrentToken()) ) {
			pRight= parseBinopRHS( GetBinopPrecedence(binop), pRight );
			if( !pRight ) {
				cerr << "Could not parse binop rhs for binop rhs\n";
				return NULL;
			} // end if error
		} // end if parsing rhs

		// Before we construct the binop AST node, check for an assignment expression
		// These are special-cased: the LHS must be a variable
		if( binop == TOKEN_ASSIGN && !dynamic_pointer_cast<VariableAST>(pLeft) ) {
			cerr << "The left-hand side of an assignment expression must be a variable\n";
			return NULL;
		} // end if assignment

		// If the lhs and rhs don't have the same type, check for an implicit conversion
		if( pLeft->GetType() != pRight->GetType() ) {
			shared_ptr<const TypeAST> pType= GetBinaryOpsImplicitCastType( pLeft, pRight );
			if( !pType ) {
				cerr << "No implicit conversion allows a binary operation between expressions of type \"" + pLeft->GetType()->GetName() + "\" and \"" + pRight->GetType()->GetName() + "\"\n";
				return NULL;
			} // end if no implicit conversion

			// Cast the LHS and RHS as necessary
			if( *pLeft->GetType() != *pType ) pLeft.reset( new CastAST(pLeft, pType) );
			if( *pRight->GetType() != *pType ) pRight.reset( new CastAST(pRight, pType) );
		} // end if not same type

		// Merge LHS and RHS
		pLeft.reset( new BinopAST(binop, pLeft, pRight) );
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
	if( m_pLexer->GetCurrentToken() != TOKEN_DEF ) {
		cerr << "Expected \"def\" while parsing function prototype\n";
		return null_ret;
	} // end if error
	m_pLexer->GetNextToken();

	// Parse the identifier for the function's name
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing function prototype\n";
		return null_ret;
	} // end if no identifier

	// Store the identifier and eat it
	string strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();

	// Parse the argument list
	if( m_pLexer->GetCurrentToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' while parsing function prototype argument list\n";
		return null_ret;
	} // end if no left paren

	// Eat the lparen
	m_pLexer->GetNextToken();

	vector< shared_ptr<DeclarationAST> > pArgs;
	while( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		// typename identifier ','
		if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
			cerr << "Expected an identifier (type) while parsing function prototype argument list, found \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\"\n";
			return null_ret;
		}
		shared_ptr<const TypeAST> pType( parseType() );

		// Prevent void arguments
		if( *pType == *BuiltinTypeAST::GetVoid() ) {
			cerr << "Cannot have an argument of type void, while parsing function prototype argument list\n";
			return null_ret;
		} // end if void type

		if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
			cerr << "Expected an identifier (variable) while parsing function prototype argument list, found \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\"\n";
			return null_ret;
		}

		string strVariable= m_pLexer->GetIdentifier();
		m_pLexer->GetNextToken();

		// Create a declaration for this argument
		// Don't add it to scope until later, if this is a full function definition
		shared_ptr<DeclarationAST> pDeclaration( new DeclarationAST(strVariable, pType) );

		// Now we can have a comma or rparen
		if( m_pLexer->GetCurrentToken() == TOKEN_COMMA ) {
			// Eat the comma before parsing the next argument
			m_pLexer->GetNextToken();
		} else if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
			cerr << "Expected ',' or ')' while parsing function definition argument list, found \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\"\n";
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
			cerr << "Expected a typename while parsing function return type\n";
			return null_ret;
		} // end if no identifier

		pReturnType= parseType();
	} // end if return type specified

	// Now lookup the prototype, and create it if it does not exist
	shared_ptr<PrototypeAST> pPrototype= findPrototypeInScope( strName );
	if( !pPrototype ) pPrototype.reset( new PrototypeAST(strName, pReturnType, pArgs) );
	bool bPrototypeDoesNotExist= addPrototypeToScope( pPrototype ); ASSERT( bPrototypeDoesNotExist );

	// Now we expect either an opening brace for a function definition, or a semicolon for a function prototype
	if( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON ) {
		// Eat the semicolon and return the prototype
		m_pLexer->GetNextToken();
		return ret_pair_type( pPrototype, nullptr );
	} // end if function prototype

	// Now look for the opening brace
	if( m_pLexer->GetCurrentToken() != TOKEN_LBRACE ) {
		cerr << "Expected '{' while parsing function definition\n";
		return null_ret;
	} // end if no left brace

	// See if this function is already defined in scope. If it is, give an error and return,
	// since a function can have multiple prototypes but only one body.
	if( isFunctionDefinedInScope(strName) ) {
		cerr << "Function \"" << strName << "\" was already defined in scope\n";
		return null_ret;
	} // end if already defined

	// If this is a full function definition, start a new scope, and add the variable declarations so the function body can reference them
	ScopeSentry s_scope( this );
	for( uint iArg=0; iArg<pArgs.size(); ++iArg ) if( !addVariableToScope(pArgs[iArg]) ) return null_ret;

	// Set the expected return type for this function, so the expressions for return statements
	// can be implicitly casted to this type
	setExpectedReturnType( pReturnType );

	// Parse the function's body
	shared_ptr<BlockAST> pBody( parseBlock() );
	if( !pBody ) {
		cerr << "Could not parse function body\n";
		return null_ret;
	} // end if couldn't parse body

	// Add the function to scope
	shared_ptr<FunctionAST> pFunction( new FunctionAST(pPrototype, pBody) );
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

	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_LBRACE );
	vector<shared_ptr<StmtAST>> pExprs;
	m_pLexer->GetNextToken();
	while( m_pLexer->GetCurrentToken() != TOKEN_RBRACE ) {
		shared_ptr<StmtAST> pExpr( parseStatement() );
		if( !pExpr ) {
			cerr << "Could not parse statement while parsing a block\n";
			return NULL;
		} // end if parse error

		pExprs.push_back( pExpr );
	} // end while looking for a right brace

	// Eat the right brace
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RBRACE );
	m_pLexer->GetNextToken();

	return shared_ptr<BlockAST>( new BlockAST(pExprs) );
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
		::= while_statement
	*/

	switch( m_pLexer->GetCurrentToken() ) {
	case TOKEN_RETURN: return parseReturnStatement();
	case TOKEN_LBRACE: return parseBlock();
	case TOKEN_IF: return parseConditionalStatement();
	case TOKEN_FOR: return parseForStatement();
	case TOKEN_WHILE: return parseWhileStatement();
	// Parse everything else as a primary statement, since they can start
	// with different kinds of tokens
	default: return parsePrimaryStatement();
	} // end switch current token
} // end Parser::parseStatement()


//! Parses a primary statement
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
		shared_ptr<ExprAST> pExpr= parseExpression();
		if( !pExpr ) {
			cerr << "Could not parse expression while parsing statement\n";
			return NULL;
		} // end if could not parse

		// We expect to eat a semicolon
		if( m_pLexer->GetCurrentToken() != TOKEN_SEMICOLON ) {
			cerr << "Expected ';' after expression while parsing statement\n";
			return NULL;
		} // end if no semicolon
		m_pLexer->GetNextToken();

		// Return the statement expression
		return shared_ptr<PrimaryStmtAST>( new ExprStmtAST(pExpr) );
	}
} // end Parser::parsePrimaryStatement()


//! Parses an expression
shared_ptr<ExprAST> Parser::parseExpression() {
	/*
	expression
		::= unary_op_expr binoprhs
	*/

	// Parse the LHS
	shared_ptr<ExprAST> pLeft= parseUnaryOpExpression();
	if( !pLeft ) {
		cerr << "Could not parse unary op expression for expression\n";
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

	if( m_pLexer->GetCurrentToken() != TOKEN_RETURN ) {
		cerr << "Expected \"return\"\n";
		return NULL;
	} // end if no "return"

	// Eat "return"
	m_pLexer->GetNextToken();

	// Parse the expression, if we have one
	shared_ptr<ExprAST> pExpr;
	if( m_pLexer->GetCurrentToken() != TOKEN_SEMICOLON ) {
		pExpr= parseExpression();
		if( !pExpr ) return NULL;
	} // end if parsing return expression

	// Now eat the semicolon
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON );
	m_pLexer->GetNextToken();

	// Check for the expected return type. If it doesn't exist, that means this
	// statement is not inside a function body, and that's an error.
	if( !getExpectedReturnType() ) {
		cerr << "Found a return statement not inside a function body\n";
		return NULL;
	} // end if no return type

	// Implicitly cast to the expected return type, if we have a different type
	if( pExpr && *pExpr->GetType() != *getExpectedReturnType() ) {
		if( IsImplicitCastAllowed(pExpr, getExpectedReturnType()) ) {
			pExpr.reset( new CastAST(pExpr, getExpectedReturnType()) );
		} else {
			cerr << "Implicit cast from \"" + pExpr->GetType()->GetName() + "\" to \"" + getExpectedReturnType()->GetName() + "\" is not allowed, while parsing return statement\n";
			return NULL;
		}
	} // end if not type type

	return shared_ptr<ReturnAST>( new ReturnAST(pExpr) );
} // end Parser::parseReturnStatement()


//! Parses a variable declaration
shared_ptr<DeclarationAST> Parser::parseVariableDeclaration() {
	/* variable_declaration
		::= type identifier ';'
		::= type identifier '=' expression ';'
	*/

	// Parse the type
	shared_ptr<const TypeAST> pType= parseType();
	if( !pType ) {
		cerr << "Could not parse type for variable declaration\n";
		return NULL;
	} // end if could not parse type

	// Disallow variables of type "void"
	if( *pType == *BuiltinTypeAST::GetVoid() ) {
		cerr << "Cannot declare a variable of type \"void\"\n";
		return NULL;
	} // end if void

	// Now parse the variable identifier
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing a variable declaration\n";
		return NULL;
	} // end if no variable identifier

	string strName= m_pLexer->GetIdentifier();

	// Advance to the next token. We expect either a semicolon, in which case
	// we are done, or an equals sign, in which case we will parse another expression
	m_pLexer->GetNextToken();
	shared_ptr<ExprAST> pInitializer;
	if( m_pLexer->GetCurrentToken() == TOKEN_ASSIGN ) {
		// Eat the '='
		m_pLexer->GetNextToken();

		// Parse the right-hand side
		pInitializer= parseExpression();
		if( !pInitializer ) return NULL;
	} else if( m_pLexer->GetCurrentToken() != TOKEN_SEMICOLON ) {
		cerr << "Expected ';' or '=' while parsing a variable declaration, found \"" + Lexer::StringifyToken(m_pLexer->GetCurrentToken()) + " " + m_pLexer->GetIdentifier() +  "\"\n";
		return NULL;
	}

	// Now eat the current token, which we expect to be a semicolon
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON );
	m_pLexer->GetNextToken();

	// Check for implicit casts if we have an initializer
	if( pInitializer ) {
		if( *pType != *pInitializer->GetType() ) {
			if( !IsImplicitCastAllowed(pInitializer, pType) ) {
				cerr << "Cannot implicitly convert variable initializer of type \"" + pInitializer->GetType()->GetName() + "\" to declaration of type \"" + pType->GetName() + "\"\n";
				return NULL;
			} // end if no implicit cast

			// If we got here, construct the implicit cast
			pInitializer.reset( new CastAST(pInitializer, pType) );
		} // end if not same type
	} // end if checking for implicit cast

	// At this point, if the initializer exists, it should have the same type as the declaration
	ASSERT( !pInitializer || *pInitializer->GetType() == *pType );

	// Add the variable to the current scope and then return the declaration
	shared_ptr<DeclarationAST> pDeclaration( new DeclarationAST(strName, pType, pInitializer) );
	addVariableToScope( pDeclaration );
	return pDeclaration;
} // end Parser::parseVariableDeclaration()


//! Parses a variable identifier
shared_ptr<VariableAST> Parser::parseVariable() {
	/* variable ::= identifier */
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing a variable\n";
		return NULL;
	} // end if no identifier

	string strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();

	// Find the declaration
	shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strName );
	if( !pDeclaration ) {
		cerr << "Variable \"" << strName << "\" was not declared in the current scope\n";
		return NULL;
	} // end if not declared

	return shared_ptr<VariableAST>( new VariableAST(pDeclaration) );
} // end Parser::parseVariable()


//! Parses a type identifier
shared_ptr<const TypeAST> Parser::parseType() {
	/* type ::= identifier */
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing a type\n";
		return NULL;
	} // end if no identifier

	string strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();

	// Look up the type
	shared_ptr<const TypeAST> pType= findTypeInScope( strName );
	if( !pType ) {
		cerr << "Type \"" + strName + "\" was not declared in scope\n";
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
			pArrayLen= parseExpression();
			if( !pArrayLen ) {
				cerr << "Could not parse expression for array length while parsing type\n";
				return NULL;
			} // end if parse error
		} // end if parsing length

		// We expect a right bracket
		if( m_pLexer->GetCurrentToken() != TOKEN_RBRACKET ) {
			cerr << "Expected ']' after array length expression while parsing type\n";
			return NULL;
		} // end if no ']'

		// Eat the right bracket
		m_pLexer->GetNextToken();

		// Build the array type
		pType.reset( new ArrayTypeAST(pType, pArrayLen) );
	} // end if array type

	return pType;
} // end Parser::parseType()


//! Parses a function call expression, having already parsed the function name
shared_ptr<CallAST> Parser::parseCallExpression( const string& strName ) {
	/* expression_list
		::= expression ',' expression_list

	   call_expr
	    ::= identifier '(' ')'
	    ::= identifier '(' expression_list ')'
	*/

	// Look up the function prototype and check if we succeeded
	shared_ptr<PrototypeAST> pPrototype= findPrototypeInScope( strName );
	if( !pPrototype ) {
		cerr << "Function \"" << strName << "\" was not declared in the current scope, while parsing a call expression\n";
		return NULL;
	} // end if lookup failed

	if( m_pLexer->GetCurrentToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' while parsing function call argument list\n";
		return NULL;
	} // end if no lparen

	// Eat the left paren
	m_pLexer->GetNextToken();

	// Now parse all the arguments
	vector< shared_ptr<ExprAST> > pArgs;
	while( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		shared_ptr<ExprAST> pExpr( parseExpression() );
		if( !pExpr ) {
			cerr << "Could not parse expression in function call argument list\n";
			return NULL;
		} // end if parse error

		pArgs.push_back( pExpr );

		// We expect a right paren or a comma; eat the comma if there's one
		if( m_pLexer->GetCurrentToken() == TOKEN_COMMA ) {
			m_pLexer->GetNextToken();
		} else if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
			cerr << "Expected ',' or ')' while parsing call expression\n";
			return NULL;
		}
	} // end while parsing arguments

	// Eat the right paren
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RPAREN );
	m_pLexer->GetNextToken();

	// Verify the function's signature
	if( pArgs.size() != pPrototype->GetArgs().size() ) {
		cerr << "Wrong number of arguments in function call. Found " << pArgs.size() << " but expected " << pPrototype->GetArgs().size() << "\n";
		return NULL;
	} // end if wrong number of arguments

	// Check for implicit casts for each of the arguments
	for( uint iArg=0; iArg<pArgs.size(); ++iArg ) {
		// Skip this argument if it already has the right type
		if( *pArgs[iArg]->GetType() == *pPrototype->GetArgs()[iArg]->GetType() ) continue;

		// Check for an implicit conversion
		if( IsImplicitCastAllowed(pArgs[iArg], pPrototype->GetArgs()[iArg]->GetType()) ) {
			pArgs[iArg].reset( new CastAST(pArgs[iArg], pPrototype->GetArgs()[iArg]->GetType()) );
		} else {
			cerr << "Argument " << 1+iArg << " for function " << strName << " cannot be implicitly converted from \"" + pArgs[iArg]->GetType()->GetName() + "\" to \"" + pPrototype->GetArgs()[iArg]->GetType()->GetName() + "\"\n";
			return NULL;
		}
	} // end for argument
	
	return shared_ptr<CallAST>( new CallAST(pPrototype, pArgs) );
} // end Parser::parseCallExpression()


//! Parses a cast expression having already parsed the type name
shared_ptr<CastAST> Parser::parseCastExpression( const string& strType ) {
	/*
	cast_expr
		::= identifier paren_expr
	*/

	// Look up the type
	shared_ptr<const TypeAST> pType= findTypeInScope( strType );
	if( !pType ) {
		cerr << "Type \"" + strType + "\" was not declared in the current scope while parsing a cast expression\n";
		return NULL;
	} // end if type not found

	// Now parse the paren expression
	shared_ptr<ExprAST> pExpr= parseParenExpression();
	if( !pExpr ) {
		cerr << "Expected parenthesized expression while parsing cast expression\n";
		return NULL;
	} // end if parse error

	// TODO: Check if the cast is valid
	if( !IsCastAllowed(pExpr->GetType(), pType) ) {
		cerr << "Cannot cast from \"" + pExpr->GetType()->GetName() + "\" to \"" + pType->GetName() + "\" while parsing a cast expression\n";
		return NULL;
	} // end if void

	return shared_ptr<CastAST>( new CastAST(pExpr, pType) );
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
		cerr << "Expected a literal\n";
		return NULL;
	} // end if not a literal

	// Make the AST node
	shared_ptr<LiteralAST> pRet= makeLiteral( m_pLexer->GetCurrentToken(), m_pLexer->GetLiteral() );

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
	if( m_pLexer->GetCurrentToken() != TOKEN_IF ) {
		cerr << "Expected \"if\" while parsing a conditional statement\n";
		return NULL;
	} // end if no "if"
	m_pLexer->GetNextToken();

	// Find the left paren and eat it
	if( m_pLexer->GetCurrentToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' after \"if\" while parsing a conditional statement\n";
		return NULL;
	} // end if no lparen
	m_pLexer->GetNextToken();

	// Parse the expression up to the right paren
	shared_ptr<ExprAST> pCondExpr( parseExpression() );
	if( !pCondExpr ) {
		cerr << "Expected an expression between parentheses while parsing a conditional statement\n";
		return NULL;
	} // end if no expression

	// Now find and eat the right paren
	if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		cerr << "Expected ')' after condition while parsing a conditional statement\n";
		return NULL;
	} // end if no right paren
	m_pLexer->GetNextToken();

	// Parse the statement in its own scope
	shared_ptr<StmtAST> pStatement;
	{
		ScopeSentry s_scope( this );
		pStatement= parseStatement();
	}
	if( !pStatement ) {
		cerr << "Expected a statement while parsing a conditional statement\n";
		return NULL;
	} // end if no block

	// Check for "else". If we find an else, keep parsing recursively
	shared_ptr<StmtAST> pElseStmt;
	if( m_pLexer->GetCurrentToken() == TOKEN_ELSE ) {
		// Eat the else
		m_pLexer->GetNextToken();

		// Now we either expect a left brace or "if"
		if( m_pLexer->GetCurrentToken() == TOKEN_LBRACE ) {
			{
				// Parse the statement in its own scope
				ScopeSentry s_scope( this );
				pElseStmt= parseStatement();
			}
			if( !pElseStmt ) {
				cerr << "Expected an else statement while parsing a conditional statement\n";
				return NULL;
			} // end if no else block
		} else if( m_pLexer->GetCurrentToken() == TOKEN_IF ) {
			// Parse the next "if" block
			shared_ptr<ConditionalAST> pNextConditional;
			{
				ScopeSentry s_scope( this );
				pNextConditional= parseConditionalStatement();
			}
			if( !pNextConditional ) {
				cerr << "Expected an else if block while parsing a conditional statement\n";
				return NULL;
			} // end if no else block

			// Build a block containing the next conditional
			vector<shared_ptr<StmtAST>> pExprs;
			pExprs.push_back( pNextConditional );
			pElseStmt.reset( new BlockAST(pExprs) );
		} else {
			cerr << "Expected '{' or \"if\" after \"else\" while parsing a conditional statement\n";
			return NULL;
		}
	}

	return shared_ptr<ConditionalAST>( new ConditionalAST(pCondExpr, pStatement, pElseStmt) );
} // end Parser::parseConditionalStatement()


//! Parses a for statement
shared_ptr<ForAST> Parser::parseForStatement() {
	/*
	for_statement
		::= 'for' '(' primary_statement expression ';' expression ')' statement
	*/

	// Start a new scope for any variable that might be declared in the initializer statement
	ScopeSentry s_scope( this );
	
	// We expect "for"
	if( m_pLexer->GetCurrentToken() != TOKEN_FOR ) {
		cerr << "Expected \"for\" while parsing for statement\n";
		return NULL;
	} // end if no for
	m_pLexer->GetNextToken();

	// Now we expect a lparen
	if( m_pLexer->GetCurrentToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' after \"for\" while parsing for statement\n";
		return NULL;
	} // end if no lparen
	m_pLexer->GetNextToken();

	// Parse the initializer statement
	shared_ptr<PrimaryStmtAST> pInitializer= parsePrimaryStatement();
	if( !pInitializer ) {
		cerr << "Could not parse initializer statement in for statement\n";
		return NULL;
	} // end if error

	// Do not eat any semicolon, since it was parsed as part of the statement

	// Parse the condition expression
	shared_ptr<ExprAST> pCondition= parseExpression();
	if( !pCondition ) {
		cerr << "Could not parse condition expression in for statement\n";
		return NULL;
	} // end if error

	// Eat the semicolon
	if( m_pLexer->GetCurrentToken() != TOKEN_SEMICOLON ) {
		cerr << "Expected ';' after condition expression in for statement\n";
		return NULL;
	} // end if no semicolon
	m_pLexer->GetNextToken();

	// Parse the update expression
	shared_ptr<ExprAST> pUpdate= parseExpression();
	if( !pUpdate ) {
		cerr << "Could not parse update expression in for statement\n";
		return NULL;
	} // end if error

	// Eat the rparen
	if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		cerr << "Expected ')' after update expression in for statement\n";
		return NULL;
	} // end if no rparen
	m_pLexer->GetNextToken();

	// Parse the body statement in its own scope
	shared_ptr<StmtAST> pBody;
	{
		ScopeSentry s_scopeInner( this );
		pBody= parseStatement();
	}
	if( !pBody ) {
		cerr << "Could not parse body in for statement\n";
		return NULL;
	} // end if error

	// Check that the condition expression has type "bool"
	// TODO: Check convertability once we add casting?
	if( pCondition->GetType() != BuiltinTypeAST::GetBool() ) {
		cerr << "For statement condition expression must evaluate to bool\n";
		return NULL;
	} // end if non-bool condition

	return shared_ptr<ForAST>( new ForAST(pInitializer, pCondition, pUpdate, pBody) );
} // end Parser::parseForStatement()


//! Parses a while statement
shared_ptr<WhileAST> Parser::parseWhileStatement() {
	/*
	while_statement
		::= while '(' expression ')' statement
	*/

	// We expect "while"
	if( m_pLexer->GetCurrentToken() != TOKEN_WHILE ) {
		cerr << "Expected \"while\" while parsing while statement\n";
		return NULL;
	} // end if no "while"
	m_pLexer->GetNextToken();

	// We expect '('
	if( m_pLexer->GetCurrentToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' after \"while\" while parsing while statement\n";
		return NULL;
	} // end if no lparen
	m_pLexer->GetNextToken();

	// Parse the condition
	shared_ptr<ExprAST> pCondition= parseExpression();
	if( !pCondition ) {
		cerr << "Could not parse condition expression while parsing while statement\n";
		return NULL;
	} // end if parse error

	// We expect ')'
	if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		cerr << "Expected ')' after condition expression while parsing while statement\n";
		return NULL;
	} // end if no rparen
	m_pLexer->GetNextToken();

	// Parse the statement
	shared_ptr<StmtAST> pBody= parseStatement();
	if( !pBody ) {
		cerr << "Could not parse body statement while parsing while statement\n";
		return NULL;
	} // end if parse error

	// Create the while expression
	return shared_ptr<WhileAST>( new WhileAST(pCondition, pBody) );
} // end Parser::parseWhileStatement()


//! Creates a numeric literal, given the type and its string representation
shared_ptr<LiteralAST> Parser::makeLiteral( Token token, const string& strLiteral ) {
	if( !Lexer::IsLiteralToken(token) ) {
		ASSERT( false );
		return NULL;
	} // end if not literal token

	LiteralAST* pRet= NULL;
	switch( token ) {
	case TOKEN_LITERAL_INT:
		return shared_ptr<LiteralAST>( new IntegerAST(strLiteral) );
	case TOKEN_LITERAL_FLOAT:
		return shared_ptr<DoubleAST>( new DoubleAST(strLiteral) );
	case TOKEN_LITERAL_BOOL:
		return shared_ptr<BoolAST>( new BoolAST(strLiteral) );
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
		cerr << "Variable \"" << strName << "\" was already declared in scope\n";
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
		cerr << "Function prototype \"" << strName << "\" was already declared in scope with a different signature\n";
		return false;
	} // end if function exists in scope

	// Insert the function
	m_parseScope.prototypes[strName]= pPrototype;
	return true;
} // end Parser::addPrototypeToScope()


//! Adds a type to the current scope. Returns false if the type
//! already exists in scope
bool Parser::addTypeToScope( const shared_ptr<const TypeAST>& pType ) {
	// Lookup the type by name and fail if it exists
	const string& strName= pType->GetName();
	map<string, shared_ptr<const TypeAST>>::iterator itType= m_parseScope.types.find( strName );
	if( itType != m_parseScope.types.end() ) {
		cerr << "Type \"" + strName + "\" was already declared in scope\n";
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


//! Non-inline destructor
Parser::~Parser() { }