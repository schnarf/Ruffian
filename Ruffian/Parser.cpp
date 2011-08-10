#include "common.h"
#include "Parser.h"
#include "AST.h"
#include "Lexer.h"

//! Initialize with parser
Parser::Parser( const shared_ptr<Lexer>& pLexer ) :
	m_pLexer(pLexer) {

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

	// Check for "def"
	if( m_pLexer->GetCurrentToken() != TOKEN_DEF ) {
		cerr << "Expected \"def\" while parsing function prototype\n";
		return null_ret;
	} // end if error

	// Parse the identifier for the function's name
	if( m_pLexer->GetNextToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing function prototype\n";
		return null_ret;
	} // end if no identifier

	string strName= m_pLexer->GetIdentifier();

	// Parse the argument list
	if( m_pLexer->GetNextToken() != TOKEN_LPAREN ) {
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
		shared_ptr<TypeAST> pType( parseType() );

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

	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RPAREN );

	// Parse the '->' before the return type
	if( m_pLexer->GetNextToken() != TOKEN_ARROW ) {
		cerr << "Expected '->' while parsing function definition\n";
		return null_ret;
	} // end if no arrow
	
	// Parse the return type
	if( m_pLexer->GetNextToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected a typename while parsing function return type\n";
		return null_ret;
	} // end if no identifier
	
	shared_ptr<TypeAST> pReturnType( parseType() );

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
	/* statement
		::= variable_declaration
		::= return_stmt
		::= assignment_stmt
		::= block
		::= conditional_stmt
	*/

	switch( m_pLexer->GetCurrentToken() ) {
	case TOKEN_VAR: return parseVariableDeclaration();
	case TOKEN_RETURN: return parseReturnStatement();
	case TOKEN_IDENTIFIER: return parseAssignmentExpression();
	case TOKEN_LBRACE: return parseBlock();
	case TOKEN_IF: return parseConditionalStatement();
	default:
		cerr << "Expected a primary expression\n";
		return NULL;
	} // end switch current token
} // end Parser::parseStatement()


//! Parses an expression
shared_ptr<ExprAST> Parser::parseExpression( bool bSemicolon, bool bComma, bool bRparen ) {
	/* expression_list
		::= ''
		::= expression ',' expression_list

	   call_expr
	    ::= identifier '(' expression_list ')'

	   binop
		::= '=='
		::= '+'
		::= '-'
		::= '*'
		::= '/'

	   binop_expr
		::= expression binop expression

	   paren_expr
	    ::= '(' expression ')'

	   expression
	    ::= identifier
		::= literal
		::= call_expr
		::= binop_expr
		::= paren_expr
	*/

	// If we have a semicolon already, then this is an empty expression
	//if( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON ) return new EmptyAST;

	ASSERT( bSemicolon || bComma || bRparen );

	// Now parse until we hit a terminator
	// We store the expression we are building
	shared_ptr<ExprAST> pExpr;
	while( !(bSemicolon && m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON)
		&& !(bComma && m_pLexer->GetCurrentToken() == TOKEN_COMMA)
		&& !(bRparen && m_pLexer->GetCurrentToken() == TOKEN_RPAREN) ) {

		// Make an AST node for the potential identifier or literal, then eat the identifier/literal
		Token lastToken= m_pLexer->GetCurrentToken();
		shared_ptr<ExprAST> pLastExpr;
		string strIdentifier;
		if( lastToken == TOKEN_IDENTIFIER ) {
			pLastExpr.reset( new VariableAST( findVariableInScope(m_pLexer->GetIdentifier())) );
			strIdentifier= m_pLexer->GetIdentifier();
		} else if( Lexer::IsLiteralToken(lastToken) ) {
			pLastExpr= makeLiteral( lastToken, m_pLexer->GetLiteral() );
			ASSERT( pLastExpr );
		}

		if( pLastExpr ) m_pLexer->GetNextToken();

		// Now we'll try to distinguish what kind of expression this is. If we have a terminator,
		// it's a variable expression.
		// If we have a lparen, it's a function call or paren expression
		// If we have a binary opertor, it's a binop expression
		if( (bSemicolon && m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON)
			|| (bComma && m_pLexer->GetCurrentToken() == TOKEN_COMMA)
			|| (bRparen && m_pLexer->GetCurrentToken() == TOKEN_RPAREN) ) {
			
			// Do not eat the terminator
			ASSERT( !pExpr );
			pExpr.swap( pLastExpr );
			break;
		} // end if semicolon
		else if( m_pLexer->GetCurrentToken() == TOKEN_LPAREN && lastToken == TOKEN_IDENTIFIER ) {
			ASSERT( !pExpr );
			pExpr= parseCallExpression( strIdentifier );
			if( !pExpr ) {
				cerr << "Could not parse call expression while parsing expression\n";
				return NULL;
			} // end if parse error
		} // end if function call
		else if( lastToken == TOKEN_LPAREN && !pLastExpr ) {
			ASSERT( !pExpr );
			// Eat the lparen
			m_pLexer->GetNextToken();
			// Parse the expression up until the rparen
			pExpr= parseExpression( false, false, true );
			if( !pExpr ) {
				cerr << "Could not parse paren expression while parsing expression\n";
				return NULL;
			} // end if parse error

			// Eat the rparen
			ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RPAREN );
			m_pLexer->GetNextToken();
		} // end if paren expression
		else if( Lexer::IsBinopToken(m_pLexer->GetCurrentToken()) ) {
			// This is a binary operator. The expression we've parsed so far is going to be the LHS.
			// The rest will be the RHS
			if( pExpr == NULL ) {
				pExpr= pLastExpr;
			} // end if no LHS
			else {
				pLastExpr.reset();
			}

			// Eat the binary operator
			Token binop= m_pLexer->GetCurrentToken();
			m_pLexer->GetNextToken();

			// Parse the RHS
			shared_ptr<ExprAST> pRight( parseExpression(bSemicolon, bComma, bRparen) );
			if( !pRight ) {
				cerr << "Expected a RHS to the binary operator \"" << Lexer::StringifyToken(binop) << "\" while parsing an expression\n";
				return NULL;
			} // end if no RHS

			// Now create the binop, giving it ownership of the expressions
			pExpr.reset( new BinopAST(binop, pExpr, pRight) );
		} else if( m_pLexer->GetCurrentToken() == TOKEN_LPAREN ) {
			// Paren expression
			ASSERT( !pExpr );
		} else {
			// This is a parse error
			cerr << "Unexpected token \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\" while parsing an expression\n";
			return NULL;
		}
	} // end while parsing
	
	if( !pExpr ) {
		cerr << "Expected an expression but could not parse anything\n";
		return NULL;
	}

	return pExpr;
} // end Parser:parseExpression()


//! Parses a return statment
shared_ptr<ReturnAST> Parser::parseReturnStatement() {
	/* return_stmt
		::= 'return' expression ';'
	*/

	if( m_pLexer->GetCurrentToken() != TOKEN_RETURN ) {
		cerr << "Expected \"return\"\n";
		return NULL;
	} // end if no "return"

	// Eat "return"
	m_pLexer->GetNextToken();

	// Parse the expression
	shared_ptr<ExprAST> pExpr( parseExpression(true, false, false) );
	if( !pExpr ) return NULL;

	// Now eat the semicolon
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON );
	m_pLexer->GetNextToken();

	return shared_ptr<ReturnAST>( new ReturnAST(pExpr) );
} // end Parser::parseReturnStatement()


//! Parses a variable declaration
shared_ptr<DeclarationAST> Parser::parseVariableDeclaration() {
	/* variable_declaration
		::= 'var' identifier identifier ';'
		::= 'var' identifier identifier '=' expression ';'
	*/

	// Check for "var"
	if( m_pLexer->GetCurrentToken() != TOKEN_VAR ) {
		cerr << "Expected \"var\"\n";
		return NULL;
	} // end if no "var"

	// Parse the typename identifier
	if( m_pLexer->GetNextToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing a variable declaration typename\n";
		return NULL;
	} // end if no typename

	string strType= m_pLexer->GetIdentifier();

	// Now parse the identifier
	if( m_pLexer->GetNextToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing a variable declaration\n";
		return NULL;
	} // end if no identifier

	string strName= m_pLexer->GetIdentifier();

	// Advance to the next token. We expect either a semicolon, in which case
	// we are done, or an equals sign, in which case we will parse another expression
	m_pLexer->GetNextToken();
	shared_ptr<ExprAST> pInitializer;
	if( m_pLexer->GetCurrentToken() == TOKEN_ASSIGN ) {
		// Eat the '='
		m_pLexer->GetNextToken();

		// Parse the right-hand side
		pInitializer= parseExpression( true, false, false );
		if( !pInitializer ) return NULL;
	} else if( m_pLexer->GetCurrentToken() != TOKEN_SEMICOLON ) {
		cerr << "Expected ';' or '=' while parsing a variable declaration\n";
		return NULL;
	}

	// Now eat the current token, which we expect to be a semicolon, then create the declaration AST
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON );
	m_pLexer->GetNextToken();
	// Add the variable to the current scope and then return the declaration
	shared_ptr<DeclarationAST> pDeclaration( new DeclarationAST(strName, shared_ptr<TypeAST>(new TypeAST(strType)), pInitializer) );
	addVariableToScope( pDeclaration );
	return pDeclaration;
} // end Parser::parseVariableDeclaration()


//! Parses an assignment expression
shared_ptr<AssignmentAST> Parser::parseAssignmentExpression() {
	// TODO: allow expressions to be lvalues, like array[2]

	/* assignment_stmt
		::= identifier '=' expression ';'
	*/
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing an assignment expression\n";
		return NULL;
	} // end if no identifier

	string strTarget= m_pLexer->GetIdentifier();

	if( m_pLexer->GetNextToken() != TOKEN_ASSIGN ) {
		cerr << "Expected '=' while parsing an assignment expression\n";
		return NULL;
	} // end if no '='

	// Eat the equals, then try to parse an expression
	m_pLexer->GetNextToken();
	shared_ptr<ExprAST> pExpr( parseExpression(true, false, false) );

	if( !pExpr ) {
		cerr << "Expected an expression while parsing an assignment expression\n";
		return NULL;
	} // end if no expression

	// Now we expect a semicolon, which we should eat
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON );
	m_pLexer->GetNextToken();

	// Look up the variable declaration
	shared_ptr<DeclarationAST> pDeclaration= findVariableInScope( strTarget );
	if( !pDeclaration ) {
		cerr << "Variable \"" << strTarget << "\" was not declared in the current scope, in assignment expression\n";
		return NULL;
	} // end if not declared

	return shared_ptr<AssignmentAST>( new AssignmentAST(shared_ptr<VariableAST>(new VariableAST(pDeclaration)), pExpr) );
} // end Parser::parseAssignmentExpression()


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
shared_ptr<TypeAST> Parser::parseType() {
	/* type ::= identifier */
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing a type\n";
		return NULL;
	} // end if no identifier

	string strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();
	return shared_ptr<TypeAST>( new TypeAST(strName) );
} // end Parser::parseType()


//! Parses a function call expression, having already parsed the function name
shared_ptr<CallAST> Parser::parseCallExpression( const string& strName ) {
	/* expression_list
		::= ''
		::= expression ',' expression_list

	   call_expr
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
		shared_ptr<ExprAST> pExpr( parseExpression(false, true, true) );
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

	for( uint iArg=0; iArg<pArgs.size(); ++iArg ) {
		if( pArgs[iArg]->GetType() != pPrototype->GetArgs()[iArg]->GetType() ) {
			cerr << "Argument " << 1+iArg << " for function " << strName << " has the wrong type\n";
			return NULL;
		} // end if wrong type
	} // end for argument
	
	return shared_ptr<CallAST>( new CallAST(pPrototype, pArgs) );
} // end parseCallExpression()


//! Parses a numeric literal
shared_ptr<LiteralAST> Parser::parseLiteral() {
	/* literal
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
	/* conditional_stmt
		::= if '(' expression ')' block
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
	shared_ptr<ExprAST> pCondExpr( parseExpression(false, false, true) );
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

	// Parse the block
	shared_ptr<BlockAST> pBlock( parseBlock() );
	if( !pBlock ) {
		cerr << "Expected a block while parsing a conditional statement\n";
		return NULL;
	} // end if no block

	// Check for "else". If we find an else, keep parsing recursively
	shared_ptr<BlockAST> pElseBlock;
	if( m_pLexer->GetCurrentToken() == TOKEN_ELSE ) {
		// Eat the else
		m_pLexer->GetNextToken();

		// Now we either expect a left brace or "if"
		if( m_pLexer->GetCurrentToken() == TOKEN_LBRACE ) {
			pElseBlock= parseBlock();
			if( !pElseBlock ) {
				cerr << "Expected an else block while parsing a conditional statement\n";
				return NULL;
			} // end if no else block
		} else if( m_pLexer->GetCurrentToken() == TOKEN_IF ) {
			// Parse the next "if" block
			shared_ptr<ConditionalAST> pNextConditional( parseConditionalStatement() );
			if( !pNextConditional ) {
				cerr << "Expected an else if block while parsing a conditional statement\n";
				return NULL;
			} // end if no else block

			// Build a block containing the next conditional
			vector<shared_ptr<StmtAST>> pExprs;
			pExprs.push_back( pNextConditional );
			pElseBlock.reset( new BlockAST(pExprs) );
		} else {
			cerr << "Expected '{' or \"if\" after \"else\" while parsing a conditional statement\n";
			return NULL;
		}
	}

	return shared_ptr<ConditionalAST>( new ConditionalAST(pCondExpr, pBlock, pElseBlock) );
} // end Parser::parseConditionalStatement()


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
		return shared_ptr<FloatAST>( new FloatAST(strLiteral) );
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
	cout << "Variable " << strName << " added to scope\n";
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
	map<string, shared_ptr<PrototypeAST>>::iterator itProto= m_parseScope.prototypes.find(strName);
	if( itProto != m_parseScope.prototypes.end() && *itProto->second != *pPrototype ) {
		cerr << "Function prototype \"" << strName << "\" was already declared in scope with a different signature\n";
		return false;
	} // end if function exists in scope

	// Insert the function
	m_parseScope.prototypes[strName]= pPrototype;
	return true;
} // end Parser::addPrototypeToScope()


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


//! Returns whether the specified function is defined in scope. If this is called
//! for a function that hasn't even been prototyped, also returns FALSE.
bool Parser::isFunctionDefinedInScope( const string& strName ) {
	return m_parseScope.definedFunctions.find( strName ) != m_parseScope.definedFunctions.end();
} // end Parser::isFunctionDefinedInScope()


//! Non-inline destructor
Parser::~Parser() { }