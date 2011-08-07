#include "common.h"
#include "Parser.h"
#include "Lexer.h"
#include "FunctionAST.h"
#include "Scope.h"

//! Initialize with parser
Parser::Parser( const shared_ptr<Lexer>& pLexer ) :
	m_pLexer(pLexer) {
} // end Parser::Parser()


//! Runs the main parsing loop
void Parser::Run() {
	while( 1 ) {
		switch( m_pLexer->GetCurrentToken() ) {
		case TOKEN_EOF: return;
		case TOKEN_DEF: handleFunctionDefinition(); break;
		default:
			cerr << "Unexpected token \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\".\n";
			return;
		} // end switch current token
	} // end while not EOF
} // end Parser::Run()


//! Handles a function definition
void Parser::handleFunctionDefinition() {
	if( FunctionAST* pFunction= parseFunctionDefinition() ) {
		cout << "Parsed a function definition\n";
		pFunction->Codegen( m_scope )->dump();
	} else {
		// Skip token for error recovery
		m_pLexer->GetNextToken();
	}
} // end Parser::handleFunctionDefinition()


//! Parses a function definition
FunctionAST* Parser::parseFunctionDefinition() {
	/*
		function_definition ::= 'def' identifier '(' argument_list ')' -> identifier '{' expression_list '}'
		argument_list ::=
			''
			identifier identifier ',' argument_list
	*/

	// Check for "def"
	if( m_pLexer->GetCurrentToken() != TOKEN_DEF ) {
		cerr << "Expected \"def\" while parsing function definition\n";
		return NULL;
	} // end if error

	// Parse the identifier for the function's name
	if( m_pLexer->GetNextToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing function definition\n";
		return NULL;
	} // end if no identifier

	string strName= m_pLexer->GetIdentifier();

	// Parse the argument list
	if( m_pLexer->GetNextToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' while parsing function definition argument list\n";
		return NULL;
	} // end if no left paren

	// Eat the lparen
	m_pLexer->GetNextToken();

	vector< pair<TypeAST*, VariableAST*> > args;
	while( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		// typename identifier ','
		if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
			cerr << "Expected an identifier (type) while parsing function definition argument list, found \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\"\n";
			return NULL;
		}
		TypeAST* pType= parseType();

		if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
			cerr << "Expected an identifier (variable) while parsing function definition argument list, found \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\"\n";
			return NULL;
		}
		VariableAST* pVariable= parseVariable();

		// Now we can have a comma or rparen
		if( m_pLexer->GetCurrentToken() == TOKEN_COMMA ) {
			// Eat the comma before parsing the next argument
			m_pLexer->GetNextToken();
		} else if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
			cerr << "Expected ',' or ')' while parsing function definition argument list, found \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\"\n";
			return NULL;
		}

		args.push_back( make_pair(pType, pVariable) );
	} // end while parsing argument list

	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RPAREN );

	// Parse the '->' before the return type
	if( m_pLexer->GetNextToken() != TOKEN_ARROW ) {
		cerr << "Expected '->' while parsing function definition\n";
		return NULL;
	} // end if no arrow
	
	// Parse the return type
	if( m_pLexer->GetNextToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected a typename while parsing function return type\n";
		return NULL;
	} // end if no identifier
	
	TypeAST* pReturnType= parseType();

	 // Now look for the opening brace
	if( m_pLexer->GetCurrentToken() != TOKEN_LBRACE ) {
		cerr << "Expected '{' while parsing function definition\n";
		return NULL;
	} // end if no left brace

	// Parse the function's body
	BlockAST* pBody= parseBlock();

	return new FunctionAST( strName, pReturnType, args, pBody );

} // end Parser::parseFunctionDefinition()


//! Parses a block
BlockAST* Parser::parseBlock() {
	/* block
		::= '{' primary_expression* '}'
	*/
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_LBRACE );
	vector<PrimaryExprAST*> pExprs;
	m_pLexer->GetNextToken();
	while( m_pLexer->GetCurrentToken() != TOKEN_RBRACE ) {
		PrimaryExprAST* pExpr= parsePrimaryExpression();
		if( !pExpr ) {
			cerr << "Could not parse primary expression while parsing a block\n";
			return NULL;
		} // end if parse error

		pExprs.push_back( pExpr );
	} // end while looking for a right brace

	// Eat the right brace
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RBRACE );
	m_pLexer->GetNextToken();

	return new BlockAST( pExprs );
} // end Parser::parseBlock()


//! Parses a primary expression
PrimaryExprAST* Parser::parsePrimaryExpression() {
	/* primary_expression
		::= variable_declaration
		::= return_expression
		::= assignment_expression
		::= block
		::= conditional_expression
	*/

	switch( m_pLexer->GetCurrentToken() ) {
	case TOKEN_VAR: return parseVariableDeclaration();
	case TOKEN_RETURN: return parseReturnExpression();
	case TOKEN_IDENTIFIER: return parseAssignmentExpression();
	case TOKEN_LBRACE: return parseBlock();
	case TOKEN_IF: return parseConditionalExpression();
	default:
		cerr << "Expected a primary expression\n";
		return NULL;
	} // end switch current token
} // end Parser::parsePrimaryExpression()


//! Parses an expression
ExprAST* Parser::parseExpression( bool bSemicolon, bool bComma, bool bRparen ) {
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

	   expression
	    ::= identifier
		::= literal
		::= call_expr
		::= binop_expr
	*/

	// If we have a semicolon already, then this is an empty expression
	//if( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON ) return new EmptyAST;

	ASSERT( bSemicolon || bComma || bRparen );

	// Now parse until we hit a terminator
	// We store the expression we are building
	ExprAST* pExpr= NULL;
	while( !(bSemicolon && m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON)
		&& !(bComma && m_pLexer->GetCurrentToken() == TOKEN_COMMA)
		&& !(bRparen && m_pLexer->GetCurrentToken() == TOKEN_RPAREN) ) {

		// We expect an identifier or literal in all cases
		if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER && !Lexer::IsLiteralToken(m_pLexer->GetCurrentToken()) ) {
			cerr << "Expected an identifier or literal while parsing an expression, found \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\"\n";
			delete pExpr;
			return NULL;
		} // end if no identifier

		// Make an AST node for the potential identifier or literal, then eat the identifier/literal
		Token lastToken= m_pLexer->GetCurrentToken();
		ExprAST* pLastExpr= NULL;
		string strIdentifier;
		if( lastToken == TOKEN_IDENTIFIER ) { pLastExpr= new VariableAST( m_pLexer->GetIdentifier() ); strIdentifier= m_pLexer->GetIdentifier(); }
		else if( lastToken == TOKEN_LITERAL_INT ) pLastExpr= new IntegerAST( m_pLexer->GetIntLiteral() );
		else if( lastToken == TOKEN_LITERAL_FLOAT ) pLastExpr= new FloatAST( m_pLexer->GetFloatLiteral() );
		
		ASSERT( pLastExpr != NULL );
		m_pLexer->GetNextToken();

		// Now we'll try to distinguish what kind of expression this is. If we have a terminator,
		// it's a variable expression.
		// If we have a lparen, it's a function call.
		// If we have a binary opertor, it's a binop expression
		if( (bSemicolon && m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON)
			|| (bComma && m_pLexer->GetCurrentToken() == TOKEN_COMMA)
			|| (bRparen && m_pLexer->GetCurrentToken() == TOKEN_RPAREN) ) {
			
			// Do not eat the terminator
			ASSERT( pExpr == NULL );
			pExpr= pLastExpr;
			break;
		} // end if semicolon
		else if( m_pLexer->GetCurrentToken() == TOKEN_LPAREN && lastToken == TOKEN_IDENTIFIER ) {
			ASSERT( pExpr == NULL );
			delete pLastExpr;
			pExpr= parseCallExpression( strIdentifier );
			if( !pExpr ) {
				cerr << "Could not parse call expression while parsing expression\n";
				delete pExpr;
				return NULL;
			} // end if parse error
		} // end if function call
		else if( Lexer::IsBinopToken(m_pLexer->GetCurrentToken()) ) {
			// This is a binary operator. The expression we've parsed so far is going to be the LHS.
			// The rest will be the RHS
			if( pExpr == NULL ) {
				pExpr= pLastExpr;
			} // end if no LHS
			else {
				delete pLastExpr;
			}

			// Eat the binary operator
			Token binop= m_pLexer->GetCurrentToken();
			m_pLexer->GetNextToken();

			// Parse the RHS
			ExprAST* pRight= parseExpression( bSemicolon, bComma, bRparen );
			if( !pRight ) {
				cerr << "Expected a RHS to the binary operator \"" << Lexer::StringifyToken(binop) << "\" while parsing an expression\n";
				delete pExpr;
				return NULL;
			} // end if no RHS

			// Now create the binop
			pExpr= new BinopAST( binop, pExpr, pRight );
		} else {
			// This is a parse error
			cerr << "Unexpected token \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\" while parsing an expression\n";
			delete pExpr;
			delete pLastExpr;
			return NULL;
		}
	} // end while parsing
	
	if( pExpr == NULL ) {
		cerr << "Expected an expression but could not parse anything\n";
		return NULL;
	}

	return pExpr;
} // end Parser:parseExpression()


//! Parses a return expression
ReturnAST* Parser::parseReturnExpression() {
	/* return_expression
		::= 'return' expression
	*/

	if( m_pLexer->GetCurrentToken() != TOKEN_RETURN ) {
		cerr << "Expected \"return\"\n";
		return NULL;
	} // end if no "return"

	// Eat "return"
	m_pLexer->GetNextToken();

	// Parse the expression
	ExprAST* pExpr= parseExpression( true, false, false );
	if( pExpr == NULL ) return NULL;

	// Now eat the semicolon
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON );
	m_pLexer->GetNextToken();

	return new ReturnAST( pExpr );
} // end Parser::parseReturnExpression()


//! Parses a variable declaration
DeclarationAST* Parser::parseVariableDeclaration() {
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
	ExprAST* pInitializer= NULL;
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
	return new DeclarationAST( new VariableAST(strName), new TypeAST(strType), pInitializer );
} // end Parser::parseVariableDeclaration()


//! Parses an assignment expression
AssignmentAST* Parser::parseAssignmentExpression() {
	// TODO: allow expressions to be lvalues, like array[2]

	/* assignment_expression
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
	ExprAST* pExpr= parseExpression( true, false, false );

	if( !pExpr ) {
		cerr << "Expected an expression while parsing an assignment expression\n";
		return NULL;
	} // end if no expression

	// Now we expect a semicolon, which we should eat
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_SEMICOLON );
	m_pLexer->GetNextToken();
	return new AssignmentAST( new VariableAST(strTarget), pExpr );
} // end Parser::parseAssignmentExpression()


//! Parses a variable identifier
VariableAST* Parser::parseVariable() {
	/* variable ::= identifier */
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing a variable\n";
		return NULL;
	} // end if no identifier

	string strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();
	return new VariableAST( strName );
} // end Parser::parseVariable()


//! Parses a type identifier
TypeAST* Parser::parseType() {
	/* type ::= identifier */
	if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
		cerr << "Expected an identifier while parsing a type\n";
		return NULL;
	} // end if no identifier

	string strName= m_pLexer->GetIdentifier();
	m_pLexer->GetNextToken();
	return new TypeAST( strName );
} // end Parser::parseType()


//! Parses a function call expression, having already parsed the function name
CallAST* Parser::parseCallExpression( const string& strName ) {
	/* expression_list
		::= ''
		::= expression ',' expression_list

	   call_expr
	    ::= identifier '(' expression_list ')'
	*/

	if( m_pLexer->GetCurrentToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' while parsing function call argument list\n";
		return NULL;
	} // end if no lparen

	// Eat the left paren
	m_pLexer->GetNextToken();

	// Now parse all the arguments
	vector<ExprAST*> pArgs;
	while( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		ExprAST* pExpr= parseExpression( false, true, true );
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
			for( uint i=0; i<pArgs.size(); ++i ) delete pArgs[i];
			return NULL;
		}
	} // end while parsing arguments

	// Eat the right paren
	ASSERT( m_pLexer->GetCurrentToken() == TOKEN_RPAREN );
	m_pLexer->GetNextToken();

	return new CallAST( strName, pArgs );
} // end parseCallExpression()


//! Parses a numeric literal
LiteralAST* Parser::parseLiteral() {
	/* literal
		::= int_literal
		::= float_literal
	*/
	LiteralAST* pRet= NULL;

	if( m_pLexer->GetCurrentToken() == TOKEN_LITERAL_INT ) {
		pRet= new IntegerAST( m_pLexer->GetIntLiteral() );
	} else if( m_pLexer->GetCurrentToken() == TOKEN_LITERAL_FLOAT ) {
		pRet= new FloatAST( m_pLexer->GetFloatLiteral() );
	} else {
		cerr << "Expected an integer or float literal\n";
		return NULL;
	}

	// Now eat the token
	m_pLexer->GetNextToken();

	return pRet;
} // end Parser::parseLiteral()


//! Parses a conditional expression
ConditionalAST* Parser::parseConditionalExpression() {
	/* conditional_expression
		::= if '(' expression ')' block
		::= conditional_expression else conditional_expression
	*/

	// Eat the "if"
	if( m_pLexer->GetCurrentToken() != TOKEN_IF ) {
		cerr << "Expected \"if\" while parsing a conditional expression\n";
		return NULL;
	} // end if no "if"
	m_pLexer->GetNextToken();

	// Find the left paren and eat it
	if( m_pLexer->GetCurrentToken() != TOKEN_LPAREN ) {
		cerr << "Expected '(' after \"if\" while parsing a conditional expression\n";
		return NULL;
	} // end if no lparen
	m_pLexer->GetNextToken();

	// Parse the expression up to the right paren
	ExprAST* pCondExpr= parseExpression( false, false, true );
	if( !pCondExpr ) {
		cerr << "Expected an expression between parentheses while parsing a conditional expression\n";
		return NULL;
	} // end if no expression

	// Now find and eat the right paren
	if( m_pLexer->GetCurrentToken() != TOKEN_RPAREN ) {
		cerr << "Expected ')' after condition while parsing a conditional expression\n";
		delete pCondExpr;
		return NULL;
	} // end if no right paren
	m_pLexer->GetNextToken();

	// Parse the block
	BlockAST* pBlock= parseBlock();
	if( !pBlock ) {
		cerr << "Expected a block while parsing a conditional expression\n";
		delete pCondExpr;
		return NULL;
	} // end if no block

	// Check for "else". If we find an else, keep parsing recursively
	BlockAST* pElseBlock= NULL;
	if( m_pLexer->GetCurrentToken() == TOKEN_ELSE ) {
		// Eat the else
		m_pLexer->GetNextToken();

		// Now we either expect a left brace or "if"
		if( m_pLexer->GetCurrentToken() == TOKEN_LBRACE ) {
			pElseBlock= parseBlock();
			if( !pElseBlock ) {
				cerr << "Expected an else block while parsing a conditional expression\n";
				delete pCondExpr;
				delete pBlock;
				return NULL;
			} // end if no else block
		} else if( m_pLexer->GetCurrentToken() == TOKEN_IF ) {
			// Parse the next "if" block
			ConditionalAST* pNextConditional= parseConditionalExpression();
			if( !pNextConditional ) {
				cerr << "Expected an else if block while parsing a conditional expression\n";
				delete pCondExpr;
				delete pBlock;
				return NULL;
			} // end if no else block

			// Build a block containing the next conditional
			vector<PrimaryExprAST*> pExprs;
			pExprs.push_back( pNextConditional );
			pElseBlock= new BlockAST( pExprs );
		} else {
			cerr << "Expected '{' or \"if\" after \"else\" while parsing a conditional expression\n";
			delete pCondExpr;
			delete pBlock;
			return NULL;
		}
	}

	return new ConditionalAST( pCondExpr, pBlock, pElseBlock );
} // end Parser::parseConditionalExpression()


//! Enters a new scope
void Parser::enterNewScope() {
} // end Parser::enterNewScope()


//! Exits the current scope
void Parser::exitCurrentScope() {
} // end Parser::exitCurrentScope()

//! Non-inline destructor
Parser::~Parser() { }