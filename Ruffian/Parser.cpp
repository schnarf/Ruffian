#include "common.h"
#include "Parser.h"
#include "Lexer.h"
#include "FunctionAST.h"

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
	if( parseFunctionDefinition() ) {
		cout << "Parsed a function definition\n";
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

	vector< pair<string, string> > args;
	while( m_pLexer->GetNextToken() != TOKEN_RPAREN ) {
		// typename identifier ','
		if( m_pLexer->GetCurrentToken() != TOKEN_IDENTIFIER ) {
			cerr << "Expected an identifier while parsing function definition argument list, found \"" << Lexer::StringifyToken(m_pLexer->GetCurrentToken()) << "\"\n";
			return NULL;
		}
		string strTypename= m_pLexer->GetIdentifier();

		if( m_pLexer->GetNextToken() != TOKEN_IDENTIFIER ) {
			cerr << "Expected an identifier while parsing function definition argument list\n";
			return NULL;
		}
		string strIdentifier= m_pLexer->GetIdentifier();

		// Now we can have a comma or rparen
		m_pLexer->GetNextToken();

		if( m_pLexer->GetCurrentToken() == TOKEN_RPAREN ) break;

		if( m_pLexer->GetCurrentToken() != TOKEN_COMMA ) {
			cerr << "Expected a comma while parsing function definition argument list\n";
			return NULL;
		}

		args.push_back( make_pair(strTypename, strIdentifier) );
	} // end while parsing argument list

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
	
	string strReturnType= m_pLexer->GetIdentifier();

	 // Now look for the opening brace
	if( m_pLexer->GetNextToken() != TOKEN_LBRACE ) {
		cerr << "Expected '{' while parsing function definition\n";
		return NULL;
	} // end if no left brace

	// Now enter a new scope
	ScopeSentry s_scope( this );

	// Now look for the closing brace
	if( m_pLexer->GetNextToken() != TOKEN_RBRACE ) {
		cerr << "Expected '}' while parsing function definition\n";
		return NULL;
	} // end if no right brace

	return NULL;

} // end Parser::parseFunctionDefinition()


//! Enters a new scope
void Parser::enterNewScope() {
} // end Parser::enterNewScope()


//! Exits the current scope
void Parser::exitCurrentScope() {
} // end Parser::exitCurrentScope()

//! Non-inline destructor
Parser::~Parser() { }