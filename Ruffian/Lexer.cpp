#include "common.h"
#include "Lexer.h"
#include <cstdlib>

//! Initialize with a file
Lexer::Lexer( const shared_ptr<FILE>& pFile ) :
	m_pFile(pFile),
	m_lastChar(' '),
	m_currentToken(TOKEN_UNKNOWN),
	m_strIdentifier("") {

	// Prime the pump
	GetNextToken();

} // end Lexer::Lexer()


//! Advances to the next token, returning it
Token Lexer::GetNextToken() {
	m_currentToken= getTok();
	return m_currentToken;
} // end Lexer::GetNextToken()


//! Returns the name of the given token
string Lexer::StringifyToken( Token token ) {
	switch( token ) {
	case TOKEN_LPAREN: return "(";
	case TOKEN_RPAREN: return ")";
	case TOKEN_LBRACE: return "{";
	case TOKEN_RBRACE: return "}";
	case TOKEN_COMMA: return ",";
	case TOKEN_SEMICOLON: return ";";
	case TOKEN_ARROW: return "->";
	case TOKEN_PLUS: return "+";
	case TOKEN_MINUS: return "-";
	case TOKEN_STAR: return "*";
	case TOKEN_SLASH: return "/";
	case TOKEN_ASSIGN: return "=";
	case TOKEN_COMPARE: return "==";
	case TOKEN_LT: return "<";
	case TOKEN_GT: return ">";
	case TOKEN_LE: return "<=";
	case TOKEN_GE: return ">=";
	case TOKEN_DEF: return "def";
	case TOKEN_RETURN: return "return";
	case TOKEN_IF: return "if";
	case TOKEN_ELSE: return "else";
	case TOKEN_FOR: return "for";
	case TOKEN_LITERAL_INT: return "int literal";
	case TOKEN_LITERAL_FLOAT: return "float literal";
	case TOKEN_LITERAL_BOOL: return "bool literal";
	case TOKEN_IDENTIFIER: return "identifier";
	case TOKEN_EOF: return "eof";
	case TOKEN_UNKNOWN: return "unknown";
	default: ASSERT(false); return "unknown";
	} // end switch token
} // end Lexer::StringifyToken()


//! Returns whether the current token is a binary operator
bool Lexer::IsBinopToken( Token token ) {
	switch( token ) {
	case TOKEN_PLUS:
	case TOKEN_MINUS:
	case TOKEN_STAR:
	case TOKEN_SLASH:
	case TOKEN_COMPARE:
	case TOKEN_LT:
	case TOKEN_GT:
	case TOKEN_LE:
	case TOKEN_GE:
	case TOKEN_ASSIGN:
		return true;
	default:
		return false;
	} // end switch token
} // end Lexer::IsBinopToken()

//! Gets the next token
Token Lexer::getTok() {
	// Skip any whitespace
	while( isspace(m_lastChar) && m_lastChar != EOF ) readChar();

	// Check for anything that starts with an alphabetic character,
	// this can be either an identifier or some language keywords
	if( isalpha(m_lastChar) ) {
		// Read in all the characters that could possibly be in an identifier
		m_strIdentifier.clear();
		while( isalnum(m_lastChar) || m_lastChar == '_' ) {
			m_strIdentifier+= m_lastChar;
			readChar();
		} // end while reading identifier

		// Check for a keyword
		if( m_strIdentifier == "def" ) return TOKEN_DEF;
		else if( m_strIdentifier == "return" ) return TOKEN_RETURN;
		else if( m_strIdentifier == "if" ) return TOKEN_IF;
		else if( m_strIdentifier == "else" ) return TOKEN_ELSE;
		else if( m_strIdentifier == "for" ) return TOKEN_FOR;
		else if( m_strIdentifier == "true" || m_strIdentifier == "false" ) { m_strLiteral= m_strIdentifier; return TOKEN_LITERAL_BOOL; }
		else return TOKEN_IDENTIFIER;
	} // end if starts with alphabetic character

	// Check for a numeric literal
	if( isdigit(m_lastChar) || m_lastChar == '.' ) {
		string strNum;

		while( isdigit(m_lastChar) || m_lastChar == '.' ) {
			strNum+= m_lastChar;
			readChar();
		} // end while reading numeric literal

		// Store the literal
		m_strLiteral= strNum;

		// Try to determine if it's an integer or floating-point literal, or if it's malformed
		uint nDecimalPoints= 0;
		for( string::const_iterator itChar=strNum.begin(); itChar!=strNum.end(); ++itChar ) { if( *itChar == '.' ) ++nDecimalPoints; }
		if( nDecimalPoints == 0 ) {
			// Integer
			return TOKEN_LITERAL_INT;
		} else if( nDecimalPoints == 1 ) {
			// Float
			return TOKEN_LITERAL_FLOAT;
		} else {
			cerr << "Could not parse numeric literal \"" << strNum << "\", expected fewer decimal points\n";
			return TOKEN_UNKNOWN;
		}
	} // end if numeric literal

	// Check for symbols
	bool bEatChar= false;
	bool bSymbol= true;
	Token token;

	switch( m_lastChar ) {
	case '+': token= TOKEN_PLUS; bEatChar= true; break;
	case '*': token= TOKEN_STAR; bEatChar= true; break;
	case '/': 
		if( readChar() == '/' ) {
			// This is a comment. Skip characters until the end of the line
			while( m_lastChar != '\n' && m_lastChar != '\r' && m_lastChar != EOF ) readChar();
			// Now keep parsing
			return getTok();
		} else {
			token= TOKEN_SLASH;
			bEatChar= true;
		}
		break;
	case '-':
		if( readChar() == '>' ) { token= TOKEN_ARROW; bEatChar= true; }
		else { token= TOKEN_MINUS; bEatChar= false; }
		break;
	case '(': token= TOKEN_LPAREN; bEatChar= true; break;
	case ')': token= TOKEN_RPAREN; bEatChar= true; break;
	case '{': token= TOKEN_LBRACE; bEatChar= true; break;
	case '}': token= TOKEN_RBRACE; bEatChar= true; break;
	case ',': token= TOKEN_COMMA; bEatChar= true; break;
	case ';': token= TOKEN_SEMICOLON; bEatChar= true; break;
	case '<':
		if( readChar() == '=' ) { token= TOKEN_LE; bEatChar= true; }
		else { token= TOKEN_LT; bEatChar= false; }
		break;
	case '>':
		if( readChar() == '=' ) { token= TOKEN_GE; bEatChar= true; }
		else { token= TOKEN_GT; bEatChar= false; }
		break;
	case '=':
		if( readChar() == '=' ) { token= TOKEN_COMPARE; bEatChar= true; }
		else { token= TOKEN_ASSIGN; bEatChar= false; }
		break;
	case EOF: return TOKEN_EOF;
	default:
		bSymbol= false;
		break;
	} // end switch last character

	// If we had a symbol, advance to the next character, then return the token
	if( bSymbol ) {
		if( bEatChar ) readChar();
		return token;
	} // end if symbol token

	// If we get here, something unexpected happened
	cerr << "Unexpected character '" << m_lastChar << "' encountered when trying to parse a token.\n";
	return TOKEN_UNKNOWN;
} // end Lexer::getTok()


//! Reads a single character from our file
char Lexer::readChar() {
	size_t uRead= fread( &m_lastChar, sizeof(char), 1, m_pFile.get() );
	if( !uRead ) m_lastChar= EOF;
	return m_lastChar;
} // end Lexer::readChar()


//! Non-inline destructor
Lexer::~Lexer() { }
