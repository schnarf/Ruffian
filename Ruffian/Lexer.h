#pragma once

enum Token {
	TOKEN_LPAREN,
	TOKEN_RPAREN,
	
	TOKEN_LBRACE,
	TOKEN_RBRACE,

	TOKEN_COMMA,
	TOKEN_SEMICOLON,
	TOKEN_ARROW,

	TOKEN_PLUS,
	TOKEN_MINUS,
	TOKEN_STAR,
	TOKEN_SLASH,
	
	TOKEN_ASSIGN,
	TOKEN_COMPARE,

	TOKEN_LT,
	TOKEN_GT,
	TOKEN_LE,
	TOKEN_GE,
	
	TOKEN_DEF,
	TOKEN_VAR,
	TOKEN_RETURN,
	TOKEN_IF,
	TOKEN_ELSE,

	TOKEN_LITERAL_INT,
	TOKEN_LITERAL_FLOAT,
	TOKEN_LITERAL_BOOL,

	TOKEN_IDENTIFIER,

	TOKEN_EOF,

	TOKEN_UNKNOWN,
	NUM_TOKENS
}; // end enum Token

class Lexer {
public:
	//! Initialize with a file
	Lexer( const shared_ptr<FILE>& pFile );
	//! Non-inline destructor
	~Lexer();

	//! Gets the current token
	Token GetCurrentToken() const { return m_currentToken; }
	//! Advances to the next token, returning it
	Token GetNextToken();

	//! Gets the name of the current identifier token
	string GetIdentifier() const { ASSERT( GetCurrentToken() == TOKEN_IDENTIFIER ); return m_strIdentifier; }
	//! Gets the string representation of the current literal token
	string GetLiteral() const { ASSERT( IsLiteralToken(GetCurrentToken()) ); return m_strLiteral; }

	// Returns the name of the given token
	static string StringifyToken( Token token );
	//! Returns whether the current token is a binary operator
	static bool IsBinopToken( Token token );
	//! Returns whether the current token is a literal
	static bool IsLiteralToken( Token token ) { return token == TOKEN_LITERAL_INT || token == TOKEN_LITERAL_FLOAT || token == TOKEN_LITERAL_BOOL; }

private:
	shared_ptr<FILE> m_pFile;			//!< Our file to read from

	char m_lastChar;					//!< Our last read character
	Token m_currentToken;				//!< Our current token

	string m_strIdentifier;				//!< Identifier string, filled in for identifier tokens
	string m_strLiteral;				//!< Literal string, filled in for literal tokens

	//! Gets the next token
	Token getTok();
	//! Reads a single character from our file
	char readChar();
}; // end class Lexer