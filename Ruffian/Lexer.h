#pragma once

#include "SourceLocation.h"

enum Token {
	TOKEN_LPAREN,
	TOKEN_RPAREN,
	
	TOKEN_LBRACE,
	TOKEN_RBRACE,

	TOKEN_LBRACKET,
	TOKEN_RBRACKET,

	TOKEN_COMMA,
	TOKEN_SEMICOLON,
	TOKEN_ARROW,
  TOKEN_COLON,

	TOKEN_PLUS,
	TOKEN_MINUS,
	TOKEN_STAR,
	TOKEN_SLASH,
	
	TOKEN_ASSIGN,

	TOKEN_INCREMENT,
	TOKEN_DECREMENT,
	TOKEN_NOT,
	
	TOKEN_EQ,
	TOKEN_NEQ,

	TOKEN_LT,
	TOKEN_GT,
	TOKEN_LE,
	TOKEN_GE,

	TOKEN_AND,
	TOKEN_OR,
	
	TOKEN_DEF,
	TOKEN_RETURN,
	TOKEN_IF,
	TOKEN_ELSE,
	TOKEN_FOR,
  TOKEN_FOREACH,
	TOKEN_DO,
	TOKEN_WHILE,

	TOKEN_ARRAYSIZE,

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
	//! Initialize with a filename
  //! Throws runtime_error if the file can't be opened
	Lexer( const string& strFilename );
	//! Non-inline destructor
	~Lexer();

	//! Gets the current token
	Token GetCurrentToken() const { return m_currentToken; }
	//! Advances to the next token
	void GetNextToken();

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
	//! Returns whether the current token is a prefix unary operator
	static bool IsPreUnaryOpToken( Token token );
	//! Returns whether the current token is a postfix unary operator
	static bool IsPostUnaryOpToken( Token token );

  //! Returns our filename
  const string& GetFilename() const { return m_strFilename; }
  //! Returns our current location in the source code
  SourceLocation GetSourceLocation() const { return m_range.begin; }
  //! Returns the source range for the current token
  SourceRange GetSourceRange() const { return m_range; }
private:
  const string m_strFilename; //!< Our filename
  shared_ptr<FILE> m_pFile;   //!< Our file to read from
  SourceLocation m_loc;       //!< Current source location
  SourceRange m_range;

	char m_lastChar;					  //!< Our last read character
	Token m_currentToken;				//!< Our current token

	string m_strIdentifier;			//!< Identifier string, filled in for identifier tokens
	string m_strLiteral;				//!< Literal string, filled in for literal tokens

	//! Gets the next token
	Token getTok();
	//! Reads a single character from our file
	char readChar();
}; // end class Lexer