#pragma once

class ExprAST; class FunctionAST; class Lexer;

class Parser {
public:
	//! Initialize with parser
	Parser( const shared_ptr<Lexer>& pLexer );
	//! Non-inline destructor
	~Parser();

	//! Runs the main parsing loop
	void Run();

private:
	shared_ptr<Lexer> m_pLexer;			//!< Our lexer

	//! Handles a function definition
	void handleFunctionDefinition();

	//! Parses a function definition
	FunctionAST* parseFunctionDefinition();

	//! Enters a new scope
	void enterNewScope();
	//! Exits the current scope
	void exitCurrentScope();

	//! Sentry for entering/exiting a new scope
	class ScopeSentry {
	public:
		//! Initialize, entering a new scope
		ScopeSentry( Parser* pParser ) : m_pParser(pParser) { m_pParser->enterNewScope(); }
		//! Exit the scope
		~ScopeSentry() { m_pParser->exitCurrentScope(); }
	private:
		Parser* m_pParser;		//!< Our parser instance
	}; // end class ScopeSentry
}; // end class Parser