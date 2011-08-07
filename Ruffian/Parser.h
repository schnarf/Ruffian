#pragma once

class Lexer;
class AssignmentAST; class DeclarationAST; class ExprAST; class FunctionAST; class ReturnAST; class VariableAST; class TypeAST; class BlockAST; class PrimaryExprAST; class CallAST;

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
	//! Parses a block
	BlockAST* parseBlock();
	//! Parses a primary expression
	PrimaryExprAST* parsePrimaryExpression();
	//! Parses an expression
	ExprAST* parseExpression( bool bSemicolon, bool bComma, bool bRparen );
	//! Parses a return expression
	ReturnAST* parseReturnExpression();
	//! Parses a variable declaration
	DeclarationAST* parseVariableDeclaration();
	//! Parses an assignment expression
	AssignmentAST* parseAssignmentExpression();
	//! Parses a variable identifier
	VariableAST* parseVariable();
	//! Parses a type identifier
	TypeAST* parseType();
	//! Parses a function call expression, having already parsed the function name
	CallAST* parseCallExpression( const string& strName );

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