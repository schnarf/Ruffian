#pragma once

class Lexer;
class AssignmentAST; class DeclarationAST; class ExprAST; class FunctionAST; class ReturnAST; class VariableAST; class TypeAST; class BlockAST; class PrimaryExprAST; class CallAST; class LiteralAST; class ConditionalAST; class StmtAST; class PrototypeAST; class ModuleAST;

class Parser {
public:
	//! Initialize with parser
	Parser( const shared_ptr<Lexer>& pLexer );
	//! Non-inline destructor
	~Parser();

	//! Runs the main parsing loop, returning the parsed module at the
	//! root of the AST on success, or NULL on failure.
	ModuleAST* Run();

private:
	shared_ptr<Lexer> m_pLexer;			//!< Our lexer

	//! Parses a function declaration or definition.
	//! Returns either a prototype or full function
	pair<PrototypeAST*, FunctionAST*> parseFunctionDeclarationOrDefinition();
	//! Parses a block
	BlockAST* parseBlock();
	//! Parses a statement
	StmtAST* parseStatement();
	//! Parses an expression
	ExprAST* parseExpression( bool bSemicolon, bool bComma, bool bRparen );
	//! Parses a return statement
	ReturnAST* parseReturnStatement();
	//! Parses a variable declaration statement
	DeclarationAST* parseVariableDeclaration();
	//! Parses an assignment expression
	AssignmentAST* parseAssignmentExpression();
	//! Parses a variable identifier
	VariableAST* parseVariable();
	//! Parses a type identifier
	TypeAST* parseType();
	//! Parses a function call expression, having already parsed the function name
	CallAST* parseCallExpression( const string& strName );
	//! Parses a numeric literal
	LiteralAST* parseLiteral();
	//! Parses a conditional statement
	ConditionalAST* parseConditionalStatement();

	//! Adds a variable declaration to the current scope. Returns false
	//! if the variable already exists in scope
	bool addVariableToScope( DeclarationAST* pDeclaration );
	//! Adds a function prototype to the current scope. Returns false if the
	//! prototype already exists in scope, but with a different signature
	bool addPrototypeToScope( PrototypeAST* pPrototype );
	//! Adds a function declaration to the current scope. Returns false
	//! if the function already exists in scope
	bool addFunctionToScope( FunctionAST* pFunction );

	//! Looks for a variable declaration in scope. Returns NULL if it does not exist,
	//! but does not give any error messages
	DeclarationAST* findVariableInScope( const string& strName );
	//! Looks up a function prototype in scope. Returns NULL if it does not exist,
	//! but does not give any error messages
	PrototypeAST* findPrototypeInScope( const string& strName );
	//! Looks up a function declaration in scope. Returns NULL if it does not exists,
	//! but does not give any error messages
	FunctionAST* findFunctionInScope( const string& strName );

	//! Holds the scope that the parser needs to know about
	struct ParseScope {
		map<string, DeclarationAST*> variables;		//!< Table of variable declarations
		map<string, PrototypeAST*> prototypes;		//!< Table of function prototypes
		map<string, FunctionAST*> functions;		//!< Table of function declarations
	}; // end struct ParseScope

	ParseScope m_parseScope;

	//! Sentry for entering/exiting a new scope
	class ScopeSentry {
	public:
		//! Initialize, entering a new scope
		ScopeSentry( Parser* pParser ) : m_pParser(pParser), m_oldScope(m_pParser->m_parseScope) {}
		//! Exit the scope
		~ScopeSentry() { m_pParser->m_parseScope= m_oldScope; }
	private:
		Parser* m_pParser;			//!< Our parser instance
		ParseScope m_oldScope;		//!< Our old scope
	}; // end class ScopeSentry
}; // end class Parser