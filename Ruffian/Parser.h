#pragma once

class Lexer;
enum Token;
class DeclarationAST; class ExprAST; class FunctionAST; class ReturnAST; class VariableAST; class TypeAST; class BlockAST; class PrimaryExprAST; class CallAST; class LiteralAST; class ConditionalAST; class StmtAST; class PrototypeAST; class ModuleAST; class ForAST; class PrimaryStmtAST;

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


	//! Parses a primary expression
	shared_ptr<ExprAST> parsePrimaryExpression();
	//! Parses an identifier expression (variable or function call)
	shared_ptr<ExprAST> parseIdentifierExpression();
	//! Parses a literal expression
	shared_ptr<LiteralAST> parseLiteralExpression();
	//! Parses a parenthesized expression
	shared_ptr<ExprAST> parseParenExpression();

	//! Parses a unary operator expression
	shared_ptr<ExprAST> parseUnaryOpExpression();

	//! Parses the right-hand side of a binary expression, given the left-hand side and its highest-precedence operator
	shared_ptr<ExprAST> parseBinopRHS( int precedence, shared_ptr<ExprAST> pLeft );

	//! Parses a function declaration or definition.
	//! Returns either a prototype or full function
	pair<shared_ptr<PrototypeAST>, shared_ptr<FunctionAST>> parseFunctionDeclarationOrDefinition();
	//! Parses a block
	shared_ptr<BlockAST> parseBlock();
	//! Parses a statement
	shared_ptr<StmtAST> parseStatement();
	//! Parses a primary statement
	shared_ptr<PrimaryStmtAST> parsePrimaryStatement();
	//! Parses an expression
	shared_ptr<ExprAST> parseExpression();
	//! Parses a return statement
	shared_ptr<ReturnAST> parseReturnStatement();
	//! Parses a variable declaration statement, with the type already parsed
	shared_ptr<DeclarationAST> parseVariableDeclaration();
	//! Parses a variable identifier
	shared_ptr<VariableAST> parseVariable();
	//! Parses a type identifier
	shared_ptr<const TypeAST> parseType();
	//! Parses a function call expression, having already parsed the function name
	shared_ptr<CallAST> parseCallExpression( const string& strName );
	//! Parses a numeric literal
	shared_ptr<LiteralAST> parseLiteral();
	//! Parses a conditional statement
	shared_ptr<ConditionalAST> parseConditionalStatement();
	//! Parses a for statement
	shared_ptr<ForAST> parseForStatement();

	//! Creates a numeric literal, given the type and its string representation
	static shared_ptr<LiteralAST> makeLiteral( Token token, const string& strLiteral );

	//! Adds a variable declaration to the current scope. Returns false
	//! if the variable already exists in scope
	bool addVariableToScope( const shared_ptr<DeclarationAST>& pDeclaration );
	//! Adds a function prototype to the current scope. Returns false if the
	//! prototype already exists in scope, but with a different signature
	bool addPrototypeToScope( const shared_ptr<PrototypeAST>& pPrototype );
	//! Adds a type to the current scope. Returns false if the type
	//! already exists in scope
	bool addTypeToScope( const shared_ptr<const TypeAST>& pType );
	//! Sets that the function has been defined in scope. This should only
	//! ever be called once per function.
	void setFunctionDefinedInScope( const string& strName );

	//! Looks for a variable declaration in scope. Returns NULL if it does not exist,
	//! but does not give any error messages
	shared_ptr<DeclarationAST> findVariableInScope( const string& strName );
	//! Looks up a function prototype in scope. Returns NULL if it does not exist,
	//! but does not give any error messages
	shared_ptr<PrototypeAST> findPrototypeInScope( const string& strName );
	//! Looks up a type in scope. Returns NULL if it does not exist,
	//! but does not give any error messages
	shared_ptr<const TypeAST> findTypeInScope( const string& strName );
	//! Returns whether the specified function is defined in scope. If this is called
	//! for a function that hasn't even been prototyped, also returns FALSE.
	bool isFunctionDefinedInScope( const string& strName );

	//! Holds the scope that the parser needs to know about
	struct ParseScope {
		map<string, shared_ptr<DeclarationAST>> variables;	//!< Table of variable declarations
		map<string, shared_ptr<PrototypeAST>> prototypes;	//!< Table of function prototypes
		map<string, shared_ptr<const TypeAST>> types;		//!< Table of types
		set<string> definedFunctions;						//!< Set of defined functions
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