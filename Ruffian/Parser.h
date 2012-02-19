#pragma once

class Lexer; class DiagContext; class SourceRange;
enum Token;
class DeclarationAST; class ExprAST; class FunctionAST; class ReturnAST; class VariableAST; class TypeAST;
class BlockAST; class PrimaryExprAST; class CallAST; class LiteralAST; class ConditionalAST; class StmtAST; class ArraysizeAST;
class PrototypeAST; class ModuleAST; class ForAST; class ForRangeAST; class PrimaryStmtAST; class CastAST; class WhileAST;

class Parser {
public:
	//! Initialize
	Parser();
	//! Non-inline destructor
	~Parser();

	//! Runs the main parsing loop with the given lexer
  //! Returns TRUE on success or FALSE on failure
	bool Run( Lexer& lexer, DiagContext& diagContext );
  //! Creates a module AST node from everything we've parsed so far
  ModuleAST* CreateModule();

private:
	Lexer* m_pLexer;			        //!< Our lexer
  DiagContext* m_pDiagContext;  //!< Our diagnost, const SourceRange& rangeNameic context

  vector<shared_ptr<PrototypeAST>> m_pPrototypes;     //!< Our list of function prototypes
	vector<shared_ptr<FunctionAST>> m_pFunctions;       //!< Our list of function definitions
	
	//! Parses a primary expression.
	shared_ptr<ExprAST> parsePrimaryExpression();
	//! Parses an identifier expression (variable or function call)
	shared_ptr<ExprAST> parseIdentifierExpression();
	//! Parses a literal expression
	shared_ptr<LiteralAST> parseLiteralExpression();
	//! Parses a parenthesized expression
	shared_ptr<ExprAST> parseParenExpression();
	//! Parses an arraysize expression
	shared_ptr<ArraysizeAST> parseArraySizeExpression();

	//! Parses a unary operator expression.
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
	//! Parses a primary statement.
	shared_ptr<PrimaryStmtAST> parsePrimaryStatement();
	//! Parses an expression.
	shared_ptr<ExprAST> parseExpression();
	//! Parses a return statement
	shared_ptr<ReturnAST> parseReturnStatement();
	//! Parses a variable declaration.
	shared_ptr<DeclarationAST> parseVariableDeclaration();
	//! Parses a variable identifier
	shared_ptr<VariableAST> parseVariable();
	//! Parses a type identifier.
	shared_ptr<const TypeAST> parseType();
	//! Parses a function call expression, having already parsed the function name
	shared_ptr<CallAST> parseCallExpression( const string& strName, const SourceRange& rangeName );
	//! Parses a cast expression having already parsed the type name
	shared_ptr<CastAST> parseCastExpression( const string& strType, const SourceRange& rangeType );
	//! Parses a numeric literal
	shared_ptr<LiteralAST> parseLiteral();
	//! Parses a conditional statement
	shared_ptr<ConditionalAST> parseConditionalStatement();
	//! Parses a for statement
	shared_ptr<StmtAST> parseForStatement();
  //! Parses a for range statement
  shared_ptr<ForRangeAST> parseForRangeStatement();
	//! Parses a while statement
	shared_ptr<WhileAST> parseWhileStatement();

	//! Creates a numeric literal, given the type and its string representation
	static shared_ptr<LiteralAST> makeLiteral( Token token, const string& strLiteral, const SourceRange& range );

	//! Adds a variable declaration to the current scope. Returns false
	//! if the variable already exists in scope
	bool addVariableToScope( const shared_ptr<DeclarationAST>& pDeclaration );
	//! Adds a function prototype to the current scope. Returns false if the
	//! prototype already exists in scope, but with a different signature
	bool addPrototypeToScope( const shared_ptr<PrototypeAST>& pPrototype );
	//! Adds a type to the current scope. Returns false if the type
	//! already exists in scope
	bool addTypeToScope( const shared_ptr<const TypeAST>& pType, const SourceRange& range );
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

  //! Called when we expect to find \a token.
  //! If \a token is not found, emits an error message and returns FALSE,
  //! otherwise returns TRUE
  bool expectToken( Token token );
  //! Called when we expect to find and consume \a token
  //! If \a token is not found, emits an error message and return FALSE,
  //! otherwise eats the token and returns TRUE
  bool expectEatToken( Token token );

	//! Sets the expected return type, should be used inside function bodies
	void setExpectedReturnType( const shared_ptr<const TypeAST>& pReturnType ) { m_parseScope.pReturnType= pReturnType; }
	//! Gets the expected return type, should be used inside function bodies
	const shared_ptr<const TypeAST>& getExpectedReturnType() const { return m_parseScope.pReturnType; }

	//! Holds the scope that the parser needs to know about
	struct ParseScope {
		map<string, shared_ptr<DeclarationAST>> variables;	//!< Table of variable declarations
		map<string, shared_ptr<PrototypeAST>> prototypes;	//!< Table of function prototypes
		map<string, shared_ptr<const TypeAST>> types;		//!< Table of types
		set<string> definedFunctions;						//!< Set of defined functions
		shared_ptr<const TypeAST> pReturnType;				//!< Expected return type, only set in function definitions
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