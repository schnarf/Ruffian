#include "common.h"
#include "Lexer.h"
#include "Parser.h"
#include <cstdio>
#include <cstdlib>

int main( int argc, char* argv[] ) {

	llvm::InitializeNativeTarget();

	// We expect one argument: a filename
	if( argc != 2 ) {
		cerr << "Usage: " << argv[0] << " filename\n";
		return 1;
	}

	// Try to open the file
	shared_ptr<FILE> pFile( fopen(argv[1], "r"), fclose );
	if( !pFile.get() ) {
		cerr << "Could not open " << argv[1] << " for reading\n";
		return 1;
	}

	// Create a lexer to read this file
	shared_ptr<Lexer> pLexer( new Lexer(pFile) );
	
	// Create a parser and run the main parsing loop
	unique_ptr<Parser> pParser( new Parser(pLexer) );
	ModuleAST* pModule= pParser->Run();
	if( !pModule ) return 1;

	cout << "Parsed the module successfully.\n";

	return 0;
}