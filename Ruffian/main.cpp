#include "common.h"
#include "Lexer.h"
#include "Parser.h"
#include <cstdio>
#include <cstdlib>

int main( int argc, char* argv[] ) {

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
	
	// Create a parser and run the main loop
	shared_ptr<Parser> pParser( new Parser(pLexer) );
	pParser->Run();
	
	return 0;
}