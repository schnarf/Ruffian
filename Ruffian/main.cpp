#include "common.h"
#include "Codegen.h"
#include "CodegenContext.h"
#include "Lexer.h"
#include "Parser.h"
#include <cstdio>
#include <cstdlib>
#include "llvm/Support/DynamicLibrary.h"

static void printInt( int64 i ) {
	cout << i << endl;
} // end printInt()

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

	// Create a codegen and generate code for the module
	unique_ptr<Codegen> pCodegen( new Codegen );
	
	// Add our environment's functions
	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getInt64Ty(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printInt", pCodegen->GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printInt", (void*)printInt );
	}

	bool bCodegenSuccess= pCodegen->Run( pModule );
	if( !bCodegenSuccess ) return 1;

	cout << "Ran codegen successfully.\n";

	// Look up the main() function
	llvm::Function* pMainFunction= pCodegen->GetContext()->GetModule()->getFunction( "main" );
	if( !pMainFunction ) {
		cerr << "Could not resolve main()\n";
		return 1;
	} // end if no main function
	if( pMainFunction->getReturnType() != llvm::Type::getInt64Ty(llvm::getGlobalContext()) ) {
		cerr << "main() was found but does not return an int64\n";
		return 1;
	} // end if wrong return type
	if( pMainFunction->getArgumentList().size() != 1 ) {
		cerr << "main() was found but does not take exactly 1 argument\n";
		return 1;
	} // end if wrong argument count
	if( pMainFunction->getArgumentList().front().getType() != llvm::Type::getInt64Ty(llvm::getGlobalContext()) ) {
		cerr << "main() must take an argument of type int64\n";
		return 1;
	} // end if wrong argument type

	// JIT the function, returning a function pointer
	int64 (*mainFunc)(int64)= (int64(*)(int64))pCodegen->GetContext()->GetExecutionEngine()->getPointerToFunction( pMainFunction );
	cout << "main() returned " << mainFunc(23) << endl;

	return 0;
}