#include "common.h"
#include "Codegen.h"
#include "CodegenContext.h"
#include "Lexer.h"
#include "Parser.h"
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include "llvm/Support/DynamicLibrary.h"

static void printInt( int i ) {
	cout << i << endl;
} // end printInt()

static void printNewline() {
	cout << endl;
} // end printNewline()

static void printFloat( float f ) {
	cout << f << endl;
} // end printFloat()

static void printDouble( double f ) {
	cout << f << endl;
} // end printDouble()

static int rand_lib() {
	return rand() - 5000;
} // end rand_lib()

static char* malloc_lib( uint64 nBytes ) {
	return (char*)malloc( nBytes );
} // end malloc_lib()

static void free_lib( char* p ) {
	free( p );
} // end free_lib()

int main( int argc, char* argv[] ) {

	llvm::InitializeNativeTarget();

	// We expect one or two arguments
	if( argc != 2 && argc != 3) {
		cerr << "Usage: " << argv[0] << "[-v] filename\n";
		return 1;
	}

  // If we got two arguments, parse the first as a flag
  bool bVerbose= false;
  if( argc == 3 ) {
    const char* const strFlag= argv[1];
    if( strcmp(strFlag, "-v") == 0 ) {
      bVerbose= true;
    } else {
      cerr << "Unknown option " << strFlag;
    }
  }

	// Try to open the file
  const char* const pFilename= argc == 2 ? argv[1] : argv[2];
	shared_ptr<FILE> pFile( fopen(pFilename, "r"), fclose );
	if( !pFile.get() ) {
		cerr << "Could not open " << pFilename << " for reading\n";
		return 1;
	}

	// Create a lexer to read this file
	shared_ptr<Lexer> pLexer( new Lexer(pFile) );
	
	// Create a parser and run the main parsing loop
	unique_ptr<Parser> pParser( new Parser(pLexer) );
	ModuleAST* pModule= pParser->Run();
	if( !pModule ) return 1;

	if( bVerbose ) cout << "Parsed the module successfully.\n";

	// Create a codegen and generate code for the module
	unique_ptr<Codegen> pCodegen( new Codegen );
	
	// Add our environment's functions
	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getInt32Ty(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printInt", pCodegen->GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printInt", (void*)printInt );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printNewline", pCodegen->GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printNewline", (void*)printNewline );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getFloatTy(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printFloat", pCodegen->GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printFloat", (void*)printFloat );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getDoubleTy(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printDouble", pCodegen->GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printDouble", (void*)printDouble );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getInt32Ty(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "rand", pCodegen->GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "rand", (void*)rand_lib );
		srand( time(NULL) );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getInt64Ty(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getInt8PtrTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "malloc", pCodegen->GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "malloc", (void*)malloc_lib );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getInt8PtrTy(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "free", pCodegen->GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "free", (void*)free_lib );
	}

	bool bCodegenSuccess= pCodegen->Run( pModule );
	if( !bCodegenSuccess ) return 1;

	if( bVerbose ) cout << "Ran codegen successfully.\n";

	// Look up the main() function
	llvm::Function* pMainFunction= pCodegen->GetContext()->GetModule()->getFunction( "main" );
	if( !pMainFunction ) {
		cerr << "Could not resolve main()\n";
		return 1;
	} // end if no main function
	if( pMainFunction->getReturnType() != llvm::Type::getInt32Ty(llvm::getGlobalContext()) ) {
		cerr << "main() was found but does not return an int64\n";
		return 1;
	} // end if wrong return type
	if( pMainFunction->getArgumentList().size() != 1 ) {
		cerr << "main() was found but does not take exactly 1 argument\n";
		return 1;
	} // end if wrong argument count
	if( pMainFunction->getArgumentList().front().getType() != llvm::Type::getInt32Ty(llvm::getGlobalContext()) ) {
		cerr << "main() must take an argument of type int32\n";
		return 1;
	} // end if wrong argument type

	// JIT the function, returning a function pointer
	int32 (*mainFunc)(int32)= (int32(*)(int32))pCodegen->GetContext()->GetExecutionEngine()->getPointerToFunction( pMainFunction );
	pCodegen->GetContext()->GetModule()->dump();
  int mainRet= mainFunc(23);
	if( bVerbose ) cout << "main() returned " << mainRet << endl;

	return mainRet;
}