#include "common.h"
#include "Codegen.h"
#include "CodegenContext.h"
#include "Lexer.h"
#include "Parser.h"
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include "llvm/Support/DynamicLibrary.h"
#include "ModuleAST.h"

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

	// We expect two to three arguments
	if( argc != 3 && argc != 4 ) {
		cerr << "Usage: " << argv[0] << "[-v] includepath filename\n";
		return 1;
	}

  // If we got two arguments, parse the first as a flag
  bool bVerbose= false;
  if( argc == 4 ) {
    const char* const strFlag= argv[1];
    if( strcmp(strFlag, "-v") == 0 ) {
      bVerbose= true;
    } else {
      cerr << "Unknown option " << strFlag;
    }
  }

  // Grab the include path
  const char* const pIncludePath= argc == 3 ? argv[1] : argv[2];

  // Try to open the standard library
  string strLibFilename= string(pIncludePath) + string("/stdlib.rf");
  shared_ptr<FILE> pLibFile( fopen(strLibFilename.c_str(), "r"), fclose );
  if( !pLibFile.get() ) {
    cerr << "Could not open " << strLibFilename << " for reading\n";
    return 1;
  }

	// Try to open the file
  const char* const pFilename= argc == 3 ? argv[2] : argv[3];
	shared_ptr<FILE> pFile( fopen(pFilename, "r"), fclose );
	if( !pFile.get() ) {
		cerr << "Could not open " << pFilename << " for reading\n";
		return 1;
	}

  // Create our parser. We'll feed the library file and then the source file to it.
  Parser parser;
  unique_ptr<Lexer> pLibLexer( new Lexer(pLibFile) );
  unique_ptr<Lexer> pSourceLexer( new Lexer(pFile) );
  if( !parser.Run(pLibLexer.get()) ) {
    cerr << "Failed to parse the library\n";
    return 1;
  }
  if( !parser.Run(pSourceLexer.get()) ) {
    cerr << "Failed to parse the source\n";
    return 1;
  }

  // Assemble the module
	unique_ptr<ModuleAST> pModule( parser.CreateModule() );

	if( bVerbose ) cout << "Parsed the module successfully.\n";

	// Create a codegen and generate code for the module
	Codegen codegen;
	
	// Add our environment's functions
	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getInt32Ty(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printInt", codegen.GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printInt", (void*)printInt );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printNewline", codegen.GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printNewline", (void*)printNewline );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getFloatTy(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printFloat", codegen.GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printFloat", (void*)printFloat );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getDoubleTy(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "printDouble", codegen.GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "printDouble", (void*)printDouble );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getInt32Ty(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "rand", codegen.GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "rand", (void*)rand_lib );
		srand( time(NULL) );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getInt64Ty(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getInt8PtrTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "malloc", codegen.GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "malloc", (void*)malloc_lib );
	}

	{
		vector<const llvm::Type*> pArgTypes;
		pArgTypes.push_back( llvm::Type::getInt8PtrTy(llvm::getGlobalContext()) );
		llvm::FunctionType* pFunctionType= llvm::FunctionType::get( llvm::Type::getVoidTy(llvm::getGlobalContext()), pArgTypes, false );
		llvm::Function* pFunction= llvm::Function::Create( pFunctionType, llvm::Function::ExternalLinkage, "free", codegen.GetContext()->GetModule() );
		llvm::sys::DynamicLibrary::AddSymbol( "free", (void*)free_lib );
	}

	bool bCodegenSuccess= codegen.Run( pModule.get() );
	if( !bCodegenSuccess ) return 1;

	if( bVerbose ) cout << "Ran codegen successfully.\n";

	// Look up the main() function
	llvm::Function* pMainFunction= codegen.GetContext()->GetModule()->getFunction( "main" );
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
	int32 (*mainFunc)(int32)= (int32(*)(int32))codegen.GetContext()->GetExecutionEngine()->getPointerToFunction( pMainFunction );
	codegen.GetContext()->GetModule()->dump();
  int mainRet= mainFunc(23);
	if( bVerbose ) cout << "main() returned " << mainRet << endl;

	return mainRet;
}