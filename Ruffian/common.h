#pragma once

#include <iostream>
using std::cout; using std::cerr; using std::endl;

#include <map>
using std::map;

#include <memory>
using std::shared_ptr; using std::unique_ptr;
using std::dynamic_pointer_cast; using std::static_pointer_cast;

#include <set>
using std::set;

#include <string>
using std::string;

#include <vector>
using std::vector;

#include <utility>
using std::pair; using std::make_pair;

#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
using llvm::Value;

#if _DEBUG
	#include <cassert>
	#define ASSERT(cond) assert(cond)
#else
	#define ASSERT(cond)
#endif

typedef signed long long int int64;
typedef unsigned long long int uint64;
static_assert( sizeof(int64) == 8, "int64 must be 8 bytes" );
static_assert( sizeof(uint64) == 8, "uint64 must be 8 bytes" );

typedef signed int int32;
typedef unsigned int uint32;
typedef uint32 uint;
static_assert( sizeof(int32) == 4, "int32 must be 4 bytes" );
static_assert( sizeof(uint32) == 4, "uint32 must be 4 bytes" );