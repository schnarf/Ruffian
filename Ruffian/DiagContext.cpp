#include "common.h"
#include "DiagContext.h"
#include "DiagStream.h"
#include "SourceLocation.h"
#include <iostream>

//! Initialize with filename
DiagContext::DiagContext( const string& strFilename ) :
  m_strFilename(strFilename) {
}

namespace {
  const char* errorToString( ErrorMessage error ) {
    switch( error ) {
#define ERROR_INC(err, msg) case err: return msg;
#include "diag_errors.inc"
#undef ERROR_INC
    default:
      ASSERT(false);
      return "";
    }
  }
}

//! Logs an error
DiagStream DiagContext::Error( ErrorMessage error, const SourceRange& range ) {
  return DiagStream( *this, errorToString(error), range );
}


//! Called by DiagStream with the formatted error message
void DiagContext::diagStreamError( const string& strError, const SourceRange& range ) {
  std::cerr << m_strFilename << "(" << range.begin.iLine << "): " << strError << std::endl;
}