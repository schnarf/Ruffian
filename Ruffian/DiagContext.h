#pragma once

class DiagStream;
struct SourceRange;

// Define the ErrorMessage enum
enum ErrorMessage {
#define ERROR_INC(err, msg) err,
#include "diag_errors.inc"
#undef ERROR_INC
};

class DiagContext {
public:
  //! Initialize with filename
  DiagContext( const string& strFilename );
  //! Logs an error
  DiagStream Error( ErrorMessage error, const SourceRange& range );
private:
  const string m_strFilename;

  friend class DiagStream;
  //! Called by DiagStream with the formatted error message
  void diagStreamError( const string& strError, const SourceRange& range );
}; // end class DiagContext
