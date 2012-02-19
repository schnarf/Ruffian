#pragma once

#include "DiagContext.h"
#include "FormatStream.h"
#include "SourceLocation.h"

class DiagStream {
public:
  //! Replaces the next format specifier with \a str
  DiagStream& operator<<( const string& str );
  //! Replaces the next format specified with \a uValue
  DiagStream& operator<<( const size_t uValue );

  //! On destruction, send the formatted result to our DiagContext
  ~DiagStream();
private:
  friend class DiagContext;
  //! Initialize with DiagContext and format string
  DiagStream( DiagContext& context, const string& fmt, const SourceRange& range );

  DiagContext m_context;
  FormatStream m_stream;
  SourceRange m_range;
}; // end class DiagStream