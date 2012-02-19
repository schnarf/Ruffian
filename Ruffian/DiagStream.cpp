#include "common.h"
#include "DiagStream.h"

//! Replaces the next format specifier with \a str
DiagStream& DiagStream::operator<<( const string& str ) {
  m_stream << str;
  return *this;
}

//! Replaces the next format specified with \a uValue
DiagStream& DiagStream::operator<<( const size_t uValue ) {
  m_stream << uValue;
  return *this;
}

//! Initialize with DiagContext and format string
DiagStream::DiagStream( DiagContext& context, const string& fmt, const SourceRange& range ) :
  m_context(context),
  m_stream(fmt),
  m_range(range) {
}

//! On destruction, send the formatted result to our DiagContext
DiagStream::~DiagStream() {
  m_context.diagStreamError( m_stream, m_range );
}