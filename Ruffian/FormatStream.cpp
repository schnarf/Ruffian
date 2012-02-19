#include "common.h"
#include "FormatStream.h"
#include <sstream>

//! Initialize with our format string
FormatStream::FormatStream( const string& fmt ) :
  m_fmt(fmt),
  m_iPos(0) {

} // end FormatStream()


//! Replaces the next format specifier with \a str
FormatStream& FormatStream::operator<<( const string& str ) {
  // Search for the next '%'
  string::size_type iPos= m_fmt.find( '%', m_iPos );
  if( iPos == string::npos ) {
    ASSERT( false );
    m_iPos= m_fmt.length();
    return *this;
  }

  // Append the portion of the string [m_iPos, iPos)
  m_str.append( m_fmt.begin()+m_iPos, m_fmt.begin()+iPos );
  // Append the string for the %
  m_str.append( str );

  // Next time, start searching after the %
  m_iPos= iPos + 1;

  return *this;
} // end FormatStream::operator<<()


//! Replaces the next format specifier with \a uValue
FormatStream& FormatStream::operator<<( size_t uValue ) {
  std::stringstream stream;
  stream << uValue;
  return *this << stream.str();
} // end FormatStream::operator<<()


//! Returns our formatted string
FormatStream::operator string() const {
  // Make sure there are no more format specifiers in the format string
  ASSERT( m_fmt.find('%', m_iPos) == string::npos );

  // Append the rest of our format string
  string ret= m_str;
  ret.append( m_fmt.begin()+m_iPos, m_fmt.end() );
  return ret;
} // end FormatStream::operator string()