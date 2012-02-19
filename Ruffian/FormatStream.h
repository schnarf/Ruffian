#pragma once

class FormatStream {
public:
  //! Initialize with our format string
  FormatStream( const string& fmt );
  //! Replaces the next format specifier with \a str
  FormatStream& operator<<( const string& str );
  //! Replaces the next format specifier with \a uValue
  FormatStream& operator<<( size_t uValue );
  //! Returns our formatted string
  operator string() const;
private:
  const string m_fmt;
  string m_str;                 //!< The formatted portion of our string so far
                                //!< This represents m_fmt up to and not including  m_iPos
  string::size_type m_iPos;     //!< Position in the format string
}; // end class FormatStream