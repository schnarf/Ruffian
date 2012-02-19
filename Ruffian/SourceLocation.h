#pragma once

struct SourceLocation {
  long iLine, iCol;   // 1-indexed line and column
  SourceLocation() : iLine(0), iCol(0) {}
}; // end struct SourceLocation

inline bool operator==( const SourceLocation& a, const SourceLocation& b ) {
  return a.iLine == b.iLine && a.iCol == b.iCol;
}

struct SourceRange {
  SourceLocation begin, end;
  SourceRange() : begin(SourceLocation()), end(SourceLocation()) {}
  SourceRange( const SourceLocation& begin_, const SourceLocation& end_ ) :
    begin(begin_), end(end_) {}
}; // end struct SourceRange