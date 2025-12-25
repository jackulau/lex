//! Position tracking for source code locations.
//!
//! This module provides types for tracking positions in source code,
//! including line/column information and byte offsets.

use std::fmt;

/// A location in source code (line and column, 1-indexed).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Location {
    /// Line number (1-indexed).
    pub line: u32,
    /// Column number (1-indexed, counted in Unicode code points).
    pub column: u32,
}

impl Location {
    /// Create a new location at line 1, column 1.
    pub fn new() -> Self {
        Self { line: 1, column: 1 }
    }

    /// Create a location at the specified line and column.
    pub fn at(line: u32, column: u32) -> Self {
        Self { line, column }
    }

    /// Advance the location by one character.
    pub fn advance(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else if c == '\t' {
            // Tab to next 4-column boundary
            self.column = ((self.column - 1) / 4 + 1) * 4 + 1;
        } else {
            self.column += 1;
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A span in source code with start/end byte offsets and locations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Byte offset from start of source (inclusive).
    pub start: usize,
    /// Byte offset from start of source (exclusive).
    pub end: usize,
    /// Human-readable start location.
    pub start_loc: Location,
    /// Human-readable end location.
    pub end_loc: Location,
}

impl Span {
    /// Create a new span.
    pub fn new(start: usize, end: usize, start_loc: Location, end_loc: Location) -> Self {
        Self {
            start,
            end,
            start_loc,
            end_loc,
        }
    }

    /// Create a zero-width span at a position.
    pub fn point(pos: usize, loc: Location) -> Self {
        Self {
            start: pos,
            end: pos,
            start_loc: loc,
            end_loc: loc,
        }
    }

    /// Get the length in bytes.
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Check if the span is empty.
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Create a span that encompasses both spans.
    pub fn merge(self, other: Span) -> Span {
        let (start, start_loc) = if self.start <= other.start {
            (self.start, self.start_loc)
        } else {
            (other.start, other.start_loc)
        };
        let (end, end_loc) = if self.end >= other.end {
            (self.end, self.end_loc)
        } else {
            (other.end, other.end_loc)
        };
        Span {
            start,
            end,
            start_loc,
            end_loc,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::point(0, Location::new())
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start_loc == self.end_loc {
            write!(f, "{}", self.start_loc)
        } else {
            write!(f, "{}-{}", self.start_loc, self.end_loc)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_location_new() {
        let loc = Location::new();
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 1);
    }

    #[test]
    fn test_location_advance() {
        let mut loc = Location::new();
        loc.advance('a');
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 2);

        loc.advance('\n');
        assert_eq!(loc.line, 2);
        assert_eq!(loc.column, 1);
    }

    #[test]
    fn test_location_tab() {
        let mut loc = Location::new();
        loc.advance('\t');
        assert_eq!(loc.column, 5); // Tabs align to 4-column boundaries
    }

    #[test]
    fn test_span_merge() {
        let span1 = Span::new(0, 5, Location::at(1, 1), Location::at(1, 6));
        let span2 = Span::new(5, 10, Location::at(1, 6), Location::at(1, 11));
        let merged = span1.merge(span2);
        assert_eq!(merged.start, 0);
        assert_eq!(merged.end, 10);
    }
}
