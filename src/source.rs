//! Source text abstraction with cursor for lexical analysis.
//!
//! This module provides a cursor over UTF-8 source text that tracks
//! byte positions and line/column locations.

use crate::span::{Location, Span};

/// A cursor over source text with position tracking.
///
/// The cursor provides character-by-character iteration over UTF-8 text
/// while tracking both byte positions (for slicing) and line/column
/// locations (for error messages).
pub struct Source<'a> {
    /// Original source text (UTF-8).
    text: &'a str,
    /// Current byte position.
    pos: usize,
    /// Current location (line, column).
    loc: Location,
}

impl<'a> Source<'a> {
    /// Create a new source cursor.
    pub fn new(text: &'a str) -> Self {
        Self {
            text,
            pos: 0,
            loc: Location::new(),
        }
    }

    /// Get the original source text.
    pub fn text(&self) -> &'a str {
        self.text
    }

    /// Get the current byte position.
    pub fn position(&self) -> usize {
        self.pos
    }

    /// Get the current location (line, column).
    pub fn location(&self) -> Location {
        self.loc
    }

    /// Check if at end of input.
    pub fn is_eof(&self) -> bool {
        self.pos >= self.text.len()
    }

    /// Get the remaining source text from current position.
    pub fn remaining(&self) -> &'a str {
        &self.text[self.pos..]
    }

    /// Peek at the next character without consuming it.
    pub fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    /// Peek at the nth character ahead (0 = current).
    pub fn peek_nth(&self, n: usize) -> Option<char> {
        self.remaining().chars().nth(n)
    }

    /// Check if the remaining text starts with the given string.
    pub fn starts_with(&self, s: &str) -> bool {
        self.remaining().starts_with(s)
    }

    /// Consume and return the next character.
    pub fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        self.loc.advance(c);
        Some(c)
    }

    /// Consume characters while the predicate holds.
    /// Returns the starting position.
    pub fn skip_while<F>(&mut self, pred: F) -> usize
    where
        F: Fn(char) -> bool,
    {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if pred(c) {
                self.advance();
            } else {
                break;
            }
        }
        start
    }

    /// Consume characters while the predicate holds, return the slice.
    pub fn take_while<F>(&mut self, pred: F) -> &'a str
    where
        F: Fn(char) -> bool,
    {
        let start = self.pos;
        self.skip_while(pred);
        &self.text[start..self.pos]
    }

    /// Try to consume a specific string. Returns true if successful.
    pub fn consume(&mut self, s: &str) -> bool {
        if self.starts_with(s) {
            for _ in s.chars() {
                self.advance();
            }
            true
        } else {
            false
        }
    }

    /// Get a slice of the source text.
    pub fn slice(&self, start: usize, end: usize) -> &'a str {
        &self.text[start..end]
    }

    /// Create a span from a saved position to the current position.
    pub fn span_from(&self, start: usize, start_loc: Location) -> Span {
        Span::new(start, self.pos, start_loc, self.loc)
    }

    /// Save the current position for later span creation.
    pub fn save(&self) -> (usize, Location) {
        (self.pos, self.loc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_new() {
        let source = Source::new("hello");
        assert_eq!(source.text(), "hello");
        assert_eq!(source.position(), 0);
        assert!(!source.is_eof());
    }

    #[test]
    fn test_peek() {
        let source = Source::new("abc");
        assert_eq!(source.peek(), Some('a'));
        assert_eq!(source.peek_nth(0), Some('a'));
        assert_eq!(source.peek_nth(1), Some('b'));
        assert_eq!(source.peek_nth(2), Some('c'));
        assert_eq!(source.peek_nth(3), None);
    }

    #[test]
    fn test_advance() {
        let mut source = Source::new("ab");
        assert_eq!(source.advance(), Some('a'));
        assert_eq!(source.position(), 1);
        assert_eq!(source.advance(), Some('b'));
        assert_eq!(source.position(), 2);
        assert!(source.is_eof());
        assert_eq!(source.advance(), None);
    }

    #[test]
    fn test_unicode() {
        let mut source = Source::new("日本語");
        assert_eq!(source.peek(), Some('日'));
        assert_eq!(source.advance(), Some('日'));
        assert_eq!(source.position(), 3); // UTF-8 encoding of '日' is 3 bytes
        assert_eq!(source.location().column, 2);
    }

    #[test]
    fn test_newlines() {
        let mut source = Source::new("a\nb");
        source.advance(); // 'a'
        assert_eq!(source.location().line, 1);
        source.advance(); // '\n'
        assert_eq!(source.location().line, 2);
        assert_eq!(source.location().column, 1);
    }

    #[test]
    fn test_take_while() {
        let mut source = Source::new("abc123");
        let letters = source.take_while(|c| c.is_alphabetic());
        assert_eq!(letters, "abc");
        assert_eq!(source.peek(), Some('1'));
    }

    #[test]
    fn test_consume() {
        let mut source = Source::new("==>");
        assert!(source.consume("=="));
        assert_eq!(source.peek(), Some('>'));
        assert!(!source.consume("!="));
    }

    #[test]
    fn test_starts_with() {
        let source = Source::new("hello world");
        assert!(source.starts_with("hello"));
        assert!(!source.starts_with("world"));
    }

    #[test]
    fn test_span_from() {
        let mut source = Source::new("hello");
        let (start, start_loc) = source.save();
        source.advance();
        source.advance();
        let span = source.span_from(start, start_loc);
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 2);
        assert_eq!(source.slice(span.start, span.end), "he");
    }
}
