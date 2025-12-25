//! Lexer error types.
//!
//! This module defines error types for lexical analysis failures.

use crate::span::{Location, Span};
use std::fmt;

/// A lexical analysis error.
#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
    /// The kind of error.
    pub kind: LexErrorKind,
    /// Location in source where the error occurred.
    pub span: Span,
}

impl LexError {
    /// Create a new lexer error.
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Create an unexpected character error.
    pub fn unexpected_char(c: char, span: Span) -> Self {
        Self::new(LexErrorKind::UnexpectedChar(c), span)
    }

    /// Create an unterminated string error.
    pub fn unterminated_string(span: Span) -> Self {
        Self::new(LexErrorKind::UnterminatedString, span)
    }

    /// Create an unterminated comment error.
    pub fn unterminated_comment(span: Span) -> Self {
        Self::new(LexErrorKind::UnterminatedComment, span)
    }

    /// Create an unterminated character literal error.
    pub fn unterminated_char(span: Span) -> Self {
        Self::new(LexErrorKind::UnterminatedChar, span)
    }

    /// Create an invalid escape sequence error.
    pub fn invalid_escape(c: char, span: Span) -> Self {
        Self::new(LexErrorKind::InvalidEscape(c), span)
    }

    /// Create an invalid number literal error.
    pub fn invalid_number(msg: String, span: Span) -> Self {
        Self::new(LexErrorKind::InvalidNumber(msg), span)
    }

    /// Create an invalid unicode escape error.
    pub fn invalid_unicode_escape(msg: String, span: Span) -> Self {
        Self::new(LexErrorKind::InvalidUnicodeEscape(msg), span)
    }

    /// Create an empty character literal error.
    pub fn empty_char_literal(span: Span) -> Self {
        Self::new(LexErrorKind::EmptyCharLiteral, span)
    }

    /// Create a multi-character literal error.
    pub fn multi_char_literal(span: Span) -> Self {
        Self::new(LexErrorKind::MultiCharLiteral, span)
    }

    /// Get the starting location of the error.
    pub fn location(&self) -> Location {
        self.span.start_loc
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.kind, self.span.start_loc)
    }
}

impl std::error::Error for LexError {}

/// The kind of lexical error.
#[derive(Debug, Clone, PartialEq)]
pub enum LexErrorKind {
    /// Unexpected character in input.
    UnexpectedChar(char),
    /// Unterminated string literal.
    UnterminatedString,
    /// Unterminated block comment.
    UnterminatedComment,
    /// Unterminated character literal.
    UnterminatedChar,
    /// Newline in string literal (when not allowed).
    NewlineInString,
    /// Invalid escape sequence.
    InvalidEscape(char),
    /// Invalid number literal.
    InvalidNumber(String),
    /// Invalid unicode escape sequence.
    InvalidUnicodeEscape(String),
    /// Empty character literal.
    EmptyCharLiteral,
    /// Multi-character character literal.
    MultiCharLiteral,
    /// Unexpected end of file.
    UnexpectedEof,
}

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexErrorKind::UnexpectedChar(c) => {
                if c.is_ascii_graphic() {
                    write!(f, "unexpected character '{}'", c)
                } else {
                    write!(f, "unexpected character U+{:04X}", *c as u32)
                }
            }
            LexErrorKind::UnterminatedString => write!(f, "unterminated string literal"),
            LexErrorKind::UnterminatedComment => write!(f, "unterminated block comment"),
            LexErrorKind::UnterminatedChar => write!(f, "unterminated character literal"),
            LexErrorKind::NewlineInString => write!(f, "newline in string literal"),
            LexErrorKind::InvalidEscape(c) => write!(f, "invalid escape sequence '\\{}'", c),
            LexErrorKind::InvalidNumber(msg) => write!(f, "invalid number literal: {}", msg),
            LexErrorKind::InvalidUnicodeEscape(msg) => {
                write!(f, "invalid unicode escape: {}", msg)
            }
            LexErrorKind::EmptyCharLiteral => write!(f, "empty character literal"),
            LexErrorKind::MultiCharLiteral => {
                write!(f, "character literal may only contain one codepoint")
            }
            LexErrorKind::UnexpectedEof => write!(f, "unexpected end of file"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let span = Span::new(0, 1, Location::at(1, 1), Location::at(1, 2));
        let error = LexError::unexpected_char('@', span);
        let msg = format!("{}", error);
        assert!(msg.contains("unexpected character '@'"));
        assert!(msg.contains("1:1"));
    }

    #[test]
    fn test_error_kind_display() {
        assert_eq!(
            format!("{}", LexErrorKind::UnterminatedString),
            "unterminated string literal"
        );
        assert_eq!(
            format!("{}", LexErrorKind::InvalidEscape('q')),
            "invalid escape sequence '\\q'"
        );
    }

    #[test]
    fn test_unicode_char_display() {
        // Non-printable character
        let kind = LexErrorKind::UnexpectedChar('\x00');
        let msg = format!("{}", kind);
        assert!(msg.contains("U+0000"));
    }
}
