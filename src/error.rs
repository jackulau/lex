//! Lexer error types.
//!
//! This module defines error types for lexical analysis failures.

use crate::span::{Location, Span};
use core::fmt;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::string::String;

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

#[cfg(feature = "std")]
impl std::error::Error for LexError {}

/// Format an error with source snippet for display.
///
/// This creates a rustc-style error message showing the actual line of code
/// where the error occurred, with a caret pointing to the error location.
///
/// # Example output
/// ```text
/// error: unexpected character '@'
///   --> 1:5
///    |
///  1 | let @x = 42;
///    |     ^
/// ```
///
/// This function requires the `alloc` feature.
#[cfg(feature = "alloc")]
pub fn format_error_with_source(error: &LexError, source: &str) -> String {
    use alloc::format;
    use alloc::string::ToString;
    let mut output = String::new();
    let loc = error.span.start_loc;
    let line_num = loc.line as usize;
    let col = loc.column as usize;

    // Error message
    output.push_str(&format!("error: {}\n", error.kind));

    // Location indicator
    output.push_str(&format!("  --> {}:{}\n", loc.line, loc.column));

    // Get the source line
    if let Some(source_line) = source.lines().nth(line_num - 1) {
        // Calculate gutter width based on line number
        let gutter_width = line_num.to_string().len().max(1);

        // Empty gutter line
        output.push_str(&format!("{:>width$} |\n", "", width = gutter_width));

        // Source line with line number
        output.push_str(&format!(
            "{:>width$} | {}\n",
            line_num,
            source_line,
            width = gutter_width
        ));

        // Underline/caret line
        // Calculate visual column position accounting for tabs
        let visual_col = calculate_visual_column(source_line, col);
        let underline_len = calculate_underline_length(error, source);

        output.push_str(&format!("{:>width$} |", "", width = gutter_width));
        output.push_str(&format!(
            " {:>indent$}{}\n",
            "",
            "^".repeat(underline_len.max(1)),
            indent = visual_col - 1
        ));
    }

    output
}

/// Calculate the visual column position accounting for tabs.
#[cfg(feature = "alloc")]
fn calculate_visual_column(line: &str, column: usize) -> usize {
    let mut visual_col = 1;
    for (i, c) in line.chars().enumerate() {
        if i + 1 >= column {
            break;
        }
        if c == '\t' {
            // Tab to next 4-column boundary
            visual_col = ((visual_col - 1) / 4 + 1) * 4 + 1;
        } else {
            visual_col += 1;
        }
    }
    visual_col
}

/// Calculate the underline length based on error span.
#[cfg(feature = "alloc")]
fn calculate_underline_length(error: &LexError, source: &str) -> usize {
    // For single-line errors, use the span length
    if error.span.start_loc.line == error.span.end_loc.line {
        let span_len = error.span.end - error.span.start;
        if span_len > 0 {
            // Count chars, not bytes
            let slice = &source[error.span.start..error.span.end.min(source.len())];
            return slice.chars().count();
        }
    }
    // Default to single caret for multi-line or point spans
    1
}

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

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::format;

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

    #[test]
    fn test_format_error_with_source_basic() {
        let source = "let @x = 42;";
        let span = Span::new(4, 5, Location::at(1, 5), Location::at(1, 6));
        let error = LexError::unexpected_char('@', span);

        let formatted = format_error_with_source(&error, source);

        assert!(formatted.contains("error: unexpected character '@'"));
        assert!(formatted.contains("--> 1:5"));
        assert!(formatted.contains("let @x = 42;"));
        assert!(formatted.contains("^"));
    }

    #[test]
    fn test_format_error_with_source_multiline() {
        let source = "let x = 1;\nlet @y = 2;";
        let span = Span::new(15, 16, Location::at(2, 5), Location::at(2, 6));
        let error = LexError::unexpected_char('@', span);

        let formatted = format_error_with_source(&error, source);

        assert!(formatted.contains("--> 2:5"));
        assert!(formatted.contains("let @y = 2;"));
    }

    #[test]
    fn test_format_error_with_source_underline_length() {
        let source = r#""unterminated"#;
        let span = Span::new(0, 13, Location::at(1, 1), Location::at(1, 14));
        let error = LexError::unterminated_string(span);

        let formatted = format_error_with_source(&error, source);

        // Should underline the entire unterminated string (13 chars)
        assert!(formatted.contains("^^^^^^^^^^^^^"));
    }

    #[test]
    fn test_calculate_visual_column_with_tabs() {
        // Tab at start moves to column 5
        let line = "\tfoo";
        assert_eq!(calculate_visual_column(line, 2), 5);

        // Regular chars
        let line2 = "abc";
        assert_eq!(calculate_visual_column(line2, 3), 3);
    }
}
