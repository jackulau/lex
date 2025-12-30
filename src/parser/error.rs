//! Parser error types.
//!
//! This module provides comprehensive error types for the parser with
//! source location support and rustc-style error formatting.

use crate::error::LexError;
use crate::span::Span;
use crate::token::TokenKind;
use core::fmt;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{
    format,
    string::{String, ToString},
    vec::Vec,
};

/// A parser error.
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    /// The kind of error.
    pub kind: ParseErrorKind,
    /// The span where the error occurred.
    pub span: Span,
}

impl ParseError {
    /// Create a new parse error.
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Create an unexpected token error.
    pub fn unexpected_token(expected: &str, found: TokenKind, span: Span) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedToken {
                expected: expected.into(),
                found,
            },
            span,
        )
    }

    /// Create an unexpected EOF error.
    pub fn unexpected_eof(expected: &str, span: Span) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedEof {
                expected: expected.into(),
            },
            span,
        )
    }

    /// Create a missing token error.
    pub fn expected_token(expected: TokenKind, found: TokenKind, span: Span) -> Self {
        Self::new(ParseErrorKind::ExpectedToken { expected, found }, span)
    }

    /// Create an invalid expression error.
    pub fn invalid_expression(message: &str, span: Span) -> Self {
        Self::new(
            ParseErrorKind::InvalidExpression(message.into()),
            span,
        )
    }

    /// Create an invalid statement error.
    pub fn invalid_statement(message: &str, span: Span) -> Self {
        Self::new(ParseErrorKind::InvalidStatement(message.into()), span)
    }

    /// Create an invalid pattern error.
    pub fn invalid_pattern(message: &str, span: Span) -> Self {
        Self::new(ParseErrorKind::InvalidPattern(message.into()), span)
    }

    /// Create an invalid type error.
    pub fn invalid_type(message: &str, span: Span) -> Self {
        Self::new(ParseErrorKind::InvalidType(message.into()), span)
    }

    /// Create an invalid item error.
    pub fn invalid_item(message: &str, span: Span) -> Self {
        Self::new(ParseErrorKind::InvalidItem(message.into()), span)
    }

    /// Create an error from a lexer error.
    pub fn from_lex_error(error: LexError) -> Self {
        Self::new(ParseErrorKind::LexError(error.kind.clone()), error.span)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.kind, self.span.start_loc)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ParseError {}

impl From<LexError> for ParseError {
    fn from(error: LexError) -> Self {
        Self::from_lex_error(error)
    }
}

/// The kind of parse error.
#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    /// An unexpected token was encountered.
    UnexpectedToken {
        /// What was expected.
        expected: String,
        /// What was found.
        found: TokenKind,
    },
    /// Unexpected end of file.
    UnexpectedEof {
        /// What was expected.
        expected: String,
    },
    /// Expected a specific token but found something else.
    ExpectedToken {
        /// The expected token kind.
        expected: TokenKind,
        /// What was found.
        found: TokenKind,
    },
    /// Invalid expression.
    InvalidExpression(String),
    /// Invalid statement.
    InvalidStatement(String),
    /// Invalid pattern.
    InvalidPattern(String),
    /// Invalid type.
    InvalidType(String),
    /// Invalid item.
    InvalidItem(String),
    /// A lexer error was encountered.
    LexError(crate::error::LexErrorKind),
    /// Duplicate definition.
    DuplicateDefinition {
        /// The name that was duplicated.
        name: String,
    },
    /// Missing required item.
    MissingRequired(String),
    /// Invalid visibility modifier.
    InvalidVisibility(String),
    /// Generic error with message.
    Custom(String),
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::UnexpectedToken { expected, found } => {
                write!(f, "expected {}, found {}", expected, found)
            }
            ParseErrorKind::UnexpectedEof { expected } => {
                write!(f, "unexpected end of file, expected {}", expected)
            }
            ParseErrorKind::ExpectedToken { expected, found } => {
                write!(f, "expected '{}', found '{}'", expected, found)
            }
            ParseErrorKind::InvalidExpression(msg) => {
                write!(f, "invalid expression: {}", msg)
            }
            ParseErrorKind::InvalidStatement(msg) => {
                write!(f, "invalid statement: {}", msg)
            }
            ParseErrorKind::InvalidPattern(msg) => {
                write!(f, "invalid pattern: {}", msg)
            }
            ParseErrorKind::InvalidType(msg) => {
                write!(f, "invalid type: {}", msg)
            }
            ParseErrorKind::InvalidItem(msg) => {
                write!(f, "invalid item: {}", msg)
            }
            ParseErrorKind::LexError(kind) => {
                write!(f, "lexer error: {}", kind)
            }
            ParseErrorKind::DuplicateDefinition { name } => {
                write!(f, "duplicate definition of '{}'", name)
            }
            ParseErrorKind::MissingRequired(what) => {
                write!(f, "missing required {}", what)
            }
            ParseErrorKind::InvalidVisibility(msg) => {
                write!(f, "invalid visibility: {}", msg)
            }
            ParseErrorKind::Custom(msg) => {
                write!(f, "{}", msg)
            }
        }
    }
}

/// Format a parse error with source context for display.
///
/// This creates a rustc-style error message.
#[cfg(feature = "alloc")]
pub fn format_parse_error(error: &ParseError, source: &str) -> String {
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
            visual_col = ((visual_col - 1) / 4 + 1) * 4 + 1;
        } else {
            visual_col += 1;
        }
    }
    visual_col
}

/// Calculate the underline length based on error span.
#[cfg(feature = "alloc")]
fn calculate_underline_length(error: &ParseError, source: &str) -> usize {
    if error.span.start_loc.line == error.span.end_loc.line {
        let span_len = error.span.end.saturating_sub(error.span.start);
        if span_len > 0 && error.span.end <= source.len() {
            let slice = &source[error.span.start..error.span.end];
            return slice.chars().count();
        }
    }
    1
}

/// The result type for parser operations.
pub type ParseResult<T> = Result<T, ParseError>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Location;

    fn dummy_span() -> Span {
        Span::new(0, 5, Location::at(1, 1), Location::at(1, 6))
    }

    #[test]
    fn test_unexpected_token_error() {
        let error = ParseError::unexpected_token("expression", TokenKind::Semicolon, dummy_span());
        let msg = format!("{}", error);
        assert!(msg.contains("expected expression"));
        assert!(msg.contains(";"));
    }

    #[test]
    fn test_unexpected_eof_error() {
        let error = ParseError::unexpected_eof("}", dummy_span());
        let msg = format!("{}", error);
        assert!(msg.contains("unexpected end of file"));
        assert!(msg.contains("}"));
    }

    #[test]
    fn test_format_parse_error() {
        let source = "let x = ;";
        let span = Span::new(8, 9, Location::at(1, 9), Location::at(1, 10));
        let error = ParseError::unexpected_token("expression", TokenKind::Semicolon, span);

        let formatted = format_parse_error(&error, source);
        assert!(formatted.contains("error:"));
        assert!(formatted.contains("let x = ;"));
        assert!(formatted.contains("^"));
    }

    #[test]
    fn test_from_lex_error() {
        let lex_error = LexError::unexpected_char('@', dummy_span());
        let parse_error = ParseError::from(lex_error);

        if let ParseErrorKind::LexError(_) = parse_error.kind {
            // ok
        } else {
            panic!("Expected LexError variant");
        }
    }
}
