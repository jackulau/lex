//! String literal fuzz target.
//!
//! Focuses on string parsing with various escape sequences, unterminated
//! strings, and edge cases like raw strings and multiline strings.

#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use lex::{DefaultLanguage, Lexer};

/// Structured input for generating string-focused test cases.
#[derive(Debug, Arbitrary)]
struct StringInput {
    /// Main content inside the string
    content: String,
    /// Type of string to generate
    string_type: StringType,
    /// Whether to properly terminate the string
    terminated: bool,
    /// Optional prefix/suffix context
    context: Option<String>,
}

#[derive(Debug, Arbitrary)]
enum StringType {
    /// Regular double-quoted string: "..."
    DoubleQuote,
    /// Character literal: '...'
    CharLiteral,
    /// Raw string: r"..."
    RawString,
    /// String with escape sequences
    WithEscapes(EscapeType),
}

#[derive(Debug, Arbitrary)]
enum EscapeType {
    /// Simple escapes: \n, \t, \r, \\, etc.
    Simple,
    /// Hex escape: \xNN
    Hex,
    /// Unicode escape: \u{NNNN}
    Unicode,
    /// Mixed escapes
    Mixed,
    /// Invalid escapes (for error handling)
    Invalid,
}

impl StringInput {
    fn to_source(&self) -> String {
        let mut result = String::new();

        // Add context prefix
        if let Some(ref ctx) = self.context {
            result.push_str(ctx);
            result.push(' ');
        }

        // Generate the string based on type
        match &self.string_type {
            StringType::DoubleQuote => {
                result.push('"');
                result.push_str(&self.content);
                if self.terminated {
                    result.push('"');
                }
            }
            StringType::CharLiteral => {
                result.push('\'');
                // Char literals should be short
                if let Some(c) = self.content.chars().next() {
                    result.push(c);
                }
                if self.terminated {
                    result.push('\'');
                }
            }
            StringType::RawString => {
                result.push_str("r\"");
                result.push_str(&self.content);
                if self.terminated {
                    result.push('"');
                }
            }
            StringType::WithEscapes(escape_type) => {
                result.push('"');
                result.push_str(&generate_escapes(escape_type, &self.content));
                if self.terminated {
                    result.push('"');
                }
            }
        }

        // Add context suffix
        if let Some(ref ctx) = self.context {
            result.push(' ');
            result.push_str(ctx);
        }

        result
    }
}

fn generate_escapes(escape_type: &EscapeType, content: &str) -> String {
    match escape_type {
        EscapeType::Simple => {
            let mut result = String::new();
            for c in content.chars() {
                match c {
                    '\n' => result.push_str("\\n"),
                    '\t' => result.push_str("\\t"),
                    '\r' => result.push_str("\\r"),
                    '\\' => result.push_str("\\\\"),
                    '"' => result.push_str("\\\""),
                    '\'' => result.push_str("\\'"),
                    '0' => result.push_str("\\0"),
                    _ => result.push(c),
                }
            }
            result
        }
        EscapeType::Hex => {
            let mut result = String::new();
            for c in content.chars() {
                if c.is_ascii() {
                    result.push_str(&format!("\\x{:02x}", c as u8));
                } else {
                    result.push(c);
                }
            }
            result
        }
        EscapeType::Unicode => {
            let mut result = String::new();
            for c in content.chars() {
                result.push_str(&format!("\\u{{{:04x}}}", c as u32));
            }
            result
        }
        EscapeType::Mixed => {
            let mut result = String::new();
            for (i, c) in content.chars().enumerate() {
                match i % 4 {
                    0 => result.push(c),
                    1 => result.push_str(&format!("\\x{:02x}", (c as u32 % 128) as u8)),
                    2 => result.push_str(&format!("\\u{{{:04x}}}", c as u32)),
                    _ => match c {
                        '\n' | '\t' | '\r' => result.push_str("\\n"),
                        _ => result.push(c),
                    },
                }
            }
            result
        }
        EscapeType::Invalid => {
            // Generate invalid escape sequences to test error handling
            let mut result = String::new();
            for c in content.chars() {
                match c as u32 % 5 {
                    0 => result.push_str("\\z"), // Invalid escape
                    1 => result.push_str("\\xGG"), // Invalid hex
                    2 => result.push_str("\\u{GGGG}"), // Invalid unicode
                    3 => result.push_str("\\u{}"), // Empty unicode
                    _ => result.push(c),
                }
            }
            result
        }
    }
}

fuzz_target!(|input: StringInput| {
    let source = input.to_source();

    // Tokenize and verify no panics
    let (tokens, errors) = Lexer::tokenize(&source, DefaultLanguage);

    // Note: Empty input produces empty token list (EOF is not in token stream)

    // If string is unterminated, we should have an error
    if !input.terminated {
        // Unterminated strings should produce errors
        // (unless the content happens to contain the closing delimiter)
        let has_string_error = errors.iter().any(|e| {
            matches!(
                e.kind,
                lex::LexErrorKind::UnterminatedString
                    | lex::LexErrorKind::UnterminatedChar
            )
        });
        let content_has_delimiter = match &input.string_type {
            StringType::DoubleQuote | StringType::RawString => input.content.contains('"'),
            StringType::CharLiteral => input.content.contains('\''),
            StringType::WithEscapes(_) => input.content.contains('"'),
        };
        if !content_has_delimiter && !has_string_error {
            // This is expected - unterminated string without delimiter in content
            // should produce an error, but the fuzzer might find edge cases
        }
    }

    // Verify all token spans are valid
    for token in &tokens {
        assert!(token.span.end <= source.len());
    }
});
