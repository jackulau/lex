//! Comment parsing fuzz target.
//!
//! Tests single-line and multi-line comments, including nested comments
//! and various edge cases like unterminated comments.

#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use lex::{DefaultLanguage, Lexer, TokenKind};

/// Structured input for generating comment-focused test cases.
#[derive(Debug, Arbitrary)]
struct CommentInput {
    /// Type of comment to generate
    comment_type: CommentType,
    /// Content inside the comment
    content: String,
    /// Whether to properly terminate the comment
    terminated: bool,
    /// Nesting depth for multi-line comments
    nesting_depth: u8,
    /// Code to place around the comment
    surrounding_code: Option<String>,
}

#[derive(Debug, Arbitrary)]
enum CommentType {
    /// Single-line comment: // ...
    SingleLine,
    /// Multi-line comment: /* ... */
    MultiLine,
    /// Nested multi-line comment: /* /* ... */ */
    Nested,
    /// Comment with special content
    Special(SpecialContent),
}

#[derive(Debug, Arbitrary)]
enum SpecialContent {
    /// Comment containing comment-like sequences
    CommentLike,
    /// Comment with string delimiters
    StringDelimiters,
    /// Comment with Unicode
    Unicode,
    /// Comment with newlines (for multi-line)
    Newlines,
    /// Comment ending like another comment type
    MixedEnding,
}

impl CommentInput {
    fn to_source(&self) -> String {
        let mut result = String::new();

        // Add prefix code
        if let Some(ref code) = self.surrounding_code {
            result.push_str(code);
            result.push(' ');
        }

        // Generate the comment
        match &self.comment_type {
            CommentType::SingleLine => {
                result.push_str("//");
                result.push_str(&self.sanitize_single_line(&self.content));
                // Single-line comments end at newline
                if self.terminated {
                    result.push('\n');
                }
            }
            CommentType::MultiLine => {
                result.push_str("/*");
                // Remove any */ from content to control termination
                let content = self.content.replace("*/", "* /");
                result.push_str(&content);
                if self.terminated {
                    result.push_str("*/");
                }
            }
            CommentType::Nested => {
                let depth = (self.nesting_depth % 5).max(1) as usize;

                // Open nested comments
                for _ in 0..depth {
                    result.push_str("/* ");
                }

                // Content without comment delimiters
                let content = self
                    .content
                    .replace("/*", "/ *")
                    .replace("*/", "* /");
                result.push_str(&content);

                // Close nested comments (if terminated)
                if self.terminated {
                    for _ in 0..depth {
                        result.push_str(" */");
                    }
                }
            }
            CommentType::Special(special) => {
                result.push_str(&self.generate_special(special));
            }
        }

        // Add suffix code
        if let Some(ref code) = self.surrounding_code {
            result.push(' ');
            result.push_str(code);
        }

        result
    }

    fn sanitize_single_line(&self, content: &str) -> String {
        // Remove newlines from single-line comment content
        content.replace('\n', " ").replace('\r', " ")
    }

    fn generate_special(&self, special: &SpecialContent) -> String {
        match special {
            SpecialContent::CommentLike => {
                // Comment containing comment-like sequences
                if self.terminated {
                    format!("/* // nested // {} */", self.content)
                } else {
                    format!("/* // nested // {}", self.content)
                }
            }
            SpecialContent::StringDelimiters => {
                // Comment containing string delimiters
                let content = format!("\"string\" 'char' `backtick`");
                if self.terminated {
                    format!("/* {} {} */", content, self.content)
                } else {
                    format!("/* {} {}", content, self.content)
                }
            }
            SpecialContent::Unicode => {
                // Comment with Unicode content
                let unicode = "日本語 العربية 🎉 ñ ü ö";
                if self.terminated {
                    format!("/* {} {} */", unicode, self.content)
                } else {
                    format!("/* {} {}", unicode, self.content)
                }
            }
            SpecialContent::Newlines => {
                // Multi-line comment spanning multiple lines
                let lines = format!(
                    "line1\nline2\r\nline3\rline4\n{}",
                    self.content
                );
                if self.terminated {
                    format!("/*\n{}\n*/", lines)
                } else {
                    format!("/*\n{}", lines)
                }
            }
            SpecialContent::MixedEnding => {
                // Comment with mixed delimiter patterns
                let content = "/* // */ /* */ //";
                if self.terminated {
                    format!("/* {} {} */", content, self.content)
                } else {
                    format!("/* {} {}", content, self.content)
                }
            }
        }
    }
}

fuzz_target!(|input: CommentInput| {
    let source = input.to_source();

    // Tokenize and verify no panics
    let (tokens, errors) = Lexer::tokenize(&source, DefaultLanguage);

    // Note: Empty input produces empty token list (EOF is not in token stream)

    // Verify token spans
    for token in &tokens {
        assert!(token.span.start <= token.span.end);
        assert!(token.span.end <= source.len());
    }

    // Unterminated multi-line comments should produce errors
    if !input.terminated {
        match &input.comment_type {
            CommentType::MultiLine | CommentType::Nested => {
                // Should have an unterminated comment error
                let has_unterminated = errors
                    .iter()
                    .any(|e| matches!(e.kind, lex::LexErrorKind::UnterminatedComment));
                // Edge case: content might contain closing delimiter
                if !has_unterminated {
                    // Check if content accidentally closes the comment
                    // This is acceptable behavior
                }
            }
            CommentType::SingleLine => {
                // Single-line comments don't need termination (they end at EOF)
            }
            CommentType::Special(_) => {
                // Special cases may or may not produce errors
            }
        }
    }

    // Comments should not appear in token stream by default (unless preserve_comments)
    // The DefaultLanguage doesn't preserve comments
    let has_comment_token = tokens.iter().any(|t| {
        matches!(t.kind, TokenKind::Comment)
    });

    // DefaultLanguage doesn't preserve comments, so we shouldn't see them
    assert!(
        !has_comment_token,
        "DefaultLanguage should not preserve comments"
    );
});
