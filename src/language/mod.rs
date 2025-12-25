//! Pluggable language specification system.
//!
//! This module defines the `LanguageSpec` trait that allows the lexer
//! to be configured for different programming languages.

mod builder;
mod default;

pub use builder::LanguageBuilder;
pub use default::DefaultLanguage;

use crate::token::TokenKind;

/// Trait for defining language-specific lexical rules.
///
/// Implement this trait to configure the lexer for a specific programming
/// language. The default implementation provides a C-like language.
pub trait LanguageSpec {
    /// Get the keyword table for this language.
    fn keywords(&self) -> &[&'static str];

    /// Check if a string is a keyword, return its ID.
    fn lookup_keyword(&self, name: &str) -> Option<u16> {
        self.keywords()
            .iter()
            .position(|&kw| kw == name)
            .map(|i| i as u16)
    }

    /// Get the keyword name for a keyword ID.
    fn keyword_name(&self, id: u16) -> Option<&'static str> {
        self.keywords().get(id as usize).copied()
    }

    /// Get operator definitions (ordered by length, longest first).
    fn operators(&self) -> &[OperatorDef];

    /// Get comment style configuration.
    fn comment_styles(&self) -> &CommentStyles;

    /// Get string literal configuration.
    fn string_config(&self) -> &StringConfig;

    /// Should preserve whitespace tokens?
    fn preserve_whitespace(&self) -> bool {
        false
    }

    /// Should preserve comment tokens?
    fn preserve_comments(&self) -> bool {
        false
    }
}

/// Operator definition for maximal munch ordering.
#[derive(Debug, Clone)]
pub struct OperatorDef {
    /// The operator text (e.g., "==", "!=").
    pub text: &'static str,
    /// The token kind to produce.
    pub kind: TokenKind,
}

impl OperatorDef {
    /// Create a new operator definition.
    pub const fn new(text: &'static str, kind: TokenKind) -> Self {
        Self { text, kind }
    }
}

/// Comment style configuration.
#[derive(Debug, Clone)]
pub struct CommentStyles {
    /// Single-line comment prefix (e.g., "//").
    pub single_line: Option<&'static str>,
    /// Multi-line comment delimiters (e.g., ("/*", "*/")).
    pub multi_line: Option<(&'static str, &'static str)>,
    /// Allow nested multi-line comments.
    pub nested: bool,
}

impl CommentStyles {
    /// Create a new comment style configuration.
    pub const fn new() -> Self {
        Self {
            single_line: None,
            multi_line: None,
            nested: false,
        }
    }

    /// Set single-line comment prefix.
    pub const fn with_single_line(mut self, prefix: &'static str) -> Self {
        self.single_line = Some(prefix);
        self
    }

    /// Set multi-line comment delimiters.
    pub const fn with_multi_line(mut self, start: &'static str, end: &'static str) -> Self {
        self.multi_line = Some((start, end));
        self
    }

    /// Enable nested multi-line comments.
    pub const fn with_nested(mut self, nested: bool) -> Self {
        self.nested = nested;
        self
    }
}

impl Default for CommentStyles {
    fn default() -> Self {
        Self::new()
    }
}

/// String literal configuration.
#[derive(Debug, Clone)]
pub struct StringConfig {
    /// Allowed string delimiters (e.g., '"', '\'').
    pub delimiters: &'static [char],
    /// Allow multi-line strings.
    pub multiline: bool,
    /// Raw string prefix (e.g., "r" for r"...").
    pub raw_prefix: Option<&'static str>,
}

impl StringConfig {
    /// Create a new string configuration.
    pub const fn new() -> Self {
        Self {
            delimiters: &['"'],
            multiline: false,
            raw_prefix: None,
        }
    }

    /// Set allowed delimiters.
    pub const fn with_delimiters(mut self, delimiters: &'static [char]) -> Self {
        self.delimiters = delimiters;
        self
    }

    /// Enable multi-line strings.
    pub const fn with_multiline(mut self, multiline: bool) -> Self {
        self.multiline = multiline;
        self
    }

    /// Set raw string prefix.
    pub const fn with_raw_prefix(mut self, prefix: &'static str) -> Self {
        self.raw_prefix = Some(prefix);
        self
    }
}

impl Default for StringConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_language_keywords() {
        let lang = DefaultLanguage;
        assert!(lang.lookup_keyword("if").is_some());
        assert!(lang.lookup_keyword("else").is_some());
        assert!(lang.lookup_keyword("fn").is_some());
        assert!(lang.lookup_keyword("notakeyword").is_none());
    }

    #[test]
    fn test_keyword_name_lookup() {
        let lang = DefaultLanguage;
        let id = lang.lookup_keyword("if").unwrap();
        assert_eq!(lang.keyword_name(id), Some("if"));
    }

    #[test]
    fn test_comment_styles() {
        let styles = CommentStyles::new()
            .with_single_line("//")
            .with_multi_line("/*", "*/");
        assert_eq!(styles.single_line, Some("//"));
        assert_eq!(styles.multi_line, Some(("/*", "*/")));
    }
}
