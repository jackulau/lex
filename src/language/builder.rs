//! Fluent builder for creating custom language specifications.

use super::{CommentStyles, LanguageSpec, OperatorDef, StringConfig};
use crate::token::TokenKind;

/// Builder for creating custom language specifications.
///
/// # Example
///
/// ```
/// use lex::language::{LanguageBuilder, LanguageSpec};
/// use lex::token::TokenKind;
///
/// let my_lang = LanguageBuilder::new()
///     .keywords(&["if", "else", "func", "var"])
///     .operator("==", TokenKind::EqEq)
///     .operator("!=", TokenKind::BangEq)
///     .single_line_comment("#")
///     .build();
///
/// assert!(my_lang.lookup_keyword("func").is_some());
/// assert!(my_lang.lookup_keyword("fn").is_none()); // Not in our language
/// ```
pub struct LanguageBuilder {
    keywords: Vec<&'static str>,
    operators: Vec<OperatorDef>,
    comment_styles: CommentStyles,
    string_config: StringConfig,
    preserve_whitespace: bool,
    preserve_comments: bool,
}

impl LanguageBuilder {
    /// Create a new language builder with default settings.
    pub fn new() -> Self {
        Self {
            keywords: Vec::new(),
            operators: Vec::new(),
            comment_styles: CommentStyles::new(),
            string_config: StringConfig::new(),
            preserve_whitespace: false,
            preserve_comments: false,
        }
    }

    /// Add keywords to the language.
    pub fn keywords(mut self, kws: &[&'static str]) -> Self {
        self.keywords.extend_from_slice(kws);
        self
    }

    /// Add a single keyword.
    pub fn keyword(mut self, kw: &'static str) -> Self {
        self.keywords.push(kw);
        self
    }

    /// Add an operator.
    pub fn operator(mut self, text: &'static str, kind: TokenKind) -> Self {
        self.operators.push(OperatorDef::new(text, kind));
        self
    }

    /// Set single-line comment prefix.
    pub fn single_line_comment(mut self, prefix: &'static str) -> Self {
        self.comment_styles.single_line = Some(prefix);
        self
    }

    /// Set multi-line comment delimiters.
    pub fn multi_line_comment(mut self, start: &'static str, end: &'static str) -> Self {
        self.comment_styles.multi_line = Some((start, end));
        self
    }

    /// Enable nested multi-line comments.
    pub fn nested_comments(mut self, nested: bool) -> Self {
        self.comment_styles.nested = nested;
        self
    }

    /// Set string delimiters.
    pub fn string_delimiters(mut self, delimiters: &'static [char]) -> Self {
        self.string_config.delimiters = delimiters;
        self
    }

    /// Enable multi-line strings.
    pub fn multiline_strings(mut self, enabled: bool) -> Self {
        self.string_config.multiline = enabled;
        self
    }

    /// Set raw string prefix.
    pub fn raw_string_prefix(mut self, prefix: &'static str) -> Self {
        self.string_config.raw_prefix = Some(prefix);
        self
    }

    /// Preserve whitespace tokens.
    pub fn preserve_whitespace(mut self, preserve: bool) -> Self {
        self.preserve_whitespace = preserve;
        self
    }

    /// Preserve comment tokens.
    pub fn preserve_comments(mut self, preserve: bool) -> Self {
        self.preserve_comments = preserve;
        self
    }

    /// Build the language specification.
    pub fn build(mut self) -> CustomLanguage {
        // Sort operators by length (longest first) for maximal munch
        self.operators.sort_by(|a, b| b.text.len().cmp(&a.text.len()));

        CustomLanguage {
            keywords: self.keywords,
            operators: self.operators,
            comment_styles: self.comment_styles,
            string_config: self.string_config,
            preserve_whitespace: self.preserve_whitespace,
            preserve_comments: self.preserve_comments,
        }
    }
}

impl Default for LanguageBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// A custom language specification built by `LanguageBuilder`.
pub struct CustomLanguage {
    keywords: Vec<&'static str>,
    operators: Vec<OperatorDef>,
    comment_styles: CommentStyles,
    string_config: StringConfig,
    preserve_whitespace: bool,
    preserve_comments: bool,
}

impl LanguageSpec for CustomLanguage {
    fn keywords(&self) -> &[&'static str] {
        &self.keywords
    }

    fn operators(&self) -> &[OperatorDef] {
        &self.operators
    }

    fn comment_styles(&self) -> &CommentStyles {
        &self.comment_styles
    }

    fn string_config(&self) -> &StringConfig {
        &self.string_config
    }

    fn preserve_whitespace(&self) -> bool {
        self.preserve_whitespace
    }

    fn preserve_comments(&self) -> bool {
        self.preserve_comments
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builder_keywords() {
        let lang = LanguageBuilder::new()
            .keywords(&["if", "else"])
            .keyword("while")
            .build();

        assert!(lang.lookup_keyword("if").is_some());
        assert!(lang.lookup_keyword("else").is_some());
        assert!(lang.lookup_keyword("while").is_some());
        assert!(lang.lookup_keyword("for").is_none());
    }

    #[test]
    fn test_builder_operators() {
        let lang = LanguageBuilder::new()
            .operator("==", TokenKind::EqEq)
            .operator("=", TokenKind::Eq)
            .build();

        let ops = lang.operators();
        // Should be sorted by length, longest first
        assert_eq!(ops[0].text, "==");
        assert_eq!(ops[1].text, "=");
    }

    #[test]
    fn test_builder_comments() {
        let lang = LanguageBuilder::new()
            .single_line_comment("#")
            .multi_line_comment("(*", "*)")
            .nested_comments(true)
            .build();

        let styles = lang.comment_styles();
        assert_eq!(styles.single_line, Some("#"));
        assert_eq!(styles.multi_line, Some(("(*", "*)")));
        assert!(styles.nested);
    }

    #[test]
    fn test_builder_strings() {
        let lang = LanguageBuilder::new()
            .string_delimiters(&['"', '\'', '`'])
            .multiline_strings(true)
            .raw_string_prefix("r")
            .build();

        let config = lang.string_config();
        assert_eq!(config.delimiters, &['"', '\'', '`']);
        assert!(config.multiline);
        assert_eq!(config.raw_prefix, Some("r"));
    }

    #[test]
    fn test_builder_preserve_options() {
        let lang = LanguageBuilder::new()
            .preserve_whitespace(true)
            .preserve_comments(true)
            .build();

        assert!(lang.preserve_whitespace());
        assert!(lang.preserve_comments());
    }
}
