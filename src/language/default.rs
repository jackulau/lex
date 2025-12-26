//! Default C-like language specification.

use super::{CommentStyles, LanguageSpec, OperatorDef, StringConfig};
use crate::token::TokenKind;

/// Default C-like language specification.
///
/// This provides a reasonable default for languages with C-like syntax:
/// - Keywords: if, else, while, for, fn, let, return, true, false, etc.
/// - Operators: arithmetic, comparison, logical, bitwise
/// - Comments: // and /* */
/// - Strings: double-quoted with escape sequences
pub struct DefaultLanguage;

impl LanguageSpec for DefaultLanguage {
    fn keywords(&self) -> &[&'static str] {
        &[
            "if", "else", "while", "for", "loop", "fn", "let", "const", "mut", "return", "break",
            "continue", "true", "false", "null", "nil", "struct", "enum", "impl", "trait", "type",
            "pub", "mod", "use", "as", "in", "match", "self", "Self",
        ]
    }

    fn operators(&self) -> &[OperatorDef] {
        // Ordered by length (longest first) for maximal munch
        static OPERATORS: &[OperatorDef] = &[
            // Three-character operators
            OperatorDef::new("<<=", TokenKind::LtLt), // Could add compound assignment
            OperatorDef::new(">>=", TokenKind::GtGt),
            // Two-character operators
            OperatorDef::new("==", TokenKind::EqEq),
            OperatorDef::new("!=", TokenKind::BangEq),
            OperatorDef::new("<=", TokenKind::LtEq),
            OperatorDef::new(">=", TokenKind::GtEq),
            OperatorDef::new("&&", TokenKind::AmpAmp),
            OperatorDef::new("||", TokenKind::PipePipe),
            OperatorDef::new("<<", TokenKind::LtLt),
            OperatorDef::new(">>", TokenKind::GtGt),
            OperatorDef::new("::", TokenKind::ColonColon),
            OperatorDef::new("->", TokenKind::Arrow),
            OperatorDef::new("=>", TokenKind::FatArrow),
            // Single-character operators
            OperatorDef::new("+", TokenKind::Plus),
            OperatorDef::new("-", TokenKind::Minus),
            OperatorDef::new("*", TokenKind::Star),
            OperatorDef::new("/", TokenKind::Slash),
            OperatorDef::new("%", TokenKind::Percent),
            OperatorDef::new("=", TokenKind::Eq),
            OperatorDef::new("<", TokenKind::Lt),
            OperatorDef::new(">", TokenKind::Gt),
            OperatorDef::new("!", TokenKind::Bang),
            OperatorDef::new("&", TokenKind::Amp),
            OperatorDef::new("|", TokenKind::Pipe),
            OperatorDef::new("^", TokenKind::Caret),
            OperatorDef::new("~", TokenKind::Tilde),
        ];
        OPERATORS
    }

    fn comment_styles(&self) -> &CommentStyles {
        static STYLES: CommentStyles = CommentStyles {
            single_line: Some("//"),
            multi_line: Some(("/*", "*/")),
            nested: false,
        };
        &STYLES
    }

    fn string_config(&self) -> &StringConfig {
        static CONFIG: StringConfig = StringConfig {
            delimiters: &['"'],
            multiline: false,
            raw_prefix: Some("r"),
        };
        &CONFIG
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let lang = DefaultLanguage;
        assert!(lang.lookup_keyword("if").is_some());
        assert!(lang.lookup_keyword("else").is_some());
        assert!(lang.lookup_keyword("fn").is_some());
        assert!(lang.lookup_keyword("let").is_some());
        assert!(lang.lookup_keyword("return").is_some());
        assert!(lang.lookup_keyword("true").is_some());
        assert!(lang.lookup_keyword("false").is_some());
    }

    #[test]
    fn test_operators_ordered_by_length() {
        let lang = DefaultLanguage;
        let ops = lang.operators();

        // Verify that longer operators come before shorter ones
        // This is important for maximal munch
        let eq_eq_pos = ops.iter().position(|op| op.text == "==").unwrap();
        let eq_pos = ops.iter().position(|op| op.text == "=").unwrap();
        assert!(eq_eq_pos < eq_pos, "== should come before = for maximal munch");

        let lt_eq_pos = ops.iter().position(|op| op.text == "<=").unwrap();
        let lt_pos = ops.iter().position(|op| op.text == "<").unwrap();
        assert!(lt_eq_pos < lt_pos, "<= should come before < for maximal munch");
    }

    #[test]
    fn test_comment_styles() {
        let lang = DefaultLanguage;
        let styles = lang.comment_styles();
        assert_eq!(styles.single_line, Some("//"));
        assert_eq!(styles.multi_line, Some(("/*", "*/")));
        assert!(!styles.nested);
    }

    #[test]
    fn test_string_config() {
        let lang = DefaultLanguage;
        let config = lang.string_config();
        assert!(config.delimiters.contains(&'"'));
        assert!(!config.multiline);
    }
}
