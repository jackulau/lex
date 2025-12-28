//! WebAssembly bindings for browser-based tools.
//!
//! This module provides WASM-compatible exports for using the lexer
//! in web browsers and other JavaScript environments.
//!
//! # Usage from JavaScript
//!
//! ```javascript
//! import init, { tokenize, tokenizeWithConfig, LexerConfig } from './lex.js';
//!
//! await init();
//!
//! // Tokenize with default language (C-like)
//! const result = tokenize('let x = 42;');
//! console.log(result.tokens);
//! console.log(result.errors);
//!
//! // Tokenize with custom configuration
//! const config = new LexerConfig();
//! config.addKeyword('func');
//! config.addKeyword('var');
//! config.setSingleLineComment('#');
//! const customResult = tokenizeWithConfig('func main() {}', config);
//! ```

use alloc::string::String;
use alloc::vec::Vec;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

use crate::error::{LexError, LexErrorKind};
use crate::language::{CommentStyles, DefaultLanguage, LanguageSpec, OperatorDef, StringConfig};
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

/// A JSON-serializable token for JavaScript consumption.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JsToken {
    /// The type of token (e.g., "Ident", "IntLiteral", "Plus").
    pub kind: String,
    /// The actual text of the token from source.
    pub lexeme: String,
    /// Start byte offset (inclusive).
    pub start: usize,
    /// End byte offset (exclusive).
    pub end: usize,
    /// Start line (1-indexed).
    pub start_line: u32,
    /// Start column (1-indexed).
    pub start_column: u32,
    /// End line (1-indexed).
    pub end_line: u32,
    /// End column (1-indexed).
    pub end_column: u32,
    /// For keyword tokens, the keyword text; for bool literals, the value.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,
}

impl JsToken {
    fn from_token(token: &Token, source: &str, language: &impl LanguageSpec) -> Self {
        let lexeme = if token.span.start < source.len() && token.span.end <= source.len() {
            source[token.span.start..token.span.end].to_string()
        } else {
            String::new()
        };

        let (kind, value) = token_kind_to_string(&token.kind, language);

        JsToken {
            kind,
            lexeme,
            start: token.span.start,
            end: token.span.end,
            start_line: token.span.start_loc.line,
            start_column: token.span.start_loc.column,
            end_line: token.span.end_loc.line,
            end_column: token.span.end_loc.column,
            value,
        }
    }
}

fn token_kind_to_string(kind: &TokenKind, language: &impl LanguageSpec) -> (String, Option<String>) {
    use alloc::format;

    match kind {
        TokenKind::Keyword(id) => {
            let keyword_name = language.keyword_name(id.id()).unwrap_or("unknown");
            ("Keyword".into(), Some(keyword_name.into()))
        }
        TokenKind::Ident => ("Ident".into(), None),
        TokenKind::IntLiteral => ("IntLiteral".into(), None),
        TokenKind::FloatLiteral => ("FloatLiteral".into(), None),
        TokenKind::StringLiteral => ("StringLiteral".into(), None),
        TokenKind::RawStringLiteral => ("RawStringLiteral".into(), None),
        TokenKind::CharLiteral => ("CharLiteral".into(), None),
        TokenKind::BoolLiteral(b) => ("BoolLiteral".into(), Some(format!("{}", b))),
        TokenKind::Plus => ("Plus".into(), None),
        TokenKind::Minus => ("Minus".into(), None),
        TokenKind::Star => ("Star".into(), None),
        TokenKind::Slash => ("Slash".into(), None),
        TokenKind::Percent => ("Percent".into(), None),
        TokenKind::Eq => ("Eq".into(), None),
        TokenKind::EqEq => ("EqEq".into(), None),
        TokenKind::BangEq => ("BangEq".into(), None),
        TokenKind::Lt => ("Lt".into(), None),
        TokenKind::Gt => ("Gt".into(), None),
        TokenKind::LtEq => ("LtEq".into(), None),
        TokenKind::GtEq => ("GtEq".into(), None),
        TokenKind::Bang => ("Bang".into(), None),
        TokenKind::AmpAmp => ("AmpAmp".into(), None),
        TokenKind::PipePipe => ("PipePipe".into(), None),
        TokenKind::Amp => ("Amp".into(), None),
        TokenKind::Pipe => ("Pipe".into(), None),
        TokenKind::Caret => ("Caret".into(), None),
        TokenKind::Tilde => ("Tilde".into(), None),
        TokenKind::LtLt => ("LtLt".into(), None),
        TokenKind::GtGt => ("GtGt".into(), None),
        TokenKind::LParen => ("LParen".into(), None),
        TokenKind::RParen => ("RParen".into(), None),
        TokenKind::LBrace => ("LBrace".into(), None),
        TokenKind::RBrace => ("RBrace".into(), None),
        TokenKind::LBracket => ("LBracket".into(), None),
        TokenKind::RBracket => ("RBracket".into(), None),
        TokenKind::Semicolon => ("Semicolon".into(), None),
        TokenKind::Comma => ("Comma".into(), None),
        TokenKind::Colon => ("Colon".into(), None),
        TokenKind::ColonColon => ("ColonColon".into(), None),
        TokenKind::Dot => ("Dot".into(), None),
        TokenKind::Arrow => ("Arrow".into(), None),
        TokenKind::FatArrow => ("FatArrow".into(), None),
        TokenKind::Comment => ("Comment".into(), None),
        TokenKind::Whitespace => ("Whitespace".into(), None),
        TokenKind::Eof => ("Eof".into(), None),
        TokenKind::Error => ("Error".into(), None),
    }
}

/// A JSON-serializable lexer error for JavaScript consumption.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JsLexError {
    /// The error message.
    pub message: String,
    /// Error kind identifier.
    pub kind: String,
    /// Start byte offset.
    pub start: usize,
    /// End byte offset.
    pub end: usize,
    /// Line number (1-indexed).
    pub line: u32,
    /// Column number (1-indexed).
    pub column: u32,
}

impl JsLexError {
    fn from_error(error: &LexError) -> Self {
        use alloc::format;

        let (kind, message) = match &error.kind {
            LexErrorKind::UnexpectedChar(c) => {
                ("UnexpectedChar".into(), format!("Unexpected character '{}'", c))
            }
            LexErrorKind::UnterminatedString => {
                ("UnterminatedString".into(), "Unterminated string literal".into())
            }
            LexErrorKind::UnterminatedComment => {
                ("UnterminatedComment".into(), "Unterminated block comment".into())
            }
            LexErrorKind::UnterminatedChar => {
                ("UnterminatedChar".into(), "Unterminated character literal".into())
            }
            LexErrorKind::NewlineInString => {
                ("NewlineInString".into(), "Newline in string literal".into())
            }
            LexErrorKind::InvalidEscape(c) => {
                ("InvalidEscape".into(), format!("Invalid escape sequence '\\{}'", c))
            }
            LexErrorKind::InvalidNumber(msg) => {
                ("InvalidNumber".into(), format!("Invalid number: {}", msg))
            }
            LexErrorKind::InvalidUnicodeEscape(msg) => {
                ("InvalidUnicodeEscape".into(), format!("Invalid unicode escape: {}", msg))
            }
            LexErrorKind::EmptyCharLiteral => {
                ("EmptyCharLiteral".into(), "Empty character literal".into())
            }
            LexErrorKind::MultiCharLiteral => {
                ("MultiCharLiteral".into(), "Character literal may only contain one codepoint".into())
            }
            LexErrorKind::UnexpectedEof => {
                ("UnexpectedEof".into(), "Unexpected end of file".into())
            }
        };

        JsLexError {
            message,
            kind,
            start: error.span.start,
            end: error.span.end,
            line: error.span.start_loc.line,
            column: error.span.start_loc.column,
        }
    }
}

/// The result of tokenizing source code.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenizeResult {
    /// The tokens produced.
    pub tokens: Vec<JsToken>,
    /// Any errors encountered.
    pub errors: Vec<JsLexError>,
    /// Whether tokenization completed without errors.
    pub success: bool,
}

/// Tokenize source code using the default C-like language.
///
/// Returns a JSON string containing the tokens and any errors.
#[wasm_bindgen]
pub fn tokenize(source: &str) -> String {
    let language = DefaultLanguage;
    let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);

    let js_tokens: Vec<JsToken> = tokens
        .iter()
        .map(|t| JsToken::from_token(t, source, &language))
        .collect();

    let js_errors: Vec<JsLexError> = errors.iter().map(JsLexError::from_error).collect();

    let result = TokenizeResult {
        success: js_errors.is_empty(),
        tokens: js_tokens,
        errors: js_errors,
    };

    serde_json::to_string(&result).unwrap_or_else(|_| r#"{"error":"serialization failed"}"#.into())
}

/// Configuration for the lexer, allowing custom language definitions.
#[wasm_bindgen]
pub struct LexerConfig {
    keywords: Vec<&'static str>,
    single_line_comment: Option<&'static str>,
    multi_line_comment: Option<(&'static str, &'static str)>,
    nested_comments: bool,
    preserve_whitespace: bool,
    preserve_comments: bool,
}

#[wasm_bindgen]
impl LexerConfig {
    /// Create a new lexer configuration with defaults.
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            keywords: Vec::new(),
            single_line_comment: None,
            multi_line_comment: None,
            nested_comments: false,
            preserve_whitespace: false,
            preserve_comments: false,
        }
    }

    /// Add a keyword to the language.
    /// Note: Keywords must be static strings. For JavaScript interop,
    /// we provide a set of common keywords.
    #[wasm_bindgen(js_name = addKeyword)]
    pub fn add_keyword(&mut self, keyword: &str) {
        // We need to leak the string to get a 'static lifetime.
        // This is acceptable for WASM as configurations are typically
        // created once and reused.
        let leaked: &'static str = Box::leak(keyword.to_string().into_boxed_str());
        self.keywords.push(leaked);
    }

    /// Set the single-line comment prefix (e.g., "//", "#").
    #[wasm_bindgen(js_name = setSingleLineComment)]
    pub fn set_single_line_comment(&mut self, prefix: &str) {
        let leaked: &'static str = Box::leak(prefix.to_string().into_boxed_str());
        self.single_line_comment = Some(leaked);
    }

    /// Set the multi-line comment delimiters (e.g., "/*" and "*/").
    #[wasm_bindgen(js_name = setMultiLineComment)]
    pub fn set_multi_line_comment(&mut self, start: &str, end: &str) {
        let start_leaked: &'static str = Box::leak(start.to_string().into_boxed_str());
        let end_leaked: &'static str = Box::leak(end.to_string().into_boxed_str());
        self.multi_line_comment = Some((start_leaked, end_leaked));
    }

    /// Enable or disable nested multi-line comments.
    #[wasm_bindgen(js_name = setNestedComments)]
    pub fn set_nested_comments(&mut self, nested: bool) {
        self.nested_comments = nested;
    }

    /// Enable or disable preserving whitespace tokens.
    #[wasm_bindgen(js_name = setPreserveWhitespace)]
    pub fn set_preserve_whitespace(&mut self, preserve: bool) {
        self.preserve_whitespace = preserve;
    }

    /// Enable or disable preserving comment tokens.
    #[wasm_bindgen(js_name = setPreserveComments)]
    pub fn set_preserve_comments(&mut self, preserve: bool) {
        self.preserve_comments = preserve;
    }
}

impl Default for LexerConfig {
    fn default() -> Self {
        Self::new()
    }
}

// Internal wrapper to implement LanguageSpec for custom configs
struct CustomLanguageWrapper {
    keywords: Vec<&'static str>,
    operators: Vec<OperatorDef>,
    comment_styles: CommentStyles,
    string_config: StringConfig,
    preserve_whitespace: bool,
    preserve_comments: bool,
}

impl LanguageSpec for CustomLanguageWrapper {
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

/// Tokenize source code with a custom configuration.
///
/// Returns a JSON string containing the tokens and any errors.
#[wasm_bindgen(js_name = tokenizeWithConfig)]
pub fn tokenize_with_config(source: &str, config: &LexerConfig) -> String {
    // Build comment styles
    let mut comment_styles = CommentStyles::new();
    if let Some(prefix) = config.single_line_comment {
        comment_styles = comment_styles.with_single_line(prefix);
    }
    if let Some((start, end)) = config.multi_line_comment {
        comment_styles = comment_styles.with_multi_line(start, end);
    }
    comment_styles = comment_styles.with_nested(config.nested_comments);

    // Use default operators (same as DefaultLanguage)
    let operators = default_operators();

    let keywords = config.keywords.clone();
    let language = CustomLanguageWrapper {
        keywords: keywords.clone(),
        operators,
        comment_styles,
        string_config: StringConfig::new()
            .with_delimiters(&['"', '\''])
            .with_raw_prefix("r"),
        preserve_whitespace: config.preserve_whitespace,
        preserve_comments: config.preserve_comments,
    };

    // Create a second instance for token conversion (since tokenize consumes language)
    let language_for_display = CustomLanguageWrapper {
        keywords,
        operators: default_operators(),
        comment_styles: CommentStyles::new(),
        string_config: StringConfig::new(),
        preserve_whitespace: false,
        preserve_comments: false,
    };

    let (tokens, errors) = Lexer::tokenize(source, language);

    let js_tokens: Vec<JsToken> = tokens
        .iter()
        .map(|t| JsToken::from_token(t, source, &language_for_display))
        .collect();

    let js_errors: Vec<JsLexError> = errors.iter().map(JsLexError::from_error).collect();

    let result = TokenizeResult {
        success: js_errors.is_empty(),
        tokens: js_tokens,
        errors: js_errors,
    };

    serde_json::to_string(&result).unwrap_or_else(|_| r#"{"error":"serialization failed"}"#.into())
}

/// Get information about the default language keywords.
#[wasm_bindgen(js_name = getDefaultKeywords)]
pub fn get_default_keywords() -> String {
    let keywords = DefaultLanguage.keywords();
    serde_json::to_string(keywords).unwrap_or_else(|_| "[]".into())
}

/// Get the library version.
#[wasm_bindgen(js_name = getVersion)]
pub fn get_version() -> String {
    env!("CARGO_PKG_VERSION").into()
}

// Default operators matching DefaultLanguage
fn default_operators() -> Vec<OperatorDef> {
    use crate::token::TokenKind::*;
    alloc::vec![
        // Three-character operators (none currently)
        // Two-character operators (longest first for maximal munch)
        OperatorDef::new("==", EqEq),
        OperatorDef::new("!=", BangEq),
        OperatorDef::new("<=", LtEq),
        OperatorDef::new(">=", GtEq),
        OperatorDef::new("&&", AmpAmp),
        OperatorDef::new("||", PipePipe),
        OperatorDef::new("<<", LtLt),
        OperatorDef::new(">>", GtGt),
        OperatorDef::new("::", ColonColon),
        OperatorDef::new("->", Arrow),
        OperatorDef::new("=>", FatArrow),
        // Single-character operators
        OperatorDef::new("+", Plus),
        OperatorDef::new("-", Minus),
        OperatorDef::new("*", Star),
        OperatorDef::new("/", Slash),
        OperatorDef::new("%", Percent),
        OperatorDef::new("=", Eq),
        OperatorDef::new("<", Lt),
        OperatorDef::new(">", Gt),
        OperatorDef::new("!", Bang),
        OperatorDef::new("&", Amp),
        OperatorDef::new("|", Pipe),
        OperatorDef::new("^", Caret),
        OperatorDef::new("~", Tilde),
        OperatorDef::new("(", LParen),
        OperatorDef::new(")", RParen),
        OperatorDef::new("{", LBrace),
        OperatorDef::new("}", RBrace),
        OperatorDef::new("[", LBracket),
        OperatorDef::new("]", RBracket),
        OperatorDef::new(";", Semicolon),
        OperatorDef::new(",", Comma),
        OperatorDef::new(":", Colon),
        OperatorDef::new(".", Dot),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple() {
        let result = tokenize("let x = 42;");
        let parsed: TokenizeResult = serde_json::from_str(&result).unwrap();
        assert!(parsed.success);
        assert!(!parsed.tokens.is_empty());
    }

    #[test]
    fn test_tokenize_with_errors() {
        let result = tokenize("let @ = 42;");
        let parsed: TokenizeResult = serde_json::from_str(&result).unwrap();
        assert!(!parsed.success);
        assert!(!parsed.errors.is_empty());
    }

    #[test]
    fn test_custom_config() {
        let mut config = LexerConfig::new();
        config.add_keyword("mykey");
        config.set_single_line_comment("#");

        let result = tokenize_with_config("mykey x # comment", &config);
        let parsed: TokenizeResult = serde_json::from_str(&result).unwrap();
        assert!(parsed.success);

        // First token should be a keyword
        assert_eq!(parsed.tokens[0].kind, "Keyword");
        assert_eq!(parsed.tokens[0].value, Some("mykey".into()));
    }

    #[test]
    fn test_get_default_keywords() {
        let keywords = get_default_keywords();
        let parsed: Vec<String> = serde_json::from_str(&keywords).unwrap();
        assert!(parsed.contains(&"if".to_string()));
        assert!(parsed.contains(&"fn".to_string()));
    }
}
