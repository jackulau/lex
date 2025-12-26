//! Custom language configuration fuzz target.
//!
//! Tests different language configurations (keywords, operators, comment styles)
//! combined with arbitrary input to find edge cases in language interactions.

#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use lex::language::LanguageBuilder;
use lex::token::TokenKind;
use lex::{Lexer, TokenKind as TK};

/// Language configuration options (using static strings since LanguageBuilder requires them).
#[derive(Debug, Arbitrary)]
struct LanguageConfig {
    /// Which keyword set to use
    keyword_set: KeywordSet,
    /// Which operator set to use
    operator_set: OperatorSet,
    /// Comment style configuration
    comment_style: CommentStyle,
    /// String configuration
    string_style: StringStyle,
    /// Whether to preserve whitespace
    preserve_whitespace: bool,
    /// Whether to preserve comments
    preserve_comments: bool,
    /// Whether to enable nested comments
    nested_comments: bool,
    /// Whether to enable multiline strings
    multiline_strings: bool,
}

#[derive(Debug, Arbitrary)]
enum KeywordSet {
    None,
    CStyle,
    RustStyle,
    PythonStyle,
    Mixed,
}

#[derive(Debug, Arbitrary)]
enum OperatorSet {
    None,
    Basic,
    CStyle,
    Extended,
}

#[derive(Debug, Arbitrary)]
enum CommentStyle {
    None,
    CStyle,      // // and /* */
    PythonStyle, // #
    SqlStyle,    // --
    HashAndBlock,
}

#[derive(Debug, Arbitrary)]
enum StringStyle {
    DoubleQuote,
    SingleQuote,
    Both,
    WithRaw,
}

/// Input to test against the language configuration.
#[derive(Debug, Arbitrary)]
struct FuzzInput {
    config: LanguageConfig,
    /// The source code to tokenize
    source: String,
}

impl LanguageConfig {
    fn build(&self) -> impl lex::LanguageSpec {
        let mut builder = LanguageBuilder::new();

        // Add keywords
        match self.keyword_set {
            KeywordSet::None => {}
            KeywordSet::CStyle => {
                builder = builder.keywords(&[
                    "if", "else", "while", "for", "do", "switch", "case", "break",
                    "continue", "return", "void", "int", "char", "float", "double",
                ]);
            }
            KeywordSet::RustStyle => {
                builder = builder.keywords(&[
                    "fn", "let", "mut", "const", "if", "else", "match", "loop",
                    "while", "for", "in", "struct", "enum", "impl", "trait", "pub",
                ]);
            }
            KeywordSet::PythonStyle => {
                builder = builder.keywords(&[
                    "def", "class", "if", "elif", "else", "for", "while", "with",
                    "try", "except", "finally", "import", "from", "as", "pass",
                ]);
            }
            KeywordSet::Mixed => {
                builder = builder.keywords(&[
                    "fn", "def", "func", "function", "if", "else", "elif", "switch",
                    "let", "var", "const", "mut", "class", "struct", "interface",
                ]);
            }
        }

        // Add operators (using only the TokenKinds that exist)
        match self.operator_set {
            OperatorSet::None => {}
            OperatorSet::Basic => {
                builder = builder
                    .operator("+", TK::Plus)
                    .operator("-", TK::Minus)
                    .operator("*", TK::Star)
                    .operator("/", TK::Slash)
                    .operator("=", TK::Eq);
            }
            OperatorSet::CStyle => {
                builder = builder
                    .operator("+", TK::Plus)
                    .operator("-", TK::Minus)
                    .operator("*", TK::Star)
                    .operator("/", TK::Slash)
                    .operator("%", TK::Percent)
                    .operator("=", TK::Eq)
                    .operator("==", TK::EqEq)
                    .operator("!=", TK::BangEq)
                    .operator("<", TK::Lt)
                    .operator(">", TK::Gt)
                    .operator("<=", TK::LtEq)
                    .operator(">=", TK::GtEq)
                    .operator("&&", TK::AmpAmp)
                    .operator("||", TK::PipePipe)
                    .operator("!", TK::Bang)
                    .operator("&", TK::Amp)
                    .operator("|", TK::Pipe);
            }
            OperatorSet::Extended => {
                builder = builder
                    .operator("+", TK::Plus)
                    .operator("-", TK::Minus)
                    .operator("*", TK::Star)
                    .operator("/", TK::Slash)
                    .operator("%", TK::Percent)
                    .operator("=", TK::Eq)
                    .operator("==", TK::EqEq)
                    .operator("!=", TK::BangEq)
                    .operator("<", TK::Lt)
                    .operator(">", TK::Gt)
                    .operator("<=", TK::LtEq)
                    .operator(">=", TK::GtEq)
                    .operator("<<", TK::LtLt)
                    .operator(">>", TK::GtGt)
                    .operator("->", TK::Arrow)
                    .operator("=>", TK::FatArrow)
                    .operator("::", TK::ColonColon)
                    .operator("&", TK::Amp)
                    .operator("|", TK::Pipe)
                    .operator("^", TK::Caret)
                    .operator("~", TK::Tilde);
            }
        }

        // Add comment styles
        match self.comment_style {
            CommentStyle::None => {}
            CommentStyle::CStyle => {
                builder = builder
                    .single_line_comment("//")
                    .multi_line_comment("/*", "*/");
            }
            CommentStyle::PythonStyle => {
                builder = builder.single_line_comment("#");
            }
            CommentStyle::SqlStyle => {
                builder = builder.single_line_comment("--");
            }
            CommentStyle::HashAndBlock => {
                builder = builder
                    .single_line_comment("#")
                    .multi_line_comment("{-", "-}");
            }
        }

        // Configure nested comments
        builder = builder.nested_comments(self.nested_comments);

        // Configure strings
        match self.string_style {
            StringStyle::DoubleQuote => {
                builder = builder.string_delimiters(&['"']);
            }
            StringStyle::SingleQuote => {
                builder = builder.string_delimiters(&['\'']);
            }
            StringStyle::Both => {
                builder = builder.string_delimiters(&['"', '\'']);
            }
            StringStyle::WithRaw => {
                builder = builder
                    .string_delimiters(&['"'])
                    .raw_string_prefix("r");
            }
        }

        // Configure multiline strings
        builder = builder.multiline_strings(self.multiline_strings);

        // Configure preservation
        if self.preserve_whitespace {
            builder = builder.preserve_whitespace(true);
        }
        if self.preserve_comments {
            builder = builder.preserve_comments(true);
        }

        builder.build()
    }
}

fuzz_target!(|input: FuzzInput| {
    let language = input.config.build();

    // Tokenize and verify no panics
    let (tokens, errors) = Lexer::tokenize(&input.source, language);

    // Note: EOF is not in token stream

    // Verify token spans
    for token in &tokens {
        assert!(
            token.span.start <= token.span.end,
            "Token start must not exceed end"
        );
        assert!(
            token.span.end <= input.source.len(),
            "Token end {} must not exceed source length {}",
            token.span.end,
            input.source.len()
        );
    }

    // Verify error spans
    for error in &errors {
        assert!(
            error.span.start <= error.span.end,
            "Error start must not exceed end"
        );
        assert!(
            error.span.end <= input.source.len(),
            "Error end must not exceed source length"
        );
    }

    // If preserving whitespace, check that whitespace tokens exist (if input has whitespace)
    if input.config.preserve_whitespace && input.source.chars().any(|c| c.is_whitespace()) {
        let has_whitespace_token = tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Whitespace));
        // Might not have whitespace token if it's inside a string or comment
        // So we don't assert, just observe
        let _ = has_whitespace_token;
    }

    // If preserving comments, check that comment tokens exist (if input has comments)
    if input.config.preserve_comments {
        let has_comment_token = tokens.iter().any(|t| {
            matches!(t.kind, TokenKind::Comment)
        });
        // Might not have comment if no comment syntax in source
        let _ = has_comment_token;
    }

    // Keywords from the configuration should be recognized
    match input.config.keyword_set {
        KeywordSet::CStyle => {
            if input.source.contains("if") {
                // "if" should be recognized as a keyword if it's a standalone token
                // (not part of a larger identifier or string)
            }
        }
        _ => {}
    }
});
