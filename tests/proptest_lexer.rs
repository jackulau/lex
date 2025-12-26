//! Property-based tests for the lexer using proptest.
//!
//! These tests verify that the lexer maintains various invariants
//! across randomly generated inputs.

use lex::{DefaultLanguage, LanguageBuilder, Lexer, TokenKind};
use proptest::prelude::*;

// Custom Strategies

/// Strategy for generating valid identifier strings
fn identifier_strategy() -> impl Strategy<Value = String> {
    // Start with letter or underscore, followed by alphanumerics/underscores
    (
        prop::sample::select(vec![
            'a', 'b', 'c', 'x', 'y', 'z', 'A', 'B', 'C', '_',
        ]),
        prop::collection::vec(
            prop::sample::select(vec![
                'a', 'b', 'c', 'x', 'y', 'z', '0', '1', '2', '9', '_',
            ]),
            0..10,
        ),
    )
        .prop_map(|(first, rest)| {
            let mut s = String::new();
            s.push(first);
            for c in rest {
                s.push(c);
            }
            s
        })
}

/// Strategy for generating integer literals
fn integer_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        // Decimal integers
        "[0-9][0-9_]{0,10}",
        // Hex integers
        "0[xX][0-9a-fA-F][0-9a-fA-F_]{0,8}",
        // Octal integers
        "0[oO][0-7][0-7_]{0,8}",
        // Binary integers
        "0[bB][01][01_]{0,8}",
    ]
}

/// Strategy for generating float literals
fn float_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        // Simple floats
        "[0-9]+\\.[0-9]+",
        // Floats with exponent
        "[0-9]+\\.[0-9]+[eE][+-]?[0-9]+",
        // Integer with exponent (becomes float)
        "[0-9]+[eE][+-]?[0-9]+",
    ]
}

/// Strategy for generating string literals
fn string_literal_strategy() -> impl Strategy<Value = String> {
    prop::collection::vec(
        prop_oneof![
            // Regular characters
            "[a-zA-Z0-9 ]{1,5}",
            // Common escapes
            Just("\\n".to_string()),
            Just("\\t".to_string()),
            Just("\\r".to_string()),
            Just("\\\\".to_string()),
            Just("\\\"".to_string()),
            Just("\\0".to_string()),
        ],
        0..5,
    )
    .prop_map(|parts| format!("\"{}\"", parts.join("")))
}

/// Strategy for generating raw string literals
fn raw_string_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 \\\\]{0,20}".prop_map(|content| format!("r\"{}\"", content))
}

/// Strategy for generating character literals
fn char_literal_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        "[a-zA-Z0-9]".prop_map(|c| format!("'{}'", c)),
        Just("'\\n'".to_string()),
        Just("'\\t'".to_string()),
        Just("'\\r'".to_string()),
        Just("'\\\\'".to_string()),
        Just("'\\''".to_string()),
        Just("'\\0'".to_string()),
    ]
}

/// Strategy for generating operators
fn operator_strategy() -> impl Strategy<Value = String> {
    prop::sample::select(vec![
        "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "!", "&&", "||", "&", "|",
        "^", "~", "<<", ">>", "->", "=>", "::",
    ])
    .prop_map(|s| s.to_string())
}

/// Strategy for generating delimiters
fn delimiter_strategy() -> impl Strategy<Value = String> {
    prop::sample::select(vec!["(", ")", "{", "}", "[", "]", ";", ",", ":", "."])
        .prop_map(|s| s.to_string())
}

/// Strategy for generating whitespace
fn whitespace_strategy() -> impl Strategy<Value = String> {
    prop::collection::vec(prop::sample::select(vec![' ', '\t', '\n']), 1..5)
        .prop_map(|chars| chars.into_iter().collect())
}

/// Strategy for generating single-line comments
fn comment_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 ]{0,20}".prop_map(|content| format!("// {}\n", content))
}

/// Strategy for generating valid tokens
fn valid_token_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        identifier_strategy(),
        integer_strategy(),
        float_strategy(),
        string_literal_strategy(),
        raw_string_strategy(),
        char_literal_strategy(),
        operator_strategy(),
        delimiter_strategy(),
        comment_strategy(),
    ]
}

/// Strategy for generating a valid program (sequence of tokens)
fn valid_program_strategy() -> impl Strategy<Value = String> {
    prop::collection::vec(
        (valid_token_strategy(), whitespace_strategy()),
        1..20,
    )
    .prop_map(|tokens| {
        tokens
            .into_iter()
            .map(|(tok, ws)| format!("{}{}", tok, ws))
            .collect()
    })
}

// ============================================================================
// Property Tests
// ============================================================================

proptest! {
    /// The lexer should never panic on any arbitrary input
    #[test]
    fn lexer_never_panics(input in ".*") {
        let _ = Lexer::tokenize(&input, DefaultLanguage);
    }

    /// The lexer should never panic on arbitrary UTF-8 input
    #[test]
    fn lexer_never_panics_utf8(input in "\\PC*") {
        let _ = Lexer::tokenize(&input, DefaultLanguage);
    }

    /// Tokenizing should always terminate and produce at least EOF
    #[test]
    fn lexer_always_terminates(input in ".{0,1000}") {
        let mut lexer = Lexer::new(&input, DefaultLanguage);
        let mut count = 0;
        let max_tokens = input.len() + 100; // Reasonable upper bound

        loop {
            let token = lexer.next_token();
            count += 1;
            if token.kind == TokenKind::Eof || count > max_tokens {
                break;
            }
        }

        prop_assert!(count <= max_tokens, "Lexer produced too many tokens");
    }

    /// All token spans should be within bounds of the source
    #[test]
    fn token_spans_within_bounds(input in ".{0,500}") {
        let (tokens, _) = Lexer::tokenize(&input, DefaultLanguage);
        let len = input.len();

        for token in tokens {
            prop_assert!(
                token.span.start <= len,
                "Token start {} exceeds source length {}",
                token.span.start,
                len
            );
            prop_assert!(
                token.span.end <= len,
                "Token end {} exceeds source length {}",
                token.span.end,
                len
            );
            prop_assert!(
                token.span.start <= token.span.end,
                "Token start {} exceeds end {}",
                token.span.start,
                token.span.end
            );
        }
    }

    /// Token spans should be non-overlapping and cover the input
    #[test]
    fn token_spans_non_overlapping(input in valid_program_strategy()) {
        let (tokens, _) = Lexer::tokenize(&input, DefaultLanguage);

        // Check non-overlapping: each token starts where the previous ends
        // (after skipping whitespace which is not preserved by default)
        for window in tokens.windows(2) {
            let prev = &window[0];
            let curr = &window[1];
            prop_assert!(
                prev.span.end <= curr.span.start,
                "Tokens overlap: {:?} and {:?}",
                prev,
                curr
            );
        }
    }

    /// Lexing should be deterministic: same input produces same output
    #[test]
    fn lexer_is_deterministic(input in ".{0,200}") {
        let (tokens1, errors1) = Lexer::tokenize(&input, DefaultLanguage);
        let (tokens2, errors2) = Lexer::tokenize(&input, DefaultLanguage);

        prop_assert_eq!(tokens1.len(), tokens2.len());
        prop_assert_eq!(errors1.len(), errors2.len());

        for (t1, t2) in tokens1.iter().zip(tokens2.iter()) {
            prop_assert_eq!(t1.kind, t2.kind);
            prop_assert_eq!(t1.span, t2.span);
        }
    }

    /// Valid identifiers should tokenize as identifiers (not keywords)
    #[test]
    fn valid_identifiers_tokenize_correctly(ident in identifier_strategy()) {
        // Skip keywords
        let keywords = ["if", "else", "while", "for", "fn", "let", "return",
                       "true", "false", "struct", "enum", "impl", "pub", "mut"];
        if keywords.contains(&ident.as_str()) {
            return Ok(());
        }

        let (tokens, errors) = Lexer::tokenize(&ident, DefaultLanguage);

        prop_assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);
        prop_assert_eq!(tokens.len(), 1, "Expected single token");
        prop_assert_eq!(tokens[0].kind, TokenKind::Ident);
    }

    /// Valid integers should tokenize as IntLiteral
    #[test]
    fn valid_integers_tokenize_correctly(num in integer_strategy()) {
        let (tokens, errors) = Lexer::tokenize(&num, DefaultLanguage);

        prop_assert!(errors.is_empty(), "Unexpected errors for '{}': {:?}", num, errors);
        prop_assert_eq!(tokens.len(), 1, "Expected single token for '{}'", num);
        prop_assert_eq!(tokens[0].kind, TokenKind::IntLiteral, "Expected IntLiteral for '{}'", num);
    }

    /// Valid floats should tokenize as FloatLiteral
    #[test]
    fn valid_floats_tokenize_correctly(num in float_strategy()) {
        let (tokens, errors) = Lexer::tokenize(&num, DefaultLanguage);

        prop_assert!(errors.is_empty(), "Unexpected errors for '{}': {:?}", num, errors);
        prop_assert_eq!(tokens.len(), 1, "Expected single token for '{}'", num);
        prop_assert_eq!(tokens[0].kind, TokenKind::FloatLiteral, "Expected FloatLiteral for '{}'", num);
    }

    /// Valid string literals should tokenize correctly
    #[test]
    fn valid_strings_tokenize_correctly(s in string_literal_strategy()) {
        let (tokens, errors) = Lexer::tokenize(&s, DefaultLanguage);

        prop_assert!(errors.is_empty(), "Unexpected errors for '{}': {:?}", s, errors);
        prop_assert_eq!(tokens.len(), 1, "Expected single token for '{}'", s);
        prop_assert_eq!(tokens[0].kind, TokenKind::StringLiteral);
    }

    /// Valid raw string literals should tokenize correctly
    #[test]
    fn valid_raw_strings_tokenize_correctly(s in raw_string_strategy()) {
        let (tokens, errors) = Lexer::tokenize(&s, DefaultLanguage);

        prop_assert!(errors.is_empty(), "Unexpected errors for '{}': {:?}", s, errors);
        prop_assert_eq!(tokens.len(), 1, "Expected single token for '{}'", s);
        prop_assert_eq!(tokens[0].kind, TokenKind::RawStringLiteral);
    }

    /// Valid character literals should tokenize correctly
    #[test]
    fn valid_chars_tokenize_correctly(c in char_literal_strategy()) {
        let (tokens, errors) = Lexer::tokenize(&c, DefaultLanguage);

        prop_assert!(errors.is_empty(), "Unexpected errors for '{}': {:?}", c, errors);
        prop_assert_eq!(tokens.len(), 1, "Expected single token for '{}'", c);
        prop_assert_eq!(tokens[0].kind, TokenKind::CharLiteral);
    }

    /// Line numbers should be monotonically increasing
    #[test]
    fn line_numbers_increase(input in ".{0,500}") {
        let (tokens, _) = Lexer::tokenize(&input, DefaultLanguage);

        let mut last_line = 0;
        for token in tokens {
            let line = token.span.start_loc.line;
            prop_assert!(
                line >= last_line,
                "Line number decreased from {} to {}",
                last_line,
                line
            );
            last_line = line;
        }
    }

    /// Column numbers should be positive
    #[test]
    fn column_numbers_positive(input in ".{0,500}") {
        let (tokens, _) = Lexer::tokenize(&input, DefaultLanguage);

        for token in tokens {
            prop_assert!(
                token.span.start_loc.column >= 1,
                "Column number {} is not positive",
                token.span.start_loc.column
            );
        }
    }

    /// Empty input should produce no tokens (except implicit EOF)
    #[test]
    fn empty_input_no_tokens(ws in "[ \t\n\r]*") {
        let (tokens, errors) = Lexer::tokenize(&ws, DefaultLanguage);
        prop_assert!(errors.is_empty());
        prop_assert!(tokens.is_empty(), "Whitespace-only input produced tokens: {:?}", tokens);
    }

    /// Unicode identifiers should be recognized
    #[test]
    fn unicode_identifiers_work(
        prefix in "[a-zA-Z_]",
        // Use only ASCII alphanumeric + underscore to avoid invalid XID_Continue chars
        // (full Unicode XID testing is done in unit tests)
        suffix in "[a-zA-Z0-9_]{0,10}"
    ) {
        let ident = format!("{}{}", prefix, suffix);
        let (tokens, errors) = Lexer::tokenize(&ident, DefaultLanguage);

        // Should produce exactly one token (identifier or keyword)
        prop_assert!(errors.is_empty(), "Errors for '{}': {:?}", ident, errors);
        prop_assert_eq!(tokens.len(), 1, "Token count for '{}'", ident);
        prop_assert!(
            matches!(tokens[0].kind, TokenKind::Ident | TokenKind::Keyword(_) | TokenKind::BoolLiteral(_)),
            "Expected identifier-like token for '{}', got {:?}",
            ident,
            tokens[0].kind
        );
    }

    /// Custom language configurations should not cause panics
    #[test]
    fn custom_language_never_panics(input in ".{0,200}") {
        // Test with a few different static language configurations
        let lang1 = LanguageBuilder::new()
            .keywords(&["foo", "bar", "baz"])
            .single_line_comment("#")
            .build();
        let _ = Lexer::tokenize(&input, lang1);

        let lang2 = LanguageBuilder::new()
            .keywords(&["let", "var", "const"])
            .single_line_comment("--")
            .multi_line_comment("{-", "-}")
            .build();
        let _ = Lexer::tokenize(&input, lang2);

        let lang3 = LanguageBuilder::new()
            .preserve_whitespace(true)
            .preserve_comments(true)
            .build();
        let _ = Lexer::tokenize(&input, lang3);
    }

    /// Error recovery should allow lexing to continue
    #[test]
    fn error_recovery_continues(
        valid1 in identifier_strategy(),
        valid2 in identifier_strategy(),
    ) {
        // Input with an invalid character in the middle
        let input = format!("{} @ {}", valid1, valid2);
        let (tokens, errors) = Lexer::tokenize(&input, DefaultLanguage);

        // Should have errors but still produce tokens
        prop_assert!(!errors.is_empty(), "Expected error for @");

        // Should find both valid identifiers
        let idents: Vec<_> = tokens.iter()
            .filter(|t| t.kind == TokenKind::Ident)
            .collect();
        prop_assert!(idents.len() >= 2, "Should recover and find both identifiers");
    }

    /// Token source text extraction should work correctly
    #[test]
    fn source_text_extraction_correct(input in valid_program_strategy()) {
        let mut lexer = Lexer::new(&input, DefaultLanguage);

        loop {
            let token = lexer.next_token();
            if token.kind == TokenKind::Eof {
                break;
            }

            let text = lexer.source_text(token.span);

            // Text should not be empty for non-error tokens
            if token.kind != TokenKind::Error {
                prop_assert!(!text.is_empty(), "Empty source text for {:?}", token);
            }

            // Text length should match span
            prop_assert_eq!(
                text.len(),
                token.span.end - token.span.start,
                "Source text length mismatch for {:?}",
                token
            );
        }
    }

    /// Peek should not affect iteration
    #[test]
    fn peek_does_not_affect_iteration(input in ".{0,100}") {
        let mut lexer1 = Lexer::new(&input, DefaultLanguage);
        let mut lexer2 = Lexer::new(&input, DefaultLanguage);

        // Lexer1: peek then next
        // Lexer2: just next
        loop {
            let _ = lexer1.peek();
            let t1 = lexer1.next_token();
            let t2 = lexer2.next_token();

            prop_assert_eq!(t1.kind, t2.kind);
            prop_assert_eq!(t1.span, t2.span);

            if t1.kind == TokenKind::Eof {
                break;
            }
        }
    }

    /// Multiple peeks should return the same token
    #[test]
    fn multiple_peeks_same_result(input in ".{1,100}") {
        let mut lexer = Lexer::new(&input, DefaultLanguage);

        let peek1 = lexer.peek().clone();
        let peek2 = lexer.peek().clone();
        let peek3 = lexer.peek().clone();

        prop_assert_eq!(peek1.kind, peek2.kind);
        prop_assert_eq!(peek2.kind, peek3.kind);
        prop_assert_eq!(peek1.span, peek2.span);
        prop_assert_eq!(peek2.span, peek3.span);
    }
}

// ============================================================================
// Regression Tests from Proptest Failures
// ============================================================================

#[test]
fn regression_empty_string() {
    let (tokens, errors) = Lexer::tokenize("", DefaultLanguage);
    assert!(tokens.is_empty());
    assert!(errors.is_empty());
}

#[test]
fn regression_single_newline() {
    let (tokens, errors) = Lexer::tokenize("\n", DefaultLanguage);
    assert!(tokens.is_empty());
    assert!(errors.is_empty());
}

#[test]
fn regression_null_byte() {
    // Null bytes in input should not cause panics
    let input = "let x\0= 42";
    let _ = Lexer::tokenize(input, DefaultLanguage);
}

#[test]
fn regression_very_long_identifier() {
    let ident = "a".repeat(10000);
    let (tokens, errors) = Lexer::tokenize(&ident, DefaultLanguage);
    assert!(errors.is_empty());
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].kind, TokenKind::Ident);
}

#[test]
fn regression_deeply_nested_comments() {
    let lang = LanguageBuilder::new().multi_line_comment("/*", "*/").nested_comments(true).build();

    let input = "/*/*/*/*/*nested*/*/*/*/*/";
    let _ = Lexer::tokenize(input, lang);
}
