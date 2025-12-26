//! Main lexer fuzz target.
//!
//! Tests arbitrary UTF-8 input against the lexer to find panics,
//! crashes, and invariant violations.

#![no_main]

use libfuzzer_sys::fuzz_target;
use lex::{DefaultLanguage, Lexer};

fuzz_target!(|data: &str| {
    // Core invariant: tokenizing any valid UTF-8 string should never panic
    let (tokens, errors) = Lexer::tokenize(data, DefaultLanguage);

    // Verify basic invariants
    verify_invariants(data, &tokens, &errors);
});

/// Verify fundamental lexer invariants that must hold for any input.
fn verify_invariants(source: &str, tokens: &[lex::Token], errors: &[lex::LexError]) {
    // Note: The Iterator implementation does NOT include EOF in the token stream.
    // Empty input produces an empty token list, which is correct.

    // Invariant 1: All token spans must be within source bounds
    let source_len = source.len();
    for token in tokens {
        assert!(
            token.span.start <= source_len,
            "Token start {} exceeds source length {}",
            token.span.start,
            source_len
        );
        assert!(
            token.span.end <= source_len,
            "Token end {} exceeds source length {}",
            token.span.end,
            source_len
        );
        assert!(
            token.span.start <= token.span.end,
            "Token start {} exceeds end {}",
            token.span.start,
            token.span.end
        );
    }

    // Invariant 2: Token spans should not overlap
    let mut prev_end = 0;
    for token in tokens {
        assert!(
            token.span.start >= prev_end,
            "Token at {} overlaps with previous token ending at {}",
            token.span.start,
            prev_end
        );
        prev_end = token.span.end;
    }

    // Invariant 3: Error locations must be within source bounds
    for error in errors {
        assert!(
            error.span.start <= source_len,
            "Error start {} exceeds source length {}",
            error.span.start,
            source_len
        );
        assert!(
            error.span.end <= source_len,
            "Error end {} exceeds source length {}",
            error.span.end,
            source_len
        );
    }

    // Invariant 4: Line numbers must be positive (1-indexed)
    for token in tokens {
        assert!(
            token.span.start_loc.line >= 1,
            "Line number must be >= 1"
        );
        assert!(
            token.span.end_loc.line >= 1,
            "Line number must be >= 1"
        );
    }

    // Invariant 5: Column numbers must be positive (1-indexed)
    for token in tokens {
        assert!(
            token.span.start_loc.column >= 1,
            "Column number must be >= 1"
        );
        assert!(
            token.span.end_loc.column >= 1,
            "Column number must be >= 1"
        );
    }

    // Invariant 6: Positions should be monotonically non-decreasing
    let mut prev_line = 1;
    let mut prev_col = 1;
    for token in tokens {
        let loc = &token.span.start_loc;
        if loc.line == prev_line {
            assert!(
                loc.column >= prev_col,
                "Column {} decreased from {} on same line",
                loc.column,
                prev_col
            );
        } else {
            assert!(
                loc.line >= prev_line,
                "Line {} decreased from {}",
                loc.line,
                prev_line
            );
        }
        prev_line = token.span.end_loc.line;
        prev_col = token.span.end_loc.column;
    }
}
