//! Incremental re-lexing support.
//!
//! This module provides the ability to efficiently re-tokenize only the changed
//! regions of source text, reusing tokens from previous tokenizations where possible.
//!
//! # Algorithm
//!
//! When text is edited, the incremental lexer:
//! 1. Finds the first token affected by the edit (overlapping or after the edit start)
//! 2. Re-lexes from a safe starting point in the new text
//! 3. Continues until reaching a synchronization point where tokens align
//! 4. Reuses unchanged tokens with adjusted positions
//!
//! # Example
//!
//! ```rust
//! use lex::{IncrementalLexer, DefaultLanguage, Edit};
//!
//! let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);
//!
//! // Initial tokenization
//! let tokens = lexer.tokens();
//! assert_eq!(tokens.len(), 5);
//!
//! // Apply an edit: change "42" to "100"
//! let edit = Edit::new(8, 10, "100");
//! lexer.apply_edit(edit);
//!
//! // Only the affected region is re-lexed
//! let new_tokens = lexer.tokens();
//! assert_eq!(new_tokens.len(), 5);
//! ```

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{string::String, vec::Vec};

use crate::error::LexError;
use crate::language::LanguageSpec;
use crate::lexer::Lexer;
use crate::span::{Location, Span};
use crate::token::{Token, TokenKind};

/// Represents an edit operation on source text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edit {
    /// Byte offset where the edit starts (inclusive).
    pub start: usize,
    /// Byte offset where the edit ends (exclusive) - in the OLD text.
    pub end: usize,
    /// The new text to insert (replacing start..end).
    pub new_text: String,
}

impl Edit {
    /// Create a new edit.
    pub fn new(start: usize, end: usize, new_text: impl Into<String>) -> Self {
        Self {
            start,
            end,
            new_text: new_text.into(),
        }
    }

    /// Create an insertion edit (no deletion).
    pub fn insert(pos: usize, text: impl Into<String>) -> Self {
        Self::new(pos, pos, text)
    }

    /// Create a deletion edit (no insertion).
    pub fn delete(start: usize, end: usize) -> Self {
        Self::new(start, end, "")
    }

    /// Get the byte delta caused by this edit.
    /// Positive means text grew, negative means text shrank.
    pub fn delta(&self) -> isize {
        self.new_text.len() as isize - (self.end - self.start) as isize
    }
}

/// Statistics about an incremental lexing operation.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct IncrementalStats {
    /// Number of tokens that were reused from the cache.
    pub tokens_reused: usize,
    /// Number of tokens that were newly lexed.
    pub tokens_lexed: usize,
    /// Whether full re-lexing was performed.
    pub full_relex: bool,
}

/// An incremental lexer that caches tokens and efficiently re-tokenizes
/// only changed regions.
pub struct IncrementalLexer<L: LanguageSpec> {
    /// Current source text.
    source: String,
    /// Language specification.
    language: L,
    /// Cached tokens from last tokenization.
    tokens: Vec<Token>,
    /// Cached errors from last tokenization.
    errors: Vec<LexError>,
    /// Whether the cache is valid.
    cache_valid: bool,
    /// Statistics from the last operation.
    last_stats: IncrementalStats,
}

impl<L: LanguageSpec + Clone> IncrementalLexer<L> {
    /// Create a new incremental lexer with the given source and language.
    pub fn new(source: impl Into<String>, language: L) -> Self {
        let source = source.into();
        let mut lexer = Self {
            source,
            language,
            tokens: Vec::new(),
            errors: Vec::new(),
            cache_valid: false,
            last_stats: IncrementalStats::default(),
        };
        lexer.ensure_tokenized();
        lexer
    }

    /// Get the current source text.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get the cached tokens.
    pub fn tokens(&mut self) -> &[Token] {
        self.ensure_tokenized();
        &self.tokens
    }

    /// Get the cached errors.
    pub fn errors(&mut self) -> &[LexError] {
        self.ensure_tokenized();
        &self.errors
    }

    /// Get statistics from the last tokenization/edit.
    pub fn last_stats(&self) -> IncrementalStats {
        self.last_stats
    }

    /// Get the language specification.
    pub fn language(&self) -> &L {
        &self.language
    }

    /// Apply an edit to the source text and incrementally re-tokenize.
    ///
    /// Returns the statistics about the operation.
    pub fn apply_edit(&mut self, edit: Edit) -> IncrementalStats {
        // Validate edit bounds
        if edit.start > self.source.len() || edit.end > self.source.len() || edit.start > edit.end {
            // Invalid edit - do nothing
            return self.last_stats;
        }

        // Apply the edit to the source text
        let mut new_source = String::with_capacity(
            self.source.len() - (edit.end - edit.start) + edit.new_text.len(),
        );
        new_source.push_str(&self.source[..edit.start]);
        new_source.push_str(&edit.new_text);
        new_source.push_str(&self.source[edit.end..]);

        let old_source = core::mem::replace(&mut self.source, new_source);

        // Perform incremental re-lexing
        self.last_stats = self.incremental_relex(&old_source, &edit);
        self.cache_valid = true;
        self.last_stats
    }

    /// Apply multiple edits in order.
    ///
    /// Note: Edits are applied sequentially, so later edits should account
    /// for position changes from earlier edits.
    pub fn apply_edits(&mut self, edits: impl IntoIterator<Item = Edit>) -> IncrementalStats {
        let mut total_stats = IncrementalStats::default();
        for edit in edits {
            let stats = self.apply_edit(edit);
            total_stats.tokens_reused += stats.tokens_reused;
            total_stats.tokens_lexed += stats.tokens_lexed;
            total_stats.full_relex |= stats.full_relex;
        }
        self.last_stats = total_stats;
        total_stats
    }

    /// Replace the entire source text.
    pub fn set_source(&mut self, source: impl Into<String>) {
        self.source = source.into();
        self.cache_valid = false;
        self.tokens.clear();
        self.errors.clear();
        self.ensure_tokenized();
    }

    /// Force a full re-tokenization.
    pub fn relex_all(&mut self) {
        self.cache_valid = false;
        self.ensure_tokenized();
    }

    /// Ensure tokens are up to date.
    fn ensure_tokenized(&mut self) {
        if !self.cache_valid {
            let (tokens, errors) = Lexer::tokenize(&self.source, self.language.clone());
            self.last_stats = IncrementalStats {
                tokens_reused: 0,
                tokens_lexed: tokens.len(),
                full_relex: true,
            };
            self.tokens = tokens;
            self.errors = errors;
            self.cache_valid = true;
        }
    }

    /// Perform incremental re-lexing after an edit.
    fn incremental_relex(&mut self, old_source: &str, edit: &Edit) -> IncrementalStats {
        let delta = edit.delta();

        // Find the first token that is affected by the edit
        let first_affected = self.find_first_affected_token(edit.start);

        // If no tokens are affected (edit is after all tokens), we might just
        // need to lex the new portion
        let Some(first_affected_idx) = first_affected else {
            // Edit is after all existing tokens - lex from edit point
            return self.lex_from_position(edit.start, delta);
        };

        // Determine the safe starting position for re-lexing.
        // We need to start from a position where the lexer state would be
        // the same as if we were lexing from the beginning.
        let relex_start_idx = self.find_safe_relex_start(first_affected_idx);
        let relex_byte_start = if relex_start_idx > 0 {
            self.tokens[relex_start_idx - 1].span.end
        } else {
            0
        };

        // Calculate where to look for synchronization in the new text
        let edit_end_in_new = (edit.start as isize + edit.new_text.len() as isize) as usize;

        // Lex from the safe starting point
        let mut new_tokens = Vec::new();
        let mut new_errors = Vec::new();
        let mut lexer = Lexer::new(&self.source[relex_byte_start..], self.language.clone());

        // Track position offset for the relative lexer
        let offset = relex_byte_start;

        loop {
            let token = lexer.next_token();
            if token.kind == TokenKind::Eof {
                break;
            }

            // Adjust span to absolute positions
            let adjusted_token = self.adjust_token_span(token, offset);

            // Check if we've synchronized with old tokens
            if adjusted_token.span.start >= edit_end_in_new {
                // We're past the edit region - check for sync
                if let Some(sync_idx) =
                    self.find_sync_point(&adjusted_token, relex_start_idx, delta, old_source)
                {
                    // Found sync point - reuse remaining tokens
                    new_tokens.push(adjusted_token);
                    let stats = self.merge_with_cached(
                        relex_start_idx,
                        sync_idx,
                        new_tokens,
                        new_errors,
                        delta,
                    );
                    return stats;
                }
            }

            new_tokens.push(adjusted_token);
        }

        // Collect any errors
        for error in lexer.errors() {
            let adjusted_error = self.adjust_error_span(error.clone(), offset);
            new_errors.push(adjusted_error);
        }

        // No sync point found - replace all tokens from relex_start_idx
        let tokens_reused = relex_start_idx;
        let tokens_lexed = new_tokens.len();

        self.tokens.truncate(relex_start_idx);
        self.tokens.append(&mut new_tokens);
        self.errors = new_errors;

        IncrementalStats {
            tokens_reused,
            tokens_lexed,
            full_relex: false,
        }
    }

    /// Find the index of the first token affected by an edit at the given position.
    fn find_first_affected_token(&self, edit_start: usize) -> Option<usize> {
        // Binary search for the first token that overlaps with or comes after edit_start.
        // A token is affected if its span overlaps with the edit or comes after it.
        let idx = self.tokens.partition_point(|t| t.span.end <= edit_start);
        if idx < self.tokens.len() {
            Some(idx)
        } else if !self.tokens.is_empty() {
            // Edit is after all tokens - the edit might be in trailing whitespace/comments.
            // Return the last token so we re-lex from its end position.
            Some(self.tokens.len() - 1)
        } else {
            None
        }
    }

    /// Find a safe position to start re-lexing from.
    ///
    /// This backs up to find a position where the lexer state would be
    /// unambiguous (not inside a string, comment, etc.).
    fn find_safe_relex_start(&self, affected_idx: usize) -> usize {
        // Back up one token when possible. This handles cases where:
        // 1. The edit is at a token boundary and might merge tokens
        // 2. The edit is in whitespace/comments between tokens
        // A more sophisticated implementation could analyze token types.
        if affected_idx > 0 {
            affected_idx - 1
        } else {
            0
        }
    }

    /// Try to find a synchronization point where we can reuse cached tokens.
    fn find_sync_point(
        &self,
        new_token: &Token,
        search_start_idx: usize,
        delta: isize,
        old_source: &str,
    ) -> Option<usize> {
        // Look for a cached token that:
        // 1. Has the same kind as new_token
        // 2. When position-adjusted, would have the same text content
        for (i, old_token) in self.tokens[search_start_idx..].iter().enumerate() {
            let old_idx = search_start_idx + i;

            // Calculate where this old token would be in new positions
            let old_start = old_token.span.start;
            let expected_new_start = (old_start as isize + delta) as usize;

            // Check if positions match
            if new_token.span.start != expected_new_start {
                continue;
            }

            // Check if token kinds match
            if new_token.kind != old_token.kind {
                continue;
            }

            // Check if token text matches
            let old_text = &old_source[old_token.span.start..old_token.span.end];
            let new_text = &self.source[new_token.span.start..new_token.span.end];
            if old_text == new_text {
                return Some(old_idx);
            }
        }
        None
    }

    /// Merge newly lexed tokens with cached tokens at sync point.
    fn merge_with_cached(
        &mut self,
        relex_start_idx: usize,
        sync_idx: usize,
        mut new_tokens: Vec<Token>,
        new_errors: Vec<LexError>,
        delta: isize,
    ) -> IncrementalStats {
        // Tokens before relex_start_idx are unchanged
        // Tokens from relex_start_idx to sync_idx are replaced by new_tokens
        // Tokens from sync_idx+1 onwards are reused with position adjustment

        let tokens_reused_before = relex_start_idx;
        let tokens_lexed = new_tokens.len();

        // Collect tokens to reuse (after sync point, with adjusted positions)
        let reused_after: Vec<Token> = self.tokens[sync_idx + 1..]
            .iter()
            .map(|t| self.shift_token_span(t.clone(), delta))
            .collect();

        let tokens_reused_after = reused_after.len();

        // Build new token list
        self.tokens.truncate(relex_start_idx);
        // Remove the sync token from new_tokens since we counted it in lexed
        // Actually keep it since it's the newly lexed version
        self.tokens.append(&mut new_tokens);
        self.tokens.extend(reused_after);

        // Update errors
        self.errors = new_errors;

        IncrementalStats {
            tokens_reused: tokens_reused_before + tokens_reused_after,
            tokens_lexed,
            full_relex: false,
        }
    }

    /// Lex from a position when edit is after all existing tokens.
    fn lex_from_position(&mut self, start: usize, _delta: isize) -> IncrementalStats {
        if start >= self.source.len() {
            // Nothing to lex
            return IncrementalStats {
                tokens_reused: self.tokens.len(),
                tokens_lexed: 0,
                full_relex: false,
            };
        }

        // Lex from the start position
        let mut lexer = Lexer::new(&self.source[start..], self.language.clone());
        let mut new_tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            if token.kind == TokenKind::Eof {
                break;
            }
            let adjusted = self.adjust_token_span(token, start);
            new_tokens.push(adjusted);
        }

        let tokens_reused = self.tokens.len();
        let tokens_lexed = new_tokens.len();

        self.tokens.append(&mut new_tokens);
        // Note: errors from lexer would need to be adjusted too
        for error in lexer.errors() {
            self.errors.push(self.adjust_error_span(error.clone(), start));
        }

        IncrementalStats {
            tokens_reused,
            tokens_lexed,
            full_relex: false,
        }
    }

    /// Adjust a token's span by adding an offset.
    fn adjust_token_span(&self, mut token: Token, offset: usize) -> Token {
        token.span.start += offset;
        token.span.end += offset;
        // Location is computed fresh by the lexer relative to the slice,
        // so we need to recalculate it for the full source
        token.span.start_loc = self.location_at_byte(token.span.start);
        token.span.end_loc = self.location_at_byte(token.span.end);
        token
    }

    /// Adjust an error's span by adding an offset.
    fn adjust_error_span(&self, mut error: LexError, offset: usize) -> LexError {
        error.span.start += offset;
        error.span.end += offset;
        error.span.start_loc = self.location_at_byte(error.span.start);
        error.span.end_loc = self.location_at_byte(error.span.end);
        error
    }

    /// Shift a token's span by a signed delta.
    fn shift_token_span(&self, mut token: Token, delta: isize) -> Token {
        token.span.start = (token.span.start as isize + delta) as usize;
        token.span.end = (token.span.end as isize + delta) as usize;
        // Recalculate locations
        token.span.start_loc = self.location_at_byte(token.span.start);
        token.span.end_loc = self.location_at_byte(token.span.end);
        token
    }

    /// Calculate the Location at a given byte offset.
    fn location_at_byte(&self, byte_offset: usize) -> Location {
        let mut loc = Location::new();
        for c in self.source[..byte_offset.min(self.source.len())].chars() {
            loc.advance(c);
        }
        loc
    }
}

/// Get the source text for a span from an IncrementalLexer.
impl<L: LanguageSpec + Clone> IncrementalLexer<L> {
    /// Get the source text for a token's span.
    pub fn token_text(&self, token: &Token) -> &str {
        &self.source[token.span.start..token.span.end]
    }

    /// Get the source text for a span.
    pub fn span_text(&self, span: Span) -> &str {
        &self.source[span.start..span.end]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::DefaultLanguage;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::vec;

    #[test]
    fn test_edit_new() {
        let edit = Edit::new(5, 10, "hello");
        assert_eq!(edit.start, 5);
        assert_eq!(edit.end, 10);
        assert_eq!(edit.new_text, "hello");
        assert_eq!(edit.delta(), 0); // 5 removed, 5 added
    }

    #[test]
    fn test_edit_insert() {
        let edit = Edit::insert(5, "hello");
        assert_eq!(edit.delta(), 5);
    }

    #[test]
    fn test_edit_delete() {
        let edit = Edit::delete(5, 10);
        assert_eq!(edit.delta(), -5);
    }

    #[test]
    fn test_incremental_lexer_new() {
        let lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);
        assert_eq!(lexer.source(), "let x = 42;");
    }

    #[test]
    fn test_initial_tokenization() {
        let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5); // let, x, =, 42, ;
    }

    #[test]
    fn test_simple_edit() {
        let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);
        let initial_tokens = lexer.tokens().len();
        assert_eq!(initial_tokens, 5);

        // Change "42" to "100"
        let edit = Edit::new(8, 10, "100");
        let stats = lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "let x = 100;");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5);

        // Verify some tokens were reused
        assert!(!stats.full_relex);
    }

    #[test]
    fn test_insert_at_beginning() {
        let mut lexer = IncrementalLexer::new("x = 42;", DefaultLanguage);

        let edit = Edit::insert(0, "let ");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "let x = 42;");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5);
    }

    #[test]
    fn test_delete_token() {
        let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);

        // Delete "let "
        let edit = Edit::delete(0, 4);
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "x = 42;");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 4); // x, =, 42, ;
    }

    #[test]
    fn test_insert_in_middle() {
        let mut lexer = IncrementalLexer::new("let x = ;", DefaultLanguage);

        // Insert "42"
        let edit = Edit::insert(8, "42");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "let x = 42;");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5);
    }

    #[test]
    fn test_multiple_edits() {
        let mut lexer = IncrementalLexer::new("let a = 1;", DefaultLanguage);

        // Change 'a' to 'x'
        lexer.apply_edit(Edit::new(4, 5, "x"));
        assert_eq!(lexer.source(), "let x = 1;");

        // Change '1' to '42'
        lexer.apply_edit(Edit::new(8, 9, "42"));
        assert_eq!(lexer.source(), "let x = 42;");

        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5);
    }

    #[test]
    fn test_edit_changes_token_boundary() {
        let mut lexer = IncrementalLexer::new("foo bar", DefaultLanguage);

        // Remove space, making it "foobar"
        let edit = Edit::delete(3, 4);
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "foobar");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 1); // Single identifier
    }

    #[test]
    fn test_edit_splits_token() {
        let mut lexer = IncrementalLexer::new("foobar", DefaultLanguage);

        // Insert space, making it "foo bar"
        let edit = Edit::insert(3, " ");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "foo bar");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 2); // Two identifiers
    }

    #[test]
    fn test_empty_source() {
        let mut lexer = IncrementalLexer::new("", DefaultLanguage);
        let tokens = lexer.tokens();
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_edit_empty_source() {
        let mut lexer = IncrementalLexer::new("", DefaultLanguage);

        let edit = Edit::insert(0, "let x = 42;");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "let x = 42;");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5);
    }

    #[test]
    fn test_delete_all() {
        let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);

        let edit = Edit::delete(0, 11);
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "");
        let tokens = lexer.tokens();
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_token_positions_after_edit() {
        let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);

        // Insert "long_" before "x", making it "let long_x = 42;"
        let edit = Edit::insert(4, "long_");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "let long_x = 42;");
        let tokens: Vec<_> = lexer.tokens().to_vec();

        // Verify positions are correct
        assert_eq!(lexer.token_text(&tokens[0]), "let");
        assert_eq!(lexer.token_text(&tokens[1]), "long_x");
        assert_eq!(lexer.token_text(&tokens[2]), "=");
        assert_eq!(lexer.token_text(&tokens[3]), "42");
        assert_eq!(lexer.token_text(&tokens[4]), ";");

        // Check that spans are correct
        assert_eq!(tokens[0].span.start, 0);
        assert_eq!(tokens[0].span.end, 3);
        assert_eq!(tokens[1].span.start, 4);
        assert_eq!(tokens[1].span.end, 10);
    }

    #[test]
    fn test_location_tracking_after_edit() {
        let mut lexer = IncrementalLexer::new("a\nb", DefaultLanguage);

        // Insert "x\n" at the start
        let edit = Edit::insert(0, "x\n");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "x\na\nb");
        let tokens = lexer.tokens();

        // Verify line numbers
        assert_eq!(tokens[0].span.start_loc.line, 1); // 'x'
        assert_eq!(tokens[1].span.start_loc.line, 2); // 'a'
        assert_eq!(tokens[2].span.start_loc.line, 3); // 'b'
    }

    #[test]
    fn test_string_literal_edit() {
        let mut lexer = IncrementalLexer::new(r#"let s = "hello";"#, DefaultLanguage);

        // Change "hello" to "world"
        let edit = Edit::new(9, 14, "world");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), r#"let s = "world";"#);
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[3].kind, TokenKind::StringLiteral);
    }

    #[test]
    fn test_comment_edit() {
        let mut lexer = IncrementalLexer::new("let x = 42; // comment", DefaultLanguage);

        // Modify the comment
        let edit = Edit::new(15, 22, "updated");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "let x = 42; // updated");
        let tokens = lexer.tokens();
        // Comments are skipped by default, so still 5 tokens
        assert_eq!(tokens.len(), 5);
    }

    #[test]
    fn test_append_to_end() {
        let mut lexer = IncrementalLexer::new("let x = 42", DefaultLanguage);

        // Append semicolon
        let edit = Edit::insert(10, ";");
        let stats = lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "let x = 42;");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5);

        // Some tokens should be reused (we back up for safety, so not all are reused)
        assert!(stats.tokens_reused >= 2);
        assert!(!stats.full_relex);
    }

    #[test]
    fn test_invalid_edit_bounds() {
        let mut lexer = IncrementalLexer::new("hello", DefaultLanguage);

        // Invalid edit (start > end)
        let edit = Edit::new(5, 3, "x");
        lexer.apply_edit(edit);

        // Source should be unchanged
        assert_eq!(lexer.source(), "hello");
    }

    #[test]
    fn test_edit_out_of_bounds() {
        let mut lexer = IncrementalLexer::new("hello", DefaultLanguage);

        // Edit beyond source length
        let edit = Edit::new(10, 15, "x");
        lexer.apply_edit(edit);

        // Source should be unchanged
        assert_eq!(lexer.source(), "hello");
    }

    #[test]
    fn test_stats_full_relex() {
        let lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);

        // Initial tokenization is a full relex
        assert!(lexer.last_stats().full_relex);
        assert_eq!(lexer.last_stats().tokens_lexed, 5);
        assert_eq!(lexer.last_stats().tokens_reused, 0);
    }

    #[test]
    fn test_set_source() {
        let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);

        lexer.set_source("fn main() {}");
        assert_eq!(lexer.source(), "fn main() {}");
        // Verify new tokens
        let token_count = lexer.tokens().len();
        assert!(token_count >= 4); // fn, main, (, ), {, }
    }

    #[test]
    fn test_relex_all() {
        let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);
        let initial_tokens = lexer.tokens().to_vec();

        lexer.relex_all();
        let new_tokens = lexer.tokens();

        assert_eq!(initial_tokens.len(), new_tokens.len());
        assert!(lexer.last_stats().full_relex);
    }

    #[test]
    fn test_errors_after_edit() {
        let mut lexer = IncrementalLexer::new(r#"let s = "hello";"#, DefaultLanguage);

        // Make the string unterminated
        let edit = Edit::delete(14, 15); // Remove closing quote
        lexer.apply_edit(edit);

        // Should now have an error
        assert!(!lexer.errors().is_empty());
    }

    #[test]
    fn test_fix_error_with_edit() {
        let mut lexer = IncrementalLexer::new(r#"let s = "hello;"#, DefaultLanguage);
        assert!(!lexer.errors().is_empty()); // Unterminated string

        // Fix by adding closing quote
        let edit = Edit::insert(14, "\"");
        lexer.apply_edit(edit);

        assert!(lexer.errors().is_empty());
    }

    #[test]
    fn test_unicode_edit() {
        let mut lexer = IncrementalLexer::new("let 日本語 = 42;", DefaultLanguage);

        // Edit after unicode identifier
        // "let " (4 bytes) + "日本語" (9 bytes) + " = " (3 bytes) = 16 bytes before "42"
        // "42" is at bytes 16-18
        let edit = Edit::new(16, 18, "100");
        lexer.apply_edit(edit);

        assert_eq!(lexer.source(), "let 日本語 = 100;");
        let tokens = lexer.tokens();
        assert_eq!(tokens.len(), 5);
    }
}
