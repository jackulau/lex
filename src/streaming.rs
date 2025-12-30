//! Async/streaming tokenization for large inputs.
//!
//! This module provides memory-efficient tokenization strategies for processing
//! large source files without loading all tokens into memory at once.
//!
//! # Features
//!
//! - **Streaming Iterator**: Lazy token generation with minimal memory footprint
//! - **Chunked Processing**: Process large files in configurable chunks
//! - **Callback API**: Real-time token handling via visitor pattern
//! - **Batch Processing**: Efficient batch token retrieval
//!
//! # Example
//!
//! ```rust
//! use lex::{StreamingLexer, DefaultLanguage};
//!
//! // Streaming iteration
//! let mut lexer = StreamingLexer::new("let x = 42;", DefaultLanguage);
//! while let Some(token) = lexer.next_token() {
//!     println!("{:?}", token);
//! }
//!
//! // Callback-based processing
//! let mut lexer = StreamingLexer::new("let x = 42;", DefaultLanguage);
//! lexer.process(|token| {
//!     println!("Token: {:?}", token);
//!     true // continue processing
//! });
//! ```

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::string::{String, ToString};
#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::Vec;

use crate::error::LexError;
use crate::language::LanguageSpec;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

/// Configuration for streaming tokenization.
#[derive(Debug, Clone)]
pub struct StreamingConfig {
    /// Chunk size in bytes for processing large inputs.
    /// Default: 64KB
    pub chunk_size: usize,
    /// Buffer size for token batching.
    /// Default: 256 tokens
    pub batch_size: usize,
    /// Whether to preserve whitespace tokens.
    pub preserve_whitespace: bool,
    /// Whether to preserve comment tokens.
    pub preserve_comments: bool,
}

impl Default for StreamingConfig {
    fn default() -> Self {
        Self {
            chunk_size: 64 * 1024, // 64KB
            batch_size: 256,
            preserve_whitespace: false,
            preserve_comments: false,
        }
    }
}

impl StreamingConfig {
    /// Create a new streaming configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the chunk size for processing.
    pub fn chunk_size(mut self, size: usize) -> Self {
        self.chunk_size = size;
        self
    }

    /// Set the batch size for token retrieval.
    pub fn batch_size(mut self, size: usize) -> Self {
        self.batch_size = size;
        self
    }

    /// Enable whitespace preservation.
    pub fn preserve_whitespace(mut self, preserve: bool) -> Self {
        self.preserve_whitespace = preserve;
        self
    }

    /// Enable comment preservation.
    pub fn preserve_comments(mut self, preserve: bool) -> Self {
        self.preserve_comments = preserve;
        self
    }
}

/// A streaming lexer that processes tokens lazily.
///
/// Unlike the standard `Lexer` which can collect all tokens at once,
/// `StreamingLexer` is designed for memory-efficient processing of large inputs.
pub struct StreamingLexer<'a, L: LanguageSpec> {
    /// Underlying lexer.
    lexer: Lexer<'a, L>,
    /// Current position in tokens (for tracking).
    token_count: usize,
    /// Whether EOF has been reached.
    finished: bool,
    /// Configuration.
    config: StreamingConfig,
}

impl<'a, L: LanguageSpec> StreamingLexer<'a, L> {
    /// Create a new streaming lexer.
    pub fn new(source: &'a str, language: L) -> Self {
        Self {
            lexer: Lexer::new(source, language),
            token_count: 0,
            finished: false,
            config: StreamingConfig::default(),
        }
    }

    /// Create a new streaming lexer with custom configuration.
    pub fn with_config(source: &'a str, language: L, config: StreamingConfig) -> Self {
        Self {
            lexer: Lexer::new(source, language),
            token_count: 0,
            finished: false,
            config,
        }
    }

    /// Get the next token, or None if finished.
    pub fn next_token(&mut self) -> Option<Token> {
        if self.finished {
            return None;
        }

        let token = self.lexer.next_token();
        if token.kind == TokenKind::Eof {
            self.finished = true;
            return None;
        }

        self.token_count += 1;
        Some(token)
    }

    /// Peek at the next token without consuming it.
    pub fn peek(&mut self) -> Option<&Token> {
        if self.finished {
            return None;
        }

        let token = self.lexer.peek();
        if token.kind == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    }

    /// Get the number of tokens processed so far.
    pub fn token_count(&self) -> usize {
        self.token_count
    }

    /// Check if the lexer has finished processing.
    pub fn is_finished(&self) -> bool {
        self.finished
    }

    /// Get any accumulated errors.
    pub fn errors(&self) -> &[LexError] {
        self.lexer.errors()
    }

    /// Check if there were any errors.
    pub fn has_errors(&self) -> bool {
        self.lexer.has_errors()
    }

    /// Get the configuration.
    pub fn config(&self) -> &StreamingConfig {
        &self.config
    }

    /// Get the source text for a token.
    pub fn token_text(&self, token: &Token) -> &'a str {
        self.lexer.source_text(token.span)
    }

    /// Process all tokens with a callback.
    ///
    /// The callback receives each token and returns `true` to continue
    /// or `false` to stop early.
    ///
    /// Returns the number of tokens processed.
    pub fn process<F>(&mut self, mut callback: F) -> usize
    where
        F: FnMut(Token) -> bool,
    {
        let mut count = 0;
        while let Some(token) = self.next_token() {
            count += 1;
            if !callback(token) {
                break;
            }
        }
        count
    }

    /// Process all tokens with a callback that also receives the source text.
    ///
    /// Returns the number of tokens processed.
    pub fn process_with_text<F>(&mut self, mut callback: F) -> usize
    where
        F: FnMut(Token, &str) -> bool,
    {
        let mut count = 0;
        while let Some(token) = self.next_token() {
            let text = self.lexer.source_text(token.span);
            count += 1;
            if !callback(token, text) {
                break;
            }
        }
        count
    }

    /// Collect the next batch of tokens.
    ///
    /// Returns up to `batch_size` tokens, or fewer if EOF is reached.
    pub fn next_batch(&mut self) -> Vec<Token> {
        let mut batch = Vec::with_capacity(self.config.batch_size);
        for _ in 0..self.config.batch_size {
            match self.next_token() {
                Some(token) => batch.push(token),
                None => break,
            }
        }
        batch
    }

    /// Create an iterator over tokens.
    pub fn iter(&mut self) -> StreamingTokenIterator<'_, 'a, L> {
        StreamingTokenIterator { lexer: self }
    }

    /// Skip tokens until a predicate is satisfied.
    ///
    /// Returns the first token matching the predicate, or None if EOF.
    pub fn skip_until<F>(&mut self, mut predicate: F) -> Option<Token>
    where
        F: FnMut(&Token) -> bool,
    {
        while let Some(token) = self.next_token() {
            if predicate(&token) {
                return Some(token);
            }
        }
        None
    }

    /// Take tokens while a predicate is satisfied.
    ///
    /// Returns all tokens that match the predicate.
    pub fn take_while<F>(&mut self, mut predicate: F) -> Vec<Token>
    where
        F: FnMut(&Token) -> bool,
    {
        let mut tokens = Vec::new();
        while let Some(token) = self.peek() {
            if !predicate(token) {
                break;
            }
            if let Some(t) = self.next_token() {
                tokens.push(t);
            }
        }
        tokens
    }

    /// Find the next token of a specific kind.
    pub fn find_kind(&mut self, kind: TokenKind) -> Option<Token> {
        self.skip_until(|t| t.kind == kind)
    }

    /// Collect all tokens matching a filter.
    pub fn filter_collect<F>(&mut self, mut predicate: F) -> Vec<Token>
    where
        F: FnMut(&Token) -> bool,
    {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            if predicate(&token) {
                tokens.push(token);
            }
        }
        tokens
    }
}

/// An iterator adapter for `StreamingLexer`.
pub struct StreamingTokenIterator<'l, 'a, L: LanguageSpec> {
    lexer: &'l mut StreamingLexer<'a, L>,
}

impl<'l, 'a, L: LanguageSpec> Iterator for StreamingTokenIterator<'l, 'a, L> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next_token()
    }
}

/// A chunked tokenizer for processing very large inputs.
///
/// This tokenizer processes the input in fixed-size chunks, maintaining
/// state across chunk boundaries for proper token handling.
pub struct ChunkedTokenizer<L: LanguageSpec + Clone> {
    /// Source text being processed.
    source: String,
    /// Current byte position in source.
    position: usize,
    /// Language specification.
    language: L,
    /// Configuration.
    config: StreamingConfig,
    /// Buffer for partial tokens at chunk boundaries.
    partial_buffer: String,
    /// Total tokens produced.
    total_tokens: usize,
    /// Accumulated errors.
    errors: Vec<LexError>,
}

impl<L: LanguageSpec + Clone> ChunkedTokenizer<L> {
    /// Create a new chunked tokenizer.
    pub fn new(source: impl Into<String>, language: L) -> Self {
        Self {
            source: source.into(),
            position: 0,
            language,
            config: StreamingConfig::default(),
            partial_buffer: String::new(),
            total_tokens: 0,
            errors: Vec::new(),
        }
    }

    /// Create a new chunked tokenizer with configuration.
    pub fn with_config(source: impl Into<String>, language: L, config: StreamingConfig) -> Self {
        Self {
            source: source.into(),
            position: 0,
            language,
            config,
            partial_buffer: String::new(),
            total_tokens: 0,
            errors: Vec::new(),
        }
    }

    /// Get the source text.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get the current position.
    pub fn position(&self) -> usize {
        self.position
    }

    /// Check if processing is complete.
    pub fn is_finished(&self) -> bool {
        self.position >= self.source.len()
    }

    /// Get total tokens produced.
    pub fn total_tokens(&self) -> usize {
        self.total_tokens
    }

    /// Get accumulated errors.
    pub fn errors(&self) -> &[LexError] {
        &self.errors
    }

    /// Process the next chunk and return tokens.
    ///
    /// Returns `None` when all input has been processed.
    pub fn next_chunk(&mut self) -> Option<Vec<Token>> {
        if self.is_finished() && self.partial_buffer.is_empty() {
            return None;
        }

        // Calculate chunk bounds
        let chunk_end = (self.position + self.config.chunk_size).min(self.source.len());

        // Find a safe boundary (end of line or whitespace if possible)
        let safe_end = self.find_safe_boundary(chunk_end);

        // Build the chunk to process (partial buffer + new chunk)
        let chunk = if self.partial_buffer.is_empty() {
            &self.source[self.position..safe_end]
        } else {
            self.partial_buffer.push_str(&self.source[self.position..safe_end]);
            self.partial_buffer.as_str()
        };

        // Tokenize the chunk
        let (tokens, errors) = Lexer::tokenize(chunk, self.language.clone());

        // Handle potential partial token at the end
        let (complete_tokens, leftover) = self.handle_chunk_boundary(&tokens, chunk, safe_end);

        // Update state
        self.partial_buffer = leftover;
        self.position = safe_end;
        self.total_tokens += complete_tokens.len();
        self.errors.extend(errors);

        Some(complete_tokens)
    }

    /// Find a safe chunk boundary (preferably at whitespace or newline).
    fn find_safe_boundary(&self, target: usize) -> usize {
        if target >= self.source.len() {
            return self.source.len();
        }

        // Look backward for a safe boundary (whitespace or newline)
        let search_range = target.saturating_sub(64)..target;
        for i in (search_range.start..search_range.end).rev() {
            let c = self.source.as_bytes().get(i).copied();
            if matches!(c, Some(b'\n') | Some(b' ') | Some(b'\t')) {
                // Make sure we're at a char boundary
                if self.source.is_char_boundary(i + 1) {
                    return i + 1;
                }
            }
        }

        // No safe boundary found, use target if it's a char boundary
        if self.source.is_char_boundary(target) {
            target
        } else {
            // Find next char boundary
            (target..self.source.len())
                .find(|&i| self.source.is_char_boundary(i))
                .unwrap_or(self.source.len())
        }
    }

    /// Handle potential partial tokens at chunk boundaries.
    fn handle_chunk_boundary(
        &self,
        tokens: &[Token],
        _chunk: &str,
        chunk_end: usize,
    ) -> (Vec<Token>, String) {
        if tokens.is_empty() {
            return (Vec::new(), String::new());
        }

        // Check if the last token might be incomplete
        // (e.g., string/comment that continues into next chunk)
        let last_token = &tokens[tokens.len() - 1];

        // If we're not at EOF and the last token ends exactly at chunk boundary,
        // it might be partial (especially for strings/comments)
        if chunk_end < self.source.len()
            && last_token.span.end == _chunk.len()
            && Self::is_potentially_incomplete(last_token.kind)
        {
            // Keep the last token in the partial buffer
            let complete = tokens[..tokens.len() - 1].to_vec();
            let leftover = _chunk[last_token.span.start..].to_string();
            (complete, leftover)
        } else {
            (tokens.to_vec(), String::new())
        }
    }

    /// Check if a token kind might be incomplete at a chunk boundary.
    fn is_potentially_incomplete(kind: TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::StringLiteral
                | TokenKind::RawStringLiteral
                | TokenKind::CharLiteral
                | TokenKind::Comment
                | TokenKind::Error
        )
    }

    /// Process all chunks with a callback.
    ///
    /// The callback receives each chunk's tokens and returns `true` to continue
    /// or `false` to stop.
    pub fn process_all<F>(&mut self, mut callback: F)
    where
        F: FnMut(Vec<Token>) -> bool,
    {
        while let Some(tokens) = self.next_chunk() {
            if !callback(tokens) {
                break;
            }
        }
    }

    /// Collect all tokens (processes entire input).
    pub fn collect_all(&mut self) -> Vec<Token> {
        let mut all_tokens = Vec::new();
        while let Some(mut tokens) = self.next_chunk() {
            all_tokens.append(&mut tokens);
        }
        all_tokens
    }

    /// Reset the tokenizer to the beginning.
    pub fn reset(&mut self) {
        self.position = 0;
        self.partial_buffer.clear();
        self.total_tokens = 0;
        self.errors.clear();
    }
}

/// Statistics about streaming tokenization.
#[derive(Debug, Clone, Copy, Default)]
pub struct StreamingStats {
    /// Total tokens produced.
    pub total_tokens: usize,
    /// Number of chunks processed.
    pub chunks_processed: usize,
    /// Total bytes processed.
    pub bytes_processed: usize,
    /// Number of errors encountered.
    pub error_count: usize,
}

/// A token visitor trait for callback-based processing.
pub trait TokenVisitor {
    /// Called for each token.
    /// Return `true` to continue, `false` to stop.
    fn visit(&mut self, token: &Token, text: &str) -> bool;

    /// Called when an error is encountered.
    fn on_error(&mut self, _error: &LexError) {}

    /// Called when processing is complete.
    fn on_complete(&mut self, _stats: StreamingStats) {}
}

/// Process source with a visitor.
pub fn visit_tokens<L: LanguageSpec, V: TokenVisitor>(
    source: &str,
    language: L,
    visitor: &mut V,
) -> StreamingStats {
    let mut lexer = StreamingLexer::new(source, language);
    let mut stats = StreamingStats::default();

    while let Some(token) = lexer.next_token() {
        let text = lexer.token_text(&token);
        stats.total_tokens += 1;
        if !visitor.visit(&token, text) {
            break;
        }
    }

    for error in lexer.errors() {
        visitor.on_error(error);
        stats.error_count += 1;
    }

    stats.bytes_processed = source.len();
    visitor.on_complete(stats);
    stats
}

/// A buffered token stream that can look ahead multiple tokens.
pub struct BufferedTokenStream<'a, L: LanguageSpec> {
    /// Underlying streaming lexer.
    lexer: StreamingLexer<'a, L>,
    /// Lookahead buffer.
    buffer: Vec<Token>,
    /// Current position in buffer.
    position: usize,
}

impl<'a, L: LanguageSpec> BufferedTokenStream<'a, L> {
    /// Create a new buffered token stream.
    pub fn new(source: &'a str, language: L) -> Self {
        Self {
            lexer: StreamingLexer::new(source, language),
            buffer: Vec::new(),
            position: 0,
        }
    }

    /// Ensure the buffer has at least `n` tokens ahead.
    fn ensure_buffer(&mut self, n: usize) {
        while self.buffer.len() < self.position + n {
            match self.lexer.next_token() {
                Some(token) => self.buffer.push(token),
                None => break,
            }
        }
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Option<Token> {
        self.ensure_buffer(1);
        if self.position < self.buffer.len() {
            let token = self.buffer[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    /// Peek at the token `n` positions ahead (0 = next token).
    pub fn peek(&mut self, n: usize) -> Option<&Token> {
        self.ensure_buffer(n + 1);
        self.buffer.get(self.position + n)
    }

    /// Look at the next token without consuming.
    pub fn peek_next(&mut self) -> Option<&Token> {
        self.peek(0)
    }

    /// Check if there are more tokens.
    pub fn has_more(&mut self) -> bool {
        self.peek(0).is_some()
    }

    /// Get the source text for a token.
    pub fn token_text(&self, token: &Token) -> &'a str {
        self.lexer.token_text(token)
    }

    /// Consume tokens while a predicate is true.
    pub fn consume_while<F>(&mut self, mut predicate: F) -> Vec<Token>
    where
        F: FnMut(&Token) -> bool,
    {
        let mut tokens = Vec::new();
        while let Some(token) = self.peek_next() {
            if !predicate(token) {
                break;
            }
            if let Some(t) = self.next_token() {
                tokens.push(t);
            }
        }
        tokens
    }

    /// Skip tokens while a predicate is true.
    pub fn skip_while<F>(&mut self, mut predicate: F) -> usize
    where
        F: FnMut(&Token) -> bool,
    {
        let mut count = 0;
        while let Some(token) = self.peek_next() {
            if !predicate(token) {
                break;
            }
            self.next_token();
            count += 1;
        }
        count
    }

    /// Get accumulated errors.
    pub fn errors(&self) -> &[LexError] {
        self.lexer.errors()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::DefaultLanguage;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::vec;

    #[test]
    fn test_streaming_lexer_basic() {
        let mut lexer = StreamingLexer::new("let x = 42;", DefaultLanguage);

        let mut tokens = Vec::new();
        while let Some(token) = lexer.next_token() {
            tokens.push(token);
        }

        assert_eq!(tokens.len(), 5);
        assert!(lexer.is_finished());
    }

    #[test]
    fn test_streaming_lexer_peek() {
        let mut lexer = StreamingLexer::new("let x", DefaultLanguage);

        // Peek should not consume
        let peeked = lexer.peek().map(|t| t.kind);
        assert!(matches!(peeked, Some(TokenKind::Keyword(_))));

        // Next should give same token
        let next = lexer.next_token().map(|t| t.kind);
        assert!(matches!(next, Some(TokenKind::Keyword(_))));

        // Second token
        let next2 = lexer.next_token().map(|t| t.kind);
        assert_eq!(next2, Some(TokenKind::Ident));
    }

    #[test]
    fn test_streaming_lexer_callback() {
        let mut lexer = StreamingLexer::new("a b c", DefaultLanguage);
        let mut count = 0;

        lexer.process(|_token| {
            count += 1;
            true
        });

        assert_eq!(count, 3);
    }

    #[test]
    fn test_streaming_lexer_early_stop() {
        let mut lexer = StreamingLexer::new("a b c d e", DefaultLanguage);
        let mut count = 0;

        lexer.process(|_token| {
            count += 1;
            count < 3 // Stop after 3 tokens
        });

        assert_eq!(count, 3);
        assert!(!lexer.is_finished()); // Should have more tokens
    }

    #[test]
    fn test_streaming_lexer_batch() {
        let config = StreamingConfig::default().batch_size(2);
        let mut lexer = StreamingLexer::with_config("a b c d e", DefaultLanguage, config);

        let batch1 = lexer.next_batch();
        assert_eq!(batch1.len(), 2);

        let batch2 = lexer.next_batch();
        assert_eq!(batch2.len(), 2);

        let batch3 = lexer.next_batch();
        assert_eq!(batch3.len(), 1);

        let batch4 = lexer.next_batch();
        assert!(batch4.is_empty());
    }

    #[test]
    fn test_streaming_lexer_skip_until() {
        let mut lexer = StreamingLexer::new("a b c d", DefaultLanguage);

        // Skip until we find an identifier at position 4 (the 'c')
        let mut count = 0;
        let found = lexer.skip_until(|_t| {
            count += 1;
            count == 3 // Find the third token (c)
        });

        assert!(found.is_some());
        assert_eq!(lexer.token_count(), 3); // a, b, c processed
    }

    #[test]
    fn test_streaming_lexer_take_while() {
        let mut lexer = StreamingLexer::new("a b c 42 d", DefaultLanguage);

        // Take while identifiers
        let idents = lexer.take_while(|t| t.kind == TokenKind::Ident);
        assert_eq!(idents.len(), 3); // a, b, c
    }

    #[test]
    fn test_streaming_lexer_filter_collect() {
        let mut lexer = StreamingLexer::new("let x = 42; let y = 100;", DefaultLanguage);

        // Collect only identifiers
        let idents = lexer.filter_collect(|t| t.kind == TokenKind::Ident);
        assert_eq!(idents.len(), 2); // x, y
    }

    #[test]
    fn test_chunked_tokenizer_basic() {
        let source = "let x = 42; let y = 100;";
        let mut tokenizer = ChunkedTokenizer::new(source, DefaultLanguage);

        let all_tokens = tokenizer.collect_all();
        assert_eq!(all_tokens.len(), 10); // let, x, =, 42, ;, let, y, =, 100, ;
    }

    #[test]
    fn test_chunked_tokenizer_small_chunks() {
        let source = "let x = 42;";
        let config = StreamingConfig::default().chunk_size(4);
        let mut tokenizer = ChunkedTokenizer::with_config(source, DefaultLanguage, config);

        let all_tokens = tokenizer.collect_all();
        assert_eq!(all_tokens.len(), 5);
    }

    #[test]
    fn test_chunked_tokenizer_callback() {
        let source = "a b c d e";
        let mut tokenizer = ChunkedTokenizer::new(source, DefaultLanguage);
        let mut chunk_count = 0;

        tokenizer.process_all(|tokens| {
            chunk_count += 1;
            !tokens.is_empty()
        });

        assert!(chunk_count >= 1);
    }

    #[test]
    fn test_chunked_tokenizer_reset() {
        let source = "let x = 42;";
        let mut tokenizer = ChunkedTokenizer::new(source, DefaultLanguage);

        let tokens1 = tokenizer.collect_all();
        tokenizer.reset();
        let tokens2 = tokenizer.collect_all();

        assert_eq!(tokens1.len(), tokens2.len());
    }

    #[test]
    fn test_buffered_token_stream() {
        let mut stream = BufferedTokenStream::new("a b c d", DefaultLanguage);

        // Peek ahead
        assert!(stream.peek(0).is_some());
        assert!(stream.peek(1).is_some());
        assert!(stream.peek(2).is_some());

        // Consume first
        let first = stream.next_token();
        assert!(first.is_some());

        // Peek should now show b, c, d
        assert!(stream.peek(0).is_some());
    }

    #[test]
    fn test_buffered_stream_consume_while() {
        let mut stream = BufferedTokenStream::new("a b c 42 d", DefaultLanguage);

        let idents = stream.consume_while(|t| t.kind == TokenKind::Ident);
        assert_eq!(idents.len(), 3);

        // Next should be the int
        let next = stream.next_token();
        assert_eq!(next.map(|t| t.kind), Some(TokenKind::IntLiteral));
    }

    struct CountingVisitor {
        count: usize,
        error_count: usize,
    }

    impl TokenVisitor for CountingVisitor {
        fn visit(&mut self, _token: &Token, _text: &str) -> bool {
            self.count += 1;
            true
        }

        fn on_error(&mut self, _error: &LexError) {
            self.error_count += 1;
        }
    }

    #[test]
    fn test_token_visitor() {
        let mut visitor = CountingVisitor {
            count: 0,
            error_count: 0,
        };

        let stats = visit_tokens("let x = 42;", DefaultLanguage, &mut visitor);

        assert_eq!(visitor.count, 5);
        assert_eq!(stats.total_tokens, 5);
    }

    #[test]
    fn test_streaming_config() {
        let config = StreamingConfig::new()
            .chunk_size(1024)
            .batch_size(100)
            .preserve_whitespace(true)
            .preserve_comments(true);

        assert_eq!(config.chunk_size, 1024);
        assert_eq!(config.batch_size, 100);
        assert!(config.preserve_whitespace);
        assert!(config.preserve_comments);
    }

    #[test]
    fn test_empty_source() {
        let mut lexer = StreamingLexer::new("", DefaultLanguage);
        assert!(lexer.next_token().is_none());
        assert!(lexer.is_finished());
    }

    #[test]
    fn test_streaming_stats() {
        let stats = StreamingStats::default();
        assert_eq!(stats.total_tokens, 0);
        assert_eq!(stats.chunks_processed, 0);
        assert_eq!(stats.bytes_processed, 0);
        assert_eq!(stats.error_count, 0);
    }
}
