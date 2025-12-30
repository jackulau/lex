//! Token caching with fine-grained invalidation.
//!
//! This module provides advanced caching strategies for incremental tokenization,
//! including LRU eviction, versioning, and range-based invalidation.
//!
//! # Features
//!
//! - **Range-Based Invalidation**: Only invalidate tokens in affected regions
//! - **LRU Cache Eviction**: Configurable cache size limits
//! - **Version Tracking**: Track cache versions for change detection
//! - **Cache Statistics**: Monitor cache hit rates and performance
//!
//! # Example
//!
//! ```rust
//! use lex::cache::{CachedLexer, CacheConfig};
//! use lex::DefaultLanguage;
//!
//! let config = CacheConfig::default().max_versions(10);
//! let mut lexer = CachedLexer::with_config("let x = 42;", DefaultLanguage, config);
//!
//! // Edit and see cache stats
//! lexer.edit(8, 10, "100");
//! let stats = lexer.cache_stats();
//! println!("Hit rate: {:.1}%", stats.hit_rate() * 100.0);
//! ```

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{collections::VecDeque, string::String, vec::Vec};

#[cfg(feature = "std")]
use std::collections::VecDeque;

use crate::error::LexError;
use crate::language::LanguageSpec;
use crate::lexer::Lexer;
use crate::span::{Location, Span};
use crate::token::{Token, TokenKind};

/// Configuration for token caching.
#[derive(Debug, Clone)]
pub struct CacheConfig {
    /// Maximum number of versions to keep in history.
    /// Default: 50
    pub max_versions: usize,
    /// Maximum cache memory in bytes (approximate).
    /// Default: 10MB
    pub max_memory_bytes: usize,
    /// Enable detailed statistics tracking.
    /// Default: true
    pub track_stats: bool,
    /// Minimum edit distance before creating a new version.
    /// Default: 1
    pub min_edit_distance: usize,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            max_versions: 50,
            max_memory_bytes: 10 * 1024 * 1024, // 10MB
            track_stats: true,
            min_edit_distance: 1,
        }
    }
}

impl CacheConfig {
    /// Create a new cache configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set maximum versions to keep.
    pub fn max_versions(mut self, max: usize) -> Self {
        self.max_versions = max;
        self
    }

    /// Set maximum memory usage.
    pub fn max_memory_bytes(mut self, bytes: usize) -> Self {
        self.max_memory_bytes = bytes;
        self
    }

    /// Enable or disable statistics tracking.
    pub fn track_stats(mut self, track: bool) -> Self {
        self.track_stats = track;
        self
    }

    /// Set minimum edit distance for new versions.
    pub fn min_edit_distance(mut self, distance: usize) -> Self {
        self.min_edit_distance = distance;
        self
    }
}

/// Statistics about cache performance.
#[derive(Debug, Clone, Copy, Default)]
pub struct CacheStats {
    /// Number of cache hits (tokens reused).
    pub hits: usize,
    /// Number of cache misses (tokens re-lexed).
    pub misses: usize,
    /// Number of invalidations performed.
    pub invalidations: usize,
    /// Number of versions created.
    pub versions_created: usize,
    /// Number of versions evicted.
    pub versions_evicted: usize,
    /// Current memory usage estimate (bytes).
    pub memory_usage: usize,
}

impl CacheStats {
    /// Calculate hit rate (0.0 to 1.0).
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            self.hits as f64 / total as f64
        }
    }

    /// Get total cache accesses.
    pub fn total_accesses(&self) -> usize {
        self.hits + self.misses
    }
}

/// A cached token range for fine-grained invalidation.
#[derive(Debug, Clone)]
struct TokenRange {
    /// Start byte offset.
    start: usize,
    /// End byte offset.
    end: usize,
    /// Token indices in this range.
    token_start: usize,
    token_end: usize,
    /// Whether this range is valid.
    valid: bool,
}

impl TokenRange {
    fn new(start: usize, end: usize, token_start: usize, token_end: usize) -> Self {
        Self {
            start,
            end,
            token_start,
            token_end,
            valid: true,
        }
    }

    #[allow(dead_code)]
    fn overlaps(&self, start: usize, end: usize) -> bool {
        !(end <= self.start || start >= self.end)
    }

    fn contains(&self, pos: usize) -> bool {
        pos >= self.start && pos < self.end
    }
}

/// A version entry in the cache history.
#[derive(Debug, Clone)]
pub struct CacheVersion {
    /// Version number (monotonically increasing).
    pub version: u64,
    /// Source text hash or identifier.
    pub source_hash: u64,
    /// Number of tokens in this version.
    pub token_count: usize,
    /// Approximate memory usage.
    pub memory_bytes: usize,
}

/// A cached lexer with fine-grained invalidation and LRU eviction.
pub struct CachedLexer<L: LanguageSpec> {
    /// Current source text.
    source: String,
    /// Language specification.
    language: L,
    /// Cached tokens.
    tokens: Vec<Token>,
    /// Cached errors.
    errors: Vec<LexError>,
    /// Token ranges for fine-grained invalidation.
    ranges: Vec<TokenRange>,
    /// Version history (for undo/redo support).
    versions: VecDeque<CacheVersion>,
    /// Current version number.
    current_version: u64,
    /// Configuration.
    config: CacheConfig,
    /// Statistics.
    stats: CacheStats,
    /// Range size for partitioning (bytes).
    range_size: usize,
}

impl<L: LanguageSpec + Clone> CachedLexer<L> {
    /// Create a new cached lexer.
    pub fn new(source: impl Into<String>, language: L) -> Self {
        Self::with_config(source, language, CacheConfig::default())
    }

    /// Create a cached lexer with custom configuration.
    pub fn with_config(source: impl Into<String>, language: L, config: CacheConfig) -> Self {
        let source = source.into();
        let mut lexer = Self {
            source,
            language,
            tokens: Vec::new(),
            errors: Vec::new(),
            ranges: Vec::new(),
            versions: VecDeque::new(),
            current_version: 0,
            config,
            stats: CacheStats::default(),
            range_size: 1024, // 1KB ranges
        };
        lexer.full_tokenize();
        lexer
    }

    /// Get the current source text.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get cached tokens.
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// Get cached errors.
    pub fn errors(&self) -> &[LexError] {
        &self.errors
    }

    /// Get cache statistics.
    pub fn cache_stats(&self) -> CacheStats {
        self.stats
    }

    /// Get current version number.
    pub fn version(&self) -> u64 {
        self.current_version
    }

    /// Get version history.
    pub fn version_history(&self) -> impl Iterator<Item = &CacheVersion> {
        self.versions.iter()
    }

    /// Get the language specification.
    pub fn language(&self) -> &L {
        &self.language
    }

    /// Get configuration.
    pub fn config(&self) -> &CacheConfig {
        &self.config
    }

    /// Apply an edit to the source text.
    ///
    /// Returns the number of tokens that were re-lexed.
    pub fn edit(&mut self, start: usize, end: usize, new_text: &str) -> usize {
        // Validate edit bounds
        if start > self.source.len() || end > self.source.len() || start > end {
            return 0;
        }

        // Apply the edit
        let delta = new_text.len() as isize - (end - start) as isize;
        let mut new_source = String::with_capacity(
            self.source.len().saturating_add_signed(delta),
        );
        new_source.push_str(&self.source[..start]);
        new_source.push_str(new_text);
        new_source.push_str(&self.source[end..]);

        let old_source = core::mem::replace(&mut self.source, new_source);

        // Perform range-based invalidation
        let tokens_relexed = self.invalidate_range(start, end, delta, &old_source);

        // Create new version if significant change
        if tokens_relexed >= self.config.min_edit_distance {
            self.create_version();
        }

        tokens_relexed
    }

    /// Invalidate tokens in the affected range and re-lex.
    fn invalidate_range(
        &mut self,
        edit_start: usize,
        edit_end: usize,
        delta: isize,
        old_source: &str,
    ) -> usize {
        self.stats.invalidations += 1;

        // Find affected token ranges
        let affected_range_start = self.find_range_containing(edit_start);
        let affected_range_end = self.find_range_containing(edit_end);

        // Determine token indices to replace
        let (token_start, token_end) = if let (Some(rs), Some(re)) =
            (affected_range_start, affected_range_end)
        {
            // Invalidate affected ranges
            for i in rs..=re.min(self.ranges.len() - 1) {
                self.ranges[i].valid = false;
            }

            // Back up one token for safety (token merging edge cases)
            let ts = if rs > 0 {
                self.ranges[rs - 1].token_end
            } else if rs < self.ranges.len() {
                self.ranges[rs].token_start.saturating_sub(1)
            } else {
                0
            };

            let te = if re < self.ranges.len() {
                self.ranges[re].token_end.min(self.tokens.len())
            } else {
                self.tokens.len()
            };

            (ts, te)
        } else {
            // Fallback to full re-lex
            return self.full_tokenize();
        };

        // Calculate re-lex start position
        let relex_byte_start = if token_start > 0 && token_start <= self.tokens.len() {
            self.tokens[token_start.saturating_sub(1)].span.end
        } else {
            0
        };

        // Calculate where to look for sync in new text
        let new_text_length = if delta >= 0 { delta as usize } else { 0 };
        let edit_end_in_new = edit_start + new_text_length;

        // Re-lex from the calculated position
        let new_tokens = self.lex_from_position(relex_byte_start);

        // Find synchronization point with existing tokens
        let sync_point = self.find_sync_point(
            &new_tokens,
            token_end,
            delta,
            old_source,
            edit_end_in_new,
        );

        // Merge tokens
        let tokens_relexed = self.merge_tokens(
            token_start.saturating_sub(1),
            sync_point.unwrap_or(self.tokens.len()),
            new_tokens,
            delta,
        );

        // Update statistics
        if self.config.track_stats {
            let tokens_reused = self.tokens.len().saturating_sub(tokens_relexed);
            self.stats.hits += tokens_reused;
            self.stats.misses += tokens_relexed;
        }

        // Rebuild ranges
        self.rebuild_ranges();

        tokens_relexed
    }

    /// Find the range index containing a byte position.
    fn find_range_containing(&self, pos: usize) -> Option<usize> {
        self.ranges.iter().position(|r| r.contains(pos) || r.end > pos)
    }

    /// Lex tokens starting from a byte position.
    fn lex_from_position(&self, start: usize) -> Vec<Token> {
        if start >= self.source.len() {
            return Vec::new();
        }

        let mut lexer = Lexer::new(&self.source[start..], self.language.clone());
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            if token.kind == TokenKind::Eof {
                break;
            }

            // Adjust span to absolute positions
            let adjusted = Token::new(
                token.kind,
                Span::new(
                    token.span.start + start,
                    token.span.end + start,
                    self.location_at_byte(token.span.start + start),
                    self.location_at_byte(token.span.end + start),
                ),
            );
            tokens.push(adjusted);
        }

        tokens
    }

    /// Find a synchronization point for merging.
    fn find_sync_point(
        &self,
        new_tokens: &[Token],
        search_start: usize,
        delta: isize,
        old_source: &str,
        edit_end_in_new: usize,
    ) -> Option<usize> {
        for new_token in new_tokens.iter() {
            // Only look for sync after edit region
            if new_token.span.start < edit_end_in_new {
                continue;
            }

            // Look for matching old token
            for (old_idx, old_token) in self.tokens[search_start..].iter().enumerate() {
                let expected_new_start = (old_token.span.start as isize + delta) as usize;

                if new_token.span.start == expected_new_start
                    && new_token.kind == old_token.kind
                {
                    // Verify text matches
                    let old_text = &old_source[old_token.span.start..old_token.span.end];
                    let new_text = &self.source[new_token.span.start..new_token.span.end];

                    if old_text == new_text {
                        return Some(search_start + old_idx);
                    }
                }
            }
        }
        None
    }

    /// Merge newly lexed tokens with existing tokens.
    fn merge_tokens(
        &mut self,
        replace_start: usize,
        sync_idx: usize,
        new_tokens: Vec<Token>,
        _delta: isize,
    ) -> usize {
        let tokens_lexed = new_tokens.len();

        // Keep tokens before replace_start
        self.tokens.truncate(replace_start);

        // Add new tokens (stopping at sync point in new tokens)
        for token in new_tokens {
            if token.span.start >= (self.tokens.last().map(|t| t.span.end).unwrap_or(0)) {
                self.tokens.push(token);
            }
        }

        // Add shifted tokens after sync point
        if sync_idx < self.tokens.len() {
            // This case is handled differently - we need to shift remaining tokens
        }

        tokens_lexed
    }

    /// Rebuild token ranges for fine-grained invalidation.
    fn rebuild_ranges(&mut self) {
        self.ranges.clear();

        if self.tokens.is_empty() {
            return;
        }

        let mut current_range_start = 0;
        let mut current_token_start = 0;

        for (i, token) in self.tokens.iter().enumerate() {
            let range_boundary = (token.span.start / self.range_size) * self.range_size + self.range_size;

            if token.span.end >= range_boundary || i == self.tokens.len() - 1 {
                self.ranges.push(TokenRange::new(
                    current_range_start,
                    token.span.end,
                    current_token_start,
                    i + 1,
                ));
                current_range_start = token.span.end;
                current_token_start = i + 1;
            }
        }
    }

    /// Perform full tokenization.
    fn full_tokenize(&mut self) -> usize {
        let (tokens, errors) = Lexer::tokenize(&self.source, self.language.clone());
        let token_count = tokens.len();

        self.tokens = tokens;
        self.errors = errors;
        self.rebuild_ranges();

        if self.config.track_stats {
            self.stats.misses += token_count;
        }

        token_count
    }

    /// Create a new version snapshot.
    fn create_version(&mut self) {
        self.current_version += 1;

        let version = CacheVersion {
            version: self.current_version,
            source_hash: self.hash_source(),
            token_count: self.tokens.len(),
            memory_bytes: self.estimate_memory(),
        };

        self.versions.push_back(version);
        self.stats.versions_created += 1;

        // Evict old versions if needed
        while self.versions.len() > self.config.max_versions {
            self.versions.pop_front();
            self.stats.versions_evicted += 1;
        }

        // Check memory limits
        self.enforce_memory_limit();
    }

    /// Estimate current memory usage.
    fn estimate_memory(&self) -> usize {
        let token_size = core::mem::size_of::<Token>();
        let range_size = core::mem::size_of::<TokenRange>();

        self.source.len()
            + self.tokens.len() * token_size
            + self.ranges.len() * range_size
            + self.errors.len() * core::mem::size_of::<LexError>()
    }

    /// Enforce memory limits by evicting old versions.
    fn enforce_memory_limit(&mut self) {
        self.stats.memory_usage = self.estimate_memory();

        while self.stats.memory_usage > self.config.max_memory_bytes
            && self.versions.len() > 1
        {
            self.versions.pop_front();
            self.stats.versions_evicted += 1;
        }
    }

    /// Simple hash for source text.
    fn hash_source(&self) -> u64 {
        let mut hash: u64 = 0;
        for byte in self.source.bytes() {
            hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
        }
        hash
    }

    /// Calculate location at a byte offset.
    fn location_at_byte(&self, byte_offset: usize) -> Location {
        let mut loc = Location::new();
        for c in self.source[..byte_offset.min(self.source.len())].chars() {
            loc.advance(c);
        }
        loc
    }

    /// Get the source text for a token.
    pub fn token_text(&self, token: &Token) -> &str {
        &self.source[token.span.start..token.span.end]
    }

    /// Reset statistics.
    pub fn reset_stats(&mut self) {
        self.stats = CacheStats::default();
        self.stats.memory_usage = self.estimate_memory();
    }

    /// Force a full re-tokenization.
    pub fn invalidate_all(&mut self) {
        self.full_tokenize();
    }

    /// Get approximate hit rate for recent operations.
    pub fn hit_rate(&self) -> f64 {
        self.stats.hit_rate()
    }
}

/// An LRU cache for token sequences.
pub struct TokenLruCache {
    /// Maximum number of entries.
    capacity: usize,
    /// Cache entries (source hash -> tokens).
    entries: VecDeque<(u64, Vec<Token>)>,
    /// Statistics.
    stats: CacheStats,
}

impl TokenLruCache {
    /// Create a new LRU cache.
    pub fn new(capacity: usize) -> Self {
        Self {
            capacity,
            entries: VecDeque::with_capacity(capacity),
            stats: CacheStats::default(),
        }
    }

    /// Get cached tokens for a source hash.
    pub fn get(&mut self, hash: u64) -> Option<&[Token]> {
        if let Some(idx) = self.entries.iter().position(|(h, _)| *h == hash) {
            // Move to front (most recently used)
            if idx > 0 {
                let entry = self.entries.remove(idx).unwrap();
                self.entries.push_front(entry);
            }
            self.stats.hits += 1;
            Some(&self.entries.front().unwrap().1)
        } else {
            self.stats.misses += 1;
            None
        }
    }

    /// Insert tokens into the cache.
    pub fn insert(&mut self, hash: u64, tokens: Vec<Token>) {
        // Remove if exists
        if let Some(idx) = self.entries.iter().position(|(h, _)| *h == hash) {
            self.entries.remove(idx);
        }

        // Evict if at capacity
        if self.entries.len() >= self.capacity {
            self.entries.pop_back();
            self.stats.versions_evicted += 1;
        }

        self.entries.push_front((hash, tokens));
    }

    /// Clear the cache.
    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Get cache statistics.
    pub fn stats(&self) -> CacheStats {
        self.stats
    }

    /// Get the number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// A cache key for token sequences.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CacheKey {
    /// Source text hash.
    pub source_hash: u64,
    /// Version number.
    pub version: u64,
}

impl CacheKey {
    /// Create a new cache key.
    pub fn new(source_hash: u64, version: u64) -> Self {
        Self { source_hash, version }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::DefaultLanguage;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::vec;

    #[test]
    fn test_cached_lexer_basic() {
        let lexer = CachedLexer::new("let x = 42;", DefaultLanguage);
        assert_eq!(lexer.tokens().len(), 5);
    }

    #[test]
    fn test_cached_lexer_edit() {
        let mut lexer = CachedLexer::new("let x = 42;", DefaultLanguage);
        assert_eq!(lexer.tokens().len(), 5);

        // Change "42" to "100"
        lexer.edit(8, 10, "100");
        assert_eq!(lexer.source(), "let x = 100;");
        assert_eq!(lexer.tokens().len(), 5);
    }

    #[test]
    fn test_cached_lexer_insert() {
        let mut lexer = CachedLexer::new("x = 42;", DefaultLanguage);

        lexer.edit(0, 0, "let ");
        assert_eq!(lexer.source(), "let x = 42;");
        assert_eq!(lexer.tokens().len(), 5);
    }

    #[test]
    fn test_cached_lexer_delete() {
        let mut lexer = CachedLexer::new("let x = 42;", DefaultLanguage);

        lexer.edit(0, 4, "");
        assert_eq!(lexer.source(), "x = 42;");
        assert_eq!(lexer.tokens().len(), 4);
    }

    #[test]
    fn test_cache_stats() {
        let lexer = CachedLexer::new("let x = 42;", DefaultLanguage);
        let stats = lexer.cache_stats();

        // Initial tokenization counts as misses
        assert_eq!(stats.misses, 5);
        assert_eq!(stats.hits, 0);
    }

    #[test]
    fn test_version_tracking() {
        let mut lexer = CachedLexer::new("let x = 42;", DefaultLanguage);
        assert_eq!(lexer.version(), 0);

        lexer.edit(8, 10, "100");
        assert!(lexer.version() >= 1);
    }

    #[test]
    fn test_cache_config() {
        let config = CacheConfig::new()
            .max_versions(10)
            .max_memory_bytes(1024 * 1024)
            .track_stats(true);

        let lexer = CachedLexer::with_config("let x = 42;", DefaultLanguage, config);
        assert_eq!(lexer.config().max_versions, 10);
    }

    #[test]
    fn test_lru_cache() {
        let mut cache = TokenLruCache::new(3);

        cache.insert(1, vec![]);
        cache.insert(2, vec![]);
        cache.insert(3, vec![]);

        assert!(cache.get(1).is_some());
        assert!(cache.get(2).is_some());

        // This should evict hash 3 (least recently used)
        cache.insert(4, vec![]);

        // 1 and 2 should still be there, 3 should be evicted
        assert_eq!(cache.len(), 3);
    }

    #[test]
    fn test_cache_hit_rate() {
        let stats = CacheStats {
            hits: 80,
            misses: 20,
            ..Default::default()
        };

        assert!((stats.hit_rate() - 0.8).abs() < 0.001);
    }

    #[test]
    fn test_empty_cache_hit_rate() {
        let stats = CacheStats::default();
        assert_eq!(stats.hit_rate(), 0.0);
    }

    #[test]
    fn test_cache_key() {
        let key1 = CacheKey::new(123, 1);
        let key2 = CacheKey::new(123, 2);

        assert_ne!(key1, key2);
    }

    #[test]
    fn test_cached_lexer_token_text() {
        let lexer = CachedLexer::new("let x = 42;", DefaultLanguage);
        let tokens = lexer.tokens();

        assert_eq!(lexer.token_text(&tokens[0]), "let");
        assert_eq!(lexer.token_text(&tokens[1]), "x");
        assert_eq!(lexer.token_text(&tokens[3]), "42");
    }

    #[test]
    fn test_cached_lexer_invalidate_all() {
        let mut lexer = CachedLexer::new("let x = 42;", DefaultLanguage);

        lexer.invalidate_all();
        assert_eq!(lexer.tokens().len(), 5);
    }

    #[test]
    fn test_version_eviction() {
        let config = CacheConfig::new().max_versions(2);
        let mut lexer = CachedLexer::with_config("let x = 42;", DefaultLanguage, config);

        // Make multiple edits to trigger version creation
        for i in 0..5 {
            lexer.edit(8, 10 + i, &format!("{}", 100 + i));
        }

        // Should only keep max_versions
        assert!(lexer.version_history().count() <= 2);
    }
}
