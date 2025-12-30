//! # Lex
//!
//! A lexer/tokenizer and parser built from scratch in Rust.
//!
//! This library provides a flexible lexical analyzer that breaks source code
//! into tokens, and a full recursive descent parser that builds Abstract Syntax
//! Trees (ASTs). It supports full Unicode, precise position tracking, and
//! pluggable language rules.
//!
//! ## Features
//!
//! - **Unicode Support**: Full Unicode identifier support (UAX #31)
//! - **Position Tracking**: Precise line and column numbers for all tokens
//! - **Pluggable Languages**: Define custom keywords, operators, and comment styles
//! - **Incremental Re-lexing**: Efficiently re-tokenize only changed regions
//! - **Streaming Tokenization**: Memory-efficient processing for large inputs
//! - **Token Caching**: LRU caching with fine-grained invalidation
//! - **AST Parser**: Full recursive descent parser with precedence climbing
//! - **Zero Dependencies**: Core lexing uses only the standard library
//! - **`no_std` Support**: Works in embedded environments with `alloc`
//! - **WebAssembly Support**: Compile to WASM for browser-based tools
//!
//! ## `no_std` Support
//!
//! This crate supports `no_std` environments. By default, the `std` feature is enabled.
//! To use in a `no_std` environment with an allocator:
//!
//! ```toml
//! [dependencies]
//! lex = { version = "0.1", default-features = false, features = ["alloc"] }
//! ```
//!
//! ## WebAssembly Support
//!
//! To compile for WebAssembly:
//!
//! ```bash
//! # Add WASM target and install wasm-bindgen
//! rustup target add wasm32-unknown-unknown
//! cargo install wasm-bindgen-cli
//!
//! # Build and generate bindings
//! cargo build --target wasm32-unknown-unknown --features wasm --release
//! wasm-bindgen target/wasm32-unknown-unknown/release/lex.wasm \
//!     --out-dir pkg --target web
//! ```
//!
//! Then use in JavaScript:
//!
//! ```javascript
//! import init, { tokenize, LexerConfig, tokenizeWithConfig } from './pkg/lex.js';
//!
//! await init();
//!
//! // Tokenize with default language
//! const result = JSON.parse(tokenize('let x = 42;'));
//! console.log(result.tokens);
//!
//! // Custom language configuration
//! const config = new LexerConfig();
//! config.addKeyword('func');
//! config.setSingleLineComment('#');
//! const customResult = JSON.parse(tokenizeWithConfig('func main() {}', config));
//! ```
//!
//! ## Quick Start
//!
//! ```rust
//! use lex::{Lexer, DefaultLanguage};
//!
//! let source = "let x = 42;";
//! let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
//!
//! for token in &tokens {
//!     println!("{:?}", token);
//! }
//! ```
//!
//! ## Custom Languages
//!
//! ```rust
//! use lex::language::{LanguageBuilder, LanguageSpec};
//! use lex::token::TokenKind;
//! use lex::Lexer;
//!
//! let my_lang = LanguageBuilder::new()
//!     .keywords(&["func", "var", "const"])
//!     .operator("==", TokenKind::EqEq)
//!     .single_line_comment("#")
//!     .build();
//!
//! let (tokens, errors) = Lexer::tokenize("var x = 42", my_lang);
//! ```
//!
//! ## Incremental Re-lexing
//!
//! For editor-like use cases, use `IncrementalLexer` to efficiently re-tokenize
//! only the changed regions of source text:
//!
//! ```rust
//! use lex::{IncrementalLexer, DefaultLanguage, Edit};
//!
//! let mut lexer = IncrementalLexer::new("let x = 42;", DefaultLanguage);
//!
//! // Apply an edit: change "42" to "100"
//! let edit = Edit::new(8, 10, "100");
//! let stats = lexer.apply_edit(edit);
//!
//! println!("Reused {} tokens, lexed {} new", stats.tokens_reused, stats.tokens_lexed);
//! assert_eq!(lexer.tokens().len(), 5);
//! ```
//!
//! ## Streaming Tokenization
//!
//! For processing large files with minimal memory footprint:
//!
//! ```rust
//! use lex::{StreamingLexer, DefaultLanguage};
//!
//! let mut lexer = StreamingLexer::new("let x = 42;", DefaultLanguage);
//!
//! // Process tokens one at a time
//! while let Some(token) = lexer.next_token() {
//!     println!("{:?}", token);
//! }
//!
//! // Or use callback-based processing
//! let mut lexer = StreamingLexer::new("let x = 42;", DefaultLanguage);
//! lexer.process(|token| {
//!     println!("{:?}", token);
//!     true // continue processing
//! });
//! ```
//!
//! ## Token Caching with LRU Eviction
//!
//! For advanced caching with fine-grained invalidation:
//!
//! ```rust
//! use lex::cache::{CachedLexer, CacheConfig};
//! use lex::DefaultLanguage;
//!
//! let config = CacheConfig::default().max_versions(10);
//! let mut lexer = CachedLexer::with_config("let x = 42;", DefaultLanguage, config);
//!
//! // Edit and see cache statistics
//! lexer.edit(8, 10, "100");
//! let stats = lexer.cache_stats();
//! println!("Cache hit rate: {:.1}%", stats.hit_rate() * 100.0);
//! ```
//!
//! ## Parsing
//!
//! Use the parser to build Abstract Syntax Trees from source code:
//!
//! ```rust
//! use lex::parser::Parser;
//! use lex::DefaultLanguage;
//!
//! let source = "fn main() { let x = 42; }";
//! let mut parser = Parser::new(source, DefaultLanguage);
//!
//! match parser.parse_program() {
//!     Ok(program) => {
//!         println!("Parsed {} items", program.items.len());
//!         // Access the AST nodes
//!         for item in &program.items {
//!             println!("{:?}", item);
//!         }
//!     }
//!     Err(errors) => {
//!         for error in errors {
//!             eprintln!("{}", error);
//!         }
//!     }
//! }
//! ```

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

// Core modules
pub mod error;
pub mod language;
pub mod lexer;
pub mod source;
pub mod span;
pub mod token;
pub mod unicode;

// AST and Parser (requires alloc for Vec, Box, String)
#[cfg(feature = "alloc")]
pub mod ast;
#[cfg(feature = "alloc")]
pub mod parser;

// Advanced lexer features
#[cfg(feature = "alloc")]
pub mod cache;
#[cfg(feature = "alloc")]
pub mod incremental;
#[cfg(feature = "alloc")]
pub mod streaming;

#[cfg(feature = "wasm")]
pub mod wasm;

// Re-export commonly used types
pub use error::{LexError, LexErrorKind};
#[cfg(feature = "alloc")]
pub use error::format_error_with_source;
#[cfg(feature = "alloc")]
pub use cache::{CachedLexer, CacheConfig, CacheStats, CacheVersion, TokenLruCache};
#[cfg(feature = "alloc")]
pub use incremental::{Edit, IncrementalLexer, IncrementalStats};
pub use language::{DefaultLanguage, LanguageBuilder, LanguageSpec};
pub use lexer::Lexer;
pub use source::Source;
pub use span::{Location, Span};
#[cfg(feature = "alloc")]
pub use streaming::{
    BufferedTokenStream, ChunkedTokenizer, StreamingConfig, StreamingLexer,
    StreamingStats, TokenVisitor,
};
pub use token::{KeywordId, Token, TokenKind};

// Parser re-exports
#[cfg(feature = "alloc")]
pub use parser::{Parser, ParseError, ParseErrorKind, ParseResult, format_parse_error};

// AST re-exports
#[cfg(feature = "alloc")]
pub use ast::{
    // Core types
    Program, Block, Ident, Path, Visibility, Mutability,
    // Expressions
    Expr, ExprKind, Literal, BinOp, UnaryOp, BinaryExpr, UnaryExpr,
    CallExpr, MethodCallExpr, FieldExpr, IndexExpr, IfExpr, MatchExpr,
    MatchArm, Pattern, PatternKind, LoopExpr, WhileExpr, ForExpr,
    AssignExpr, StructExpr, FieldInit, ClosureExpr, ClosureParam,
    RefExpr, CastExpr, RangeExpr, FieldPattern,
    // Statements
    Stmt, StmtKind, LetStmt, ConstStmt,
    // Items
    Item, ItemKind, FnItem, FnParam, StructItem, StructKind, StructField,
    TupleField, EnumItem, EnumVariant, VariantKind, TypeAliasItem,
    TraitItem, TraitItemDef, TraitItemKind, ImplItem, ImplItemDef,
    ImplItemKind, ModItem, UseItem, UseTree, UseTreeKind, ConstItem,
    StaticItem, Generics, GenericParam, TypeBound, WhereClause, WherePredicate,
    // Types
    Type, TypeKind, GenericType, GenericArg, ArrayType, ReferenceType,
    PointerType, FnType, QualifiedPathType, TraitObjectType, ImplTraitType,
};
