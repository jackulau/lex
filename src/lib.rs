//! # Lex
//!
//! A lexer/tokenizer built from scratch in Rust.
//!
//! This library provides a flexible lexical analyzer that breaks source code
//! into tokens. It supports full Unicode, precise position tracking, and
//! pluggable language rules.
//!
//! ## Features
//!
//! - **Unicode Support**: Full Unicode identifier support (UAX #31)
//! - **Position Tracking**: Precise line and column numbers for all tokens
//! - **Pluggable Languages**: Define custom keywords, operators, and comment styles
//! - **Zero Dependencies**: Core lexing uses only the standard library
//! - **`no_std` Support**: Works in embedded environments with `alloc`
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

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod error;
pub mod language;
pub mod lexer;
pub mod source;
pub mod span;
pub mod token;
pub mod unicode;

// Re-export commonly used types
pub use error::{LexError, LexErrorKind};
#[cfg(feature = "alloc")]
pub use error::format_error_with_source;
pub use language::{DefaultLanguage, LanguageBuilder, LanguageSpec};
pub use lexer::Lexer;
pub use source::Source;
pub use span::{Location, Span};
pub use token::{KeywordId, Token, TokenKind};
