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

pub mod error;
pub mod language;
pub mod lexer;
pub mod source;
pub mod span;
pub mod token;
pub mod unicode;

// Re-export commonly used types
pub use error::{LexError, LexErrorKind};
pub use language::{DefaultLanguage, LanguageBuilder, LanguageSpec};
pub use lexer::Lexer;
pub use source::Source;
pub use span::{Location, Span};
pub use token::{KeywordId, Token, TokenKind};
