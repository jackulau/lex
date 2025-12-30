//! Abstract Syntax Tree (AST) node types.
//!
//! This module defines the AST representation for programs parsed by the lexer.
//! Each node contains span information for precise error reporting.

mod expr;
mod item;
mod stmt;
mod types;

pub use expr::*;
pub use item::*;
pub use stmt::*;
pub use types::*;

use crate::span::Span;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, string::String, vec::Vec};

/// A complete program (compilation unit).
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    /// Top-level items in the program.
    pub items: Vec<Item>,
    /// The span of the entire program.
    pub span: Span,
}

impl Program {
    /// Create a new program.
    pub fn new(items: Vec<Item>, span: Span) -> Self {
        Self { items, span }
    }
}

/// A block of statements enclosed in braces.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// The statements in the block.
    pub stmts: Vec<Stmt>,
    /// Optional trailing expression (for expression blocks).
    pub expr: Option<Box<Expr>>,
    /// The span of the block.
    pub span: Span,
}

impl Block {
    /// Create a new block.
    pub fn new(stmts: Vec<Stmt>, expr: Option<Box<Expr>>, span: Span) -> Self {
        Self { stmts, expr, span }
    }
}

/// An identifier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    /// The identifier name.
    pub name: String,
    /// The span of the identifier.
    pub span: Span,
}

impl Ident {
    /// Create a new identifier.
    pub fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }
}

/// A path (e.g., `std::io::Result`).
#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    /// The segments of the path.
    pub segments: Vec<Ident>,
    /// The span of the entire path.
    pub span: Span,
}

impl Path {
    /// Create a new path from segments.
    pub fn new(segments: Vec<Ident>, span: Span) -> Self {
        Self { segments, span }
    }

    /// Create a simple path from a single identifier.
    pub fn from_ident(ident: Ident) -> Self {
        let span = ident.span;
        Self {
            segments: vec![ident],
            span,
        }
    }
}

/// Visibility modifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    /// Private (default).
    Private,
    /// Public (`pub`).
    Public,
}

impl Default for Visibility {
    fn default() -> Self {
        Self::Private
    }
}

/// Mutability modifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    /// Immutable (default).
    Immutable,
    /// Mutable (`mut`).
    Mutable,
}

impl Default for Mutability {
    fn default() -> Self {
        Self::Immutable
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Location;

    fn dummy_span() -> Span {
        Span::new(0, 0, Location::new(), Location::new())
    }

    #[test]
    fn test_ident_creation() {
        let ident = Ident::new("foo".into(), dummy_span());
        assert_eq!(ident.name, "foo");
    }

    #[test]
    fn test_path_from_ident() {
        let ident = Ident::new("foo".into(), dummy_span());
        let path = Path::from_ident(ident);
        assert_eq!(path.segments.len(), 1);
        assert_eq!(path.segments[0].name, "foo");
    }

    #[test]
    fn test_visibility_default() {
        assert_eq!(Visibility::default(), Visibility::Private);
    }

    #[test]
    fn test_mutability_default() {
        assert_eq!(Mutability::default(), Mutability::Immutable);
    }
}
