//! Statement AST nodes.
//!
//! Statements are constructs that perform actions but don't necessarily produce values.

use crate::span::Span;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, vec::Vec};

use super::{Expr, Ident, Mutability, Pattern, Type};

/// A statement node.
#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    /// The kind of statement.
    pub kind: StmtKind,
    /// The span of the statement.
    pub span: Span,
}

impl Stmt {
    /// Create a new statement.
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Create an expression statement.
    pub fn expr(expr: Expr) -> Self {
        let span = expr.span;
        Self::new(StmtKind::Expr(expr), span)
    }

    /// Create a semicolon-terminated expression statement.
    pub fn semi(expr: Expr, span: Span) -> Self {
        Self::new(StmtKind::Semi(expr), span)
    }
}

/// The kind of statement.
#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    /// A let binding (e.g., `let x = 42;`).
    Let(LetStmt),
    /// A const binding (e.g., `const MAX = 100;`).
    Const(ConstStmt),
    /// An expression statement without trailing semicolon.
    Expr(Expr),
    /// An expression statement with trailing semicolon.
    Semi(Expr),
    /// An empty statement (just `;`).
    Empty,
}

/// A let binding statement.
#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    /// The pattern being bound.
    pub pattern: Pattern,
    /// Whether this is mutable.
    pub mutability: Mutability,
    /// The type annotation (if any).
    pub ty: Option<Type>,
    /// The initializer expression (if any).
    pub init: Option<Box<Expr>>,
}

impl LetStmt {
    /// Create a new let statement.
    pub fn new(
        pattern: Pattern,
        mutability: Mutability,
        ty: Option<Type>,
        init: Option<Expr>,
    ) -> Self {
        Self {
            pattern,
            mutability,
            ty,
            init: init.map(Box::new),
        }
    }

    /// Create a simple let binding with just a name and initializer.
    pub fn simple(name: Ident, init: Expr) -> Self {
        Self {
            pattern: Pattern {
                kind: super::PatternKind::Ident(name),
                span: Span::default(),
            },
            mutability: Mutability::Immutable,
            ty: None,
            init: Some(Box::new(init)),
        }
    }
}

/// A const binding statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ConstStmt {
    /// The name of the constant.
    pub name: Ident,
    /// The type annotation (required for const).
    pub ty: Option<Type>,
    /// The initializer expression.
    pub init: Box<Expr>,
}

impl ConstStmt {
    /// Create a new const statement.
    pub fn new(name: Ident, ty: Option<Type>, init: Expr) -> Self {
        Self {
            name,
            ty,
            init: Box::new(init),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExprKind, Literal, PatternKind};
    use crate::span::Location;

    fn dummy_span() -> Span {
        Span::new(0, 0, Location::new(), Location::new())
    }

    #[test]
    fn test_let_stmt_simple() {
        let name = Ident::new("x".into(), dummy_span());
        let init = Expr::new(ExprKind::Literal(Literal::Int(42)), dummy_span());
        let let_stmt = LetStmt::simple(name.clone(), init);

        assert_eq!(let_stmt.mutability, Mutability::Immutable);
        assert!(let_stmt.ty.is_none());
        assert!(let_stmt.init.is_some());
        if let PatternKind::Ident(ident) = &let_stmt.pattern.kind {
            assert_eq!(ident.name, "x");
        } else {
            panic!("Expected ident pattern");
        }
    }

    #[test]
    fn test_expr_stmt() {
        let expr = Expr::new(ExprKind::Literal(Literal::Int(42)), dummy_span());
        let stmt = Stmt::expr(expr);

        if let StmtKind::Expr(_) = stmt.kind {
            // ok
        } else {
            panic!("Expected expr statement");
        }
    }

    #[test]
    fn test_semi_stmt() {
        let expr = Expr::new(ExprKind::Literal(Literal::Int(42)), dummy_span());
        let stmt = Stmt::semi(expr, dummy_span());

        if let StmtKind::Semi(_) = stmt.kind {
            // ok
        } else {
            panic!("Expected semi statement");
        }
    }
}
