//! Expression AST nodes.
//!
//! Expressions are constructs that evaluate to a value.

use crate::span::Span;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, string::String, vec::Vec};

use super::{Block, Ident, Path, Type};

/// An expression node.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    /// The kind of expression.
    pub kind: ExprKind,
    /// The span of the expression.
    pub span: Span,
}

impl Expr {
    /// Create a new expression.
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Create a literal expression.
    pub fn literal(lit: Literal, span: Span) -> Self {
        Self::new(ExprKind::Literal(lit), span)
    }

    /// Create an identifier expression.
    pub fn ident(name: String, span: Span) -> Self {
        Self::new(ExprKind::Ident(Ident::new(name, span)), span)
    }

    /// Create a binary expression.
    pub fn binary(op: BinOp, lhs: Expr, rhs: Expr, span: Span) -> Self {
        Self::new(
            ExprKind::Binary(BinaryExpr {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            span,
        )
    }

    /// Create a unary expression.
    pub fn unary(op: UnaryOp, operand: Expr, span: Span) -> Self {
        Self::new(
            ExprKind::Unary(UnaryExpr {
                op,
                operand: Box::new(operand),
            }),
            span,
        )
    }
}

/// The kind of expression.
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// A literal value.
    Literal(Literal),
    /// An identifier.
    Ident(Ident),
    /// A path expression (e.g., `std::io::Result`).
    Path(Path),
    /// A binary operation (e.g., `a + b`).
    Binary(BinaryExpr),
    /// A unary operation (e.g., `-x`, `!flag`).
    Unary(UnaryExpr),
    /// A function call (e.g., `foo(1, 2)`).
    Call(CallExpr),
    /// A method call (e.g., `obj.method()`).
    MethodCall(MethodCallExpr),
    /// Field access (e.g., `obj.field`).
    Field(FieldExpr),
    /// Index expression (e.g., `arr[0]`).
    Index(IndexExpr),
    /// A block expression.
    Block(Block),
    /// An if expression.
    If(IfExpr),
    /// A match expression.
    Match(MatchExpr),
    /// A loop expression.
    Loop(LoopExpr),
    /// A while loop.
    While(WhileExpr),
    /// A for loop.
    For(ForExpr),
    /// A return expression.
    Return(Option<Box<Expr>>),
    /// A break expression.
    Break(Option<Box<Expr>>),
    /// A continue expression.
    Continue,
    /// An assignment expression.
    Assign(AssignExpr),
    /// A tuple expression (e.g., `(1, 2, 3)`).
    Tuple(Vec<Expr>),
    /// An array expression (e.g., `[1, 2, 3]`).
    Array(Vec<Expr>),
    /// A struct literal (e.g., `Point { x: 1, y: 2 }`).
    Struct(StructExpr),
    /// A closure/lambda expression.
    Closure(ClosureExpr),
    /// A reference expression (e.g., `&x`, `&mut x`).
    Ref(RefExpr),
    /// A dereference expression (e.g., `*ptr`).
    Deref(Box<Expr>),
    /// A cast expression (e.g., `x as i32`).
    Cast(CastExpr),
    /// A range expression (e.g., `1..10`, `..5`, `3..`).
    Range(RangeExpr),
    /// A grouped/parenthesized expression.
    Paren(Box<Expr>),
}

/// A literal value.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// An integer literal.
    Int(i64),
    /// A floating-point literal.
    Float(f64),
    /// A string literal.
    String(String),
    /// A character literal.
    Char(char),
    /// A boolean literal.
    Bool(bool),
    /// The null literal.
    Null,
}

/// A binary expression.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    /// The binary operator.
    pub op: BinOp,
    /// The left-hand side.
    pub lhs: Box<Expr>,
    /// The right-hand side.
    pub rhs: Box<Expr>,
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,

    // Comparison
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `<=`
    Le,
    /// `>=`
    Ge,

    // Logical
    /// `&&`
    And,
    /// `||`
    Or,

    // Bitwise
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `^`
    BitXor,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
}

impl BinOp {
    /// Get the precedence of this operator.
    /// Higher numbers mean higher precedence (bind tighter).
    pub fn precedence(self) -> u8 {
        match self {
            BinOp::Or => 1,
            BinOp::And => 2,
            BinOp::BitOr => 3,
            BinOp::BitXor => 4,
            BinOp::BitAnd => 5,
            BinOp::Eq | BinOp::Ne => 6,
            BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => 7,
            BinOp::Shl | BinOp::Shr => 8,
            BinOp::Add | BinOp::Sub => 9,
            BinOp::Mul | BinOp::Div | BinOp::Mod => 10,
        }
    }

    /// Check if this operator is right-associative.
    pub fn is_right_associative(self) -> bool {
        false // All standard binary operators are left-associative
    }
}

/// A unary expression.
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    /// The unary operator.
    pub op: UnaryOp,
    /// The operand.
    pub operand: Box<Expr>,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `-` (negation)
    Neg,
    /// `!` (logical not / bitwise not)
    Not,
    /// `~` (bitwise complement)
    BitNot,
}

/// A function call expression.
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    /// The function being called.
    pub func: Box<Expr>,
    /// The arguments to the call.
    pub args: Vec<Expr>,
}

/// A method call expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MethodCallExpr {
    /// The receiver of the method call.
    pub receiver: Box<Expr>,
    /// The method name.
    pub method: Ident,
    /// The arguments to the method.
    pub args: Vec<Expr>,
}

/// A field access expression.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldExpr {
    /// The base expression.
    pub base: Box<Expr>,
    /// The field being accessed.
    pub field: Ident,
}

/// An index expression.
#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    /// The base expression being indexed.
    pub base: Box<Expr>,
    /// The index expression.
    pub index: Box<Expr>,
}

/// An if expression.
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    /// The condition.
    pub cond: Box<Expr>,
    /// The then branch.
    pub then_branch: Block,
    /// The else branch (may be another if).
    pub else_branch: Option<Box<Expr>>,
}

/// A match expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    /// The expression being matched.
    pub scrutinee: Box<Expr>,
    /// The match arms.
    pub arms: Vec<MatchArm>,
}

/// A match arm.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    /// The pattern to match.
    pub pattern: Pattern,
    /// Optional guard expression.
    pub guard: Option<Box<Expr>>,
    /// The body of the arm.
    pub body: Box<Expr>,
    /// The span of the arm.
    pub span: Span,
}

/// A pattern for pattern matching.
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    /// The kind of pattern.
    pub kind: PatternKind,
    /// The span of the pattern.
    pub span: Span,
}

/// The kind of pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    /// A wildcard pattern (`_`).
    Wildcard,
    /// An identifier pattern.
    Ident(Ident),
    /// A literal pattern.
    Literal(Literal),
    /// A tuple pattern.
    Tuple(Vec<Pattern>),
    /// A struct pattern.
    Struct(Path, Vec<FieldPattern>),
    /// An enum variant pattern.
    Variant(Path, Option<Vec<Pattern>>),
    /// A reference pattern.
    Ref(Box<Pattern>),
    /// An or-pattern (e.g., `A | B`).
    Or(Vec<Pattern>),
}

/// A field pattern in a struct pattern.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldPattern {
    /// The field name.
    pub name: Ident,
    /// The pattern for this field (if different from the field name).
    pub pattern: Option<Pattern>,
    /// The span.
    pub span: Span,
}

/// A loop expression.
#[derive(Debug, Clone, PartialEq)]
pub struct LoopExpr {
    /// The loop body.
    pub body: Block,
}

/// A while loop expression.
#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {
    /// The condition.
    pub cond: Box<Expr>,
    /// The loop body.
    pub body: Block,
}

/// A for loop expression.
#[derive(Debug, Clone, PartialEq)]
pub struct ForExpr {
    /// The loop variable pattern.
    pub pattern: Pattern,
    /// The iterator expression.
    pub iter: Box<Expr>,
    /// The loop body.
    pub body: Block,
}

/// An assignment expression.
#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    /// The left-hand side (target of assignment).
    pub lhs: Box<Expr>,
    /// The right-hand side (value being assigned).
    pub rhs: Box<Expr>,
}

/// A struct literal expression.
#[derive(Debug, Clone, PartialEq)]
pub struct StructExpr {
    /// The struct path.
    pub path: Path,
    /// The field initializers.
    pub fields: Vec<FieldInit>,
    /// Optional base expression (for struct update syntax).
    pub base: Option<Box<Expr>>,
}

/// A field initializer in a struct literal.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
    /// The field name.
    pub name: Ident,
    /// The value (if not using shorthand).
    pub value: Option<Expr>,
    /// The span.
    pub span: Span,
}

/// A closure expression.
#[derive(Debug, Clone, PartialEq)]
pub struct ClosureExpr {
    /// The closure parameters.
    pub params: Vec<ClosureParam>,
    /// The return type (if specified).
    pub ret_type: Option<Type>,
    /// The closure body.
    pub body: Box<Expr>,
}

/// A closure parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct ClosureParam {
    /// The parameter pattern.
    pub pattern: Pattern,
    /// The parameter type (if specified).
    pub ty: Option<Type>,
    /// The span.
    pub span: Span,
}

/// A reference expression.
#[derive(Debug, Clone, PartialEq)]
pub struct RefExpr {
    /// Whether this is a mutable reference.
    pub mutable: bool,
    /// The expression being referenced.
    pub expr: Box<Expr>,
}

/// A cast expression.
#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    /// The expression being cast.
    pub expr: Box<Expr>,
    /// The target type.
    pub ty: Type,
}

/// A range expression.
#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpr {
    /// The start of the range (if any).
    pub start: Option<Box<Expr>>,
    /// The end of the range (if any).
    pub end: Option<Box<Expr>>,
    /// Whether this is an inclusive range (`..=`).
    pub inclusive: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Location;

    fn dummy_span() -> Span {
        Span::new(0, 0, Location::new(), Location::new())
    }

    #[test]
    fn test_literal_int() {
        let lit = Literal::Int(42);
        assert_eq!(lit, Literal::Int(42));
    }

    #[test]
    fn test_binary_expr() {
        let lhs = Expr::literal(Literal::Int(1), dummy_span());
        let rhs = Expr::literal(Literal::Int(2), dummy_span());
        let expr = Expr::binary(BinOp::Add, lhs, rhs, dummy_span());

        if let ExprKind::Binary(bin) = expr.kind {
            assert_eq!(bin.op, BinOp::Add);
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_operator_precedence() {
        // Multiplication has higher precedence than addition
        assert!(BinOp::Mul.precedence() > BinOp::Add.precedence());
        // And has higher precedence than Or
        assert!(BinOp::And.precedence() > BinOp::Or.precedence());
    }
}
