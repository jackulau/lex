//! Token types and definitions.
//!
//! This module defines the `Token` struct and `TokenKind` enum used
//! to represent lexical tokens produced by the lexer.

use crate::span::Span;
use std::fmt;

/// A token with its kind and source location.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The type of token.
    pub kind: TokenKind,
    /// Location in source code.
    pub span: Span,
}

impl Token {
    /// Create a new token.
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.kind, self.span)
    }
}

/// Opaque keyword identifier (index into language's keyword table).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct KeywordId(pub u16);

impl KeywordId {
    /// Create a new keyword ID.
    pub fn new(id: u16) -> Self {
        Self(id)
    }

    /// Get the underlying ID value.
    pub fn id(&self) -> u16 {
        self.0
    }
}

/// Token classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // === Identifiers and Keywords ===
    /// A keyword (index into language's keyword table).
    Keyword(KeywordId),
    /// An identifier.
    Ident,

    // === Literals ===
    /// Integer literal.
    IntLiteral,
    /// Floating-point literal.
    FloatLiteral,
    /// String literal.
    StringLiteral,
    /// Raw string literal (no escape processing).
    RawStringLiteral,
    /// Character literal.
    CharLiteral,
    /// Boolean literal.
    BoolLiteral(bool),

    // === Arithmetic Operators ===
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,

    // === Assignment ===
    /// `=`
    Eq,

    // === Comparison Operators ===
    /// `==`
    EqEq,
    /// `!=`
    BangEq,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,

    // === Logical Operators ===
    /// `!`
    Bang,
    /// `&&`
    AmpAmp,
    /// `||`
    PipePipe,

    // === Bitwise Operators ===
    /// `&`
    Amp,
    /// `|`
    Pipe,
    /// `^`
    Caret,
    /// `~`
    Tilde,
    /// `<<`
    LtLt,
    /// `>>`
    GtGt,

    // === Delimiters ===
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `;`
    Semicolon,
    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `::`
    ColonColon,
    /// `.`
    Dot,
    /// `->`
    Arrow,
    /// `=>`
    FatArrow,

    // === Special ===
    /// Comment (preserved if configured).
    Comment,
    /// Whitespace (preserved if configured).
    Whitespace,
    /// End of file.
    Eof,
    /// Invalid token (lexer error).
    Error,
}

impl TokenKind {
    /// Check if this is an identifier.
    pub fn is_ident(&self) -> bool {
        matches!(self, TokenKind::Ident)
    }

    /// Check if this is a keyword.
    pub fn is_keyword(&self) -> bool {
        matches!(self, TokenKind::Keyword(_))
    }

    /// Check if this is a literal.
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::IntLiteral
                | TokenKind::FloatLiteral
                | TokenKind::StringLiteral
                | TokenKind::RawStringLiteral
                | TokenKind::CharLiteral
                | TokenKind::BoolLiteral(_)
        )
    }

    /// Check if this is an operator.
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Eq
                | TokenKind::EqEq
                | TokenKind::BangEq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::LtEq
                | TokenKind::GtEq
                | TokenKind::Bang
                | TokenKind::AmpAmp
                | TokenKind::PipePipe
                | TokenKind::Amp
                | TokenKind::Pipe
                | TokenKind::Caret
                | TokenKind::Tilde
                | TokenKind::LtLt
                | TokenKind::GtGt
        )
    }

    /// Check if this is a delimiter.
    pub fn is_delimiter(&self) -> bool {
        matches!(
            self,
            TokenKind::LParen
                | TokenKind::RParen
                | TokenKind::LBrace
                | TokenKind::RBrace
                | TokenKind::LBracket
                | TokenKind::RBracket
                | TokenKind::Semicolon
                | TokenKind::Comma
                | TokenKind::Colon
                | TokenKind::ColonColon
                | TokenKind::Dot
                | TokenKind::Arrow
                | TokenKind::FatArrow
        )
    }

    /// Check if this is EOF.
    pub fn is_eof(&self) -> bool {
        matches!(self, TokenKind::Eof)
    }

    /// Check if this is an error token.
    pub fn is_error(&self) -> bool {
        matches!(self, TokenKind::Error)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Keyword(id) => write!(f, "Keyword({})", id.0),
            TokenKind::Ident => write!(f, "Ident"),
            TokenKind::IntLiteral => write!(f, "IntLiteral"),
            TokenKind::FloatLiteral => write!(f, "FloatLiteral"),
            TokenKind::StringLiteral => write!(f, "StringLiteral"),
            TokenKind::RawStringLiteral => write!(f, "RawStringLiteral"),
            TokenKind::CharLiteral => write!(f, "CharLiteral"),
            TokenKind::BoolLiteral(b) => write!(f, "BoolLiteral({})", b),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::BangEq => write!(f, "!="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::AmpAmp => write!(f, "&&"),
            TokenKind::PipePipe => write!(f, "||"),
            TokenKind::Amp => write!(f, "&"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::LtLt => write!(f, "<<"),
            TokenKind::GtGt => write!(f, ">>"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::ColonColon => write!(f, "::"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::FatArrow => write!(f, "=>"),
            TokenKind::Comment => write!(f, "Comment"),
            TokenKind::Whitespace => write!(f, "Whitespace"),
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::Error => write!(f, "Error"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Location;

    #[test]
    fn test_token_kind_checks() {
        assert!(TokenKind::Ident.is_ident());
        assert!(TokenKind::Keyword(KeywordId(0)).is_keyword());
        assert!(TokenKind::IntLiteral.is_literal());
        assert!(TokenKind::Plus.is_operator());
        assert!(TokenKind::LParen.is_delimiter());
        assert!(TokenKind::Eof.is_eof());
        assert!(TokenKind::Error.is_error());
    }

    #[test]
    fn test_token_display() {
        let token = Token::new(
            TokenKind::IntLiteral,
            Span::new(0, 2, Location::at(1, 1), Location::at(1, 3)),
        );
        let display = format!("{}", token);
        assert!(display.contains("IntLiteral"));
    }
}
