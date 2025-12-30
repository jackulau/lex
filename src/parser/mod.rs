//! Parser module for building ASTs from token streams.
//!
//! This module implements a recursive descent parser with Pratt parsing
//! (precedence climbing) for expressions. It builds on top of the lexer
//! to produce a complete Abstract Syntax Tree.
//!
//! # Example
//!
//! ```rust
//! use lex::parser::Parser;
//! use lex::DefaultLanguage;
//!
//! let source = "fn main() { let x = 42; }";
//! let mut parser = Parser::new(source, DefaultLanguage);
//! let result = parser.parse_program();
//!
//! match result {
//!     Ok(program) => println!("Parsed {} items", program.items.len()),
//!     Err(errors) => {
//!         for error in errors {
//!             eprintln!("{}", error);
//!         }
//!     }
//! }
//! ```

mod error;

pub use error::{format_parse_error, ParseError, ParseErrorKind, ParseResult};

use crate::ast::*;
use crate::language::LanguageSpec;
use crate::lexer::Lexer;
use crate::span::Span;
use crate::token::{Token, TokenKind};

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, string::String, vec, vec::Vec};

/// Restrictions on expression parsing.
#[derive(Clone, Copy, Default)]
struct Restrictions {
    /// Don't allow struct literals (e.g., in if/while conditions).
    no_struct_literal: bool,
}

/// The parser, parameterized by language specification.
pub struct Parser<'a, L: LanguageSpec> {
    /// The underlying lexer.
    lexer: Lexer<'a, L>,
    /// The source text.
    source: &'a str,
    /// Accumulated parse errors.
    errors: Vec<ParseError>,
    /// Current token.
    current: Token,
    /// Previous token (for span tracking).
    previous: Token,
    /// Current parsing restrictions.
    restrictions: Restrictions,
}

impl<'a, L: LanguageSpec> Parser<'a, L> {
    /// Create a new parser for the given source text.
    pub fn new(source: &'a str, language: L) -> Self {
        let mut lexer = Lexer::new(source, language);
        let current = lexer.next_token();
        let previous = Token::new(TokenKind::Eof, Span::default());

        Self {
            lexer,
            source,
            errors: Vec::new(),
            current,
            previous,
            restrictions: Restrictions::default(),
        }
    }

    /// Parse an expression with the given restrictions.
    fn parse_expression_with(&mut self, restrictions: Restrictions) -> ParseResult<Expr> {
        let old = self.restrictions;
        self.restrictions = restrictions;
        let result = self.parse_expression();
        self.restrictions = old;
        result
    }

    /// Parse the entire source as a program.
    pub fn parse_program(&mut self) -> Result<Program, Vec<ParseError>> {
        let start_span = self.current.span;
        let mut items = Vec::new();

        while !self.is_at_end() {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }

        // Collect any lexer errors
        for lex_error in self.lexer.errors() {
            self.errors.push(ParseError::from_lex_error(lex_error.clone()));
        }

        if self.errors.is_empty() {
            let end_span = self.previous.span;
            Ok(Program::new(items, start_span.merge(end_span)))
        } else {
            Err(core::mem::take(&mut self.errors))
        }
    }

    /// Parse a single expression (useful for REPL or expression evaluation).
    pub fn parse_expression_only(&mut self) -> Result<Expr, Vec<ParseError>> {
        match self.parse_expression() {
            Ok(expr) => {
                if self.errors.is_empty() {
                    Ok(expr)
                } else {
                    Err(core::mem::take(&mut self.errors))
                }
            }
            Err(e) => {
                self.errors.push(e);
                Err(core::mem::take(&mut self.errors))
            }
        }
    }

    // =========================================================================
    // Token Handling
    // =========================================================================

    /// Check if we're at the end of the token stream.
    fn is_at_end(&self) -> bool {
        self.current.kind == TokenKind::Eof
    }

    /// Advance to the next token.
    fn advance(&mut self) -> Token {
        self.previous = self.current.clone();
        self.current = self.lexer.next_token();
        self.previous.clone()
    }

    /// Check if the current token matches the expected kind.
    fn check(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    /// Check if the current token is a specific keyword.
    fn check_keyword(&self, name: &str) -> bool {
        if let TokenKind::Keyword(id) = self.current.kind {
            self.lexer.language().keyword_name(id.id()) == Some(name)
        } else {
            false
        }
    }

    /// Consume a token if it matches the expected kind.
    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Consume a keyword if it matches the expected name.
    fn match_keyword(&mut self, name: &str) -> bool {
        if self.check_keyword(name) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Expect a specific token kind, returning an error if not found.
    fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(ParseError::expected_token(
                kind,
                self.current.kind,
                self.current.span,
            ))
        }
    }

    /// Expect a specific keyword, returning an error if not found.
    fn expect_keyword(&mut self, name: &str) -> ParseResult<Token> {
        if self.check_keyword(name) {
            Ok(self.advance())
        } else {
            Err(ParseError::unexpected_token(
                &format!("keyword '{}'", name),
                self.current.kind,
                self.current.span,
            ))
        }
    }

    /// Get the source text for a span.
    fn source_text(&self, span: Span) -> &'a str {
        &self.source[span.start..span.end]
    }

    /// Error recovery: skip tokens until we find a synchronization point.
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            // Stop at statement boundaries
            if self.previous.kind == TokenKind::Semicolon {
                return;
            }

            // Stop at item keywords
            if self.check_keyword("fn")
                || self.check_keyword("struct")
                || self.check_keyword("enum")
                || self.check_keyword("trait")
                || self.check_keyword("impl")
                || self.check_keyword("mod")
                || self.check_keyword("use")
                || self.check_keyword("const")
                || self.check_keyword("let")
                || self.check_keyword("pub")
            {
                return;
            }

            self.advance();
        }
    }

    // =========================================================================
    // Item Parsing
    // =========================================================================

    /// Parse a top-level item.
    fn parse_item(&mut self) -> ParseResult<Item> {
        let start_span = self.current.span;

        // Parse visibility
        let visibility = self.parse_visibility()?;

        // Parse the item kind
        let kind = if self.check_keyword("fn") {
            ItemKind::Fn(self.parse_fn_item()?)
        } else if self.check_keyword("struct") {
            ItemKind::Struct(self.parse_struct_item()?)
        } else if self.check_keyword("enum") {
            ItemKind::Enum(self.parse_enum_item()?)
        } else if self.check_keyword("type") {
            ItemKind::TypeAlias(self.parse_type_alias()?)
        } else if self.check_keyword("trait") {
            ItemKind::Trait(self.parse_trait_item()?)
        } else if self.check_keyword("impl") {
            ItemKind::Impl(self.parse_impl_item()?)
        } else if self.check_keyword("mod") {
            ItemKind::Mod(self.parse_mod_item()?)
        } else if self.check_keyword("use") {
            ItemKind::Use(self.parse_use_item()?)
        } else if self.check_keyword("const") {
            ItemKind::Const(self.parse_const_item()?)
        } else {
            return Err(ParseError::invalid_item(
                "expected fn, struct, enum, type, trait, impl, mod, use, or const",
                self.current.span,
            ));
        };

        let end_span = self.previous.span;
        Ok(Item::new(kind, visibility, start_span.merge(end_span)))
    }

    /// Parse visibility modifier.
    fn parse_visibility(&mut self) -> ParseResult<Visibility> {
        if self.match_keyword("pub") {
            Ok(Visibility::Public)
        } else {
            Ok(Visibility::Private)
        }
    }

    /// Parse a function definition.
    fn parse_fn_item(&mut self) -> ParseResult<FnItem> {
        self.expect_keyword("fn")?;

        // Parse function name
        let name = self.parse_ident()?;

        // Parse optional generics
        let generics = self.parse_optional_generics()?;

        // Parse parameters
        self.expect(TokenKind::LParen)?;
        let params = self.parse_fn_params()?;
        self.expect(TokenKind::RParen)?;

        // Parse optional return type
        let ret_type = if self.match_token(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse body (or just semicolon for declarations)
        let body = if self.check(TokenKind::LBrace) {
            Some(self.parse_block()?)
        } else {
            self.expect(TokenKind::Semicolon)?;
            None
        };

        Ok(FnItem::new(name, generics, params, ret_type, body))
    }

    /// Parse function parameters.
    fn parse_fn_params(&mut self) -> ParseResult<Vec<FnParam>> {
        let mut params = Vec::new();

        if !self.check(TokenKind::RParen) {
            loop {
                // Handle self parameter
                if self.check_keyword("self") || self.check_keyword("Self") {
                    let token = self.advance();
                    let name = Ident::new(self.source_text(token.span).into(), token.span);
                    let ty = Type::new(TypeKind::SelfType, token.span);
                    params.push(FnParam::new(
                        Pattern {
                            kind: PatternKind::Ident(name),
                            span: token.span,
                        },
                        ty,
                        token.span,
                    ));
                } else if self.match_token(TokenKind::Amp) {
                    // &self or &mut self
                    let mutable = self.match_keyword("mut");
                    let self_token = self.expect_keyword("self")?;
                    let name = Ident::new("self".into(), self_token.span);
                    let inner_ty = Type::new(TypeKind::SelfType, self_token.span);
                    let ty = Type::reference(inner_ty, mutable, self_token.span);
                    params.push(FnParam::new(
                        Pattern {
                            kind: PatternKind::Ident(name),
                            span: self_token.span,
                        },
                        ty,
                        self_token.span,
                    ));
                } else {
                    params.push(self.parse_fn_param()?);
                }

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        Ok(params)
    }

    /// Parse a single function parameter.
    fn parse_fn_param(&mut self) -> ParseResult<FnParam> {
        let start_span = self.current.span;
        let pattern = self.parse_pattern()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        let end_span = self.previous.span;

        Ok(FnParam::new(pattern, ty, start_span.merge(end_span)))
    }

    /// Parse a struct definition.
    fn parse_struct_item(&mut self) -> ParseResult<StructItem> {
        self.expect_keyword("struct")?;
        let name = self.parse_ident()?;
        let generics = self.parse_optional_generics()?;

        let kind = if self.check(TokenKind::LBrace) {
            // Named fields
            self.advance();
            let fields = self.parse_struct_fields()?;
            self.expect(TokenKind::RBrace)?;
            StructKind::Named(fields)
        } else if self.check(TokenKind::LParen) {
            // Tuple struct
            self.advance();
            let fields = self.parse_tuple_fields()?;
            self.expect(TokenKind::RParen)?;
            self.expect(TokenKind::Semicolon)?;
            StructKind::Tuple(fields)
        } else {
            // Unit struct
            self.expect(TokenKind::Semicolon)?;
            StructKind::Unit
        };

        Ok(StructItem {
            name,
            generics,
            kind,
        })
    }

    /// Parse struct fields.
    fn parse_struct_fields(&mut self) -> ParseResult<Vec<StructField>> {
        let mut fields = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let start_span = self.current.span;
            let visibility = self.parse_visibility()?;
            let name = self.parse_ident()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            let end_span = self.previous.span;

            fields.push(StructField {
                visibility,
                name,
                ty,
                span: start_span.merge(end_span),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(fields)
    }

    /// Parse tuple struct fields.
    fn parse_tuple_fields(&mut self) -> ParseResult<Vec<TupleField>> {
        let mut fields = Vec::new();

        while !self.check(TokenKind::RParen) && !self.is_at_end() {
            let start_span = self.current.span;
            let visibility = self.parse_visibility()?;
            let ty = self.parse_type()?;
            let end_span = self.previous.span;

            fields.push(TupleField {
                visibility,
                ty,
                span: start_span.merge(end_span),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(fields)
    }

    /// Parse an enum definition.
    fn parse_enum_item(&mut self) -> ParseResult<EnumItem> {
        self.expect_keyword("enum")?;
        let name = self.parse_ident()?;
        let generics = self.parse_optional_generics()?;

        self.expect(TokenKind::LBrace)?;
        let variants = self.parse_enum_variants()?;
        self.expect(TokenKind::RBrace)?;

        Ok(EnumItem {
            name,
            generics,
            variants,
        })
    }

    /// Parse enum variants.
    fn parse_enum_variants(&mut self) -> ParseResult<Vec<EnumVariant>> {
        let mut variants = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let start_span = self.current.span;
            let name = self.parse_ident()?;

            let kind = if self.check(TokenKind::LParen) {
                self.advance();
                let fields = self.parse_tuple_fields()?;
                self.expect(TokenKind::RParen)?;
                VariantKind::Tuple(fields)
            } else if self.check(TokenKind::LBrace) {
                self.advance();
                let fields = self.parse_struct_fields()?;
                self.expect(TokenKind::RBrace)?;
                VariantKind::Struct(fields)
            } else {
                VariantKind::Unit
            };

            // Optional discriminant
            let discriminant = if self.match_token(TokenKind::Eq) {
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };

            let end_span = self.previous.span;
            variants.push(EnumVariant {
                name,
                kind,
                discriminant,
                span: start_span.merge(end_span),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(variants)
    }

    /// Parse a type alias.
    fn parse_type_alias(&mut self) -> ParseResult<TypeAliasItem> {
        self.expect_keyword("type")?;
        let name = self.parse_ident()?;
        let generics = self.parse_optional_generics()?;
        self.expect(TokenKind::Eq)?;
        let ty = self.parse_type()?;
        self.expect(TokenKind::Semicolon)?;

        Ok(TypeAliasItem { name, generics, ty })
    }

    /// Parse a trait definition.
    fn parse_trait_item(&mut self) -> ParseResult<TraitItem> {
        self.expect_keyword("trait")?;
        let name = self.parse_ident()?;
        let generics = self.parse_optional_generics()?;

        // Parse optional super-trait bounds
        let bounds = if self.match_token(TokenKind::Colon) {
            self.parse_type_bounds()?
        } else {
            vec![]
        };

        self.expect(TokenKind::LBrace)?;
        let mut items = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let start_span = self.current.span;

            let kind = if self.check_keyword("fn") {
                TraitItemKind::Fn(self.parse_fn_item()?)
            } else if self.check_keyword("type") {
                let alias = self.parse_type_alias()?;
                TraitItemKind::Type(alias)
            } else if self.check_keyword("const") {
                TraitItemKind::Const(self.parse_const_item()?)
            } else {
                return Err(ParseError::invalid_item(
                    "expected fn, type, or const in trait",
                    self.current.span,
                ));
            };

            let end_span = self.previous.span;
            items.push(TraitItemDef {
                kind,
                span: start_span.merge(end_span),
            });
        }

        self.expect(TokenKind::RBrace)?;

        Ok(TraitItem {
            name,
            generics,
            bounds,
            items,
        })
    }

    /// Parse an impl block.
    fn parse_impl_item(&mut self) -> ParseResult<ImplItem> {
        self.expect_keyword("impl")?;
        let generics = self.parse_optional_generics()?;

        // Try to parse trait impl: impl Trait for Type
        let (trait_path, self_ty) = if self.check_keyword("for") {
            // This shouldn't happen, but handle gracefully
            return Err(ParseError::invalid_item(
                "expected type before 'for'",
                self.current.span,
            ));
        } else {
            let first_ty = self.parse_type()?;

            if self.match_keyword("for") {
                // This is a trait impl
                let self_ty = self.parse_type()?;
                let trait_path = if let TypeKind::Path(path) = first_ty.kind {
                    Some(path)
                } else {
                    return Err(ParseError::invalid_type(
                        "expected trait path",
                        first_ty.span,
                    ));
                };
                (trait_path, self_ty)
            } else {
                (None, first_ty)
            }
        };

        self.expect(TokenKind::LBrace)?;
        let mut items = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let start_span = self.current.span;
            let visibility = self.parse_visibility()?;

            let kind = if self.check_keyword("fn") {
                ImplItemKind::Fn(self.parse_fn_item()?)
            } else if self.check_keyword("type") {
                ImplItemKind::Type(self.parse_type_alias()?)
            } else if self.check_keyword("const") {
                ImplItemKind::Const(self.parse_const_item()?)
            } else {
                return Err(ParseError::invalid_item(
                    "expected fn, type, or const in impl",
                    self.current.span,
                ));
            };

            let end_span = self.previous.span;
            items.push(ImplItemDef {
                visibility,
                kind,
                span: start_span.merge(end_span),
            });
        }

        self.expect(TokenKind::RBrace)?;

        Ok(ImplItem {
            generics,
            trait_path,
            self_ty,
            items,
        })
    }

    /// Parse a module definition.
    fn parse_mod_item(&mut self) -> ParseResult<ModItem> {
        self.expect_keyword("mod")?;
        let name = self.parse_ident()?;

        let items = if self.check(TokenKind::LBrace) {
            self.advance();
            let mut mod_items = Vec::new();
            while !self.check(TokenKind::RBrace) && !self.is_at_end() {
                mod_items.push(self.parse_item()?);
            }
            self.expect(TokenKind::RBrace)?;
            Some(mod_items)
        } else {
            self.expect(TokenKind::Semicolon)?;
            None
        };

        Ok(ModItem { name, items })
    }

    /// Parse a use declaration.
    fn parse_use_item(&mut self) -> ParseResult<UseItem> {
        self.expect_keyword("use")?;
        let tree = self.parse_use_tree()?;
        self.expect(TokenKind::Semicolon)?;

        Ok(UseItem { tree })
    }

    /// Parse a use tree.
    fn parse_use_tree(&mut self) -> ParseResult<UseTree> {
        let start_span = self.current.span;
        let path = self.parse_path()?;

        let kind = if self.match_token(TokenKind::ColonColon) {
            if self.match_token(TokenKind::Star) {
                UseTreeKind::Glob(path)
            } else if self.check(TokenKind::LBrace) {
                self.advance();
                let mut trees = Vec::new();
                while !self.check(TokenKind::RBrace) && !self.is_at_end() {
                    trees.push(self.parse_use_tree()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RBrace)?;
                UseTreeKind::Nested(path, trees)
            } else {
                // Continue path
                let rest = self.parse_use_tree()?;
                let mut combined_path = path;
                if let UseTreeKind::Simple(rest_path, alias) = rest.kind {
                    combined_path.segments.extend(rest_path.segments);
                    combined_path.span = combined_path.span.merge(rest_path.span);
                    UseTreeKind::Simple(combined_path, alias)
                } else {
                    return Err(ParseError::invalid_item(
                        "unexpected use tree",
                        rest.span,
                    ));
                }
            }
        } else {
            // Check for alias
            let alias = if self.match_keyword("as") {
                Some(self.parse_ident()?)
            } else {
                None
            };
            UseTreeKind::Simple(path, alias)
        };

        let end_span = self.previous.span;
        Ok(UseTree {
            kind,
            span: start_span.merge(end_span),
        })
    }

    /// Parse a constant item.
    fn parse_const_item(&mut self) -> ParseResult<ConstItem> {
        self.expect_keyword("const")?;
        let name = self.parse_ident()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        let init = if self.match_token(TokenKind::Eq) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;

        Ok(ConstItem { name, ty, init })
    }

    /// Parse optional generics.
    fn parse_optional_generics(&mut self) -> ParseResult<Option<Generics>> {
        if self.check(TokenKind::Lt) {
            Ok(Some(self.parse_generics()?))
        } else {
            Ok(None)
        }
    }

    /// Parse generics.
    fn parse_generics(&mut self) -> ParseResult<Generics> {
        let start_span = self.current.span;
        self.expect(TokenKind::Lt)?;

        let mut params = Vec::new();
        while !self.check(TokenKind::Gt) && !self.is_at_end() {
            let param_start = self.current.span;
            let name = self.parse_ident()?;

            // Parse optional bounds
            let bounds = if self.match_token(TokenKind::Colon) {
                self.parse_type_bounds()?
            } else {
                vec![]
            };

            // Parse optional default
            let default = if self.match_token(TokenKind::Eq) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let param_end = self.previous.span;
            params.push(GenericParam {
                name,
                bounds,
                default,
                span: param_start.merge(param_end),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::Gt)?;
        let end_span = self.previous.span;

        // Parse optional where clause
        let where_clause = if self.check_keyword("where") {
            Some(self.parse_where_clause()?)
        } else {
            None
        };

        Ok(Generics {
            params,
            where_clause,
            span: start_span.merge(end_span),
        })
    }

    /// Parse type bounds.
    fn parse_type_bounds(&mut self) -> ParseResult<Vec<TypeBound>> {
        let mut bounds = Vec::new();

        loop {
            let start_span = self.current.span;
            let path = self.parse_path()?;
            let end_span = self.previous.span;

            bounds.push(TypeBound {
                path,
                span: start_span.merge(end_span),
            });

            if !self.match_token(TokenKind::Plus) {
                break;
            }
        }

        Ok(bounds)
    }

    /// Parse a where clause.
    fn parse_where_clause(&mut self) -> ParseResult<WhereClause> {
        let start_span = self.current.span;
        self.expect_keyword("where")?;

        let mut predicates = Vec::new();
        loop {
            let pred_start = self.current.span;
            let ty = self.parse_type()?;
            self.expect(TokenKind::Colon)?;
            let bounds = self.parse_type_bounds()?;
            let pred_end = self.previous.span;

            predicates.push(WherePredicate {
                ty,
                bounds,
                span: pred_start.merge(pred_end),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }

            // Check for end of where clause
            if self.check(TokenKind::LBrace) || self.check(TokenKind::Semicolon) {
                break;
            }
        }

        let end_span = self.previous.span;
        Ok(WhereClause {
            predicates,
            span: start_span.merge(end_span),
        })
    }

    // =========================================================================
    // Block and Statement Parsing
    // =========================================================================

    /// Parse a block.
    fn parse_block(&mut self) -> ParseResult<Block> {
        let start_span = self.current.span;
        self.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        let mut final_expr = None;

        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            // Try to parse a statement
            if self.check_keyword("let") {
                stmts.push(self.parse_let_stmt()?);
            } else if self.check_keyword("const") {
                stmts.push(self.parse_const_stmt()?);
            } else {
                // Parse an expression
                let expr = self.parse_expression()?;

                if self.check(TokenKind::Semicolon) {
                    // Expression statement
                    let semi_span = self.advance().span;
                    let stmt_span = expr.span.merge(semi_span);
                    stmts.push(Stmt::semi(expr, stmt_span));
                } else if self.check(TokenKind::RBrace) {
                    // Trailing expression
                    final_expr = Some(Box::new(expr));
                    break;
                } else {
                    // Try treating it as an expression statement
                    stmts.push(Stmt::expr(expr));
                }
            }
        }

        self.expect(TokenKind::RBrace)?;
        let end_span = self.previous.span;

        Ok(Block::new(stmts, final_expr, start_span.merge(end_span)))
    }

    /// Parse a let statement.
    fn parse_let_stmt(&mut self) -> ParseResult<Stmt> {
        let start_span = self.current.span;
        self.expect_keyword("let")?;

        // Parse optional mutability
        let mutability = if self.match_keyword("mut") {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };

        // Parse pattern
        let pattern = self.parse_pattern()?;

        // Parse optional type annotation
        let ty = if self.match_token(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse optional initializer
        let init = if self.match_token(TokenKind::Eq) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;
        let end_span = self.previous.span;

        Ok(Stmt::new(
            StmtKind::Let(LetStmt::new(pattern, mutability, ty, init)),
            start_span.merge(end_span),
        ))
    }

    /// Parse a const statement.
    fn parse_const_stmt(&mut self) -> ParseResult<Stmt> {
        let start_span = self.current.span;
        self.expect_keyword("const")?;

        let name = self.parse_ident()?;

        let ty = if self.match_token(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let init = self.parse_expression()?;
        self.expect(TokenKind::Semicolon)?;
        let end_span = self.previous.span;

        Ok(Stmt::new(
            StmtKind::Const(ConstStmt::new(name, ty, init)),
            start_span.merge(end_span),
        ))
    }

    // =========================================================================
    // Expression Parsing (Pratt Parser / Precedence Climbing)
    // =========================================================================

    /// Parse an expression.
    fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_assignment()
    }

    /// Parse an assignment expression.
    fn parse_assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_or_expr()?;

        if self.match_token(TokenKind::Eq) {
            let value = self.parse_assignment()?;
            let span = expr.span.merge(value.span);
            return Ok(Expr::new(
                ExprKind::Assign(AssignExpr {
                    lhs: Box::new(expr),
                    rhs: Box::new(value),
                }),
                span,
            ));
        }

        Ok(expr)
    }

    /// Parse an or expression.
    fn parse_or_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_and_expr()?;

        while self.match_token(TokenKind::PipePipe) {
            let right = self.parse_and_expr()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(BinOp::Or, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse an and expression.
    fn parse_and_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_bitor_expr()?;

        while self.match_token(TokenKind::AmpAmp) {
            let right = self.parse_bitor_expr()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(BinOp::And, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse a bitwise or expression.
    fn parse_bitor_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_bitxor_expr()?;

        while self.match_token(TokenKind::Pipe) {
            let right = self.parse_bitxor_expr()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(BinOp::BitOr, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse a bitwise xor expression.
    fn parse_bitxor_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_bitand_expr()?;

        while self.match_token(TokenKind::Caret) {
            let right = self.parse_bitand_expr()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(BinOp::BitXor, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse a bitwise and expression.
    fn parse_bitand_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_equality()?;

        while self.match_token(TokenKind::Amp) {
            let right = self.parse_equality()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(BinOp::BitAnd, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse an equality expression.
    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_comparison()?;

        loop {
            let op = if self.match_token(TokenKind::EqEq) {
                BinOp::Eq
            } else if self.match_token(TokenKind::BangEq) {
                BinOp::Ne
            } else {
                break;
            };

            let right = self.parse_comparison()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(op, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse a comparison expression.
    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_shift()?;

        loop {
            let op = if self.match_token(TokenKind::Lt) {
                BinOp::Lt
            } else if self.match_token(TokenKind::Gt) {
                BinOp::Gt
            } else if self.match_token(TokenKind::LtEq) {
                BinOp::Le
            } else if self.match_token(TokenKind::GtEq) {
                BinOp::Ge
            } else {
                break;
            };

            let right = self.parse_shift()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(op, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse a shift expression.
    fn parse_shift(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_additive()?;

        loop {
            let op = if self.match_token(TokenKind::LtLt) {
                BinOp::Shl
            } else if self.match_token(TokenKind::GtGt) {
                BinOp::Shr
            } else {
                break;
            };

            let right = self.parse_additive()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(op, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse an additive expression.
    fn parse_additive(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_multiplicative()?;

        loop {
            let op = if self.match_token(TokenKind::Plus) {
                BinOp::Add
            } else if self.match_token(TokenKind::Minus) {
                BinOp::Sub
            } else {
                break;
            };

            let right = self.parse_multiplicative()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(op, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse a multiplicative expression.
    fn parse_multiplicative(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;

        loop {
            let op = if self.match_token(TokenKind::Star) {
                BinOp::Mul
            } else if self.match_token(TokenKind::Slash) {
                BinOp::Div
            } else if self.match_token(TokenKind::Percent) {
                BinOp::Mod
            } else {
                break;
            };

            let right = self.parse_unary()?;
            let span = expr.span.merge(right.span);
            expr = Expr::binary(op, expr, right, span);
        }

        Ok(expr)
    }

    /// Parse a unary expression.
    fn parse_unary(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;

        if self.match_token(TokenKind::Minus) {
            let operand = self.parse_unary()?;
            let span = start_span.merge(operand.span);
            return Ok(Expr::unary(UnaryOp::Neg, operand, span));
        }

        if self.match_token(TokenKind::Bang) {
            let operand = self.parse_unary()?;
            let span = start_span.merge(operand.span);
            return Ok(Expr::unary(UnaryOp::Not, operand, span));
        }

        if self.match_token(TokenKind::Tilde) {
            let operand = self.parse_unary()?;
            let span = start_span.merge(operand.span);
            return Ok(Expr::unary(UnaryOp::BitNot, operand, span));
        }

        if self.match_token(TokenKind::Star) {
            let operand = self.parse_unary()?;
            let span = start_span.merge(operand.span);
            return Ok(Expr::new(ExprKind::Deref(Box::new(operand)), span));
        }

        if self.match_token(TokenKind::Amp) {
            let mutable = self.match_keyword("mut");
            let operand = self.parse_unary()?;
            let span = start_span.merge(operand.span);
            return Ok(Expr::new(
                ExprKind::Ref(RefExpr {
                    mutable,
                    expr: Box::new(operand),
                }),
                span,
            ));
        }

        self.parse_postfix()
    }

    /// Parse a postfix expression.
    fn parse_postfix(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(TokenKind::LParen) {
                // Function call
                let args = self.parse_call_args()?;
                self.expect(TokenKind::RParen)?;
                let span = expr.span.merge(self.previous.span);
                expr = Expr::new(
                    ExprKind::Call(CallExpr {
                        func: Box::new(expr),
                        args,
                    }),
                    span,
                );
            } else if self.match_token(TokenKind::LBracket) {
                // Index expression
                let index = self.parse_expression()?;
                self.expect(TokenKind::RBracket)?;
                let span = expr.span.merge(self.previous.span);
                expr = Expr::new(
                    ExprKind::Index(IndexExpr {
                        base: Box::new(expr),
                        index: Box::new(index),
                    }),
                    span,
                );
            } else if self.match_token(TokenKind::Dot) {
                // Field access or method call
                let field = self.parse_ident()?;

                if self.check(TokenKind::LParen) {
                    // Method call
                    self.advance();
                    let args = self.parse_call_args()?;
                    self.expect(TokenKind::RParen)?;
                    let span = expr.span.merge(self.previous.span);
                    expr = Expr::new(
                        ExprKind::MethodCall(MethodCallExpr {
                            receiver: Box::new(expr),
                            method: field,
                            args,
                        }),
                        span,
                    );
                } else {
                    // Field access
                    let span = expr.span.merge(field.span);
                    expr = Expr::new(
                        ExprKind::Field(FieldExpr {
                            base: Box::new(expr),
                            field,
                        }),
                        span,
                    );
                }
            } else if self.check_keyword("as") {
                // Cast expression
                self.advance();
                let ty = self.parse_type()?;
                let span = expr.span.merge(ty.span);
                expr = Expr::new(
                    ExprKind::Cast(CastExpr {
                        expr: Box::new(expr),
                        ty,
                    }),
                    span,
                );
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse function call arguments.
    fn parse_call_args(&mut self) -> ParseResult<Vec<Expr>> {
        let mut args = Vec::new();

        if !self.check(TokenKind::RParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        Ok(args)
    }

    /// Parse a primary expression.
    fn parse_primary(&mut self) -> ParseResult<Expr> {
        // Literals
        if let TokenKind::IntLiteral = self.current.kind {
            let token = self.advance();
            let text = self.source_text(token.span);
            let value = parse_int_literal(text).ok_or_else(|| {
                ParseError::invalid_expression("invalid integer literal", token.span)
            })?;
            return Ok(Expr::literal(Literal::Int(value), token.span));
        }

        if let TokenKind::FloatLiteral = self.current.kind {
            let token = self.advance();
            let text = self.source_text(token.span);
            let value: f64 = text.replace('_', "").parse().map_err(|_| {
                ParseError::invalid_expression("invalid float literal", token.span)
            })?;
            return Ok(Expr::literal(Literal::Float(value), token.span));
        }

        if let TokenKind::StringLiteral | TokenKind::RawStringLiteral = self.current.kind {
            let token = self.advance();
            let text = self.source_text(token.span);
            // Remove quotes
            let content = if text.starts_with("r\"") {
                &text[2..text.len() - 1]
            } else {
                &text[1..text.len() - 1]
            };
            return Ok(Expr::literal(Literal::String(content.into()), token.span));
        }

        if let TokenKind::CharLiteral = self.current.kind {
            let token = self.advance();
            let text = self.source_text(token.span);
            // Parse character (simplified - doesn't handle all escapes)
            let content = &text[1..text.len() - 1];
            let ch = if content.starts_with('\\') {
                match content.chars().nth(1) {
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('\'') => '\'',
                    Some('0') => '\0',
                    _ => content.chars().next().unwrap_or(' '),
                }
            } else {
                content.chars().next().unwrap_or(' ')
            };
            return Ok(Expr::literal(Literal::Char(ch), token.span));
        }

        if let TokenKind::BoolLiteral(value) = self.current.kind {
            let token = self.advance();
            return Ok(Expr::literal(Literal::Bool(value), token.span));
        }

        // Keywords
        if self.check_keyword("null") || self.check_keyword("nil") {
            let token = self.advance();
            return Ok(Expr::literal(Literal::Null, token.span));
        }

        if self.check_keyword("if") {
            return self.parse_if_expr();
        }

        if self.check_keyword("match") {
            return self.parse_match_expr();
        }

        if self.check_keyword("loop") {
            return self.parse_loop_expr();
        }

        if self.check_keyword("while") {
            return self.parse_while_expr();
        }

        if self.check_keyword("for") {
            return self.parse_for_expr();
        }

        if self.check_keyword("return") {
            return self.parse_return_expr();
        }

        if self.check_keyword("break") {
            return self.parse_break_expr();
        }

        if self.check_keyword("continue") {
            let token = self.advance();
            return Ok(Expr::new(ExprKind::Continue, token.span));
        }

        // Block expression
        if self.check(TokenKind::LBrace) {
            let block = self.parse_block()?;
            let span = block.span;
            return Ok(Expr::new(ExprKind::Block(block), span));
        }

        // Parenthesized expression or tuple
        if self.check(TokenKind::LParen) {
            return self.parse_paren_or_tuple();
        }

        // Array expression
        if self.check(TokenKind::LBracket) {
            return self.parse_array_expr();
        }

        // Identifier or path
        if self.check(TokenKind::Ident) || self.check_keyword("self") || self.check_keyword("Self") {
            let path = self.parse_path()?;

            // Check for struct literal
            if self.check(TokenKind::LBrace) && self.can_start_struct_literal() {
                return self.parse_struct_literal(path);
            }

            // Check if it's a single identifier
            if path.segments.len() == 1 {
                let span = path.span;
                let ident = path.segments.into_iter().next().unwrap();
                return Ok(Expr::new(ExprKind::Ident(ident), span));
            }

            let span = path.span;
            return Ok(Expr::new(ExprKind::Path(path), span));
        }

        // Closure
        if self.check(TokenKind::Pipe) {
            return self.parse_closure_expr();
        }

        Err(ParseError::unexpected_token(
            "expression",
            self.current.kind,
            self.current.span,
        ))
    }

    /// Check if we can start a struct literal here (simple heuristic).
    /// Struct literals require at least `name: value` pattern to disambiguate from blocks.
    fn can_start_struct_literal(&mut self) -> bool {
        // If struct literals are restricted (e.g., in conditions), don't allow them
        if self.restrictions.no_struct_literal {
            return false;
        }

        // For a path followed by `{`, we need to determine if this is:
        // - A struct literal: `Point { x: 1, y: 2 }` or `Point { x, y }`
        // - A block expression after an identifier: `x { let y = 1; }`
        //
        // Heuristic: Look at what follows the `{`
        // - If it's `}` - could be either empty struct or empty block
        // - If it's `ident :` - definitely a struct literal
        // - If it's `let`, `if`, `while`, etc. - definitely a block
        // - If it's a literal - likely a block (expression)
        //
        // We use peek to look at the token after `{`

        if !self.check(TokenKind::LBrace) {
            return false;
        }

        // Look at what's after the `{`
        let next = self.lexer.peek().clone();

        // If next token is `}`, treat as empty block (conservative)
        if next.kind == TokenKind::RBrace {
            return false;
        }

        // If it starts with a keyword that begins statements, it's a block
        if let TokenKind::Keyword(id) = next.kind {
            if let Some(name) = self.lexer.language().keyword_name(id.id()) {
                match name {
                    "let" | "const" | "if" | "while" | "for" | "loop" | "match" | "return"
                    | "break" | "continue" => return false,
                    _ => {}
                }
            }
        }

        // If it's a literal, it's likely a block expression (e.g., `x { 42 }`)
        if next.kind.is_literal() {
            return false;
        }

        // If it's an identifier, we need to look further
        // For now, we'll be conservative and require seeing `:` pattern
        // This is tricky without multi-token lookahead

        // Best effort: if it's an identifier, check if there might be a colon
        // We can't easily look two tokens ahead, so we'll be optimistic
        // and assume struct literal if it starts with an identifier
        if next.kind == TokenKind::Ident {
            // Could be struct field, could be expression
            // Be optimistic - assume struct literal (user can use parens if needed)
            return true;
        }

        // For unary operators like `-` or `!`, it's likely an expression in a block
        if matches!(
            next.kind,
            TokenKind::Minus | TokenKind::Bang | TokenKind::Amp | TokenKind::Star
        ) {
            return false;
        }

        // Default: not a struct literal
        false
    }

    /// Parse a parenthesized expression or tuple.
    fn parse_paren_or_tuple(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect(TokenKind::LParen)?;

        if self.check(TokenKind::RParen) {
            // Unit tuple
            self.advance();
            let span = start_span.merge(self.previous.span);
            return Ok(Expr::new(ExprKind::Tuple(vec![]), span));
        }

        let first = self.parse_expression()?;

        if self.match_token(TokenKind::Comma) {
            // Tuple
            let mut elements = vec![first];
            while !self.check(TokenKind::RParen) && !self.is_at_end() {
                elements.push(self.parse_expression()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::RParen)?;
            let span = start_span.merge(self.previous.span);
            return Ok(Expr::new(ExprKind::Tuple(elements), span));
        }

        self.expect(TokenKind::RParen)?;
        let span = start_span.merge(self.previous.span);
        Ok(Expr::new(ExprKind::Paren(Box::new(first)), span))
    }

    /// Parse an array expression.
    fn parse_array_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect(TokenKind::LBracket)?;

        let mut elements = Vec::new();
        while !self.check(TokenKind::RBracket) && !self.is_at_end() {
            elements.push(self.parse_expression()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RBracket)?;
        let span = start_span.merge(self.previous.span);
        Ok(Expr::new(ExprKind::Array(elements), span))
    }

    /// Parse a struct literal.
    fn parse_struct_literal(&mut self, path: Path) -> ParseResult<Expr> {
        let start_span = path.span;
        self.expect(TokenKind::LBrace)?;

        let mut fields = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let field_start = self.current.span;
            let name = self.parse_ident()?;

            let value = if self.match_token(TokenKind::Colon) {
                Some(self.parse_expression()?)
            } else {
                None // Shorthand field init
            };

            let field_end = self.previous.span;
            fields.push(FieldInit {
                name,
                value,
                span: field_start.merge(field_end),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RBrace)?;
        let span = start_span.merge(self.previous.span);
        Ok(Expr::new(
            ExprKind::Struct(StructExpr {
                path,
                fields,
                base: None,
            }),
            span,
        ))
    }

    /// Parse a closure expression.
    fn parse_closure_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect(TokenKind::Pipe)?;

        let mut params = Vec::new();
        while !self.check(TokenKind::Pipe) && !self.is_at_end() {
            let param_start = self.current.span;
            let pattern = self.parse_pattern()?;

            let ty = if self.match_token(TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let param_end = self.previous.span;
            params.push(ClosureParam {
                pattern,
                ty,
                span: param_start.merge(param_end),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::Pipe)?;

        // Optional return type
        let ret_type = if self.match_token(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Body
        let body = if self.check(TokenKind::LBrace) {
            let block = self.parse_block()?;
            let span = block.span;
            Expr::new(ExprKind::Block(block), span)
        } else {
            self.parse_expression()?
        };

        let span = start_span.merge(body.span);
        Ok(Expr::new(
            ExprKind::Closure(ClosureExpr {
                params,
                ret_type,
                body: Box::new(body),
            }),
            span,
        ))
    }

    /// Parse an if expression.
    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect_keyword("if")?;

        // Parse condition without allowing struct literals
        let cond = self.parse_expression_with(Restrictions { no_struct_literal: true })?;
        let then_branch = self.parse_block()?;

        let else_branch = if self.match_keyword("else") {
            if self.check_keyword("if") {
                // else if
                Some(Box::new(self.parse_if_expr()?))
            } else {
                // else block
                let block = self.parse_block()?;
                let span = block.span;
                Some(Box::new(Expr::new(ExprKind::Block(block), span)))
            }
        } else {
            None
        };

        let end_span = self.previous.span;
        Ok(Expr::new(
            ExprKind::If(IfExpr {
                cond: Box::new(cond),
                then_branch,
                else_branch,
            }),
            start_span.merge(end_span),
        ))
    }

    /// Parse a match expression.
    fn parse_match_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect_keyword("match")?;

        // Parse scrutinee without allowing struct literals
        let scrutinee = self.parse_expression_with(Restrictions { no_struct_literal: true })?;
        self.expect(TokenKind::LBrace)?;

        let mut arms = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let arm_start = self.current.span;
            let pattern = self.parse_pattern()?;

            // Optional guard
            let guard = if self.match_keyword("if") {
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };

            self.expect(TokenKind::FatArrow)?;
            let body = self.parse_expression()?;
            let arm_end = body.span;

            arms.push(MatchArm {
                pattern,
                guard,
                body: Box::new(body),
                span: arm_start.merge(arm_end),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RBrace)?;
        let span = start_span.merge(self.previous.span);
        Ok(Expr::new(
            ExprKind::Match(MatchExpr {
                scrutinee: Box::new(scrutinee),
                arms,
            }),
            span,
        ))
    }

    /// Parse a loop expression.
    fn parse_loop_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect_keyword("loop")?;

        let body = self.parse_block()?;
        let span = start_span.merge(body.span);
        Ok(Expr::new(ExprKind::Loop(LoopExpr { body }), span))
    }

    /// Parse a while expression.
    fn parse_while_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect_keyword("while")?;

        // Parse condition without allowing struct literals
        let cond = self.parse_expression_with(Restrictions { no_struct_literal: true })?;
        let body = self.parse_block()?;
        let span = start_span.merge(body.span);
        Ok(Expr::new(
            ExprKind::While(WhileExpr {
                cond: Box::new(cond),
                body,
            }),
            span,
        ))
    }

    /// Parse a for expression.
    fn parse_for_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect_keyword("for")?;

        let pattern = self.parse_pattern()?;
        self.expect_keyword("in")?;
        // Parse iterator without allowing struct literals
        let iter = self.parse_expression_with(Restrictions { no_struct_literal: true })?;
        let body = self.parse_block()?;
        let span = start_span.merge(body.span);
        Ok(Expr::new(
            ExprKind::For(ForExpr {
                pattern,
                iter: Box::new(iter),
                body,
            }),
            span,
        ))
    }

    /// Parse a return expression.
    fn parse_return_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect_keyword("return")?;

        let value = if !self.check(TokenKind::Semicolon)
            && !self.check(TokenKind::RBrace)
            && !self.is_at_end()
        {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        let end_span = self.previous.span;
        Ok(Expr::new(
            ExprKind::Return(value),
            start_span.merge(end_span),
        ))
    }

    /// Parse a break expression.
    fn parse_break_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current.span;
        self.expect_keyword("break")?;

        let value = if !self.check(TokenKind::Semicolon)
            && !self.check(TokenKind::RBrace)
            && !self.is_at_end()
        {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        let end_span = self.previous.span;
        Ok(Expr::new(ExprKind::Break(value), start_span.merge(end_span)))
    }

    // =========================================================================
    // Pattern Parsing
    // =========================================================================

    /// Parse a pattern.
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let start_span = self.current.span;

        // Wildcard pattern
        if self.check(TokenKind::Ident) && self.source_text(self.current.span) == "_" {
            self.advance();
            return Ok(Pattern {
                kind: PatternKind::Wildcard,
                span: start_span,
            });
        }

        // Literal patterns
        if let TokenKind::IntLiteral = self.current.kind {
            let token = self.advance();
            let text = self.source_text(token.span);
            let value = parse_int_literal(text).unwrap_or(0);
            return Ok(Pattern {
                kind: PatternKind::Literal(Literal::Int(value)),
                span: token.span,
            });
        }

        if let TokenKind::BoolLiteral(value) = self.current.kind {
            let token = self.advance();
            return Ok(Pattern {
                kind: PatternKind::Literal(Literal::Bool(value)),
                span: token.span,
            });
        }

        if let TokenKind::StringLiteral = self.current.kind {
            let token = self.advance();
            let text = self.source_text(token.span);
            let content = &text[1..text.len() - 1];
            return Ok(Pattern {
                kind: PatternKind::Literal(Literal::String(content.into())),
                span: token.span,
            });
        }

        // Reference pattern
        if self.match_token(TokenKind::Amp) {
            let pattern = self.parse_pattern()?;
            let span = start_span.merge(pattern.span);
            return Ok(Pattern {
                kind: PatternKind::Ref(Box::new(pattern)),
                span,
            });
        }

        // Tuple pattern
        if self.check(TokenKind::LParen) {
            return self.parse_tuple_pattern();
        }

        // Identifier or path pattern
        if self.check(TokenKind::Ident) {
            let ident = self.parse_ident()?;

            // Check for path continuation
            if self.check(TokenKind::ColonColon) {
                let mut path = Path::from_ident(ident);
                while self.match_token(TokenKind::ColonColon) {
                    let next = self.parse_ident()?;
                    path.span = path.span.merge(next.span);
                    path.segments.push(next);
                }

                // Check for variant with fields
                if self.check(TokenKind::LParen) {
                    self.advance();
                    let mut fields = Vec::new();
                    while !self.check(TokenKind::RParen) && !self.is_at_end() {
                        fields.push(self.parse_pattern()?);
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    let span = start_span.merge(self.previous.span);
                    return Ok(Pattern {
                        kind: PatternKind::Variant(path, Some(fields)),
                        span,
                    });
                } else if self.check(TokenKind::LBrace) {
                    // Struct pattern
                    self.advance();
                    let fields = self.parse_field_patterns()?;
                    self.expect(TokenKind::RBrace)?;
                    let span = start_span.merge(self.previous.span);
                    return Ok(Pattern {
                        kind: PatternKind::Struct(path, fields),
                        span,
                    });
                }

                let span = path.span;
                return Ok(Pattern {
                    kind: PatternKind::Variant(path, None),
                    span,
                });
            }

            // Check for struct pattern with current ident as path
            if self.check(TokenKind::LBrace) {
                let path = Path::from_ident(ident);
                self.advance();
                let fields = self.parse_field_patterns()?;
                self.expect(TokenKind::RBrace)?;
                let span = start_span.merge(self.previous.span);
                return Ok(Pattern {
                    kind: PatternKind::Struct(path, fields),
                    span,
                });
            }

            // Simple identifier pattern
            return Ok(Pattern {
                kind: PatternKind::Ident(ident.clone()),
                span: ident.span,
            });
        }

        Err(ParseError::invalid_pattern(
            "expected pattern",
            self.current.span,
        ))
    }

    /// Parse a tuple pattern.
    fn parse_tuple_pattern(&mut self) -> ParseResult<Pattern> {
        let start_span = self.current.span;
        self.expect(TokenKind::LParen)?;

        let mut patterns = Vec::new();
        while !self.check(TokenKind::RParen) && !self.is_at_end() {
            patterns.push(self.parse_pattern()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RParen)?;
        let span = start_span.merge(self.previous.span);
        Ok(Pattern {
            kind: PatternKind::Tuple(patterns),
            span,
        })
    }

    /// Parse field patterns in a struct pattern.
    fn parse_field_patterns(&mut self) -> ParseResult<Vec<FieldPattern>> {
        let mut fields = Vec::new();

        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let field_start = self.current.span;
            let name = self.parse_ident()?;

            let pattern = if self.match_token(TokenKind::Colon) {
                Some(self.parse_pattern()?)
            } else {
                None
            };

            let field_end = self.previous.span;
            fields.push(FieldPattern {
                name,
                pattern,
                span: field_start.merge(field_end),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(fields)
    }

    // =========================================================================
    // Type Parsing
    // =========================================================================

    /// Parse a type.
    fn parse_type(&mut self) -> ParseResult<Type> {
        let start_span = self.current.span;

        // Reference type
        if self.match_token(TokenKind::Amp) {
            let mutable = self.match_keyword("mut");
            let inner = self.parse_type()?;
            let span = start_span.merge(inner.span);
            return Ok(Type::reference(inner, mutable, span));
        }

        // Pointer type
        if self.match_token(TokenKind::Star) {
            let mutable = if self.check_keyword("mut") {
                self.advance();
                true
            } else if self.check_keyword("const") {
                self.advance();
                false
            } else {
                false
            };
            let inner = self.parse_type()?;
            let span = start_span.merge(inner.span);
            return Ok(Type::new(
                TypeKind::Pointer(PointerType {
                    mutable,
                    ty: Box::new(inner),
                }),
                span,
            ));
        }

        // Tuple or unit type
        if self.check(TokenKind::LParen) {
            return self.parse_tuple_type();
        }

        // Array or slice type
        if self.check(TokenKind::LBracket) {
            return self.parse_array_or_slice_type();
        }

        // Never type
        if self.match_token(TokenKind::Bang) {
            return Ok(Type::new(TypeKind::Never, start_span));
        }

        // Self type
        if self.check_keyword("Self") {
            let token = self.advance();
            return Ok(Type::new(TypeKind::SelfType, token.span));
        }

        // Infer type
        if self.check(TokenKind::Ident) && self.source_text(self.current.span) == "_" {
            let token = self.advance();
            return Ok(Type::new(TypeKind::Infer, token.span));
        }

        // Function type
        if self.check_keyword("fn") {
            return self.parse_fn_type();
        }

        // Path type (possibly generic)
        let path = self.parse_path()?;
        let path_span = path.span;

        // Check for generic arguments
        if self.check(TokenKind::Lt) {
            let args = self.parse_generic_args()?;
            let span = start_span.merge(self.previous.span);
            return Ok(Type::new(
                TypeKind::Generic(GenericType { path, args }),
                span,
            ));
        }

        Ok(Type::new(TypeKind::Path(path), path_span))
    }

    /// Parse a tuple type.
    fn parse_tuple_type(&mut self) -> ParseResult<Type> {
        let start_span = self.current.span;
        self.expect(TokenKind::LParen)?;

        let mut types = Vec::new();
        while !self.check(TokenKind::RParen) && !self.is_at_end() {
            types.push(self.parse_type()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RParen)?;
        let span = start_span.merge(self.previous.span);
        Ok(Type::new(TypeKind::Tuple(types), span))
    }

    /// Parse an array or slice type.
    fn parse_array_or_slice_type(&mut self) -> ParseResult<Type> {
        let start_span = self.current.span;
        self.expect(TokenKind::LBracket)?;

        let elem = self.parse_type()?;

        if self.match_token(TokenKind::Semicolon) {
            // Array type with size
            let len = self.parse_expression()?;
            self.expect(TokenKind::RBracket)?;
            let span = start_span.merge(self.previous.span);
            Ok(Type::new(
                TypeKind::Array(ArrayType {
                    elem: Box::new(elem),
                    len: Box::new(len),
                }),
                span,
            ))
        } else {
            // Slice type
            self.expect(TokenKind::RBracket)?;
            let span = start_span.merge(self.previous.span);
            Ok(Type::new(TypeKind::Slice(Box::new(elem)), span))
        }
    }

    /// Parse a function type.
    fn parse_fn_type(&mut self) -> ParseResult<Type> {
        let start_span = self.current.span;
        self.expect_keyword("fn")?;
        self.expect(TokenKind::LParen)?;

        let mut params = Vec::new();
        while !self.check(TokenKind::RParen) && !self.is_at_end() {
            params.push(self.parse_type()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RParen)?;

        let ret = if self.match_token(TokenKind::Arrow) {
            self.parse_type()?
        } else {
            Type::unit(self.previous.span)
        };

        let span = start_span.merge(ret.span);
        Ok(Type::new(
            TypeKind::Fn(FnType {
                params,
                ret: Box::new(ret),
            }),
            span,
        ))
    }

    /// Parse generic arguments.
    fn parse_generic_args(&mut self) -> ParseResult<Vec<GenericArg>> {
        self.expect(TokenKind::Lt)?;

        let mut args = Vec::new();
        while !self.check(TokenKind::Gt) && !self.is_at_end() {
            // Could be a type, lifetime, or const
            let arg = GenericArg::Type(self.parse_type()?);
            args.push(arg);

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::Gt)?;
        Ok(args)
    }

    // =========================================================================
    // Helper Methods
    // =========================================================================

    /// Parse an identifier.
    fn parse_ident(&mut self) -> ParseResult<Ident> {
        if self.check(TokenKind::Ident) {
            let token = self.advance();
            let name = self.source_text(token.span).into();
            Ok(Ident::new(name, token.span))
        } else if self.check_keyword("self") || self.check_keyword("Self") {
            let token = self.advance();
            let name = self.source_text(token.span).into();
            Ok(Ident::new(name, token.span))
        } else {
            Err(ParseError::unexpected_token(
                "identifier",
                self.current.kind,
                self.current.span,
            ))
        }
    }

    /// Parse a path.
    fn parse_path(&mut self) -> ParseResult<Path> {
        let first = self.parse_ident()?;
        let mut path = Path::from_ident(first);

        while self.match_token(TokenKind::ColonColon) {
            let next = self.parse_ident()?;
            path.span = path.span.merge(next.span);
            path.segments.push(next);
        }

        Ok(path)
    }
}

/// Parse an integer literal from text.
fn parse_int_literal(text: &str) -> Option<i64> {
    let text = text.replace('_', "");

    if text.starts_with("0x") || text.starts_with("0X") {
        i64::from_str_radix(&text[2..], 16).ok()
    } else if text.starts_with("0o") || text.starts_with("0O") {
        i64::from_str_radix(&text[2..], 8).ok()
    } else if text.starts_with("0b") || text.starts_with("0B") {
        i64::from_str_radix(&text[2..], 2).ok()
    } else {
        text.parse().ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::DefaultLanguage;

    fn parse_expr(source: &str) -> Expr {
        let mut parser = Parser::new(source, DefaultLanguage);
        parser.parse_expression_only().unwrap()
    }

    fn parse_program(source: &str) -> Program {
        let mut parser = Parser::new(source, DefaultLanguage);
        parser.parse_program().unwrap()
    }

    #[test]
    fn test_integer_literals() {
        let expr = parse_expr("42");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Int(42))));

        let expr = parse_expr("0xFF");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Int(255))));

        let expr = parse_expr("0o77");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Int(63))));

        let expr = parse_expr("0b1010");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Int(10))));
    }

    #[test]
    fn test_float_literals() {
        let expr = parse_expr("3.14");
        if let ExprKind::Literal(Literal::Float(f)) = expr.kind {
            assert!((f - 3.14).abs() < 0.001);
        } else {
            panic!("Expected float literal");
        }
    }

    #[test]
    fn test_string_literals() {
        let expr = parse_expr(r#""hello""#);
        if let ExprKind::Literal(Literal::String(s)) = expr.kind {
            assert_eq!(s, "hello");
        } else {
            panic!("Expected string literal");
        }
    }

    #[test]
    fn test_bool_literals() {
        let expr = parse_expr("true");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Bool(true))));

        let expr = parse_expr("false");
        assert!(matches!(expr.kind, ExprKind::Literal(Literal::Bool(false))));
    }

    #[test]
    fn test_binary_expressions() {
        let expr = parse_expr("1 + 2");
        if let ExprKind::Binary(bin) = expr.kind {
            assert_eq!(bin.op, BinOp::Add);
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_operator_precedence() {
        // Multiplication binds tighter than addition
        let expr = parse_expr("1 + 2 * 3");
        if let ExprKind::Binary(bin) = expr.kind {
            assert_eq!(bin.op, BinOp::Add);
            if let ExprKind::Binary(rhs) = bin.rhs.kind {
                assert_eq!(rhs.op, BinOp::Mul);
            } else {
                panic!("Expected binary expression on right");
            }
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_unary_expressions() {
        let expr = parse_expr("-42");
        if let ExprKind::Unary(un) = expr.kind {
            assert_eq!(un.op, UnaryOp::Neg);
        } else {
            panic!("Expected unary expression");
        }

        let expr = parse_expr("!true");
        if let ExprKind::Unary(un) = expr.kind {
            assert_eq!(un.op, UnaryOp::Not);
        } else {
            panic!("Expected unary expression");
        }
    }

    #[test]
    fn test_function_call() {
        let expr = parse_expr("foo(1, 2, 3)");
        if let ExprKind::Call(call) = expr.kind {
            assert_eq!(call.args.len(), 3);
        } else {
            panic!("Expected call expression");
        }
    }

    #[test]
    fn test_field_access() {
        let expr = parse_expr("obj.field");
        if let ExprKind::Field(field) = expr.kind {
            assert_eq!(field.field.name, "field");
        } else {
            panic!("Expected field expression");
        }
    }

    #[test]
    fn test_method_call() {
        let expr = parse_expr("obj.method(1, 2)");
        if let ExprKind::MethodCall(call) = expr.kind {
            assert_eq!(call.method.name, "method");
            assert_eq!(call.args.len(), 2);
        } else {
            panic!("Expected method call expression");
        }
    }

    #[test]
    fn test_index_expression() {
        let expr = parse_expr("arr[0]");
        if let ExprKind::Index(_) = expr.kind {
            // ok
        } else {
            panic!("Expected index expression");
        }
    }

    #[test]
    fn test_if_expression() {
        let expr = parse_expr("if x { 1 } else { 2 }");
        if let ExprKind::If(if_expr) = expr.kind {
            assert!(if_expr.else_branch.is_some());
        } else {
            panic!("Expected if expression");
        }
    }

    #[test]
    fn test_block_expression() {
        let expr = parse_expr("{ let x = 1; x + 1 }");
        if let ExprKind::Block(block) = expr.kind {
            assert_eq!(block.stmts.len(), 1);
            assert!(block.expr.is_some());
        } else {
            panic!("Expected block expression");
        }
    }

    #[test]
    fn test_tuple_expression() {
        let expr = parse_expr("(1, 2, 3)");
        if let ExprKind::Tuple(elements) = expr.kind {
            assert_eq!(elements.len(), 3);
        } else {
            panic!("Expected tuple expression");
        }
    }

    #[test]
    fn test_array_expression() {
        let expr = parse_expr("[1, 2, 3]");
        if let ExprKind::Array(elements) = expr.kind {
            assert_eq!(elements.len(), 3);
        } else {
            panic!("Expected array expression");
        }
    }

    #[test]
    fn test_closure_expression() {
        let expr = parse_expr("|x, y| x + y");
        if let ExprKind::Closure(closure) = expr.kind {
            assert_eq!(closure.params.len(), 2);
        } else {
            panic!("Expected closure expression");
        }
    }

    #[test]
    fn test_function_item() {
        let program = parse_program("fn foo(x: i32) -> i32 { x + 1 }");
        assert_eq!(program.items.len(), 1);
        if let ItemKind::Fn(fn_item) = &program.items[0].kind {
            assert_eq!(fn_item.name.name, "foo");
            assert_eq!(fn_item.params.len(), 1);
            assert!(fn_item.ret_type.is_some());
            assert!(fn_item.body.is_some());
        } else {
            panic!("Expected function item");
        }
    }

    #[test]
    fn test_struct_item() {
        let program = parse_program("struct Point { x: i32, y: i32 }");
        assert_eq!(program.items.len(), 1);
        if let ItemKind::Struct(struct_item) = &program.items[0].kind {
            assert_eq!(struct_item.name.name, "Point");
            if let StructKind::Named(fields) = &struct_item.kind {
                assert_eq!(fields.len(), 2);
            } else {
                panic!("Expected named struct");
            }
        } else {
            panic!("Expected struct item");
        }
    }

    #[test]
    fn test_enum_item() {
        let program = parse_program("enum Option { None, Some(T) }");
        assert_eq!(program.items.len(), 1);
        if let ItemKind::Enum(enum_item) = &program.items[0].kind {
            assert_eq!(enum_item.name.name, "Option");
            assert_eq!(enum_item.variants.len(), 2);
        } else {
            panic!("Expected enum item");
        }
    }

    #[test]
    fn test_impl_item() {
        let program = parse_program("impl Point { fn new() -> Self { Point { x: 0, y: 0 } } }");
        assert_eq!(program.items.len(), 1);
        if let ItemKind::Impl(impl_item) = &program.items[0].kind {
            assert_eq!(impl_item.items.len(), 1);
        } else {
            panic!("Expected impl item");
        }
    }

    #[test]
    fn test_complex_program() {
        let source = r#"
            fn fibonacci(n: i32) -> i32 {
                if n <= 1 {
                    n
                } else {
                    fibonacci(n - 1) + fibonacci(n - 2)
                }
            }

            fn main() {
                let result = fibonacci(10);
            }
        "#;

        let program = parse_program(source);
        assert_eq!(program.items.len(), 2);
    }

    #[test]
    fn test_generics() {
        let program = parse_program("fn identity<T>(x: T) -> T { x }");
        assert_eq!(program.items.len(), 1);
        if let ItemKind::Fn(fn_item) = &program.items[0].kind {
            assert!(fn_item.generics.is_some());
            let generics = fn_item.generics.as_ref().unwrap();
            assert_eq!(generics.params.len(), 1);
            assert_eq!(generics.params[0].name.name, "T");
        } else {
            panic!("Expected function item");
        }
    }

    #[test]
    fn test_match_expression() {
        let expr = parse_expr("match x { 0 => false, _ => true }");
        if let ExprKind::Match(match_expr) = expr.kind {
            assert_eq!(match_expr.arms.len(), 2);
        } else {
            panic!("Expected match expression");
        }
    }

    #[test]
    fn test_loop_expressions() {
        let expr = parse_expr("loop { break 1 }");
        assert!(matches!(expr.kind, ExprKind::Loop(_)));

        let expr = parse_expr("while x { x = x - 1 }");
        assert!(matches!(expr.kind, ExprKind::While(_)));

        let expr = parse_expr("for i in 0 { i }");
        assert!(matches!(expr.kind, ExprKind::For(_)));
    }

    #[test]
    fn test_reference_expressions() {
        let expr = parse_expr("&x");
        if let ExprKind::Ref(ref_expr) = expr.kind {
            assert!(!ref_expr.mutable);
        } else {
            panic!("Expected ref expression");
        }

        let expr = parse_expr("&mut x");
        if let ExprKind::Ref(ref_expr) = expr.kind {
            assert!(ref_expr.mutable);
        } else {
            panic!("Expected ref expression");
        }
    }

    #[test]
    fn test_cast_expression() {
        let expr = parse_expr("x as i64");
        if let ExprKind::Cast(cast) = expr.kind {
            if let TypeKind::Path(path) = cast.ty.kind {
                assert_eq!(path.segments[0].name, "i64");
            } else {
                panic!("Expected path type");
            }
        } else {
            panic!("Expected cast expression");
        }
    }

    #[test]
    fn test_error_recovery() {
        let source = "fn foo() { @ } fn bar() { 1 }";
        let mut parser = Parser::new(source, DefaultLanguage);
        let result = parser.parse_program();

        // Should recover and parse the second function
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_type_parsing() {
        let program = parse_program("fn foo(x: &mut Vec<i32>) {}");
        if let ItemKind::Fn(fn_item) = &program.items[0].kind {
            let param_ty = &fn_item.params[0].ty;
            if let TypeKind::Reference(ref_ty) = &param_ty.kind {
                assert!(ref_ty.mutable);
            } else {
                panic!("Expected reference type");
            }
        } else {
            panic!("Expected function");
        }
    }
}
