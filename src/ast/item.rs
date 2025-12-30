//! Item AST nodes.
//!
//! Items are top-level declarations like functions, structs, enums, etc.

use crate::span::Span;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, vec::Vec};

use super::{Block, Expr, Ident, Mutability, Path, Pattern, Type, Visibility};

/// A top-level item.
#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    /// The kind of item.
    pub kind: ItemKind,
    /// The visibility of the item.
    pub visibility: Visibility,
    /// The span of the item.
    pub span: Span,
}

impl Item {
    /// Create a new item.
    pub fn new(kind: ItemKind, visibility: Visibility, span: Span) -> Self {
        Self {
            kind,
            visibility,
            span,
        }
    }
}

/// The kind of item.
#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    /// A function definition.
    Fn(FnItem),
    /// A struct definition.
    Struct(StructItem),
    /// An enum definition.
    Enum(EnumItem),
    /// A type alias.
    TypeAlias(TypeAliasItem),
    /// A trait definition.
    Trait(TraitItem),
    /// An impl block.
    Impl(ImplItem),
    /// A module definition.
    Mod(ModItem),
    /// A use declaration.
    Use(UseItem),
    /// A constant definition.
    Const(ConstItem),
    /// A static variable definition.
    Static(StaticItem),
}

/// A function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct FnItem {
    /// The function name.
    pub name: Ident,
    /// Generic parameters (if any).
    pub generics: Option<Generics>,
    /// The function parameters.
    pub params: Vec<FnParam>,
    /// The return type.
    pub ret_type: Option<Type>,
    /// The function body (None for declarations).
    pub body: Option<Block>,
}

impl FnItem {
    /// Create a new function item.
    pub fn new(
        name: Ident,
        generics: Option<Generics>,
        params: Vec<FnParam>,
        ret_type: Option<Type>,
        body: Option<Block>,
    ) -> Self {
        Self {
            name,
            generics,
            params,
            ret_type,
            body,
        }
    }
}

/// A function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct FnParam {
    /// The parameter pattern.
    pub pattern: Pattern,
    /// The parameter type.
    pub ty: Type,
    /// The span of the parameter.
    pub span: Span,
}

impl FnParam {
    /// Create a new function parameter.
    pub fn new(pattern: Pattern, ty: Type, span: Span) -> Self {
        Self { pattern, ty, span }
    }

    /// Create a simple named parameter.
    pub fn simple(name: Ident, ty: Type) -> Self {
        let span = name.span.merge(ty.span);
        Self {
            pattern: Pattern {
                kind: super::PatternKind::Ident(name),
                span,
            },
            ty,
            span,
        }
    }
}

/// Generic parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct Generics {
    /// The type parameters.
    pub params: Vec<GenericParam>,
    /// Where clause (if any).
    pub where_clause: Option<WhereClause>,
    /// The span of the generics.
    pub span: Span,
}

/// A generic parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    /// The parameter name.
    pub name: Ident,
    /// Bounds on the parameter.
    pub bounds: Vec<TypeBound>,
    /// Default value (if any).
    pub default: Option<Type>,
    /// The span.
    pub span: Span,
}

/// A type bound (trait bound).
#[derive(Debug, Clone, PartialEq)]
pub struct TypeBound {
    /// The trait path.
    pub path: Path,
    /// The span.
    pub span: Span,
}

/// A where clause.
#[derive(Debug, Clone, PartialEq)]
pub struct WhereClause {
    /// The where predicates.
    pub predicates: Vec<WherePredicate>,
    /// The span.
    pub span: Span,
}

/// A where predicate.
#[derive(Debug, Clone, PartialEq)]
pub struct WherePredicate {
    /// The type being constrained.
    pub ty: Type,
    /// The bounds.
    pub bounds: Vec<TypeBound>,
    /// The span.
    pub span: Span,
}

/// A struct definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructItem {
    /// The struct name.
    pub name: Ident,
    /// Generic parameters (if any).
    pub generics: Option<Generics>,
    /// The struct kind (named fields, tuple, unit).
    pub kind: StructKind,
}

/// The kind of struct.
#[derive(Debug, Clone, PartialEq)]
pub enum StructKind {
    /// Named fields (e.g., `struct Point { x: i32, y: i32 }`).
    Named(Vec<StructField>),
    /// Tuple struct (e.g., `struct Point(i32, i32)`).
    Tuple(Vec<TupleField>),
    /// Unit struct (e.g., `struct Unit`).
    Unit,
}

/// A named struct field.
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    /// The visibility of the field.
    pub visibility: Visibility,
    /// The field name.
    pub name: Ident,
    /// The field type.
    pub ty: Type,
    /// The span.
    pub span: Span,
}

/// A tuple struct field.
#[derive(Debug, Clone, PartialEq)]
pub struct TupleField {
    /// The visibility of the field.
    pub visibility: Visibility,
    /// The field type.
    pub ty: Type,
    /// The span.
    pub span: Span,
}

/// An enum definition.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumItem {
    /// The enum name.
    pub name: Ident,
    /// Generic parameters (if any).
    pub generics: Option<Generics>,
    /// The enum variants.
    pub variants: Vec<EnumVariant>,
}

/// An enum variant.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    /// The variant name.
    pub name: Ident,
    /// The variant kind.
    pub kind: VariantKind,
    /// Discriminant value (if specified).
    pub discriminant: Option<Box<Expr>>,
    /// The span.
    pub span: Span,
}

/// The kind of enum variant.
#[derive(Debug, Clone, PartialEq)]
pub enum VariantKind {
    /// Unit variant (e.g., `None`).
    Unit,
    /// Tuple variant (e.g., `Some(T)`).
    Tuple(Vec<TupleField>),
    /// Struct variant (e.g., `Point { x: i32, y: i32 }`).
    Struct(Vec<StructField>),
}

/// A type alias.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasItem {
    /// The alias name.
    pub name: Ident,
    /// Generic parameters (if any).
    pub generics: Option<Generics>,
    /// The aliased type.
    pub ty: Type,
}

/// A trait definition.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitItem {
    /// The trait name.
    pub name: Ident,
    /// Generic parameters (if any).
    pub generics: Option<Generics>,
    /// Super-trait bounds.
    pub bounds: Vec<TypeBound>,
    /// The trait items.
    pub items: Vec<TraitItemDef>,
}

/// An item within a trait definition.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitItemDef {
    /// The kind of trait item.
    pub kind: TraitItemKind,
    /// The span.
    pub span: Span,
}

/// The kind of trait item.
#[derive(Debug, Clone, PartialEq)]
pub enum TraitItemKind {
    /// A method (with optional default implementation).
    Fn(FnItem),
    /// An associated type.
    Type(TypeAliasItem),
    /// An associated constant.
    Const(ConstItem),
}

/// An impl block.
#[derive(Debug, Clone, PartialEq)]
pub struct ImplItem {
    /// Generic parameters (if any).
    pub generics: Option<Generics>,
    /// The trait being implemented (if this is a trait impl).
    pub trait_path: Option<Path>,
    /// The type being implemented.
    pub self_ty: Type,
    /// The impl items.
    pub items: Vec<ImplItemDef>,
}

/// An item within an impl block.
#[derive(Debug, Clone, PartialEq)]
pub struct ImplItemDef {
    /// The visibility.
    pub visibility: Visibility,
    /// The kind of impl item.
    pub kind: ImplItemKind,
    /// The span.
    pub span: Span,
}

/// The kind of impl item.
#[derive(Debug, Clone, PartialEq)]
pub enum ImplItemKind {
    /// A method.
    Fn(FnItem),
    /// An associated type.
    Type(TypeAliasItem),
    /// An associated constant.
    Const(ConstItem),
}

/// A module definition.
#[derive(Debug, Clone, PartialEq)]
pub struct ModItem {
    /// The module name.
    pub name: Ident,
    /// The module items (None for external modules: `mod foo;`).
    pub items: Option<Vec<Item>>,
}

/// A use declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct UseItem {
    /// The use tree.
    pub tree: UseTree,
}

/// A use tree.
#[derive(Debug, Clone, PartialEq)]
pub struct UseTree {
    /// The kind of use tree.
    pub kind: UseTreeKind,
    /// The span.
    pub span: Span,
}

/// The kind of use tree.
#[derive(Debug, Clone, PartialEq)]
pub enum UseTreeKind {
    /// A simple path (e.g., `use std::io`).
    Simple(Path, Option<Ident>),
    /// A glob import (e.g., `use std::io::*`).
    Glob(Path),
    /// Nested imports (e.g., `use std::{io, fs}`).
    Nested(Path, Vec<UseTree>),
}

/// A constant definition.
#[derive(Debug, Clone, PartialEq)]
pub struct ConstItem {
    /// The constant name.
    pub name: Ident,
    /// The type.
    pub ty: Type,
    /// The initializer expression.
    pub init: Option<Box<Expr>>,
}

/// A static variable definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StaticItem {
    /// Whether this is mutable.
    pub mutability: Mutability,
    /// The variable name.
    pub name: Ident,
    /// The type.
    pub ty: Type,
    /// The initializer expression.
    pub init: Option<Box<Expr>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{PatternKind, TypeKind};
    use crate::span::Location;

    fn dummy_span() -> Span {
        Span::new(0, 0, Location::new(), Location::new())
    }

    fn simple_type(name: &str) -> Type {
        Type {
            kind: TypeKind::Path(Path::from_ident(Ident::new(name.into(), dummy_span()))),
            span: dummy_span(),
        }
    }

    #[test]
    fn test_fn_item() {
        let name = Ident::new("foo".into(), dummy_span());
        let fn_item = FnItem::new(name, None, vec![], None, None);
        assert_eq!(fn_item.name.name, "foo");
        assert!(fn_item.params.is_empty());
        assert!(fn_item.ret_type.is_none());
        assert!(fn_item.body.is_none());
    }

    #[test]
    fn test_fn_param() {
        let name = Ident::new("x".into(), dummy_span());
        let ty = simple_type("i32");
        let param = FnParam::simple(name, ty);

        if let PatternKind::Ident(ident) = &param.pattern.kind {
            assert_eq!(ident.name, "x");
        } else {
            panic!("Expected ident pattern");
        }
    }

    #[test]
    fn test_struct_item() {
        let name = Ident::new("Point".into(), dummy_span());
        let fields = vec![
            StructField {
                visibility: Visibility::Private,
                name: Ident::new("x".into(), dummy_span()),
                ty: simple_type("i32"),
                span: dummy_span(),
            },
            StructField {
                visibility: Visibility::Private,
                name: Ident::new("y".into(), dummy_span()),
                ty: simple_type("i32"),
                span: dummy_span(),
            },
        ];
        let struct_item = StructItem {
            name,
            generics: None,
            kind: StructKind::Named(fields),
        };

        assert_eq!(struct_item.name.name, "Point");
        if let StructKind::Named(fields) = struct_item.kind {
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Expected named struct");
        }
    }

    #[test]
    fn test_enum_item() {
        let name = Ident::new("Option".into(), dummy_span());
        let variants = vec![
            EnumVariant {
                name: Ident::new("None".into(), dummy_span()),
                kind: VariantKind::Unit,
                discriminant: None,
                span: dummy_span(),
            },
            EnumVariant {
                name: Ident::new("Some".into(), dummy_span()),
                kind: VariantKind::Tuple(vec![TupleField {
                    visibility: Visibility::Private,
                    ty: simple_type("T"),
                    span: dummy_span(),
                }]),
                discriminant: None,
                span: dummy_span(),
            },
        ];
        let enum_item = EnumItem {
            name,
            generics: None,
            variants,
        };

        assert_eq!(enum_item.name.name, "Option");
        assert_eq!(enum_item.variants.len(), 2);
    }
}
