//! Type AST nodes.
//!
//! Types represent the type annotations in the source code.

use crate::span::Span;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, vec::Vec};

use super::{Ident, Path};

/// A type node.
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    /// The kind of type.
    pub kind: TypeKind,
    /// The span of the type.
    pub span: Span,
}

impl Type {
    /// Create a new type.
    pub fn new(kind: TypeKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Create a path type from an identifier.
    pub fn from_ident(ident: Ident) -> Self {
        let span = ident.span;
        Self::new(TypeKind::Path(Path::from_ident(ident)), span)
    }

    /// Create the unit type `()`.
    pub fn unit(span: Span) -> Self {
        Self::new(TypeKind::Tuple(vec![]), span)
    }

    /// Create a reference type.
    pub fn reference(ty: Type, mutable: bool, span: Span) -> Self {
        Self::new(
            TypeKind::Reference(ReferenceType {
                mutable,
                ty: Box::new(ty),
            }),
            span,
        )
    }

    /// Check if this is the unit type.
    pub fn is_unit(&self) -> bool {
        matches!(&self.kind, TypeKind::Tuple(types) if types.is_empty())
    }
}

/// The kind of type.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// A path type (e.g., `i32`, `std::io::Result`).
    Path(Path),
    /// A generic type (e.g., `Vec<T>`, `HashMap<K, V>`).
    Generic(GenericType),
    /// A tuple type (e.g., `(i32, String)`).
    Tuple(Vec<Type>),
    /// An array type (e.g., `[i32; 10]`).
    Array(ArrayType),
    /// A slice type (e.g., `[i32]`).
    Slice(Box<Type>),
    /// A reference type (e.g., `&T`, `&mut T`).
    Reference(ReferenceType),
    /// A pointer type (e.g., `*const T`, `*mut T`).
    Pointer(PointerType),
    /// A function type (e.g., `fn(i32) -> bool`).
    Fn(FnType),
    /// The never type (`!`).
    Never,
    /// The inferred type (`_`).
    Infer,
    /// The self type (`Self`).
    SelfType,
    /// A qualified path type (e.g., `<T as Trait>::Item`).
    QualifiedPath(QualifiedPathType),
    /// A trait object type (e.g., `dyn Trait`).
    TraitObject(TraitObjectType),
    /// An impl trait type (e.g., `impl Trait`).
    ImplTrait(ImplTraitType),
}

/// A generic type with type arguments.
#[derive(Debug, Clone, PartialEq)]
pub struct GenericType {
    /// The base path.
    pub path: Path,
    /// The type arguments.
    pub args: Vec<GenericArg>,
}

/// A generic argument.
#[derive(Debug, Clone, PartialEq)]
pub enum GenericArg {
    /// A type argument.
    Type(Type),
    /// A lifetime argument (stored as an identifier).
    Lifetime(Ident),
    /// A const argument.
    Const(Box<super::Expr>),
}

/// An array type.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    /// The element type.
    pub elem: Box<Type>,
    /// The array length expression.
    pub len: Box<super::Expr>,
}

/// A reference type.
#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceType {
    /// Whether this is a mutable reference.
    pub mutable: bool,
    /// The referenced type.
    pub ty: Box<Type>,
}

/// A pointer type.
#[derive(Debug, Clone, PartialEq)]
pub struct PointerType {
    /// Whether this is a mutable pointer.
    pub mutable: bool,
    /// The pointed-to type.
    pub ty: Box<Type>,
}

/// A function type.
#[derive(Debug, Clone, PartialEq)]
pub struct FnType {
    /// The parameter types.
    pub params: Vec<Type>,
    /// The return type.
    pub ret: Box<Type>,
}

/// A qualified path type.
#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedPathType {
    /// The type being qualified.
    pub self_ty: Box<Type>,
    /// The trait path (if any).
    pub trait_path: Option<Path>,
    /// The associated item path segments after the `::`.
    pub path: Path,
}

/// A trait object type.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitObjectType {
    /// The trait bounds.
    pub bounds: Vec<super::TypeBound>,
}

/// An impl trait type.
#[derive(Debug, Clone, PartialEq)]
pub struct ImplTraitType {
    /// The trait bounds.
    pub bounds: Vec<super::TypeBound>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Location;

    fn dummy_span() -> Span {
        Span::new(0, 0, Location::new(), Location::new())
    }

    #[test]
    fn test_type_from_ident() {
        let ident = Ident::new("i32".into(), dummy_span());
        let ty = Type::from_ident(ident);

        if let TypeKind::Path(path) = ty.kind {
            assert_eq!(path.segments.len(), 1);
            assert_eq!(path.segments[0].name, "i32");
        } else {
            panic!("Expected path type");
        }
    }

    #[test]
    fn test_unit_type() {
        let ty = Type::unit(dummy_span());
        assert!(ty.is_unit());
    }

    #[test]
    fn test_reference_type() {
        let inner = Type::from_ident(Ident::new("T".into(), dummy_span()));
        let ty = Type::reference(inner, false, dummy_span());

        if let TypeKind::Reference(ref_ty) = ty.kind {
            assert!(!ref_ty.mutable);
        } else {
            panic!("Expected reference type");
        }
    }

    #[test]
    fn test_mutable_reference_type() {
        let inner = Type::from_ident(Ident::new("T".into(), dummy_span()));
        let ty = Type::reference(inner, true, dummy_span());

        if let TypeKind::Reference(ref_ty) = ty.kind {
            assert!(ref_ty.mutable);
        } else {
            panic!("Expected reference type");
        }
    }

    #[test]
    fn test_generic_type() {
        let path = Path::from_ident(Ident::new("Vec".into(), dummy_span()));
        let arg = GenericArg::Type(Type::from_ident(Ident::new("i32".into(), dummy_span())));
        let generic = GenericType {
            path,
            args: vec![arg],
        };
        let ty = Type::new(TypeKind::Generic(generic.clone()), dummy_span());

        if let TypeKind::Generic(g) = ty.kind {
            assert_eq!(g.path.segments[0].name, "Vec");
            assert_eq!(g.args.len(), 1);
        } else {
            panic!("Expected generic type");
        }
    }

    #[test]
    fn test_tuple_type() {
        let t1 = Type::from_ident(Ident::new("i32".into(), dummy_span()));
        let t2 = Type::from_ident(Ident::new("String".into(), dummy_span()));
        let ty = Type::new(TypeKind::Tuple(vec![t1, t2]), dummy_span());

        if let TypeKind::Tuple(types) = ty.kind {
            assert_eq!(types.len(), 2);
        } else {
            panic!("Expected tuple type");
        }
    }
}
