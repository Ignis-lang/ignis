use ignis_type::{span::Span, symbol::SymbolId};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum IgnisTypeSyntax {
  I8,
  I16,
  I32,
  I64,
  U8,
  U16,
  U32,
  U64,
  F32,
  F64,
  Unknown,
  String,
  Boolean,
  Void,
  Null,
  Char,
  Vector(Box<IgnisTypeSyntax>, Option<usize>),
  Tuple(Vec<IgnisTypeSyntax>),
  Callable(Vec<IgnisTypeSyntax>, Box<IgnisTypeSyntax>),
  Pointer(Box<IgnisTypeSyntax>),
  Reference {
    inner: Box<IgnisTypeSyntax>,
    mutable: bool,
  },
  Named(SymbolId),
  Applied {
    base: Box<IgnisTypeSyntax>,
    args: Vec<IgnisTypeSyntax>,
  },
  Path {
    segments: Vec<(SymbolId, Span)>,
    args: Vec<IgnisTypeSyntax>,
    span: Span,
  },
  Union(Vec<IgnisTypeSyntax>),
  Intersection(Vec<IgnisTypeSyntax>),
}
