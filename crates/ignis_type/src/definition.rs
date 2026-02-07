use std::collections::HashMap;

use crate::{Id, Store, module::ModuleId, namespace::NamespaceId, span::Span, symbol::SymbolId, types::TypeId};

/// Inline hint for function/method codegen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum InlineMode {
  #[default]
  None,
  /// `inline function`
  Inline,
  /// `inline(always) function`
  Always,
  /// `inline(never) function`
  Never,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstValue {
  Int(i64),
  Float(ordered_float::OrderedFloat<f64>),
  Bool(bool),
  Char(char),
  String(String),
  Array(Vec<ConstValue>),
  Tuple(Vec<ConstValue>),
  Null,
}

pub type DefinitionId = Id<Definition>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolEntry {
  Single(DefinitionId),
  Overload(Vec<DefinitionId>),
}

impl SymbolEntry {
  pub fn as_single(&self) -> Option<&DefinitionId> {
    match self {
      SymbolEntry::Single(id) => Some(id),
      SymbolEntry::Overload(_) => None,
    }
  }

  pub fn is_single(&self) -> bool {
    matches!(self, SymbolEntry::Single(_))
  }

  pub fn is_overload(&self) -> bool {
    matches!(self, SymbolEntry::Overload(_))
  }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Visibility {
  Public,
  Private,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
  pub kind: DefinitionKind,
  pub name: SymbolId,
  /// Span of the entire definition (e.g., whole function/record/etc.)
  pub span: Span,
  /// Span of just the name identifier (for detecting declarations in semantic tokens)
  pub name_span: Span,
  pub visibility: Visibility,
  pub owner_module: ModuleId,
  pub owner_namespace: Option<NamespaceId>,
  pub doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefinitionKind {
  Function(FunctionDefinition),
  Variable(VariableDefinition),
  Constant(ConstantDefinition),
  Parameter(ParameterDefinition),
  Namespace(NamespaceDefinition),
  TypeAlias(TypeAliasDefinition),
  Record(RecordDefinition),
  Enum(EnumDefinition),
  Method(MethodDefinition),
  TypeParam(TypeParamDefinition),
  Field(FieldDefinition),
  Variant(VariantDefinition),
  /// Placeholder for forward references during monomorphization
  Placeholder,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamespaceDefinition {
  pub namespace_id: NamespaceId,
  pub is_extern: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDefinition {
  pub type_params: Vec<DefinitionId>,
  pub params: Vec<DefinitionId>,
  pub return_type: TypeId,
  pub is_extern: bool,
  pub is_variadic: bool,
  pub inline_mode: InlineMode,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableDefinition {
  pub type_id: TypeId,
  pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstantDefinition {
  pub type_id: TypeId,
  pub value: Option<ConstValue>,
  /// For static fields, the owning record/enum definition
  pub owner_type: Option<DefinitionId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterDefinition {
  pub type_id: TypeId,
  pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAliasDefinition {
  pub type_params: Vec<DefinitionId>,
  pub target: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordDefinition {
  pub type_params: Vec<DefinitionId>,
  pub type_id: TypeId,
  pub fields: Vec<RecordFieldDef>,
  pub instance_methods: HashMap<SymbolId, SymbolEntry>,
  pub static_methods: HashMap<SymbolId, SymbolEntry>,
  pub static_fields: HashMap<SymbolId, DefinitionId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordFieldDef {
  pub name: SymbolId,
  pub type_id: TypeId,
  pub index: u32,
  pub span: Span,
  pub def_id: DefinitionId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDefinition {
  pub type_id: TypeId,
  pub owner_type: DefinitionId,
  pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDefinition {
  pub type_params: Vec<DefinitionId>,
  pub type_id: TypeId,
  pub variants: Vec<EnumVariantDef>,
  pub variants_by_name: HashMap<SymbolId, u32>,
  pub tag_type: TypeId,
  pub static_methods: HashMap<SymbolId, SymbolEntry>,
  pub static_fields: HashMap<SymbolId, DefinitionId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantDef {
  pub name: SymbolId,
  pub payload: Vec<TypeId>,
  pub tag_value: u32,
  pub span: Span,
  pub def_id: DefinitionId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantDefinition {
  pub payload: Vec<TypeId>,
  pub owner_enum: DefinitionId,
  pub tag_value: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodDefinition {
  pub owner_type: DefinitionId,
  pub type_params: Vec<DefinitionId>,
  pub params: Vec<DefinitionId>,
  pub return_type: TypeId,
  pub is_static: bool,
  /// Whether the method has `&mut self` (true) or `&self` (false).
  /// Only meaningful for instance methods (is_static=false).
  pub self_mutable: bool,
  pub inline_mode: InlineMode,
}

/// Definition of a type parameter (T, U, etc.)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParamDefinition {
  /// Index in the owner's type parameter list
  pub index: u32,
  /// The function/record/method that declares this type param
  pub owner: DefinitionId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionStore {
  definitions: Store<Definition>,
}

impl Default for DefinitionStore {
  fn default() -> Self {
    Self::new()
  }
}

impl DefinitionStore {
  pub fn new() -> Self {
    Self {
      definitions: Store::new(),
    }
  }

  pub fn alloc(
    &mut self,
    def: Definition,
  ) -> DefinitionId {
    self.definitions.alloc(def)
  }

  pub fn get(
    &self,
    id: &DefinitionId,
  ) -> &Definition {
    self.definitions.get(id)
  }

  pub fn get_mut(
    &mut self,
    id: &DefinitionId,
  ) -> &mut Definition {
    self.definitions.get_mut(id)
  }

  /// Returns the type associated with a definition, or `None` for definitions
  /// that don't have a meaningful type (Namespace, TypeParam, Placeholder).
  pub fn try_type_of(
    &self,
    id: &DefinitionId,
  ) -> Option<&TypeId> {
    match &self.get(id).kind {
      DefinitionKind::Function(f) => Some(&f.return_type),
      DefinitionKind::Variable(v) => Some(&v.type_id),
      DefinitionKind::Constant(c) => Some(&c.type_id),
      DefinitionKind::Parameter(p) => Some(&p.type_id),
      DefinitionKind::Field(f) => Some(&f.type_id),
      DefinitionKind::Variant(v) => match &self.get(&v.owner_enum).kind {
        DefinitionKind::Enum(ed) => Some(&ed.type_id),
        _ => None,
      },
      DefinitionKind::TypeAlias(ta) => Some(&ta.target),
      DefinitionKind::Record(rd) => Some(&rd.type_id),
      DefinitionKind::Enum(ed) => Some(&ed.type_id),
      DefinitionKind::Method(md) => Some(&md.return_type),
      DefinitionKind::Namespace(_) | DefinitionKind::TypeParam(_) | DefinitionKind::Placeholder => None,
    }
  }

  /// Returns the type associated with a definition.
  ///
  /// # Panics
  /// Panics if called on Namespace, TypeParam, or Placeholder definitions.
  pub fn type_of(
    &self,
    id: &DefinitionId,
  ) -> &TypeId {
    self.try_type_of(id).unwrap_or_else(|| {
      let kind = &self.get(id).kind;
      panic!("{kind:?} does not have a type")
    })
  }

  pub fn is_mutable(
    &self,
    id: &DefinitionId,
  ) -> bool {
    match &self.definitions.get(id).kind {
      DefinitionKind::Variable(v) => v.mutable,
      DefinitionKind::Parameter(p) => p.mutable,
      _ => false,
    }
  }

  pub fn get_all(&self) -> &[Definition] {
    self.definitions.get_all()
  }

  pub fn iter(&self) -> impl Iterator<Item = (DefinitionId, &Definition)> {
    self.definitions.iter()
  }

  /// Allocate a placeholder definition that will be filled in later.
  /// Used during monomorphization to reserve an ID before the full definition is ready.
  pub fn alloc_placeholder(
    &mut self,
    name: SymbolId,
    span: Span,
    name_span: Span,
    visibility: Visibility,
    owner_module: ModuleId,
    owner_namespace: Option<NamespaceId>,
  ) -> DefinitionId {
    self.definitions.alloc(Definition {
      kind: DefinitionKind::Placeholder,
      name,
      span,
      name_span,
      visibility,
      owner_module,
      owner_namespace,
      doc: None,
    })
  }

  /// Update a placeholder definition with its real content.
  /// Panics if the definition is not a placeholder.
  pub fn update(
    &mut self,
    id: &DefinitionId,
    kind: DefinitionKind,
  ) {
    let def = self.definitions.get_mut(id);
    assert!(
      matches!(def.kind, DefinitionKind::Placeholder),
      "can only update placeholder definitions"
    );
    def.kind = kind;
  }
}
