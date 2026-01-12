use std::collections::HashMap;

use crate::{Id, Store, module::ModuleId, namespace::NamespaceId, span::Span, symbol::SymbolId, types::TypeId};

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

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Visibility {
  Public,
  Private,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
  pub kind: DefinitionKind,
  pub name: SymbolId,
  pub span: Span,
  pub visibility: Visibility,
  pub owner_module: ModuleId,
  pub owner_namespace: Option<NamespaceId>,
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamespaceDefinition {
  pub namespace_id: NamespaceId,
  pub is_extern: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDefinition {
  pub params: Vec<DefinitionId>,
  pub return_type: TypeId,
  pub is_extern: bool,
  pub is_variadic: bool,
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
  pub target: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordDefinition {
  pub type_id: TypeId,
  pub fields: Vec<RecordFieldDef>,
  pub instance_methods: HashMap<SymbolId, DefinitionId>,
  pub static_methods: HashMap<SymbolId, DefinitionId>,
  pub static_fields: HashMap<SymbolId, DefinitionId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordFieldDef {
  pub name: SymbolId,
  pub type_id: TypeId,
  pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDefinition {
  pub type_id: TypeId,
  pub variants: Vec<EnumVariantDef>,
  pub variants_by_name: HashMap<SymbolId, u32>,
  pub tag_type: TypeId,
  pub static_methods: HashMap<SymbolId, DefinitionId>,
  pub static_fields: HashMap<SymbolId, DefinitionId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantDef {
  pub name: SymbolId,
  pub payload: Vec<TypeId>,
  pub tag_value: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodDefinition {
  pub owner_type: DefinitionId,
  pub params: Vec<DefinitionId>,
  pub return_type: TypeId,
  pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionStore {
  definitions: Store<Definition>,
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

  pub fn type_of(
    &self,
    id: &DefinitionId,
  ) -> &TypeId {
    match &self.get(id).kind {
      DefinitionKind::Function(f) => &f.return_type,
      DefinitionKind::Variable(v) => &v.type_id,
      DefinitionKind::Constant(c) => &c.type_id,
      DefinitionKind::Parameter(p) => &p.type_id,
      DefinitionKind::Namespace(_) => panic!("namespaces do not have a type"),
      DefinitionKind::TypeAlias(ta) => &ta.target,
      DefinitionKind::Record(rd) => &rd.type_id,
      DefinitionKind::Enum(ed) => &ed.type_id,
      DefinitionKind::Method(md) => &md.return_type,
    }
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
}
