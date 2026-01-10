use crate::{Id, Store, module::ModuleId, span::Span, symbol::SymbolId, types::TypeId};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Definition {
  pub kind: DefinitionKind,
  pub name: SymbolId,
  pub span: Span,
  pub visibility: Visibility,
  pub owner_module: ModuleId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefinitionKind {
  Function(FunctionDefinition),
  Variable(VariableDefinition),
  Constant(ConstantDefinition),
  Parameter(ParameterDefinition),
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterDefinition {
  pub type_id: TypeId,
  pub mutable: bool,
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
}
