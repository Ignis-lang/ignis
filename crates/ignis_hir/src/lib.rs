pub mod display;
pub mod operation;
pub mod statement;

use std::collections::HashMap;

use ignis_type::{Id, Store, definition::DefinitionId, span::Span, types::TypeId, value::IgnisLiteralValue};

pub type HIRId = Id<HIRNode>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HIRKind {
  // Expression
  Literal(IgnisLiteralValue),
  Variable(DefinitionId),
  Binary {
    operation: operation::BinaryOperation,
    left: HIRId,
    right: HIRId,
  },
  Unary {
    operation: operation::UnaryOperation,
    operand: HIRId,
  },
  Call {
    callee: DefinitionId,
    args: Vec<HIRId>,
  },
  Cast {
    expression: HIRId,
    target: TypeId,
  },
  Reference {
    expression: HIRId,
    mutable: bool,
  },
  Dereference(HIRId),
  Index {
    base: HIRId,
    index: HIRId,
  },
  VectorLiteral {
    elements: Vec<HIRId>,
  },

  // Statement
  Let {
    name: DefinitionId,
    value: Option<HIRId>,
  },
  Assign {
    target: HIRId,
    value: HIRId,
    operation: Option<operation::BinaryOperation>,
  },
  Block {
    statements: Vec<HIRId>,
    expression: Option<HIRId>,
  },
  If {
    condition: HIRId,
    then_branch: HIRId,
    else_branch: Option<HIRId>,
  },
  Loop {
    condition: statement::LoopKind,
    body: HIRId,
  },
  Break,
  Continue,
  Return(Option<HIRId>),
  ExpressionStatement(HIRId),

  // Error recovery
  Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIRNode {
  pub kind: HIRKind,
  pub span: Span,
  pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct HIR {
  pub nodes: Store<HIRNode>,
  pub function_bodies: HashMap<DefinitionId, HIRId>,
  pub variables_inits: HashMap<DefinitionId, HIRId>,
  pub items: Vec<DefinitionId>,
  pub entry_point: Option<DefinitionId>,
}

impl HIR {
  pub fn new() -> Self {
    Self {
      nodes: Store::new(),
      function_bodies: HashMap::new(),
      variables_inits: HashMap::new(),
      items: Vec::new(),
      entry_point: None,
    }
  }

  pub fn alloc(
    &mut self,
    node: HIRNode,
  ) -> HIRId {
    self.nodes.alloc(node)
  }

  pub fn get(
    &self,
    id: HIRId,
  ) -> &HIRNode {
    self.nodes.get(&id)
  }
}
