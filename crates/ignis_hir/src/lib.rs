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

impl HIRKind {
  /// Offset all HIRIds in this HIRKind by the given amount.
  pub fn offset_ids(
    &mut self,
    offset: u32,
  ) {
    match self {
      HIRKind::Literal(_) | HIRKind::Variable(_) | HIRKind::Break | HIRKind::Continue | HIRKind::Error => {},
      HIRKind::Binary { left, right, .. } => {
        *left = HIRId::new(left.index() + offset);
        *right = HIRId::new(right.index() + offset);
      },
      HIRKind::Unary { operand, .. } => {
        *operand = HIRId::new(operand.index() + offset);
      },
      HIRKind::Call { args, .. } => {
        for arg in args {
          *arg = HIRId::new(arg.index() + offset);
        }
      },
      HIRKind::Cast { expression, .. } => {
        *expression = HIRId::new(expression.index() + offset);
      },
      HIRKind::Reference { expression, .. } => {
        *expression = HIRId::new(expression.index() + offset);
      },
      HIRKind::Dereference(id) => {
        *id = HIRId::new(id.index() + offset);
      },
      HIRKind::Index { base, index } => {
        *base = HIRId::new(base.index() + offset);
        *index = HIRId::new(index.index() + offset);
      },
      HIRKind::VectorLiteral { elements } => {
        for elem in elements {
          *elem = HIRId::new(elem.index() + offset);
        }
      },
      HIRKind::Let { value, .. } => {
        if let Some(v) = value {
          *v = HIRId::new(v.index() + offset);
        }
      },
      HIRKind::Assign { target, value, .. } => {
        *target = HIRId::new(target.index() + offset);
        *value = HIRId::new(value.index() + offset);
      },
      HIRKind::Block { statements, expression } => {
        for stmt in statements {
          *stmt = HIRId::new(stmt.index() + offset);
        }
        if let Some(expr) = expression {
          *expr = HIRId::new(expr.index() + offset);
        }
      },
      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => {
        *condition = HIRId::new(condition.index() + offset);
        *then_branch = HIRId::new(then_branch.index() + offset);
        if let Some(eb) = else_branch {
          *eb = HIRId::new(eb.index() + offset);
        }
      },
      HIRKind::Loop { condition, body } => {
        condition.offset_ids(offset);
        *body = HIRId::new(body.index() + offset);
      },
      HIRKind::Return(value) => {
        if let Some(v) = value {
          *v = HIRId::new(v.index() + offset);
        }
      },
      HIRKind::ExpressionStatement(id) => {
        *id = HIRId::new(id.index() + offset);
      },
    }
  }
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

  /// Merge another HIR into this one.
  /// All HIRIds from the other HIR are offset to avoid conflicts.
  /// The entry_point from the other HIR is ignored (caller should set it).
  pub fn merge(
    &mut self,
    other: HIR,
  ) {
    let offset = self.nodes.len() as u32;

    // Extend nodes with offset IDs
    for mut node in other.nodes.into_iter() {
      node.kind.offset_ids(offset);
      self.nodes.alloc(node);
    }

    // Merge function bodies with offset HIRIds
    for (def_id, hir_id) in other.function_bodies {
      let new_id = HIRId::new(hir_id.index() + offset);
      self.function_bodies.insert(def_id, new_id);
    }

    // Merge variable inits with offset HIRIds
    for (def_id, hir_id) in other.variables_inits {
      let new_id = HIRId::new(hir_id.index() + offset);
      self.variables_inits.insert(def_id, new_id);
    }

    // Merge items (avoid duplicates)
    for item in other.items {
      if !self.items.contains(&item) {
        self.items.push(item);
      }
    }
  }
}
