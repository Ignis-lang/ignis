pub mod display;
pub mod drop_schedule;
pub mod operation;
pub mod pattern;
pub mod statement;

pub use pattern::HIRPattern;

pub use drop_schedule::{DropSchedules, ExitKey};

use std::collections::HashMap;

use ignis_type::{Id, Store, definition::DefinitionId, span::Span, types::TypeId, value::IgnisLiteralValue};

pub type HIRId = Id<HIRNode>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CaptureMode {
  ByValue,
  ByRef,
  ByMutRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIRCapture {
  pub source_def: DefinitionId,
  /// Position in the env struct.
  pub field_index: u32,
  pub mode: CaptureMode,
  /// T for ByValue, *T for ByRef/ByMutRef.
  pub type_in_env: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    type_args: Vec<TypeId>,
    args: Vec<HIRId>,
  },
  /// Indirect call through a closure value.
  CallClosure {
    callee: HIRId,
    args: Vec<HIRId>,
  },
  Cast {
    expression: HIRId,
    target: TypeId,
  },
  BitCast {
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

  TypeOf(HIRId),
  SizeOf(TypeId),
  AlignOf(TypeId),
  MaxOf(TypeId),
  MinOf(TypeId),
  BuiltinLoad {
    ty: TypeId,
    ptr: HIRId,
  },
  BuiltinStore {
    ty: TypeId,
    ptr: HIRId,
    value: HIRId,
  },
  /// `@dropInPlace<T>(ptr)` — runs T's drop on the pointed-to value.
  BuiltinDropInPlace {
    ty: TypeId,
    ptr: HIRId,
  },
  /// `@dropGlue<T>()` — returns a `(*mut u8) -> void` that drops T at the given address.
  BuiltinDropGlue {
    ty: TypeId,
  },
  Panic(HIRId),
  Trap,
  BuiltinUnreachable,

  // Record/Enum operations
  FieldAccess {
    base: HIRId,
    field_index: u32,
  },
  RecordInit {
    record_def: DefinitionId,
    type_args: Vec<TypeId>,
    fields: Vec<(u32, HIRId)>, // (field_index, value)
  },
  MethodCall {
    receiver: Option<HIRId>, // None for static methods
    method: DefinitionId,
    type_args: Vec<TypeId>,
    args: Vec<HIRId>,
  },
  EnumVariant {
    enum_def: DefinitionId,
    type_args: Vec<TypeId>,
    variant_tag: u32,
    payload: Vec<HIRId>,
  },
  StaticAccess {
    def: DefinitionId,
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
  LetElse {
    pattern: HIRPattern,
    value: HIRId,
    else_block: HIRId,
  },
  Loop {
    condition: statement::LoopKind,
    body: HIRId,
  },
  Break,
  Continue,
  Return(Option<HIRId>),
  ExpressionStatement(HIRId),

  Match {
    scrutinee: HIRId,
    arms: Vec<HIRMatchArm>,
  },

  Closure {
    params: Vec<DefinitionId>,
    return_type: TypeId,
    body: HIRId,
    captures: Vec<HIRCapture>,
    /// When true, env is heap-allocated (closure escapes its defining scope).
    escapes: bool,
    /// Thunk: `(env_ptr: *mut u8, params...) -> ret`. Populated by capture analysis.
    thunk_def: Option<DefinitionId>,
    /// Drop fn: `(env_ptr: *mut u8) -> void`. None if nothing needs dropping.
    drop_def: Option<DefinitionId>,
    /// User-specified capture mode overrides (`@move x`, `@ref x`, `@refMut x`).
    capture_overrides: HashMap<DefinitionId, CaptureMode>,
  },

  // Error recovery
  Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIRMatchArm {
  pub pattern: HIRPattern,
  pub guard: Option<HIRId>,
  pub body: HIRId,
}

impl HIRKind {
  /// Offset all HIRIds in this HIRKind by the given amount.
  pub fn offset_ids(
    &mut self,
    offset: u32,
  ) {
    match self {
      HIRKind::Literal(_)
      | HIRKind::Variable(_)
      | HIRKind::Break
      | HIRKind::Continue
      | HIRKind::Error
      | HIRKind::SizeOf(_)
      | HIRKind::AlignOf(_)
      | HIRKind::MaxOf(_)
      | HIRKind::MinOf(_)
      | HIRKind::StaticAccess { .. }
      | HIRKind::Trap
      | HIRKind::BuiltinUnreachable
      | HIRKind::BuiltinDropGlue { .. } => {},
      HIRKind::Panic(id) => {
        *id = HIRId::new(id.index() + offset);
      },
      HIRKind::TypeOf(id) => {
        *id = HIRId::new(id.index() + offset);
      },
      HIRKind::BuiltinLoad { ptr, .. } | HIRKind::BuiltinDropInPlace { ptr, .. } => {
        *ptr = HIRId::new(ptr.index() + offset);
      },
      HIRKind::BuiltinStore { ptr, value, .. } => {
        *ptr = HIRId::new(ptr.index() + offset);
        *value = HIRId::new(value.index() + offset);
      },
      HIRKind::Binary { left, right, .. } => {
        *left = HIRId::new(left.index() + offset);
        *right = HIRId::new(right.index() + offset);
      },
      HIRKind::Unary { operand, .. } => {
        *operand = HIRId::new(operand.index() + offset);
      },
      HIRKind::Call { args, type_args: _, .. } => {
        for arg in args {
          *arg = HIRId::new(arg.index() + offset);
        }
      },
      HIRKind::CallClosure { callee, args } => {
        *callee = HIRId::new(callee.index() + offset);
        for arg in args {
          *arg = HIRId::new(arg.index() + offset);
        }
      },
      HIRKind::Cast { expression, .. } => {
        *expression = HIRId::new(expression.index() + offset);
      },
      HIRKind::BitCast { expression, .. } => {
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
      HIRKind::FieldAccess { base, .. } => {
        *base = HIRId::new(base.index() + offset);
      },
      HIRKind::RecordInit {
        fields, type_args: _, ..
      } => {
        for (_, value) in fields {
          *value = HIRId::new(value.index() + offset);
        }
      },
      HIRKind::MethodCall {
        receiver,
        args,
        type_args: _,
        ..
      } => {
        if let Some(recv) = receiver {
          *recv = HIRId::new(recv.index() + offset);
        }
        for arg in args {
          *arg = HIRId::new(arg.index() + offset);
        }
      },
      HIRKind::EnumVariant {
        payload, type_args: _, ..
      } => {
        for p in payload {
          *p = HIRId::new(p.index() + offset);
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
      HIRKind::LetElse { value, else_block, .. } => {
        *value = HIRId::new(value.index() + offset);
        *else_block = HIRId::new(else_block.index() + offset);
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
      HIRKind::Match { scrutinee, arms } => {
        *scrutinee = HIRId::new(scrutinee.index() + offset);
        for arm in arms {
          if let Some(g) = &mut arm.guard {
            *g = HIRId::new(g.index() + offset);
          }
          arm.body = HIRId::new(arm.body.index() + offset);
        }
      },
      HIRKind::Closure { body, .. } => {
        *body = HIRId::new(body.index() + offset);
      },
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl Default for HIR {
  fn default() -> Self {
    Self::new()
  }
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

  pub fn get_mut(
    &mut self,
    id: HIRId,
  ) -> &mut HIRNode {
    self.nodes.get_mut(&id)
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
