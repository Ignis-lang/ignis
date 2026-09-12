pub mod display;
pub mod drop_schedule;
pub mod drop_schedule_dump;
pub mod operation;
pub mod pattern;
pub mod statement;

pub use pattern::HIRPattern;

pub use drop_schedule::{DropSchedules, ExitKey, MoveSite};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinEqKind {
  Primitive,
  Str,
  Method(DefinitionId),
  Pending,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HIRKind {
  // Expression
  Literal(IgnisLiteralValue),
  Unit,
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
  TupleLiteral {
    elements: Vec<HIRId>,
  },
  MakeSlice {
    data: HIRId,
    len: HIRId,
    element_type: TypeId,
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
  BuiltinHash {
    ty: TypeId,
    value: HIRId,
    hasher: HIRId,
  },
  BuiltinEq {
    ty: TypeId,
    left: HIRId,
    right: HIRId,
    kind: BuiltinEqKind,
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
  Defer {
    body: HIRId,
  },
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
  /// Every direct child node of this kind, in a stable order.
  ///
  /// Mirrors the `HIRId` fields walked by [`HIRKind::offset_ids`]; a new variant that
  /// carries `HIRId`s must be added to both.
  pub fn child_ids(&self) -> Vec<HIRId> {
    let mut children = Vec::new();

    match self {
      HIRKind::Literal(_)
      | HIRKind::Unit
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
      HIRKind::Panic(id) | HIRKind::TypeOf(id) | HIRKind::Dereference(id) | HIRKind::ExpressionStatement(id) => {
        children.push(*id)
      },
      HIRKind::BuiltinLoad { ptr, .. } | HIRKind::BuiltinDropInPlace { ptr, .. } => children.push(*ptr),
      HIRKind::BuiltinStore { ptr, value, .. } => {
        children.push(*ptr);
        children.push(*value);
      },
      HIRKind::BuiltinHash { value, hasher, .. } => {
        children.push(*value);
        children.push(*hasher);
      },
      HIRKind::BuiltinEq { left, right, .. } | HIRKind::Binary { left, right, .. } => {
        children.push(*left);
        children.push(*right);
      },
      HIRKind::Unary { operand, .. } => children.push(*operand),
      HIRKind::Call { args, .. } => children.extend(args.iter().copied()),
      HIRKind::CallClosure { callee, args } => {
        children.push(*callee);
        children.extend(args.iter().copied());
      },
      HIRKind::Cast { expression, .. }
      | HIRKind::BitCast { expression, .. }
      | HIRKind::Reference { expression, .. } => children.push(*expression),
      HIRKind::Index { base, index } => {
        children.push(*base);
        children.push(*index);
      },
      HIRKind::VectorLiteral { elements } | HIRKind::TupleLiteral { elements } => {
        children.extend(elements.iter().copied())
      },
      HIRKind::MakeSlice { data, len, .. } => {
        children.push(*data);
        children.push(*len);
      },
      HIRKind::FieldAccess { base, .. } => children.push(*base),
      HIRKind::RecordInit { fields, .. } => children.extend(fields.iter().map(|(_, value)| *value)),
      HIRKind::MethodCall { receiver, args, .. } => {
        if let Some(receiver) = receiver {
          children.push(*receiver);
        }
        children.extend(args.iter().copied());
      },
      HIRKind::EnumVariant { payload, .. } => children.extend(payload.iter().copied()),
      HIRKind::Let { value, .. } => children.extend(value.iter().copied()),
      HIRKind::Assign { target, value, .. } => {
        children.push(*target);
        children.push(*value);
      },
      HIRKind::Block { statements, expression } => {
        children.extend(statements.iter().copied());
        children.extend(expression.iter().copied());
      },
      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => {
        children.push(*condition);
        children.push(*then_branch);
        children.extend(else_branch.iter().copied());
      },
      HIRKind::LetElse { value, else_block, .. } => {
        children.push(*value);
        children.push(*else_block);
      },
      HIRKind::Loop { condition, body } => {
        children.extend(condition.child_ids());
        children.push(*body);
      },
      HIRKind::Return(value) => children.extend(value.iter().copied()),
      HIRKind::Defer { body } | HIRKind::Closure { body, .. } => children.push(*body),
      HIRKind::Match { scrutinee, arms } => {
        children.push(*scrutinee);
        for arm in arms {
          children.extend(arm.guard.iter().copied());
          children.push(arm.body);
        }
      },
    }

    children
  }

  pub fn offset_ids(
    &mut self,
    offset: u32,
  ) {
    match self {
      HIRKind::Literal(_)
      | HIRKind::Unit
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
      HIRKind::BuiltinHash { value, hasher, .. } => {
        *value = HIRId::new(value.index() + offset);
        *hasher = HIRId::new(hasher.index() + offset);
      },
      HIRKind::BuiltinEq { left, right, .. } => {
        *left = HIRId::new(left.index() + offset);
        *right = HIRId::new(right.index() + offset);
      },
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
      HIRKind::TupleLiteral { elements } => {
        for elem in elements {
          *elem = HIRId::new(elem.index() + offset);
        }
      },
      HIRKind::MakeSlice { data, len, .. } => {
        *data = HIRId::new(data.index() + offset);
        *len = HIRId::new(len.index() + offset);
      },
      HIRKind::FieldAccess { base, .. } => {
        *base = HIRId::new(base.index() + offset);
      },
      HIRKind::RecordInit { fields, .. } => {
        for (_, value) in fields {
          *value = HIRId::new(value.index() + offset);
        }
      },
      HIRKind::MethodCall { receiver, args, .. } => {
        if let Some(recv) = receiver {
          *recv = HIRId::new(recv.index() + offset);
        }
        for arg in args {
          *arg = HIRId::new(arg.index() + offset);
        }
      },
      HIRKind::EnumVariant { payload, .. } => {
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
      HIRKind::Defer { body } => {
        *body = HIRId::new(body.index() + offset);
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
