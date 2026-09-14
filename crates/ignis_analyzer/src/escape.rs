use std::collections::{HashMap, HashSet};

use ignis_hir::{HIRId, HIRKind, HIR};
use ignis_type::{
  attribute::ParamAttr,
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
};

/// Analyze which closures escape their defining scope.
///
/// # The rule
///
/// A closure's environment holds its captures. A non-escaping closure keeps that
/// environment in the frame that created it (`struct env e; ... (u8*)&e`); an
/// escaping one must heap-allocate it, because the value outlives the frame.
/// Deciding this wrong in the "non-escaping" direction is a use-after-return, so
/// the analysis is deliberately conservative: when in doubt, escape.
///
/// A closure escapes when its **value** reaches a position that outlives the
/// frame that built it. The analysis is *position-aware*: the escaping value may
/// be the closure expression written in place, a variable that aliases one, or
/// the tail of a block / branch / match arm that produces one. Concretely, a
/// closure escapes when it appears in:
///
///   - a `return` operand, or the tail expression of a function or closure body
///     (the function's result crosses the frame boundary);
///   - an assignment whose target is a field, an index, or a static place
///     (the place outlives the assignment);
///   - a record-initializer field, an enum-variant payload, or a vector/tuple
///     literal element (the aggregate may itself outlive the frame);
///   - an argument of a call whose matching parameter is **not** `@noescape`
///     (the callee may store it), or any argument of an indirect
///     `CallClosure` (no static parameter information is available);
///   - the capture list of another closure that escapes (transitively).
///
/// # Who frees a heap environment
///
/// A heap environment is freed by the binding that owns it, exactly once: the
/// local a closure literal is bound to, or — for a closure that crosses a return
/// — the local in the caller that receives the returned value. So a closure
/// **literal** written as a bare temporary is heap-allocated only where an owner
/// exists on the other side, which is the result position. Everywhere else a
/// bare temporary keeps its stack environment: it lives in the frame that built
/// it, which is alive for the whole call it is handed to, and heap-allocating it
/// there would only produce an environment nothing frees. A closure reached
/// through a variable is heap-allocated in every escaping position, because that
/// variable's binding owns the environment and frees it at scope exit.
///
/// The gap this leaves is a callee that stores a closure it was handed past the
/// call while its caller's frame, or the binding that owns the environment, goes
/// away first. `@noescape` is the callee's promise not to do that, and it is
/// what lets a borrowed reference capture stay sound; see the
/// `ClosureEscapesWithRefCapture` diagnostic raised in `capture.rs`. Modelling
/// the storing case properly needs closure values to carry ownership through
/// aggregates, which they do not today.
///
/// A closure stays non-escaping — and keeps its stack environment — when it is
/// only bound to a local and called locally, or when it is passed to a
/// `@noescape` parameter.
///
/// Returns the set of closure HIRIds that escape.
pub fn analyze_escapes(
  hir: &HIR,
  defs: &DefinitionStore,
) -> HashSet<HIRId> {
  let mut binding_to_closure: HashMap<DefinitionId, HIRId> = HashMap::new();

  for (_hir_id, node) in hir.nodes.iter() {
    if let HIRKind::Let {
      name,
      value: Some(val_id),
    } = &node.kind
    {
      let val_node = hir.get(*val_id);
      if matches!(&val_node.kind, HIRKind::Closure { .. }) {
        binding_to_closure.insert(*name, *val_id);
      }
    }
  }

  // Fixed-point alias propagation: `let b = a` where `a` is a closure binding.
  let mut alias_to_closure: HashMap<DefinitionId, HIRId> = binding_to_closure.clone();
  let mut changed = true;

  while changed {
    changed = false;
    for (_hir_id, node) in hir.nodes.iter() {
      if let HIRKind::Let {
        name,
        value: Some(val_id),
      } = &node.kind
      {
        let val_node = hir.get(*val_id);
        if let HIRKind::Variable(src_def) = &val_node.kind
          && let Some(&closure_id) = alias_to_closure.get(src_def)
          && !alias_to_closure.contains_key(name)
        {
          alias_to_closure.insert(*name, closure_id);
          changed = true;
        }
      }
    }
  }

  let mut escaping_closures: HashSet<HIRId> = HashSet::new();

  for &body_id in hir.function_bodies.values() {
    mark_tail_expression(hir, body_id, &alias_to_closure, &mut escaping_closures);
    scan_for_escapes(hir, body_id, defs, &alias_to_closure, &mut escaping_closures);
  }

  propagate_through_captures(hir, &alias_to_closure, &mut escaping_closures);

  escaping_closures
}

/// A closure captured by an escaping closure outlives the capturing closure's
/// defining frame too, so its own environment must be heap-allocated as well.
fn propagate_through_captures(
  hir: &HIR,
  alias_to_closure: &HashMap<DefinitionId, HIRId>,
  escaping: &mut HashSet<HIRId>,
) {
  let mut changed = true;

  while changed {
    changed = false;

    let pending: Vec<HIRId> = escaping
      .iter()
      .filter_map(|id| match &hir.get(*id).kind {
        HIRKind::Closure { captures, .. } => Some(captures.clone()),
        _ => None,
      })
      .flatten()
      .filter_map(|capture| alias_to_closure.get(&capture.source_def).copied())
      .filter(|inner| !escaping.contains(inner))
      .collect();

    for inner in pending {
      escaping.insert(inner);
      changed = true;
    }
  }
}

/// Mark the tail expression of a block body: its value is the body's result, so
/// it crosses the frame boundary exactly like an explicit `return` operand.
fn mark_tail_expression(
  hir: &HIR,
  body_id: HIRId,
  alias_to_closure: &HashMap<DefinitionId, HIRId>,
  escaping: &mut HashSet<HIRId>,
) {
  if let HIRKind::Block {
    expression: Some(tail), ..
  } = &hir.get(body_id).kind
  {
    mark_escaping_result(hir, *tail, alias_to_closure, escaping);
  }
}

/// Recursively scan an HIR subtree for escape points.
fn scan_for_escapes(
  hir: &HIR,
  hir_id: HIRId,
  defs: &DefinitionStore,
  alias_to_closure: &HashMap<DefinitionId, HIRId>,
  escaping: &mut HashSet<HIRId>,
) {
  let node = hir.get(hir_id);

  match &node.kind {
    HIRKind::Return(Some(val_id)) => {
      mark_escaping_result(hir, *val_id, alias_to_closure, escaping);
      scan_for_escapes(hir, *val_id, defs, alias_to_closure, escaping);
    },

    HIRKind::Call { callee, args, .. } => {
      let callee_def = defs.get(callee);

      let param_defs: Vec<DefinitionId> = match &callee_def.kind {
        DefinitionKind::Function(fd) => fd.params.clone(),
        _ => Vec::new(),
      };

      for (i, &arg) in args.iter().enumerate() {
        let is_noescape = param_defs.get(i).is_some_and(|pid| {
          let p = defs.get(pid);
          matches!(&p.kind, DefinitionKind::Parameter(pd)
            if pd.attrs.iter().any(|a| matches!(a, ParamAttr::NoEscape)))
        });

        if !is_noescape {
          mark_escaping_argument(hir, arg, alias_to_closure, escaping);
        }

        scan_for_escapes(hir, arg, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::MethodCall {
      receiver, method, args, ..
    } => {
      if let Some(recv) = receiver {
        scan_for_escapes(hir, *recv, defs, alias_to_closure, escaping);
      }

      let method_def = defs.get(method);

      let param_defs: Vec<DefinitionId> = match &method_def.kind {
        DefinitionKind::Method(md) => md.params.clone(),
        DefinitionKind::Function(fd) => fd.params.clone(),
        _ => Vec::new(),
      };

      let param_offset = if receiver.is_some() { 1 } else { 0 };

      for (i, &arg) in args.iter().enumerate() {
        let is_noescape = param_defs.get(i + param_offset).is_some_and(|pid| {
          let p = defs.get(pid);
          matches!(&p.kind, DefinitionKind::Parameter(pd)
            if pd.attrs.iter().any(|a| matches!(a, ParamAttr::NoEscape)))
        });

        if !is_noescape {
          mark_escaping_argument(hir, arg, alias_to_closure, escaping);
        }

        scan_for_escapes(hir, arg, defs, alias_to_closure, escaping);
      }
    },

    // No static param info — args always escape.
    HIRKind::CallClosure { callee, args } => {
      scan_for_escapes(hir, *callee, defs, alias_to_closure, escaping);
      for &arg in args {
        mark_escaping_argument(hir, arg, alias_to_closure, escaping);
        scan_for_escapes(hir, arg, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::Assign { target, value, .. } => {
      let target_node = hir.get(*target);
      let is_complex_target = matches!(
        &target_node.kind,
        HIRKind::FieldAccess { .. } | HIRKind::StaticAccess { .. } | HIRKind::Index { .. }
      );

      if is_complex_target {
        mark_escaping_argument(hir, *value, alias_to_closure, escaping);
      }

      scan_for_escapes(hir, *target, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *value, defs, alias_to_closure, escaping);
    },

    HIRKind::RecordInit { fields, .. } => {
      for (_, val) in fields {
        mark_escaping_argument(hir, *val, alias_to_closure, escaping);
        scan_for_escapes(hir, *val, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::EnumVariant { payload, .. } => {
      for &p in payload {
        mark_escaping_argument(hir, p, alias_to_closure, escaping);
        scan_for_escapes(hir, p, defs, alias_to_closure, escaping);
      }
    },

    // A nested closure body is its own frame: its tail expression and inner
    // escape points are analyzed exactly like a function body's.
    HIRKind::Closure { body, .. } => {
      mark_tail_expression(hir, *body, alias_to_closure, escaping);
      scan_for_escapes(hir, *body, defs, alias_to_closure, escaping);
    },

    HIRKind::Block { statements, expression } => {
      for &stmt in statements {
        scan_for_escapes(hir, stmt, defs, alias_to_closure, escaping);
      }
      if let Some(expr) = expression {
        scan_for_escapes(hir, *expr, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::Let { value, .. } => {
      if let Some(v) = value {
        scan_for_escapes(hir, *v, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::If {
      condition,
      then_branch,
      else_branch,
    } => {
      scan_for_escapes(hir, *condition, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *then_branch, defs, alias_to_closure, escaping);
      if let Some(e) = else_branch {
        scan_for_escapes(hir, *e, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::LetElse { value, else_block, .. } => {
      scan_for_escapes(hir, *value, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *else_block, defs, alias_to_closure, escaping);
    },

    HIRKind::Loop { condition, body } => {
      match condition {
        ignis_hir::statement::LoopKind::While { condition: cond } => {
          scan_for_escapes(hir, *cond, defs, alias_to_closure, escaping);
        },
        ignis_hir::statement::LoopKind::For {
          init,
          condition: cond,
          update,
        } => {
          if let Some(i) = init {
            scan_for_escapes(hir, *i, defs, alias_to_closure, escaping);
          }
          if let Some(c) = cond {
            scan_for_escapes(hir, *c, defs, alias_to_closure, escaping);
          }
          if let Some(u) = update {
            scan_for_escapes(hir, *u, defs, alias_to_closure, escaping);
          }
        },
        ignis_hir::statement::LoopKind::Infinite => {},
      }
      scan_for_escapes(hir, *body, defs, alias_to_closure, escaping);
    },

    HIRKind::Return(None) => {},

    HIRKind::ExpressionStatement(expr) => {
      scan_for_escapes(hir, *expr, defs, alias_to_closure, escaping);
    },

    HIRKind::Binary { left, right, .. } => {
      scan_for_escapes(hir, *left, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *right, defs, alias_to_closure, escaping);
    },

    HIRKind::Unary { operand, .. } => {
      scan_for_escapes(hir, *operand, defs, alias_to_closure, escaping);
    },

    HIRKind::Cast { expression, .. } | HIRKind::BitCast { expression, .. } | HIRKind::Reference { expression, .. } => {
      scan_for_escapes(hir, *expression, defs, alias_to_closure, escaping);
    },

    HIRKind::Dereference(expr) | HIRKind::TypeOf(expr) | HIRKind::Panic(expr) => {
      scan_for_escapes(hir, *expr, defs, alias_to_closure, escaping);
    },

    HIRKind::Index { base, index } => {
      scan_for_escapes(hir, *base, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *index, defs, alias_to_closure, escaping);
    },

    HIRKind::FieldAccess { base, .. } => {
      scan_for_escapes(hir, *base, defs, alias_to_closure, escaping);
    },

    HIRKind::VectorLiteral { elements } | HIRKind::TupleLiteral { elements } => {
      for &elem in elements {
        mark_escaping_argument(hir, elem, alias_to_closure, escaping);
        scan_for_escapes(hir, elem, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::MakeSlice { data, len, .. } => {
      scan_for_escapes(hir, *data, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *len, defs, alias_to_closure, escaping);
    },

    HIRKind::Match { scrutinee, arms } => {
      scan_for_escapes(hir, *scrutinee, defs, alias_to_closure, escaping);
      for arm in arms {
        if let Some(g) = arm.guard {
          scan_for_escapes(hir, g, defs, alias_to_closure, escaping);
        }
        scan_for_escapes(hir, arm.body, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::BuiltinLoad { ptr, .. } => {
      scan_for_escapes(hir, *ptr, defs, alias_to_closure, escaping);
    },

    HIRKind::BuiltinStore { ptr, value, .. } => {
      scan_for_escapes(hir, *ptr, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *value, defs, alias_to_closure, escaping);
    },

    HIRKind::BuiltinHash { value, hasher, .. } => {
      scan_for_escapes(hir, *value, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *hasher, defs, alias_to_closure, escaping);
    },

    HIRKind::BuiltinEq { left, right, .. } => {
      scan_for_escapes(hir, *left, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *right, defs, alias_to_closure, escaping);
    },

    HIRKind::BuiltinDropInPlace { ptr, .. } => {
      scan_for_escapes(hir, *ptr, defs, alias_to_closure, escaping);
    },

    HIRKind::Defer { body } => {
      scan_for_escapes(hir, *body, defs, alias_to_closure, escaping);
    },

    // Leaf nodes
    HIRKind::Variable(_)
    | HIRKind::Literal(_)
    | HIRKind::Unit
    | HIRKind::StaticAccess { .. }
    | HIRKind::SizeOf(_)
    | HIRKind::AlignOf(_)
    | HIRKind::MaxOf(_)
    | HIRKind::MinOf(_)
    | HIRKind::Break
    | HIRKind::Continue
    | HIRKind::Trap
    | HIRKind::BuiltinUnreachable
    | HIRKind::BuiltinDropGlue { .. }
    | HIRKind::Error => {},
  }
}

/// Mark the closures a **result** can evaluate to: the operand of a `return`, or
/// the tail expression of a function or closure body.
///
/// The result may be the closure expression itself (`return (x: i32): i32 -> x + c;`),
/// a variable aliasing one (`let f = ...; return f;`), or the result of a block,
/// an `if`/`else`, or a `match` whose branches produce one. A closure literal
/// counts here because the caller's binding takes ownership of the environment.
fn mark_escaping_result(
  hir: &HIR,
  hir_id: HIRId,
  alias_to_closure: &HashMap<DefinitionId, HIRId>,
  escaping: &mut HashSet<HIRId>,
) {
  mark_escaping_value(hir, hir_id, alias_to_closure, escaping, true);
}

/// Mark the closures a value reaching a non-result escaping position can evaluate
/// to: a call argument, an assignment to a field or index, a record-initializer
/// field, an enum payload, a vector or tuple element.
///
/// A closure literal written straight into one of these positions is *not* marked:
/// nothing on the other side owns its environment, so heap-allocating it would
/// only leak it, while leaving it in the frame that built it keeps it alive for at
/// least as long as a binding in that frame would. See "Who frees a heap
/// environment" on [`analyze_escapes`].
fn mark_escaping_argument(
  hir: &HIR,
  hir_id: HIRId,
  alias_to_closure: &HashMap<DefinitionId, HIRId>,
  escaping: &mut HashSet<HIRId>,
) {
  mark_escaping_value(hir, hir_id, alias_to_closure, escaping, false);
}

fn mark_escaping_value(
  hir: &HIR,
  hir_id: HIRId,
  alias_to_closure: &HashMap<DefinitionId, HIRId>,
  escaping: &mut HashSet<HIRId>,
  include_literal: bool,
) {
  let node = hir.get(hir_id);

  match &node.kind {
    HIRKind::Closure { .. } if include_literal => {
      escaping.insert(hir_id);
    },

    HIRKind::Variable(def_id) => {
      if let Some(&closure_id) = alias_to_closure.get(def_id) {
        escaping.insert(closure_id);
      }
    },

    HIRKind::Block {
      expression: Some(tail), ..
    } => {
      mark_escaping_value(hir, *tail, alias_to_closure, escaping, include_literal);
    },

    HIRKind::If {
      then_branch,
      else_branch,
      ..
    } => {
      mark_escaping_value(hir, *then_branch, alias_to_closure, escaping, include_literal);
      if let Some(otherwise) = else_branch {
        mark_escaping_value(hir, *otherwise, alias_to_closure, escaping, include_literal);
      }
    },

    HIRKind::Match { arms, .. } => {
      for arm in arms {
        mark_escaping_value(hir, arm.body, alias_to_closure, escaping, include_literal);
      }
    },

    _ => {},
  }
}
