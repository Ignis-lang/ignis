use std::collections::{HashMap, HashSet};

use ignis_hir::{HIRId, HIRKind, HIR};
use ignis_type::{
  attribute::ParamAttr,
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
};

/// Analyze which closures escape their defining scope.
///
/// A closure escapes if its value (or an alias of it) flows to:
///   - A `Return` statement
///   - A field or static assignment target
///   - A `Call` argument where the corresponding param lacks `@noescape`
///   - A record initializer field or enum variant payload
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
    scan_for_escapes(hir, body_id, defs, &alias_to_closure, &mut escaping_closures);
  }

  escaping_closures
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
      mark_if_closure_ref(hir, *val_id, alias_to_closure, escaping);
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
          mark_if_closure_ref(hir, arg, alias_to_closure, escaping);
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
          mark_if_closure_ref(hir, arg, alias_to_closure, escaping);
        }

        scan_for_escapes(hir, arg, defs, alias_to_closure, escaping);
      }
    },

    // No static param info — args always escape.
    HIRKind::CallClosure { callee, args } => {
      scan_for_escapes(hir, *callee, defs, alias_to_closure, escaping);
      for &arg in args {
        mark_if_closure_ref(hir, arg, alias_to_closure, escaping);
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
        mark_if_closure_ref(hir, *value, alias_to_closure, escaping);
      }

      scan_for_escapes(hir, *target, defs, alias_to_closure, escaping);
      scan_for_escapes(hir, *value, defs, alias_to_closure, escaping);
    },

    HIRKind::RecordInit { fields, .. } => {
      for (_, val) in fields {
        mark_if_closure_ref(hir, *val, alias_to_closure, escaping);
        scan_for_escapes(hir, *val, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::EnumVariant { payload, .. } => {
      for &p in payload {
        mark_if_closure_ref(hir, p, alias_to_closure, escaping);
        scan_for_escapes(hir, p, defs, alias_to_closure, escaping);
      }
    },

    HIRKind::Closure { .. } => {},

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

    HIRKind::VectorLiteral { elements } => {
      for &elem in elements {
        scan_for_escapes(hir, elem, defs, alias_to_closure, escaping);
      }
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

    HIRKind::BuiltinDropInPlace { ptr, .. } => {
      scan_for_escapes(hir, *ptr, defs, alias_to_closure, escaping);
    },

    HIRKind::Defer { body } => {
      scan_for_escapes(hir, *body, defs, alias_to_closure, escaping);
    },

    // Leaf nodes
    HIRKind::Variable(_)
    | HIRKind::Literal(_)
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

/// If the given HIR node is a Variable that aliases a closure, mark that closure as escaping.
fn mark_if_closure_ref(
  hir: &HIR,
  hir_id: HIRId,
  alias_to_closure: &HashMap<DefinitionId, HIRId>,
  escaping: &mut HashSet<HIRId>,
) {
  let node = hir.get(hir_id);
  if let HIRKind::Variable(def_id) = &node.kind
    && let Some(&closure_id) = alias_to_closure.get(def_id)
  {
    escaping.insert(closure_id);
  }
}
