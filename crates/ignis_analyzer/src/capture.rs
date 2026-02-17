use std::collections::HashSet;

use ignis_diagnostics::diagnostic_report::Diagnostic;
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_hir::{CaptureMode, HIRCapture, HIRId, HIRKind, HIR};
use ignis_type::{
  definition::{
    Definition, DefinitionId, DefinitionKind, DefinitionStore, FunctionDefinition, InlineMode, ParameterDefinition,
    Visibility,
  },
  span::Span,
  symbol::SymbolTable,
  types::TypeStore,
};

/// Tracks how a captured variable is used inside a closure body.
#[derive(Default)]
struct VarUsage {
  /// Variable appears in a read position (not as assign target root).
  read: bool,
  /// Variable (or a path rooted at it) appears as an assignment target.
  mutated: bool,
  /// Variable is passed as an owned, non-Copy argument to a function call.
  moved: bool,
}

/// Determine the capture mode from usage flags.
///
/// Priority: moved > mutated > read-only.
/// - If the variable is moved (non-Copy value consumed by a call), capture by value.
/// - If the variable is mutated (assignment target), capture by mutable reference.
/// - For Copy types that are only read, ByValue avoids unnecessary indirection.
/// - For non-Copy types that are only read, capture by shared reference.
fn infer_capture_mode(
  usage: &VarUsage,
  var_type: ignis_type::types::TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
) -> CaptureMode {
  if usage.moved {
    return CaptureMode::ByValue;
  }

  if usage.mutated {
    return CaptureMode::ByMutRef;
  }

  // For Copy types that are only read, ByValue is simpler (no indirection needed).
  if types.is_copy_with_defs(&var_type, defs) {
    return CaptureMode::ByValue;
  }

  CaptureMode::ByRef
}

/// Walk the closure body HIR and record how `var_def` is used.
///
/// Three usage categories:
/// - **mutated**: the variable (or a path rooted at it) appears as an `Assign` target.
/// - **moved**: the variable is passed as an owned, non-Copy argument to a function call.
/// - **read**: the variable is referenced in any other position.
fn analyze_var_usage(
  hir: &HIR,
  hir_id: HIRId,
  var_def: DefinitionId,
  usage: &mut VarUsage,
  defs: &DefinitionStore,
  types: &TypeStore,
) {
  let node = hir.get(hir_id);

  match &node.kind {
    HIRKind::Assign { target, value, .. } => {
      if is_rooted_at(hir, *target, var_def) {
        usage.mutated = true;
      } else {
        analyze_var_usage_in_target(hir, *target, var_def, usage, defs, types);
      }

      analyze_var_usage(hir, *value, var_def, usage, defs, types);
    },

    HIRKind::Call { callee, args, .. } => {
      let callee_def = defs.get(callee);

      let param_defs: Vec<DefinitionId> = match &callee_def.kind {
        DefinitionKind::Function(fd) => fd.params.clone(),
        _ => Vec::new(),
      };

      for (i, &arg) in args.iter().enumerate() {
        if is_var_reference(hir, arg, var_def) && is_move_for_call_arg(i, &param_defs, defs, types, callee_def) {
          usage.moved = true;
        } else {
          analyze_var_usage(hir, arg, var_def, usage, defs, types);
        }
      }
    },

    HIRKind::MethodCall {
      receiver, method, args, ..
    } => {
      if let Some(recv) = receiver {
        analyze_var_usage(hir, *recv, var_def, usage, defs, types);
      }

      let method_def = defs.get(method);

      let param_defs: Vec<DefinitionId> = match &method_def.kind {
        DefinitionKind::Method(md) => md.params.clone(),
        DefinitionKind::Function(fd) => fd.params.clone(),
        _ => Vec::new(),
      };

      // Method params include self as params[0] for instance methods,
      // so user-visible args map to params[1..] for instance, params[0..] for static.
      let param_offset = if receiver.is_some() { 1 } else { 0 };

      for (i, &arg) in args.iter().enumerate() {
        if is_var_reference(hir, arg, var_def) && is_move_for_method_arg(i + param_offset, &param_defs, defs, types) {
          usage.moved = true;
        } else {
          analyze_var_usage(hir, arg, var_def, usage, defs, types);
        }
      }
    },

    // No static param info — conservatively treat non-Copy args as moves.
    HIRKind::CallClosure { callee, args } => {
      analyze_var_usage(hir, *callee, var_def, usage, defs, types);

      for &arg in args {
        if is_var_reference(hir, arg, var_def) {
          let var_ty = defs.type_of(&var_def);
          if !types.is_copy_with_defs(var_ty, defs) {
            usage.moved = true;
          } else {
            usage.read = true;
          }
        } else {
          analyze_var_usage(hir, arg, var_def, usage, defs, types);
        }
      }
    },

    HIRKind::Let { value, .. } => {
      if let Some(v) = value {
        if is_var_reference(hir, *v, var_def) {
          let var_ty = defs.type_of(&var_def);
          if !types.is_copy_with_defs(var_ty, defs) {
            usage.moved = true;
          } else {
            usage.read = true;
          }
        } else {
          analyze_var_usage(hir, *v, var_def, usage, defs, types);
        }
      }
    },

    HIRKind::Variable(def_id) => {
      if *def_id == var_def {
        usage.read = true;
      }
    },

    HIRKind::Closure { .. } => {},

    HIRKind::Block { statements, expression } => {
      for &stmt in statements {
        analyze_var_usage(hir, stmt, var_def, usage, defs, types);
      }
      if let Some(expr) = expression {
        analyze_var_usage(hir, *expr, var_def, usage, defs, types);
      }
    },

    HIRKind::Binary { left, right, .. } => {
      analyze_var_usage(hir, *left, var_def, usage, defs, types);
      analyze_var_usage(hir, *right, var_def, usage, defs, types);
    },

    HIRKind::Unary { operand, .. } => {
      analyze_var_usage(hir, *operand, var_def, usage, defs, types);
    },

    HIRKind::Cast { expression, .. } | HIRKind::BitCast { expression, .. } | HIRKind::Reference { expression, .. } => {
      analyze_var_usage(hir, *expression, var_def, usage, defs, types);
    },

    HIRKind::Dereference(expr) | HIRKind::TypeOf(expr) | HIRKind::ExpressionStatement(expr) | HIRKind::Panic(expr) => {
      analyze_var_usage(hir, *expr, var_def, usage, defs, types);
    },

    HIRKind::Index { base, index } => {
      analyze_var_usage(hir, *base, var_def, usage, defs, types);
      analyze_var_usage(hir, *index, var_def, usage, defs, types);
    },

    HIRKind::FieldAccess { base, .. } => {
      analyze_var_usage(hir, *base, var_def, usage, defs, types);
    },

    HIRKind::VectorLiteral { elements } => {
      for &elem in elements {
        analyze_var_usage(hir, elem, var_def, usage, defs, types);
      }
    },

    HIRKind::RecordInit { fields, .. } => {
      for (_, value) in fields {
        analyze_var_usage(hir, *value, var_def, usage, defs, types);
      }
    },

    HIRKind::EnumVariant { payload, .. } => {
      for &p in payload {
        analyze_var_usage(hir, p, var_def, usage, defs, types);
      }
    },

    HIRKind::If {
      condition,
      then_branch,
      else_branch,
    } => {
      analyze_var_usage(hir, *condition, var_def, usage, defs, types);
      analyze_var_usage(hir, *then_branch, var_def, usage, defs, types);
      if let Some(e) = else_branch {
        analyze_var_usage(hir, *e, var_def, usage, defs, types);
      }
    },

    HIRKind::LetElse { value, else_block, .. } => {
      analyze_var_usage(hir, *value, var_def, usage, defs, types);
      analyze_var_usage(hir, *else_block, var_def, usage, defs, types);
    },

    HIRKind::Loop { condition, body } => {
      match condition {
        ignis_hir::statement::LoopKind::While { condition: cond } => {
          analyze_var_usage(hir, *cond, var_def, usage, defs, types);
        },
        ignis_hir::statement::LoopKind::For {
          init,
          condition: cond,
          update,
        } => {
          if let Some(i) = init {
            analyze_var_usage(hir, *i, var_def, usage, defs, types);
          }
          if let Some(c) = cond {
            analyze_var_usage(hir, *c, var_def, usage, defs, types);
          }
          if let Some(u) = update {
            analyze_var_usage(hir, *u, var_def, usage, defs, types);
          }
        },
        ignis_hir::statement::LoopKind::Infinite => {},
      }
      analyze_var_usage(hir, *body, var_def, usage, defs, types);
    },

    HIRKind::Return(expr) => {
      if let Some(e) = expr {
        analyze_var_usage(hir, *e, var_def, usage, defs, types);
      }
    },

    HIRKind::Match { scrutinee, arms } => {
      analyze_var_usage(hir, *scrutinee, var_def, usage, defs, types);
      for arm in arms {
        if let Some(g) = arm.guard {
          analyze_var_usage(hir, g, var_def, usage, defs, types);
        }
        analyze_var_usage(hir, arm.body, var_def, usage, defs, types);
      }
    },

    HIRKind::BuiltinLoad { ptr, .. } => {
      analyze_var_usage(hir, *ptr, var_def, usage, defs, types);
    },

    HIRKind::BuiltinStore { ptr, value, .. } => {
      analyze_var_usage(hir, *ptr, var_def, usage, defs, types);
      analyze_var_usage(hir, *value, var_def, usage, defs, types);
    },

    HIRKind::BuiltinDropInPlace { ptr, .. } => {
      analyze_var_usage(hir, *ptr, var_def, usage, defs, types);
    },

    HIRKind::Defer { body } => {
      analyze_var_usage(hir, *body, var_def, usage, defs, types);
    },

    // Leaf nodes
    HIRKind::Literal(_)
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

/// Check if the HIR node is a direct `Variable(def_id)` reference to `var_def`.
fn is_var_reference(
  hir: &HIR,
  hir_id: HIRId,
  var_def: DefinitionId,
) -> bool {
  matches!(hir.get(hir_id).kind, HIRKind::Variable(def_id) if def_id == var_def)
}

/// Check whether an assignment target is directly rooted at `var_def`.
///
/// `x = ...`          → true  (direct mutation of x)
/// `x.field = ...`    → true  (mutation of field within x)
/// `x[i] = ...`       → true  (mutation of element within x)
/// `*x = ...`         → false (x is read/dereferenced, the pointee is mutated)
///
/// Dereference breaks the chain: `*p = v` writes to the memory `p` points at,
/// not to `p` itself.
fn is_rooted_at(
  hir: &HIR,
  target_id: HIRId,
  var_def: DefinitionId,
) -> bool {
  let node = hir.get(target_id);
  match &node.kind {
    HIRKind::Variable(def_id) => *def_id == var_def,
    HIRKind::FieldAccess { base, .. } => is_rooted_at(hir, *base, var_def),
    HIRKind::Index { base, .. } => is_rooted_at(hir, *base, var_def),
    // Dereference does NOT propagate: *p = v reads p, writes *p.
    _ => false,
  }
}

/// Walk an assign target for variable references in non-root positions
/// (e.g. the index expression in `arr[x]`).
///
/// Dereference switches to read-position tracking: `*p = v` reads `p`.
fn analyze_var_usage_in_target(
  hir: &HIR,
  target_id: HIRId,
  var_def: DefinitionId,
  usage: &mut VarUsage,
  defs: &DefinitionStore,
  types: &TypeStore,
) {
  let node = hir.get(target_id);
  match &node.kind {
    HIRKind::Index { base, index } => {
      analyze_var_usage_in_target(hir, *base, var_def, usage, defs, types);
      // The index expression is in read position.
      analyze_var_usage(hir, *index, var_def, usage, defs, types);
    },
    HIRKind::FieldAccess { base, .. } => {
      analyze_var_usage_in_target(hir, *base, var_def, usage, defs, types);
    },
    // Dereference: the inner expression is fully in read position.
    HIRKind::Dereference(inner) => {
      analyze_var_usage(hir, *inner, var_def, usage, defs, types);
    },
    _ => {},
  }
}

/// Determine if the i-th argument to a direct function call is a move.
///
/// A move occurs when the parameter's type is non-Copy. For extern functions,
/// the `@takes` attribute must also be present on the parameter.
fn is_move_for_call_arg(
  arg_index: usize,
  param_defs: &[DefinitionId],
  defs: &DefinitionStore,
  types: &TypeStore,
  callee_def: &ignis_type::definition::Definition,
) -> bool {
  let Some(&param_id) = param_defs.get(arg_index) else {
    // Variadic trailing args: conservatively treat as non-move.
    return false;
  };

  let param = defs.get(&param_id);
  let param_type = match &param.kind {
    DefinitionKind::Parameter(pd) => pd.type_id,
    _ => return false,
  };

  if types.is_copy_with_defs(&param_type, defs) {
    return false;
  }

  // For extern functions, only `@takes` params consume ownership.
  let is_extern = matches!(&callee_def.kind, DefinitionKind::Function(fd) if fd.is_extern);
  if is_extern {
    let has_takes = matches!(&param.kind, DefinitionKind::Parameter(pd)
      if pd.attrs.iter().any(|a| matches!(a, ignis_type::attribute::ParamAttr::Takes)));
    return has_takes;
  }

  true
}

/// Determine if the i-th parameter position of a method call is a move.
fn is_move_for_method_arg(
  param_index: usize,
  param_defs: &[DefinitionId],
  defs: &DefinitionStore,
  types: &TypeStore,
) -> bool {
  let Some(&param_id) = param_defs.get(param_index) else {
    return false;
  };

  let param = defs.get(&param_id);
  let param_type = match &param.kind {
    DefinitionKind::Parameter(pd) => pd.type_id,
    _ => return false,
  };

  // Methods are always non-extern in Ignis, so no @takes check needed.
  !types.is_copy_with_defs(&param_type, defs)
}

/// Collect all `Closure` HIR nodes in bottom-up order (innermost first).
///
/// Innermost closures must be processed first so their captures are populated
/// before the enclosing closure's free-var scan propagates transitive captures.
fn collect_closures_bottom_up(hir: &HIR) -> Vec<HIRId> {
  let mut result = Vec::new();
  let mut visited = HashSet::new();

  for &body_id in hir.function_bodies.values() {
    collect_closures_postorder(hir, body_id, &mut result, &mut visited);
  }

  // Also scan module-level variable/constant init expressions (e.g. `const f = lambda`).
  for &init_id in hir.variables_inits.values() {
    collect_closures_postorder(hir, init_id, &mut result, &mut visited);
  }

  result
}

/// Post-order traversal: descend into children first, then emit Closure nodes.
fn collect_closures_postorder(
  hir: &HIR,
  hir_id: HIRId,
  result: &mut Vec<HIRId>,
  visited: &mut HashSet<HIRId>,
) {
  if !visited.insert(hir_id) {
    return;
  }

  let node = hir.get(hir_id);

  match &node.kind {
    HIRKind::Closure { body, .. } => {
      collect_closures_postorder(hir, *body, result, visited);
      result.push(hir_id);
    },

    HIRKind::Block { statements, expression } => {
      for &s in statements {
        collect_closures_postorder(hir, s, result, visited);
      }
      if let Some(e) = expression {
        collect_closures_postorder(hir, *e, result, visited);
      }
    },

    HIRKind::Let { value, .. } => {
      if let Some(v) = value {
        collect_closures_postorder(hir, *v, result, visited);
      }
    },

    HIRKind::LetElse { value, else_block, .. } => {
      collect_closures_postorder(hir, *value, result, visited);
      collect_closures_postorder(hir, *else_block, result, visited);
    },

    HIRKind::If {
      condition,
      then_branch,
      else_branch,
    } => {
      collect_closures_postorder(hir, *condition, result, visited);
      collect_closures_postorder(hir, *then_branch, result, visited);
      if let Some(e) = else_branch {
        collect_closures_postorder(hir, *e, result, visited);
      }
    },

    HIRKind::Loop { condition, body } => {
      match condition {
        ignis_hir::statement::LoopKind::While { condition: cond } => {
          collect_closures_postorder(hir, *cond, result, visited);
        },
        ignis_hir::statement::LoopKind::For {
          init,
          condition: cond,
          update,
        } => {
          if let Some(i) = init {
            collect_closures_postorder(hir, *i, result, visited);
          }
          if let Some(c) = cond {
            collect_closures_postorder(hir, *c, result, visited);
          }
          if let Some(u) = update {
            collect_closures_postorder(hir, *u, result, visited);
          }
        },
        ignis_hir::statement::LoopKind::Infinite => {},
      }
      collect_closures_postorder(hir, *body, result, visited);
    },

    HIRKind::Binary { left, right, .. } => {
      collect_closures_postorder(hir, *left, result, visited);
      collect_closures_postorder(hir, *right, result, visited);
    },

    HIRKind::Unary { operand, .. } => {
      collect_closures_postorder(hir, *operand, result, visited);
    },

    HIRKind::Call { args, .. } => {
      for &a in args {
        collect_closures_postorder(hir, a, result, visited);
      }
    },

    HIRKind::CallClosure { callee, args } => {
      collect_closures_postorder(hir, *callee, result, visited);
      for &a in args {
        collect_closures_postorder(hir, a, result, visited);
      }
    },

    HIRKind::MethodCall { receiver, args, .. } => {
      if let Some(r) = receiver {
        collect_closures_postorder(hir, *r, result, visited);
      }
      for &a in args {
        collect_closures_postorder(hir, a, result, visited);
      }
    },

    HIRKind::Return(e) => {
      if let Some(e) = e {
        collect_closures_postorder(hir, *e, result, visited);
      }
    },

    HIRKind::ExpressionStatement(e) | HIRKind::Dereference(e) | HIRKind::TypeOf(e) | HIRKind::Panic(e) => {
      collect_closures_postorder(hir, *e, result, visited);
    },

    HIRKind::Cast { expression, .. } | HIRKind::BitCast { expression, .. } | HIRKind::Reference { expression, .. } => {
      collect_closures_postorder(hir, *expression, result, visited);
    },

    HIRKind::Index { base, index } => {
      collect_closures_postorder(hir, *base, result, visited);
      collect_closures_postorder(hir, *index, result, visited);
    },

    HIRKind::FieldAccess { base, .. } => {
      collect_closures_postorder(hir, *base, result, visited);
    },

    HIRKind::Assign { target, value, .. } => {
      collect_closures_postorder(hir, *target, result, visited);
      collect_closures_postorder(hir, *value, result, visited);
    },

    HIRKind::Match { scrutinee, arms } => {
      collect_closures_postorder(hir, *scrutinee, result, visited);
      for arm in arms {
        if let Some(g) = arm.guard {
          collect_closures_postorder(hir, g, result, visited);
        }
        collect_closures_postorder(hir, arm.body, result, visited);
      }
    },

    HIRKind::VectorLiteral { elements } => {
      for &e in elements {
        collect_closures_postorder(hir, e, result, visited);
      }
    },

    HIRKind::RecordInit { fields, .. } => {
      for (_, v) in fields {
        collect_closures_postorder(hir, *v, result, visited);
      }
    },

    HIRKind::EnumVariant { payload, .. } => {
      for &p in payload {
        collect_closures_postorder(hir, p, result, visited);
      }
    },

    HIRKind::BuiltinLoad { ptr, .. } => {
      collect_closures_postorder(hir, *ptr, result, visited);
    },

    HIRKind::BuiltinStore { ptr, value, .. } => {
      collect_closures_postorder(hir, *ptr, result, visited);
      collect_closures_postorder(hir, *value, result, visited);
    },

    HIRKind::BuiltinDropInPlace { ptr, .. } => {
      collect_closures_postorder(hir, *ptr, result, visited);
    },

    HIRKind::Defer { body } => {
      collect_closures_postorder(hir, *body, result, visited);
    },

    // Leaf nodes
    HIRKind::Literal(_)
    | HIRKind::Variable(_)
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

/// After HIR lowering, walk every `HIRKind::Closure` and populate its
/// `captures` field by detecting free variables in the body.
/// Also creates the thunk function definition for each closure.
///
/// A free variable is a `Variable(def_id)` reference inside the closure body
/// where `def_id` is not:
///   - a parameter of the closure itself,
///   - a local (`Let` binding) introduced inside the closure body,
///   - a definition that is not capturable (functions, types, constants, etc.).
pub fn populate_closure_captures(
  hir: &mut HIR,
  defs: &mut DefinitionStore,
  types: &mut TypeStore,
  symbols: &mut SymbolTable,
) -> Vec<Diagnostic> {
  let mut diagnostics: Vec<Diagnostic> = Vec::new();

  let closure_ids = collect_closures_bottom_up(hir);

  for (closure_index, closure_id) in closure_ids.into_iter().enumerate() {
    let (params, body, return_type, closure_span, overrides) = {
      let node = hir.get(closure_id);
      match &node.kind {
        HIRKind::Closure {
          params,
          body,
          return_type,
          capture_overrides,
          ..
        } => (
          params.clone(),
          *body,
          *return_type,
          node.span.clone(),
          capture_overrides.clone(),
        ),
        _ => unreachable!(),
      }
    };

    let mut locals = HashSet::new();
    for &p in &params {
      locals.insert(p);
    }

    collect_locals(hir, body, &mut locals);

    let mut free_vars: Vec<DefinitionId> = Vec::new();
    let mut seen = HashSet::new();
    collect_free_vars(hir, body, &locals, defs, &mut free_vars, &mut seen);

    let captures: Vec<HIRCapture> = free_vars
      .into_iter()
      .enumerate()
      .map(|(index, def_id)| {
        let def = defs.get(&def_id);
        let var_type = match &def.kind {
          DefinitionKind::Variable(vd) => vd.type_id,
          DefinitionKind::Parameter(pd) => pd.type_id,
          _ => unreachable!("non-capturable definition should have been filtered out"),
        };

        let mut usage = VarUsage::default();
        analyze_var_usage(hir, body, def_id, &mut usage, defs, types);

        let mode = if let Some(&override_mode) = overrides.get(&def_id) {
          override_mode
        } else {
          infer_capture_mode(&usage, var_type, types, defs)
        };

        let type_in_env = match mode {
          CaptureMode::ByValue => var_type,
          CaptureMode::ByRef => types.pointer(var_type, false),
          CaptureMode::ByMutRef => types.pointer(var_type, true),
        };

        HIRCapture {
          source_def: def_id,
          field_index: index as u32,
          mode,
          type_in_env,
        }
      })
      .collect();

    let thunk_def_id = create_thunk_definition(
      closure_index,
      &params,
      return_type,
      &closure_span,
      hir,
      body,
      defs,
      types,
      symbols,
    );

    let drop_def_id = if needs_drop_fn(&captures, types, defs) {
      Some(create_drop_function(closure_index, &closure_span, hir, defs, types, symbols))
    } else {
      None
    };

    let node = hir.get_mut(closure_id);
    if let HIRKind::Closure {
      captures: cap,
      thunk_def,
      drop_def,
      ..
    } = &mut node.kind
    {
      *cap = captures;
      *thunk_def = Some(thunk_def_id);
      *drop_def = drop_def_id;
    }
  }

  let escaping_closures = crate::escape::analyze_escapes(hir, defs);

  for closure_id in &escaping_closures {
    let node = hir.get_mut(*closure_id);

    if let HIRKind::Closure { escapes, captures, .. } = &mut node.kind {
      *escapes = true;

      // Escaping + ref capture = dangling reference after scope exits.
      for cap in captures.iter() {
        if matches!(cap.mode, CaptureMode::ByRef | CaptureMode::ByMutRef) {
          let var_name = symbols.get(&defs.get(&cap.source_def).name);
          let var_span = defs.get(&cap.source_def).name_span.clone();
          let closure_span = node.span.clone();

          diagnostics.push(
            DiagnosticMessage::ClosureEscapesWithRefCapture {
              closure_span,
              capture_name: var_name.to_string(),
              capture_span: var_span,
            }
            .report(),
          );
        }
      }
    }
  }

  diagnostics
}

/// Check whether a closure needs a drop function.
///
/// A drop function is needed when at least one ByValue capture has a type
/// that requires dropping (e.g. a record with `@implements(Drop)`, or a
/// type that transitively contains such a record).
fn needs_drop_fn(
  captures: &[HIRCapture],
  types: &TypeStore,
  defs: &DefinitionStore,
) -> bool {
  captures
    .iter()
    .any(|cap| cap.mode == CaptureMode::ByValue && types.needs_drop_with_defs(&cap.type_in_env, defs))
}

/// Create a synthetic drop function definition for a closure env.
///
/// The drop function takes `(env_ptr: *mut u8)` and returns void.
/// LIR lowering emits the actual field-drop logic (GetFieldPtr + DropInPlace
/// for each ByValue capture that needs dropping).
#[allow(clippy::too_many_arguments)]
fn create_drop_function(
  closure_index: usize,
  span: &Span,
  hir: &mut HIR,
  defs: &mut DefinitionStore,
  types: &mut TypeStore,
  symbols: &mut SymbolTable,
) -> DefinitionId {
  let u8_type = types.u8();
  let void_ptr_type = types.pointer(u8_type, true);
  let void_type = types.void();

  let env_param_name = symbols.intern(&format!("__closure_drop_env_{}", closure_index));
  let env_param_def_id = defs.alloc(Definition {
    kind: DefinitionKind::Parameter(ParameterDefinition {
      type_id: void_ptr_type,
      mutable: false,
      attrs: Vec::new(),
    }),
    name: env_param_name,
    span: span.clone(),
    name_span: span.clone(),
    visibility: Visibility::Private,
    owner_module: Default::default(),
    owner_namespace: None,
    doc: None,
  });

  let drop_name = symbols.intern(&format!("__closure_drop_{}", closure_index));
  let drop_def_id = defs.alloc(Definition {
    kind: DefinitionKind::Function(FunctionDefinition {
      type_params: Vec::new(),
      params: vec![env_param_def_id],
      return_type: void_type,
      is_extern: false,
      is_variadic: false,
      inline_mode: InlineMode::None,
      attrs: Vec::new(),
    }),
    name: drop_name,
    span: span.clone(),
    name_span: span.clone(),
    visibility: Visibility::Private,
    owner_module: Default::default(),
    owner_namespace: None,
    doc: None,
  });

  // Placeholder body — real drop logic is emitted by LIR lowering.
  let body_id = hir.alloc(ignis_hir::HIRNode {
    kind: HIRKind::Return(None),
    type_id: void_type,
    span: span.clone(),
  });

  hir.function_bodies.insert(drop_def_id, body_id);
  hir.items.push(drop_def_id);

  drop_def_id
}

/// Create a thunk function definition for a closure.
///
/// The thunk takes `(env_ptr: *mut u8, params...)` and wraps the closure body.
/// LIR lowering generates the env-extraction preamble.
#[allow(clippy::too_many_arguments)]
fn create_thunk_definition(
  closure_index: usize,
  original_params: &[DefinitionId],
  return_type: ignis_type::types::TypeId,
  span: &Span,
  hir: &mut HIR,
  body_hir_id: HIRId,
  defs: &mut DefinitionStore,
  types: &mut TypeStore,
  symbols: &mut SymbolTable,
) -> DefinitionId {
  let u8_type = types.u8();
  let void_ptr_type = types.pointer(u8_type, true);

  let env_param_name = symbols.intern(&format!("__closure_env_{}", closure_index));
  let env_param_def_id = defs.alloc(Definition {
    kind: DefinitionKind::Parameter(ParameterDefinition {
      type_id: void_ptr_type,
      mutable: false,
      attrs: Vec::new(),
    }),
    name: env_param_name,
    span: span.clone(),
    name_span: span.clone(),
    visibility: Visibility::Private,
    owner_module: Default::default(),
    owner_namespace: None,
    doc: None,
  });

  let mut thunk_params = vec![env_param_def_id];
  thunk_params.extend_from_slice(original_params);

  let thunk_name = symbols.intern(&format!("__closure_thunk_{}", closure_index));
  let thunk_def_id = defs.alloc(Definition {
    kind: DefinitionKind::Function(FunctionDefinition {
      type_params: Vec::new(),
      params: thunk_params,
      return_type,
      is_extern: false,
      is_variadic: false,
      inline_mode: InlineMode::None,
      attrs: Vec::new(),
    }),
    name: thunk_name,
    span: span.clone(),
    name_span: span.clone(),
    visibility: Visibility::Private,
    owner_module: Default::default(),
    owner_namespace: None,
    doc: None,
  });

  hir.function_bodies.insert(thunk_def_id, body_hir_id);
  hir.items.push(thunk_def_id);

  thunk_def_id
}

/// Recursively collect all `DefinitionId`s introduced by `Let` bindings,
/// `LetElse` patterns, `Loop` patterns, and `Match` arm patterns
/// inside the given HIR subtree.
fn collect_locals(
  hir: &HIR,
  hir_id: HIRId,
  locals: &mut HashSet<DefinitionId>,
) {
  let node = hir.get(hir_id);

  match &node.kind {
    HIRKind::Let { name, value } => {
      locals.insert(*name);
      if let Some(v) = value {
        collect_locals(hir, *v, locals);
      }
    },

    HIRKind::LetElse {
      pattern,
      value,
      else_block,
    } => {
      collect_pattern_bindings(pattern, locals);
      collect_locals(hir, *value, locals);
      collect_locals(hir, *else_block, locals);
    },

    HIRKind::Block { statements, expression } => {
      for &stmt in statements {
        collect_locals(hir, stmt, locals);
      }
      if let Some(expr) = expression {
        collect_locals(hir, *expr, locals);
      }
    },

    HIRKind::If {
      condition,
      then_branch,
      else_branch,
    } => {
      collect_locals(hir, *condition, locals);
      collect_locals(hir, *then_branch, locals);
      if let Some(e) = else_branch {
        collect_locals(hir, *e, locals);
      }
    },

    HIRKind::Loop { condition, body } => {
      match condition {
        ignis_hir::statement::LoopKind::While { condition: cond } => {
          collect_locals(hir, *cond, locals);
        },
        ignis_hir::statement::LoopKind::For {
          init,
          condition: cond,
          update,
        } => {
          if let Some(i) = init {
            collect_locals(hir, *i, locals);
          }
          if let Some(c) = cond {
            collect_locals(hir, *c, locals);
          }
          if let Some(u) = update {
            collect_locals(hir, *u, locals);
          }
        },
        ignis_hir::statement::LoopKind::Infinite => {},
      }
      collect_locals(hir, *body, locals);
    },

    HIRKind::Match { scrutinee, arms } => {
      collect_locals(hir, *scrutinee, locals);
      for arm in arms {
        collect_pattern_bindings(&arm.pattern, locals);
        if let Some(g) = arm.guard {
          collect_locals(hir, g, locals);
        }
        collect_locals(hir, arm.body, locals);
      }
    },

    HIRKind::ExpressionStatement(expr) => {
      collect_locals(hir, *expr, locals);
    },

    HIRKind::Return(Some(e)) => {
      collect_locals(hir, *e, locals);
    },

    HIRKind::Return(None) => {},

    HIRKind::Closure { .. } => {},

    _ => {},
  }
}

/// Extract all `DefinitionId` bindings from a pattern.
fn collect_pattern_bindings(
  pattern: &ignis_hir::HIRPattern,
  locals: &mut HashSet<DefinitionId>,
) {
  match pattern {
    ignis_hir::HIRPattern::Binding { def_id } => {
      locals.insert(*def_id);
    },
    ignis_hir::HIRPattern::Variant { args, .. } => {
      for arg in args {
        collect_pattern_bindings(arg, locals);
      }
    },
    ignis_hir::HIRPattern::Tuple { elements } => {
      for elem in elements {
        collect_pattern_bindings(elem, locals);
      }
    },
    ignis_hir::HIRPattern::Or { patterns } => {
      for pat in patterns {
        collect_pattern_bindings(pat, locals);
      }
    },
    ignis_hir::HIRPattern::Wildcard
    | ignis_hir::HIRPattern::Literal { .. }
    | ignis_hir::HIRPattern::Constant { .. } => {},
  }
}

/// Recursively collect all variable references that are not in `locals`
/// and refer to capturable definitions (Variable or Parameter).
fn collect_free_vars(
  hir: &HIR,
  hir_id: HIRId,
  locals: &HashSet<DefinitionId>,
  defs: &DefinitionStore,
  free_vars: &mut Vec<DefinitionId>,
  seen: &mut HashSet<DefinitionId>,
) {
  let node = hir.get(hir_id);

  match &node.kind {
    HIRKind::Variable(def_id) => {
      if !locals.contains(def_id) && !seen.contains(def_id) && is_capturable(defs, *def_id) {
        seen.insert(*def_id);
        free_vars.push(*def_id);
      }
    },

    // Propagate transitive captures from already-processed inner closures.
    HIRKind::Closure { captures, .. } => {
      for cap in captures {
        let def_id = cap.source_def;
        if !locals.contains(&def_id) && !seen.contains(&def_id) && is_capturable(defs, def_id) {
          seen.insert(def_id);
          free_vars.push(def_id);
        }
      }
    },

    HIRKind::Let { name: _, value } => {
      if let Some(v) = value {
        collect_free_vars(hir, *v, locals, defs, free_vars, seen);
      }
    },

    HIRKind::LetElse { value, else_block, .. } => {
      collect_free_vars(hir, *value, locals, defs, free_vars, seen);
      collect_free_vars(hir, *else_block, locals, defs, free_vars, seen);
    },

    HIRKind::Block { statements, expression } => {
      for &stmt in statements {
        collect_free_vars(hir, stmt, locals, defs, free_vars, seen);
      }
      if let Some(expr) = expression {
        collect_free_vars(hir, *expr, locals, defs, free_vars, seen);
      }
    },

    HIRKind::Binary { left, right, .. } => {
      collect_free_vars(hir, *left, locals, defs, free_vars, seen);
      collect_free_vars(hir, *right, locals, defs, free_vars, seen);
    },

    HIRKind::Unary { operand, .. } => {
      collect_free_vars(hir, *operand, locals, defs, free_vars, seen);
    },

    HIRKind::Call { args, .. } => {
      for &arg in args {
        collect_free_vars(hir, arg, locals, defs, free_vars, seen);
      }
    },

    HIRKind::CallClosure { callee, args } => {
      collect_free_vars(hir, *callee, locals, defs, free_vars, seen);
      for &arg in args {
        collect_free_vars(hir, arg, locals, defs, free_vars, seen);
      }
    },

    HIRKind::MethodCall { receiver, args, .. } => {
      if let Some(recv) = receiver {
        collect_free_vars(hir, *recv, locals, defs, free_vars, seen);
      }
      for &arg in args {
        collect_free_vars(hir, arg, locals, defs, free_vars, seen);
      }
    },

    HIRKind::Cast { expression, .. } | HIRKind::BitCast { expression, .. } | HIRKind::Reference { expression, .. } => {
      collect_free_vars(hir, *expression, locals, defs, free_vars, seen);
    },

    HIRKind::Dereference(expr) | HIRKind::TypeOf(expr) | HIRKind::ExpressionStatement(expr) | HIRKind::Panic(expr) => {
      collect_free_vars(hir, *expr, locals, defs, free_vars, seen);
    },

    HIRKind::Index { base, index } => {
      collect_free_vars(hir, *base, locals, defs, free_vars, seen);
      collect_free_vars(hir, *index, locals, defs, free_vars, seen);
    },

    HIRKind::FieldAccess { base, .. } => {
      collect_free_vars(hir, *base, locals, defs, free_vars, seen);
    },

    HIRKind::VectorLiteral { elements } => {
      for &elem in elements {
        collect_free_vars(hir, elem, locals, defs, free_vars, seen);
      }
    },

    HIRKind::RecordInit { fields, .. } => {
      for (_, value) in fields {
        collect_free_vars(hir, *value, locals, defs, free_vars, seen);
      }
    },

    HIRKind::EnumVariant { payload, .. } => {
      for &p in payload {
        collect_free_vars(hir, p, locals, defs, free_vars, seen);
      }
    },

    HIRKind::If {
      condition,
      then_branch,
      else_branch,
    } => {
      collect_free_vars(hir, *condition, locals, defs, free_vars, seen);
      collect_free_vars(hir, *then_branch, locals, defs, free_vars, seen);
      if let Some(e) = else_branch {
        collect_free_vars(hir, *e, locals, defs, free_vars, seen);
      }
    },

    HIRKind::Loop { condition, body } => {
      match condition {
        ignis_hir::statement::LoopKind::While { condition: cond } => {
          collect_free_vars(hir, *cond, locals, defs, free_vars, seen);
        },
        ignis_hir::statement::LoopKind::For {
          init,
          condition: cond,
          update,
        } => {
          if let Some(i) = init {
            collect_free_vars(hir, *i, locals, defs, free_vars, seen);
          }
          if let Some(c) = cond {
            collect_free_vars(hir, *c, locals, defs, free_vars, seen);
          }
          if let Some(u) = update {
            collect_free_vars(hir, *u, locals, defs, free_vars, seen);
          }
        },
        ignis_hir::statement::LoopKind::Infinite => {},
      }
      collect_free_vars(hir, *body, locals, defs, free_vars, seen);
    },

    HIRKind::Assign { target, value, .. } => {
      collect_free_vars(hir, *target, locals, defs, free_vars, seen);
      collect_free_vars(hir, *value, locals, defs, free_vars, seen);
    },

    HIRKind::Return(expr) => {
      if let Some(e) = expr {
        collect_free_vars(hir, *e, locals, defs, free_vars, seen);
      }
    },

    HIRKind::Match { scrutinee, arms } => {
      collect_free_vars(hir, *scrutinee, locals, defs, free_vars, seen);
      for arm in arms {
        if let Some(g) = arm.guard {
          collect_free_vars(hir, g, locals, defs, free_vars, seen);
        }
        collect_free_vars(hir, arm.body, locals, defs, free_vars, seen);
      }
    },

    HIRKind::BuiltinLoad { ptr, .. } => {
      collect_free_vars(hir, *ptr, locals, defs, free_vars, seen);
    },

    HIRKind::BuiltinStore { ptr, value, .. } => {
      collect_free_vars(hir, *ptr, locals, defs, free_vars, seen);
      collect_free_vars(hir, *value, locals, defs, free_vars, seen);
    },

    HIRKind::BuiltinDropInPlace { ptr, .. } => {
      collect_free_vars(hir, *ptr, locals, defs, free_vars, seen);
    },

    HIRKind::Defer { body } => {
      collect_free_vars(hir, *body, locals, defs, free_vars, seen);
    },

    // Leaf nodes: no children to recurse into
    HIRKind::Literal(_)
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

/// A definition is capturable if it refers to a runtime value (variable or parameter),
/// not a static entity (function, type, constant, etc.).
fn is_capturable(
  defs: &DefinitionStore,
  def_id: DefinitionId,
) -> bool {
  matches!(
    defs.get(&def_id).kind,
    DefinitionKind::Variable(_) | DefinitionKind::Parameter(_)
  )
}
