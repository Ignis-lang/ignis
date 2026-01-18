//! Monomorphization pass for generic instantiation.
//!
//! This module transforms HIR with generic types (`Type::Param`, `Type::Instance`)
//! into concrete HIR where all types are fully resolved. It uses a worklist algorithm
//! that discovers generic instantiations and creates concrete definitions for each.
//!
//! ## Algorithm Overview
//!
//! 1. **Discovery Phase**: Starting from root definitions (main function, public exports),
//!    scan HIR nodes for generic instantiations (calls with type_args, record inits, etc.)
//!
//! 2. **Shell Creation Phase**: For each discovered instantiation, create a concrete
//!    definition with mangled name and substituted types (but no body yet).
//!
//! 3. **Body Substitution Phase**: Clone and transform the generic definition's HIR body,
//!    replacing type parameters with concrete types and generic calls with concrete calls.
//!    This may discover new instantiations, returning to step 2 (fixpoint loop).
//!
//! ## Invariants
//!
//! - **Invariant A**: `Type::Param` never reaches LIR
//! - **Invariant D**: Post-mono, no `Type::Instance` or `Type::Param` exists
//! - **Invariant E**: `TypeStore::record()`/`enum_type()` are idempotent

use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;
use std::sync::OnceLock;

use ignis_hir::{HIR, HIRId, HIRKind, HIRNode, statement::LoopKind};
use ignis_type::definition::{
  Definition, DefinitionId, DefinitionKind, DefinitionStore, EnumDefinition, EnumVariantDef, FieldDefinition,
  FunctionDefinition, MethodDefinition, ParameterDefinition, RecordDefinition, RecordFieldDef, VariantDefinition,
  Visibility,
};
use ignis_type::symbol::SymbolTable;
use ignis_type::types::{Substitution, Type, TypeId, TypeStore};

/// Cached verbose mode flag from IGNIS_VERBOSE environment variable.
static VERBOSE_MODE: OnceLock<bool> = OnceLock::new();

/// Returns true if verbose logging is enabled via IGNIS_VERBOSE env var.
/// Result is cached to avoid repeated syscalls.
fn is_verbose() -> bool {
  *VERBOSE_MODE.get_or_init(|| std::env::var("IGNIS_VERBOSE").is_ok())
}

/// Key for identifying a generic instantiation.
///
/// Generic functions, records, and enums use `Generic` variant.
/// Methods on generic types use `Method` variant to track both
/// the owner's type arguments and the method's own type arguments.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum InstanceKey {
  /// Function or type (record/enum) instantiation
  Generic { def: DefinitionId, args: Vec<TypeId> },
  /// Method instantiation (needs owner + method args for substitution)
  Method {
    owner_def: DefinitionId,
    owner_args: Vec<TypeId>,
    method_def: DefinitionId,
    method_args: Vec<TypeId>,
  },
}

impl InstanceKey {
  pub fn generic(
    def: DefinitionId,
    args: Vec<TypeId>,
  ) -> Self {
    Self::Generic { def, args }
  }

  pub fn method(
    owner_def: DefinitionId,
    owner_args: Vec<TypeId>,
    method_def: DefinitionId,
    method_args: Vec<TypeId>,
  ) -> Self {
    Self::Method {
      owner_def,
      owner_args,
      method_def,
      method_args,
    }
  }

  /// The primary definition (for looking up the body to clone).
  pub fn primary_def(&self) -> DefinitionId {
    match self {
      Self::Generic { def, .. } => *def,
      Self::Method { method_def, .. } => *method_def,
    }
  }
}

/// Output from monomorphization.
pub struct MonoOutput {
  pub hir: HIR,
  pub defs: DefinitionStore,
  pub instance_map: HashMap<InstanceKey, DefinitionId>,
}

/// Monomorphizer transforms generic HIR into concrete HIR.
pub struct Monomorphizer<'a> {
  input_hir: &'a HIR,
  input_defs: &'a DefinitionStore,
  types: &'a mut TypeStore,
  symbols: Rc<RefCell<SymbolTable>>,

  /// Maps instantiation keys to their concrete definition IDs
  cache: HashMap<InstanceKey, DefinitionId>,
  /// Reverse cache: maps concrete definition IDs back to their InstanceKey
  /// Used to recover type arguments when receiver type is already concretized
  reverse_cache: HashMap<DefinitionId, InstanceKey>,
  /// Instantiations discovered but not yet processed
  worklist: VecDeque<InstanceKey>,
  /// Instantiations whose bodies have been substituted
  processed: HashSet<InstanceKey>,

  /// Current def remapping during body substitution (old param -> new param)
  current_def_remap: HashMap<DefinitionId, DefinitionId>,

  output_hir: HIR,
  output_defs: DefinitionStore,
}

impl<'a> Monomorphizer<'a> {
  pub fn new(
    input_hir: &'a HIR,
    input_defs: &'a DefinitionStore,
    types: &'a mut TypeStore,
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> Self {
    Self {
      input_hir,
      input_defs,
      types,
      symbols,
      cache: HashMap::new(),
      reverse_cache: HashMap::new(),
      worklist: VecDeque::new(),
      processed: HashSet::new(),
      current_def_remap: HashMap::new(),
      output_hir: HIR::new(),
      output_defs: input_defs.clone(),
    }
  }

  /// Run the monomorphization pass.
  ///
  /// `roots` should contain the entry points (main function for executables,
  /// public exports for libraries).
  pub fn run(
    mut self,
    roots: &[DefinitionId],
  ) -> MonoOutput {
    // === PASS 1: Discover instantiations from root definitions ===
    for root in roots {
      self.discover_from_root(*root);
    }

    // Process initial worklist: create concrete definition shells
    while let Some(key) = self.worklist.pop_front() {
      if self.cache.contains_key(&key) {
        continue;
      }
      let concrete_id = self.create_concrete_def_shell(&key);
      self.cache.insert(key.clone(), concrete_id);
      self.reverse_cache.insert(concrete_id, key.clone());
    }

    // === PASS 2: Substitute bodies + fixpoint for nested instantiations ===
    loop {
      // Collect keys pending body substitution
      let pending: Vec<_> = self
        .cache
        .keys()
        .filter(|k| !self.processed.contains(*k))
        .cloned()
        .collect();

      if pending.is_empty() && self.worklist.is_empty() {
        break;
      }

      // Substitute bodies for pending instantiations
      for key in pending {
        let concrete_id = *self.cache.get(&key).unwrap();
        self.substitute_body(&key, concrete_id);
        self.processed.insert(key);
      }

      // Process newly discovered instantiations
      while let Some(key) = self.worklist.pop_front() {
        if self.cache.contains_key(&key) {
          continue;
        }
        let concrete_id = self.create_concrete_def_shell(&key);
        self.cache.insert(key.clone(), concrete_id);
        self.reverse_cache.insert(concrete_id, key.clone());
      }
    }

    // Copy non-generic function bodies and items to output
    self.copy_nongeneric_items(roots);

    // Process any new instantiations discovered during copying
    // This handles generic calls found in non-generic function bodies
    loop {
      let pending: Vec<_> = self
        .cache
        .keys()
        .filter(|k| !self.processed.contains(*k))
        .cloned()
        .collect();

      if pending.is_empty() && self.worklist.is_empty() {
        break;
      }

      for key in pending {
        let concrete_id = *self.cache.get(&key).unwrap();
        self.substitute_body(&key, concrete_id);
        self.processed.insert(key);
      }

      while let Some(key) = self.worklist.pop_front() {
        if self.cache.contains_key(&key) {
          continue;
        }
        let concrete_id = self.create_concrete_def_shell(&key);
        self.cache.insert(key.clone(), concrete_id);
        self.reverse_cache.insert(concrete_id, key.clone());
      }
    }

    // Final pass: concretize all Type::Instance in definition types
    self.concretize_all_definition_types();

    if is_verbose() {
      eprintln!("[MONO] Monomorphization complete:");
      eprintln!("[MONO]   output_defs count: {}", self.output_defs.iter().count());
      eprintln!(
        "[MONO]   output_hir function_bodies count: {}",
        self.output_hir.function_bodies.len()
      );
      eprintln!("[MONO]   cache (instantiations) count: {}", self.cache.len());
    }

    MonoOutput {
      hir: self.output_hir,
      defs: self.output_defs,
      instance_map: self.cache,
    }
  }

  /// Concretize all Type::Instance in definition types to concrete Type::Record/Enum.
  /// This ensures that variable/parameter/return types are all concrete post-mono.
  fn concretize_all_definition_types(&mut self) {
    // Collect all definition IDs to process (can't iterate and mutate simultaneously)
    let def_ids: Vec<_> = self.output_defs.iter().map(|(id, _)| id).collect();

    for def_id in def_ids {
      let def = self.output_defs.get(&def_id);
      match &def.kind.clone() {
        DefinitionKind::Variable(vd) => {
          let new_ty = self.concretize_type(vd.type_id);
          if new_ty != vd.type_id {
            if let DefinitionKind::Variable(vd) = &mut self.output_defs.get_mut(&def_id).kind {
              vd.type_id = new_ty;
            }
          }
        },
        DefinitionKind::Parameter(pd) => {
          let new_ty = self.concretize_type(pd.type_id);
          if new_ty != pd.type_id {
            if let DefinitionKind::Parameter(pd) = &mut self.output_defs.get_mut(&def_id).kind {
              pd.type_id = new_ty;
            }
          }
        },
        DefinitionKind::Constant(cd) => {
          let new_ty = self.concretize_type(cd.type_id);
          if new_ty != cd.type_id {
            if let DefinitionKind::Constant(cd) = &mut self.output_defs.get_mut(&def_id).kind {
              cd.type_id = new_ty;
            }
          }
        },
        DefinitionKind::Function(fd) if fd.type_params.is_empty() => {
          // Non-generic function: concretize return type and parameters
          let new_ret = self.concretize_type(fd.return_type);
          if new_ret != fd.return_type {
            if let DefinitionKind::Function(fd) = &mut self.output_defs.get_mut(&def_id).kind {
              fd.return_type = new_ret;
            }
          }
          // Concretize parameter types
          for param_id in &fd.params.clone() {
            let (old_ty, is_param) = {
              let param_def = self.output_defs.get(param_id);
              if let DefinitionKind::Parameter(pd) = &param_def.kind {
                (pd.type_id, true)
              } else {
                (self.types.error(), false)
              }
            };
            if is_param {
              let new_ty = self.concretize_type(old_ty);
              if new_ty != old_ty {
                if let DefinitionKind::Parameter(pd) = &mut self.output_defs.get_mut(param_id).kind {
                  pd.type_id = new_ty;
                }
              }
            }
          }
        },
        DefinitionKind::Method(md) if md.type_params.is_empty() && !self.is_owner_generic(md.owner_type) => {
          // Non-generic method on non-generic owner: concretize return type and parameters
          let new_ret = self.concretize_type(md.return_type);
          if new_ret != md.return_type {
            if let DefinitionKind::Method(md) = &mut self.output_defs.get_mut(&def_id).kind {
              md.return_type = new_ret;
            }
          }
          // Concretize parameter types
          for param_id in &md.params.clone() {
            let (old_ty, is_param) = {
              let param_def = self.output_defs.get(param_id);
              if let DefinitionKind::Parameter(pd) = &param_def.kind {
                if is_verbose() {
                  eprintln!("[MONO] method param {:?} has type: {:?}", param_id, self.types.get(&pd.type_id));
                }
                (pd.type_id, true)
              } else {
                (self.types.error(), false)
              }
            };
            if is_param {
              let new_ty = self.concretize_type(old_ty);
              if is_verbose() && new_ty != old_ty {
                eprintln!(
                  "[MONO] concretize param: {:?} old_ty={:?} new_ty={:?}",
                  param_id,
                  self.types.get(&old_ty),
                  self.types.get(&new_ty)
                );
              }
              if new_ty != old_ty {
                if let DefinitionKind::Parameter(pd) = &mut self.output_defs.get_mut(param_id).kind {
                  pd.type_id = new_ty;
                }
              }
            }
          }
        },
        _ => {},
      }
    }
  }

  /// Copy non-generic function bodies and items from input to output HIR.
  fn copy_nongeneric_items(
    &mut self,
    roots: &[DefinitionId],
  ) {
    for root in roots {
      self.copy_if_nongeneric(*root);
    }

    // Copy entry point
    self.output_hir.entry_point = self.input_hir.entry_point;
  }

  fn copy_if_nongeneric(
    &mut self,
    def_id: DefinitionId,
  ) {
    // Early exit if already in output (prevents infinite recursion for recursive calls)
    if self.output_hir.function_bodies.contains_key(&def_id) {
      return;
    }

    let def = self.input_defs.get(&def_id);
    if is_verbose() {
      eprintln!(
        "[MONO] copy_if_nongeneric: def_id={:?}, name={}",
        def_id,
        self.symbols.borrow().get(&def.name)
      );
    }
    match &def.kind {
      DefinitionKind::Function(fd) if fd.type_params.is_empty() => {
        // Non-generic function: mark as pending first to prevent re-entry
        // We insert a placeholder first, then replace with actual body
        if let Some(&body_id) = self.input_hir.function_bodies.get(&def_id) {
          // Use the input body_id as placeholder (will be replaced)
          self.output_hir.function_bodies.insert(def_id, body_id);
          let new_body = self.clone_hir_tree(body_id);
          self.output_hir.function_bodies.insert(def_id, new_body);
          if is_verbose() {
            eprintln!(
              "[MONO]   -> copied function body, count now: {}",
              self.output_hir.function_bodies.len()
            );
          }
        }
        if !self.output_hir.items.contains(&def_id) {
          self.output_hir.items.push(def_id);
        }
      },
      DefinitionKind::Record(rd) if rd.type_params.is_empty() => {
        // Non-generic record: copy method bodies
        for method_entry in rd.instance_methods.values() {
          if let Some(method_id) = method_entry.as_single() {
            self.copy_if_nongeneric(*method_id);
          }
        }
        for method_entry in rd.static_methods.values() {
          if let Some(method_id) = method_entry.as_single() {
            self.copy_if_nongeneric(*method_id);
          }
        }
      },
      DefinitionKind::Method(md) if md.type_params.is_empty() => {
        // Check if owner is also non-generic
        let owner_def = self.input_defs.get(&md.owner_type);
        let owner_is_generic = match &owner_def.kind {
          DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
          DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
          _ => false,
        };

        if !owner_is_generic {
          if let Some(&body_id) = self.input_hir.function_bodies.get(&def_id) {
            let new_body = self.clone_hir_tree(body_id);
            self.output_hir.function_bodies.insert(def_id, new_body);
          }
          if !self.output_hir.items.contains(&def_id) {
            self.output_hir.items.push(def_id);
          }
        }
      },
      DefinitionKind::Enum(ed) if ed.type_params.is_empty() => {
        // Non-generic enum: copy static method bodies
        for method_entry in ed.static_methods.values() {
          if let Some(method_id) = method_entry.as_single() {
            self.copy_if_nongeneric(*method_id);
          }
        }
      },
      _ => {},
    }
  }

  /// Unwrap references/pointers to get to the underlying Instance or Record type.
  /// Returns (def_id, type_args) where def_id is the record/enum definition and
  /// type_args are the generic arguments (empty for non-generic types).
  fn unwrap_to_instance_type(
    &self,
    mut ty: TypeId,
  ) -> Option<(DefinitionId, Vec<TypeId>)> {
    loop {
      match self.types.get(&ty).clone() {
        Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
          ty = inner;
        },
        Type::Instance { generic, args } => {
          return Some((generic, args));
        },
        Type::Record(def_id) | Type::Enum(def_id) => {
          return Some((def_id, vec![]));
        },
        _ => return None,
      }
    }
  }

  /// Clone an HIR tree from input to output, returning the new root ID.
  ///
  /// This handles generic instantiations within non-generic functions by:
  /// 1. Resolving generic calls to their concrete instantiations
  /// 2. Substituting type parameters in the node's type_id
  /// 3. Concretizing any Type::Instance to Type::Record/Enum
  fn clone_hir_tree(
    &mut self,
    hir_id: HIRId,
  ) -> HIRId {
    let node = self.input_hir.get(hir_id).clone();
    let (new_kind, subst) = self.clone_hir_kind_with_subst(&node.kind);

    // Substitute type_id if we have a substitution (from generic call)
    let new_type_id = if let Some(ref s) = subst {
      self.types.substitute(node.type_id, s)
    } else {
      node.type_id
    };

    // Concretize any remaining Type::Instance to concrete Type::Record/Enum
    let concrete_type_id = self.concretize_type(new_type_id);

    self.output_hir.alloc(HIRNode {
      kind: new_kind,
      span: node.span,
      type_id: concrete_type_id,
    })
  }

  /// Build a substitution for a generic call based on callee's type params and the type args.
  fn build_call_subst(
    &self,
    callee: DefinitionId,
    type_args: &[TypeId],
  ) -> Option<Substitution> {
    if type_args.is_empty() {
      return None;
    }
    Some(Substitution::for_generic(callee, type_args))
  }

  /// Clone an HIR kind, returning the new kind and an optional substitution
  /// for nodes that involve generic instantiations (so the caller can fix type_id).
  fn clone_hir_kind_with_subst(
    &mut self,
    kind: &HIRKind,
  ) -> (HIRKind, Option<Substitution>) {
    match kind {
      HIRKind::Literal(v) => (HIRKind::Literal(v.clone()), None),
      HIRKind::Variable(def) => (HIRKind::Variable(*def), None),
      HIRKind::Binary { operation, left, right } => {
        let new_left = self.clone_hir_tree(*left);
        let new_right = self.clone_hir_tree(*right);
        (
          HIRKind::Binary {
            operation: operation.clone(),
            left: new_left,
            right: new_right,
          },
          None,
        )
      },
      HIRKind::Unary { operation, operand } => {
        let new_operand = self.clone_hir_tree(*operand);
        (
          HIRKind::Unary {
            operation: operation.clone(),
            operand: new_operand,
          },
          None,
        )
      },
      HIRKind::Call {
        callee,
        type_args,
        args,
      } => {
        // Build substitution for the call's type parameters
        let subst = self.build_call_subst(*callee, type_args);

        // Resolve generic calls to their concrete instantiation
        let concrete_callee = if !type_args.is_empty() {
          let key = InstanceKey::generic(*callee, type_args.clone());
          self.ensure_instantiated(&key)
        } else {
          // Non-generic call: ensure the callee is also copied to output
          self.copy_if_nongeneric(*callee);
          *callee
        };
        let new_args: Vec<_> = args.iter().map(|a| self.clone_hir_tree(*a)).collect();
        (
          HIRKind::Call {
            callee: concrete_callee,
            type_args: vec![], // Clear type args for concrete instantiation
            args: new_args,
          },
          subst,
        )
      },
      HIRKind::Cast { expression, target } => {
        let new_expr = self.clone_hir_tree(*expression);
        (
          HIRKind::Cast {
            expression: new_expr,
            target: *target,
          },
          None,
        )
      },
      HIRKind::BuiltinLoad { ty, ptr } => {
        let new_ptr = self.clone_hir_tree(*ptr);
        (HIRKind::BuiltinLoad { ty: *ty, ptr: new_ptr }, None)
      },
      HIRKind::BuiltinStore { ty, ptr, value } => {
        let new_ptr = self.clone_hir_tree(*ptr);
        let new_value = self.clone_hir_tree(*value);
        (
          HIRKind::BuiltinStore {
            ty: *ty,
            ptr: new_ptr,
            value: new_value,
          },
          None,
        )
      },
      HIRKind::Reference { expression, mutable } => {
        let new_expr = self.clone_hir_tree(*expression);
        (
          HIRKind::Reference {
            expression: new_expr,
            mutable: *mutable,
          },
          None,
        )
      },
      HIRKind::Dereference(expr) => {
        let new_expr = self.clone_hir_tree(*expr);
        (HIRKind::Dereference(new_expr), None)
      },
      HIRKind::Index { base, index } => {
        let new_base = self.clone_hir_tree(*base);
        let new_index = self.clone_hir_tree(*index);
        (
          HIRKind::Index {
            base: new_base,
            index: new_index,
          },
          None,
        )
      },
      HIRKind::VectorLiteral { elements } => {
        let new_elems: Vec<_> = elements.iter().map(|e| self.clone_hir_tree(*e)).collect();
        (HIRKind::VectorLiteral { elements: new_elems }, None)
      },
      HIRKind::TypeOf(expr) => {
        let new_expr = self.clone_hir_tree(*expr);
        (HIRKind::TypeOf(new_expr), None)
      },
      HIRKind::SizeOf(ty) => (HIRKind::SizeOf(*ty), None),
      HIRKind::FieldAccess { base, field_index } => {
        let new_base = self.clone_hir_tree(*base);
        (
          HIRKind::FieldAccess {
            base: new_base,
            field_index: *field_index,
          },
          None,
        )
      },
      HIRKind::RecordInit {
        record_def,
        type_args,
        fields,
      } => {
        // Build substitution for the record's type parameters
        let subst = self.build_call_subst(*record_def, type_args);

        // Resolve generic record init to concrete instantiation
        let concrete_def = if !type_args.is_empty() {
          let key = InstanceKey::generic(*record_def, type_args.clone());
          self.ensure_instantiated(&key)
        } else {
          *record_def
        };
        let new_fields: Vec<_> = fields
          .iter()
          .map(|(idx, val)| (*idx, self.clone_hir_tree(*val)))
          .collect();
        (
          HIRKind::RecordInit {
            record_def: concrete_def,
            type_args: vec![], // Clear type args for concrete instantiation
            fields: new_fields,
          },
          subst,
        )
      },
      HIRKind::MethodCall {
        receiver,
        method,
        type_args,
        args,
      } => {
        // Clone receiver first to get the concretized type
        let new_receiver = receiver.map(|r| self.clone_hir_tree(r));

        // Extract owner type args and method type args
        // For instance methods: owner_args come from receiver, method_type_args from call
        // For static methods: type_args are split between owner and method
        let (owner_args, method_type_args) = if let Some(new_recv_id) = new_receiver {
          // Instance method: extract owner args from receiver type
          let recv_ty = self.output_hir.get(new_recv_id).type_id;

          let (def_id, mut extracted_args) = if let Some((def, args)) = self.unwrap_to_instance_type(recv_ty) {
            if is_verbose() {
              eprintln!(
                "[MONO]   unwrap_to_instance_type returned: def={:?}, args.len()={}",
                def,
                args.len()
              );
            }
            (def, args)
          } else {
            panic!("Cannot extract owner from receiver type {:?}", self.types.get(&recv_ty));
          };

          // If args is empty, search cache for the concrete def's original args
          if extracted_args.is_empty() {
            if is_verbose() {
              eprintln!("[MONO]   Args empty for def={:?}, searching cache", def_id);
            }
            for (key, &cached_def) in &self.cache {
              if cached_def == def_id {
                if let InstanceKey::Generic { args: cached_args, .. } = key {
                  if is_verbose() {
                    eprintln!(
                      "[MONO]     Found in cache: args={:?}",
                      cached_args.iter().map(|t| self.types.get(t)).collect::<Vec<_>>()
                    );
                  }
                  extracted_args = cached_args.clone();
                  break;
                }
              }
            }
          }

          // Check if any args are Type::Param (incomplete resolution)
          let has_param = extracted_args
            .iter()
            .any(|ty| matches!(self.types.get(ty), Type::Param { .. }));

          if has_param {
            if is_verbose() {
              eprintln!("[MONO] Warning: MethodCall receiver has Type::Param in args");
            }
            match self.types.get(&recv_ty).clone() {
              Type::Record(rec_def) | Type::Enum(rec_def) => {
                for (key, &cached_def) in &self.cache {
                  if cached_def == rec_def {
                    if let InstanceKey::Generic { args: cached_args, .. } = key {
                      extracted_args = cached_args.clone();
                      break;
                    }
                  }
                }
              },
              _ => {},
            }
          }

          // For instance methods, type_args are the method's own type args
          (extracted_args, type_args.clone())
        } else {
          // Static method: split type_args between owner and method
          // First N args are for owner (N = owner's type param count), rest for method
          let method_def = self.input_defs.get(method);
          let (owner_param_count, method_param_count) = match &method_def.kind {
            DefinitionKind::Method(md) => {
              let owner_def = self.input_defs.get(&md.owner_type);
              let owner_count = match &owner_def.kind {
                DefinitionKind::Record(rd) => rd.type_params.len(),
                DefinitionKind::Enum(ed) => ed.type_params.len(),
                _ => 0,
              };
              (owner_count, md.type_params.len())
            },
            _ => (0, 0),
          };

          if is_verbose() {
            eprintln!(
              "[MONO] Static MethodCall: owner_param_count={}, method_param_count={}, type_args.len()={}",
              owner_param_count,
              method_param_count,
              type_args.len()
            );
          }

          // Split type_args: first owner_param_count for owner, rest for method
          let owner = type_args.iter().take(owner_param_count).cloned().collect();
          let method = type_args.iter().skip(owner_param_count).cloned().collect();
          (owner, method)
        };

        if is_verbose() {
          eprintln!(
            "[MONO] clone MethodCall: method={:?}, owner_args={:?}, method_type_args={:?}",
            method,
            owner_args.iter().map(|t| self.types.get(t)).collect::<Vec<_>>(),
            method_type_args.iter().map(|t| self.types.get(t)).collect::<Vec<_>>()
          );
        }

        let concrete_method = self.resolve_concrete_method_with_args(*method, &method_type_args, &owner_args);

        if is_verbose() {
          eprintln!("[MONO]   -> concrete_method={:?}", concrete_method);
        }

        let new_args: Vec<_> = args.iter().map(|a| self.clone_hir_tree(*a)).collect();

        // Build substitution from method's owner type and method type args
        let subst = self.build_call_subst(*method, type_args);

        (
          HIRKind::MethodCall {
            receiver: new_receiver,
            method: concrete_method,
            type_args: vec![], // Clear type args for concrete instantiation
            args: new_args,
          },
          subst,
        )
      },
      HIRKind::EnumVariant {
        enum_def,
        type_args,
        variant_tag,
        payload,
      } => {
        // Build substitution for the enum's type parameters
        let subst = self.build_call_subst(*enum_def, type_args);

        // Resolve generic enum to concrete instantiation
        let concrete_def = if !type_args.is_empty() {
          let key = InstanceKey::generic(*enum_def, type_args.clone());
          self.ensure_instantiated(&key)
        } else {
          *enum_def
        };
        let new_payload: Vec<_> = payload.iter().map(|p| self.clone_hir_tree(*p)).collect();
        (
          HIRKind::EnumVariant {
            enum_def: concrete_def,
            type_args: vec![], // Clear type args for concrete instantiation
            variant_tag: *variant_tag,
            payload: new_payload,
          },
          subst,
        )
      },
      HIRKind::StaticAccess { def } => (HIRKind::StaticAccess { def: *def }, None),
      HIRKind::Let { name, value } => {
        let new_value = value.map(|v| self.clone_hir_tree(v));
        (
          HIRKind::Let {
            name: *name,
            value: new_value,
          },
          None,
        )
      },
      HIRKind::Assign {
        target,
        value,
        operation,
      } => {
        let new_target = self.clone_hir_tree(*target);
        let new_value = self.clone_hir_tree(*value);
        (
          HIRKind::Assign {
            target: new_target,
            value: new_value,
            operation: operation.clone(),
          },
          None,
        )
      },
      HIRKind::Block { statements, expression } => {
        let new_stmts: Vec<_> = statements.iter().map(|s| self.clone_hir_tree(*s)).collect();
        let new_expr = expression.map(|e| self.clone_hir_tree(e));
        (
          HIRKind::Block {
            statements: new_stmts,
            expression: new_expr,
          },
          None,
        )
      },
      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => {
        let new_cond = self.clone_hir_tree(*condition);
        let new_then = self.clone_hir_tree(*then_branch);
        let new_else = else_branch.map(|e| self.clone_hir_tree(e));
        (
          HIRKind::If {
            condition: new_cond,
            then_branch: new_then,
            else_branch: new_else,
          },
          None,
        )
      },
      HIRKind::Loop { condition, body } => {
        let new_cond = match condition {
          LoopKind::While { condition: c } => LoopKind::While {
            condition: self.clone_hir_tree(*c),
          },
          LoopKind::For {
            init,
            condition,
            update,
          } => LoopKind::For {
            init: init.map(|i| self.clone_hir_tree(i)),
            condition: condition.map(|c| self.clone_hir_tree(c)),
            update: update.map(|u| self.clone_hir_tree(u)),
          },
          LoopKind::Infinite => LoopKind::Infinite,
        };
        let new_body = self.clone_hir_tree(*body);
        (
          HIRKind::Loop {
            condition: new_cond,
            body: new_body,
          },
          None,
        )
      },
      HIRKind::Break => (HIRKind::Break, None),
      HIRKind::Continue => (HIRKind::Continue, None),
      HIRKind::Return(expr) => {
        let new_expr = expr.map(|e| self.clone_hir_tree(e));
        (HIRKind::Return(new_expr), None)
      },
      HIRKind::ExpressionStatement(expr) => {
        let new_expr = self.clone_hir_tree(*expr);
        (HIRKind::ExpressionStatement(new_expr), None)
      },
      HIRKind::Error => (HIRKind::Error, None),
    }
  }

  // === Discovery Phase ===

  /// Discover generic instantiations starting from a root definition.
  fn discover_from_root(
    &mut self,
    def_id: DefinitionId,
  ) {
    let def = self.input_defs.get(&def_id);
    match &def.kind {
      DefinitionKind::Function(fd) if fd.type_params.is_empty() => {
        // Non-generic function: scan body for generic calls
        if let Some(&body) = self.input_hir.function_bodies.get(&def_id) {
          self.scan_hir(body);
        }
      },
      DefinitionKind::Record(rd) if rd.type_params.is_empty() => {
        // Non-generic record: scan methods
        for method_entry in rd.instance_methods.values() {
          if let Some(method_id) = method_entry.as_single() {
            self.discover_from_root(*method_id);
          }
        }
        for method_entry in rd.static_methods.values() {
          if let Some(method_id) = method_entry.as_single() {
            self.discover_from_root(*method_id);
          }
        }
      },
      DefinitionKind::Method(md) if md.type_params.is_empty() => {
        // Non-generic method on non-generic type: scan body
        let owner_def = self.input_defs.get(&md.owner_type);
        let owner_is_generic = match &owner_def.kind {
          DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
          DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
          _ => false,
        };
        if !owner_is_generic {
          if let Some(&body) = self.input_hir.function_bodies.get(&def_id) {
            self.scan_hir(body);
          }
        }
      },
      DefinitionKind::Enum(ed) if ed.type_params.is_empty() => {
        // Non-generic enum: scan static methods
        for method_entry in ed.static_methods.values() {
          if let Some(method_id) = method_entry.as_single() {
            self.discover_from_root(*method_id);
          }
        }
      },
      _ => {
        // Generic definitions are instantiated on demand
      },
    }
  }

  /// Scan HIR for generic instantiations with concrete type arguments.
  ///
  /// Note: When called from discover_from_root on non-generic functions, the context
  /// guarantees all types are concrete (no Type::Param). A future optimization could
  /// add a context_is_concrete parameter to skip the Type::Param checks in those cases.
  fn scan_hir(
    &mut self,
    hir_id: HIRId,
  ) {
    let node = self.input_hir.get(hir_id);
    match &node.kind {
      HIRKind::Call {
        callee,
        type_args,
        args,
      } => {
        if !type_args.is_empty() {
          // Check if any type arg contains Type::Param - only enqueue if fully concrete
          let has_param = type_args
            .iter()
            .any(|ty| matches!(self.types.get(ty), Type::Param { .. }));
          if !has_param {
            self.enqueue(InstanceKey::generic(*callee, type_args.clone()));
          }
        }
        for arg in args {
          self.scan_hir(*arg);
        }
      },
      HIRKind::RecordInit {
        record_def,
        type_args,
        fields,
      } => {
        if !type_args.is_empty() {
          self.enqueue(InstanceKey::generic(*record_def, type_args.clone()));
        }
        for (_, val) in fields {
          self.scan_hir(*val);
        }
      },
      HIRKind::BuiltinLoad { ptr, .. } => {
        self.scan_hir(*ptr);
      },
      HIRKind::BuiltinStore { ptr, value, .. } => {
        self.scan_hir(*ptr);
        self.scan_hir(*value);
      },
      HIRKind::MethodCall {
        receiver,
        method,
        type_args,
        args,
      } => {
        if let Some(recv) = receiver {
          let recv_ty = self.input_hir.get(*recv).type_id;
          // Unwrap references/pointers to get to the Instance type
          if let Some((generic, owner_args)) = self.unwrap_to_instance_type(recv_ty) {
            // Check if owner_args contain Type::Param - skip if so
            let has_param = owner_args
              .iter()
              .any(|ty| matches!(self.types.get(ty), Type::Param { .. }));

            if !has_param {
              // Only enqueue if there are actual generic args and no Type::Param
              if !owner_args.is_empty() {
                self.enqueue(InstanceKey::generic(generic, owner_args.clone()));
              }
              let method_def = self.input_defs.get(method);
              if let DefinitionKind::Method(md) = &method_def.kind {
                if !md.type_params.is_empty() || !owner_args.is_empty() {
                  self.enqueue(InstanceKey::method(generic, owner_args, *method, type_args.clone()));
                }
              }
            } else if is_verbose() {
              eprintln!("[MONO] Skipping enqueue - owner_args contains Type::Param");
            }
          }
          self.scan_hir(*recv);
        }
        for arg in args {
          self.scan_hir(*arg);
        }
      },
      HIRKind::EnumVariant {
        enum_def,
        type_args,
        payload,
        ..
      } => {
        if !type_args.is_empty() {
          self.enqueue(InstanceKey::generic(*enum_def, type_args.clone()));
        }
        for val in payload {
          self.scan_hir(*val);
        }
      },
      HIRKind::Block { statements, expression } => {
        for stmt in statements {
          self.scan_hir(*stmt);
        }
        if let Some(expr) = expression {
          self.scan_hir(*expr);
        }
      },
      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => {
        self.scan_hir(*condition);
        self.scan_hir(*then_branch);
        if let Some(e) = else_branch {
          self.scan_hir(*e);
        }
      },
      HIRKind::Loop { condition, body } => {
        match condition {
          LoopKind::While { condition: c } => self.scan_hir(*c),
          LoopKind::For {
            init,
            condition,
            update,
          } => {
            if let Some(i) = init {
              self.scan_hir(*i);
            }
            if let Some(c) = condition {
              self.scan_hir(*c);
            }
            if let Some(u) = update {
              self.scan_hir(*u);
            }
          },
          LoopKind::Infinite => {},
        }
        self.scan_hir(*body);
      },
      HIRKind::Let { value, .. } => {
        if let Some(v) = value {
          self.scan_hir(*v);
        }
      },
      HIRKind::Assign { target, value, .. } => {
        self.scan_hir(*target);
        self.scan_hir(*value);
      },
      HIRKind::Return(expr) => {
        if let Some(e) = expr {
          self.scan_hir(*e);
        }
      },
      HIRKind::Binary { left, right, .. } => {
        self.scan_hir(*left);
        self.scan_hir(*right);
      },
      HIRKind::Unary { operand, .. } => {
        self.scan_hir(*operand);
      },
      HIRKind::Cast { expression, .. } => {
        self.scan_hir(*expression);
      },
      HIRKind::Reference { expression, .. } => {
        self.scan_hir(*expression);
      },
      HIRKind::Dereference(expr) => {
        self.scan_hir(*expr);
      },
      HIRKind::Index { base, index } => {
        self.scan_hir(*base);
        self.scan_hir(*index);
      },
      HIRKind::FieldAccess { base, .. } => {
        self.scan_hir(*base);
      },
      HIRKind::VectorLiteral { elements } => {
        for e in elements {
          self.scan_hir(*e);
        }
      },
      HIRKind::ExpressionStatement(expr) => {
        self.scan_hir(*expr);
      },
      HIRKind::TypeOf(expr) => {
        self.scan_hir(*expr);
      },
      // Terminals
      HIRKind::Literal(_)
      | HIRKind::Variable(_)
      | HIRKind::StaticAccess { .. }
      | HIRKind::Break
      | HIRKind::Continue
      | HIRKind::Error
      | HIRKind::SizeOf(_) => {},
    }
  }

  fn enqueue(
    &mut self,
    key: InstanceKey,
  ) {
    if !self.cache.contains_key(&key) {
      if is_verbose() {
        eprintln!("[MONO] enqueue: {:?}", key);
      }
      self.worklist.push_back(key);
    }
  }

  // === Instantiation Phase ===

  /// Create a concrete definition shell for an instantiation.
  fn create_concrete_def_shell(
    &mut self,
    key: &InstanceKey,
  ) -> DefinitionId {
    let mangled_name = { self.mangle_name(key) };
    if is_verbose() {
      eprintln!("[MONO] create_concrete_def_shell: key={:?}, mangled_name={}", key, mangled_name);
    }
    let verbose = is_verbose();
    let name_sym = {
      let mut syms = self.symbols.borrow_mut();
      syms.intern(&mangled_name)
    };
    let subst = self.build_substitution(key);

    match key {
      InstanceKey::Generic { def, .. } => {
        let generic_def = self.input_defs.get(def);
        let result = match &generic_def.kind.clone() {
          DefinitionKind::Function(fd) => self.instantiate_function(generic_def, fd, &subst, name_sym),
          DefinitionKind::Record(rd) => self.instantiate_record(generic_def, rd, &subst, name_sym),
          DefinitionKind::Enum(ed) => self.instantiate_enum(generic_def, ed, &subst, name_sym),
          _ => panic!("unexpected generic def kind: {:?}", generic_def.kind),
        };
        if verbose {
          eprintln!("[MONO]   -> concrete_id={:?}", result);
        }
        result
      },
      InstanceKey::Method {
        method_def,
        owner_def,
        owner_args,
        ..
      } => {
        let generic_def = self.input_defs.get(method_def);
        if let DefinitionKind::Method(md) = &generic_def.kind.clone() {
          self.instantiate_method(generic_def, md, &subst, name_sym, *owner_def, owner_args)
        } else {
          panic!("expected method def, got: {:?}", generic_def.kind)
        }
      },
    }
  }

  fn build_substitution(
    &self,
    key: &InstanceKey,
  ) -> Substitution {
    match key {
      InstanceKey::Generic { def, args } => Substitution::for_generic(*def, args),
      InstanceKey::Method {
        owner_def,
        owner_args,
        method_def,
        method_args,
      } => Substitution::for_method(*owner_def, owner_args, *method_def, method_args),
    }
  }

  fn instantiate_function(
    &mut self,
    generic_def: &Definition,
    fd: &FunctionDefinition,
    subst: &Substitution,
    name: ignis_type::symbol::SymbolId,
  ) -> DefinitionId {
    // Create concrete parameter definitions
    let new_params: Vec<_> = fd.params.iter().map(|p| self.instantiate_param(*p, subst)).collect();
    let new_ret = self.types.substitute(fd.return_type, subst);

    let new_def = Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: vec![],
        params: new_params,
        return_type: new_ret,
        is_extern: fd.is_extern,
        is_variadic: fd.is_variadic,
      }),
      name,
      span: generic_def.span.clone(),
      name_span: generic_def.name_span.clone(),
      visibility: generic_def.visibility,
      owner_module: generic_def.owner_module,
      owner_namespace: generic_def.owner_namespace,
      doc: generic_def.doc.clone(),
    };

    self.output_defs.alloc(new_def)
  }

  fn instantiate_record(
    &mut self,
    generic_def: &Definition,
    rd: &RecordDefinition,
    subst: &Substitution,
    name: ignis_type::symbol::SymbolId,
  ) -> DefinitionId {
    // Reserve ID first (needed for self-referential types and field owner_type)
    let concrete_id = self.output_defs.alloc_placeholder(
      name,
      generic_def.span.clone(),
      generic_def.name_span.clone(),
      generic_def.visibility,
      generic_def.owner_module,
      generic_def.owner_namespace,
    );

    let new_fields: Vec<_> = rd
      .fields
      .iter()
      .map(|f| {
        let substituted = self.types.substitute(f.type_id, subst);
        let concretized = self.concretize_type(substituted);
        let field_def = Definition {
          kind: DefinitionKind::Field(FieldDefinition {
            type_id: concretized,
            owner_type: concrete_id,
            index: f.index,
          }),
          name: f.name,
          span: f.span.clone(),
          name_span: f.span.clone(),
          visibility: Visibility::Public,
          owner_module: generic_def.owner_module,
          owner_namespace: generic_def.owner_namespace,
          doc: None,
        };
        let field_def_id = self.output_defs.alloc(field_def);

        RecordFieldDef {
          name: f.name,
          type_id: concretized,
          index: f.index,
          span: f.span.clone(),
          def_id: field_def_id,
        }
      })
      .collect();

    // Create concrete type (idempotent via cache)
    let concrete_type = self.types.record(concrete_id);

    // Post-mono: instance_methods is empty, HIR uses concrete DefinitionId directly
    self.output_defs.update(
      &concrete_id,
      DefinitionKind::Record(RecordDefinition {
        type_params: vec![],
        type_id: concrete_type,
        fields: new_fields,
        instance_methods: HashMap::new(),
        static_methods: HashMap::new(),
        static_fields: rd.static_fields.clone(),
      }),
    );

    concrete_id
  }

  fn instantiate_enum(
    &mut self,
    generic_def: &Definition,
    ed: &EnumDefinition,
    subst: &Substitution,
    name: ignis_type::symbol::SymbolId,
  ) -> DefinitionId {
    // Reserve ID first so variant definitions can reference owner_enum
    let concrete_id = self.output_defs.alloc_placeholder(
      name,
      generic_def.span.clone(),
      generic_def.name_span.clone(),
      generic_def.visibility,
      generic_def.owner_module,
      generic_def.owner_namespace,
    );

    let new_variants: Vec<_> = ed
      .variants
      .iter()
      .map(|v| {
        let substituted_payload: Vec<_> = v.payload.iter().map(|ty| self.types.substitute(*ty, subst)).collect();
        let variant_def = Definition {
          kind: DefinitionKind::Variant(VariantDefinition {
            payload: substituted_payload.clone(),
            owner_enum: concrete_id,
            tag_value: v.tag_value,
          }),
          name: v.name,
          span: v.span.clone(),
          name_span: v.span.clone(),
          visibility: Visibility::Public,
          owner_module: generic_def.owner_module,
          owner_namespace: generic_def.owner_namespace,
          doc: None,
        };
        let variant_def_id = self.output_defs.alloc(variant_def);

        EnumVariantDef {
          name: v.name,
          tag_value: v.tag_value,
          payload: substituted_payload,
          span: v.span.clone(),
          def_id: variant_def_id,
        }
      })
      .collect();

    // Create concrete type
    let concrete_type = self.types.enum_type(concrete_id);

    self.output_defs.update(
      &concrete_id,
      DefinitionKind::Enum(EnumDefinition {
        type_params: vec![],
        type_id: concrete_type,
        variants: new_variants,
        variants_by_name: ed.variants_by_name.clone(),
        tag_type: ed.tag_type,
        static_methods: HashMap::new(),
        static_fields: ed.static_fields.clone(),
      }),
    );

    concrete_id
  }

  fn instantiate_method(
    &mut self,
    generic_def: &Definition,
    md: &MethodDefinition,
    subst: &Substitution,
    name: ignis_type::symbol::SymbolId,
    owner_def: DefinitionId,
    owner_args: &[TypeId],
  ) -> DefinitionId {
    let new_params: Vec<_> = md.params.iter().map(|p| self.instantiate_param(*p, subst)).collect();
    let substituted_ret = self.types.substitute(md.return_type, subst);
    let new_ret = self.concretize_type(substituted_ret);

    // Find the concrete owner record/enum by looking up owner_def with owner_args in cache
    let concrete_owner = if !owner_args.is_empty() {
      let key = InstanceKey::generic(owner_def, owner_args.to_vec());
      self.cache.get(&key).copied().unwrap_or(md.owner_type)
    } else {
      md.owner_type
    };

    let new_def = Definition {
      kind: DefinitionKind::Method(MethodDefinition {
        type_params: vec![],
        owner_type: concrete_owner,
        params: new_params,
        return_type: new_ret,
        is_static: md.is_static,
        self_mutable: md.self_mutable,
      }),
      name,
      span: generic_def.span.clone(),
      name_span: generic_def.name_span.clone(),
      visibility: generic_def.visibility,
      owner_module: generic_def.owner_module,
      owner_namespace: generic_def.owner_namespace,
      doc: generic_def.doc.clone(),
    };

    self.output_defs.alloc(new_def)
  }

  fn instantiate_param(
    &mut self,
    param_id: DefinitionId,
    subst: &Substitution,
  ) -> DefinitionId {
    let param_def = self.input_defs.get(&param_id);
    let DefinitionKind::Parameter(pd) = &param_def.kind else {
      panic!("expected parameter definition");
    };

    // Substitute type params, then concretize Type::Instance to Type::Record
    let substituted_type = self.types.substitute(pd.type_id, subst);
    if is_verbose() {
      eprintln!(
        "[MONO] instantiate_param: original={:?}, substituted={:?}",
        self.types.get(&pd.type_id),
        self.types.get(&substituted_type)
      );
    }
    let new_type = self.concretize_type(substituted_type);
    if is_verbose() {
      eprintln!("[MONO]   -> concretized={:?}", self.types.get(&new_type));
    }

    let new_def = Definition {
      kind: DefinitionKind::Parameter(ParameterDefinition {
        type_id: new_type,
        mutable: pd.mutable,
      }),
      name: param_def.name,
      span: param_def.span.clone(),
      name_span: param_def.name_span.clone(),
      visibility: param_def.visibility,
      owner_module: param_def.owner_module,
      owner_namespace: param_def.owner_namespace,
      doc: None,
    };

    self.output_defs.alloc(new_def)
  }

  // === Body Substitution Phase ===

  fn substitute_body(
    &mut self,
    key: &InstanceKey,
    concrete_id: DefinitionId,
  ) {
    let primary_def = key.primary_def();
    if let Some(&body) = self.input_hir.function_bodies.get(&primary_def) {
      let subst = self.build_substitution(key);
      if is_verbose() {
        let def_name = self
          .symbols
          .borrow()
          .get(&self.input_defs.get(&primary_def).name)
          .to_string();
        eprintln!(
          "[MONO] substitute_body: primary_def={:?} ({}), concrete_id={:?}, subst={:?}",
          primary_def, def_name, concrete_id, subst
        );
      }

      // Build mapping from old param def_ids to new param def_ids
      self.current_def_remap = self.build_def_remapping(&primary_def, &concrete_id);

      let concrete_body = self.substitute_hir(body, &subst);
      if is_verbose() {
        eprintln!("[MONO]   -> inserting body, concrete_body={:?}", concrete_body);
      }
      self.output_hir.function_bodies.insert(concrete_id, concrete_body);
      if !self.output_hir.items.contains(&concrete_id) {
        self.output_hir.items.push(concrete_id);
      }
      if is_verbose() {
        eprintln!(
          "[MONO]   -> function_bodies count now: {}",
          self.output_hir.function_bodies.len()
        );
      }
    } else {
      // Invariant: non-extern functions/methods MUST have a body.
      // If we reach here, it means the body was never stored during lowering.
      let def = self.input_defs.get(&primary_def);
      let requires_body = match &def.kind {
        DefinitionKind::Function(fd) => !fd.is_extern,
        DefinitionKind::Method(_) => true,
        _ => false,
      };

      if requires_body {
        let def_name = self.symbols.borrow().get(&def.name).to_string();
        panic!(
          "[MONO] INVARIANT VIOLATION: Missing body for non-extern definition\n\
           def_id: {:?}\n\
           name: {}\n\
           kind: {:?}\n\
           span: {:?}\n\
           module: {:?}\n\
           This likely means the body was never stored during HIR lowering.\n\
           Check lower_record_methods() / lower_enum_methods() for overload handling.",
          primary_def, def_name, def.kind, def.span, def.owner_module
        );
      }
    }

    // Clear remapping after use
    self.current_def_remap.clear();
  }

  /// Build a mapping from old definition IDs to new definition IDs.
  /// This is needed to update Variable references when instantiating generic functions.
  fn build_def_remapping(
    &self,
    generic_def_id: &DefinitionId,
    concrete_def_id: &DefinitionId,
  ) -> HashMap<DefinitionId, DefinitionId> {
    let mut remapping = HashMap::new();

    let generic_def = self.input_defs.get(generic_def_id);
    let concrete_def = self.output_defs.get(concrete_def_id);

    // Get parameter lists from both definitions
    let (generic_params, concrete_params) = match (&generic_def.kind, &concrete_def.kind) {
      (DefinitionKind::Function(g), DefinitionKind::Function(c)) => (&g.params, &c.params),
      (DefinitionKind::Method(g), DefinitionKind::Method(c)) => (&g.params, &c.params),
      _ => return remapping,
    };

    // Map each old param to its corresponding new param
    for (old_param, new_param) in generic_params.iter().zip(concrete_params.iter()) {
      remapping.insert(*old_param, *new_param);
    }

    remapping
  }

  fn substitute_hir(
    &mut self,
    hir_id: HIRId,
    subst: &Substitution,
  ) -> HIRId {
    let node = self.input_hir.get(hir_id).clone();

    let new_kind = match &node.kind {
      HIRKind::Call {
        callee,
        type_args,
        args,
      } => {
        // Substitute type params in type_args (e.g., T -> i32)
        let substituted_type_args: Vec<_> = type_args.iter().map(|ty| self.types.substitute(*ty, subst)).collect();

        // Skip instantiation if any type arg is Error (indicates analyzer bug)
        let has_error = substituted_type_args
          .iter()
          .any(|ty| matches!(self.types.get(ty), Type::Error));

        let concrete_callee = if !substituted_type_args.is_empty() && !has_error {
          let key = InstanceKey::generic(*callee, substituted_type_args.clone());
          self.ensure_instantiated(&key)
        } else {
          *callee
        };
        let new_args: Vec<_> = args.iter().map(|a| self.substitute_hir(*a, subst)).collect();

        // Compute the correct return type using the call's type args.
        // The node.type_id has type params owned by the callee, but we need to substitute
        // them with the actual type args passed to this call.
        let call_return_type = if !substituted_type_args.is_empty() && !has_error {
          let call_subst = Substitution::for_generic(*callee, &substituted_type_args);
          let substituted = self.types.substitute(node.type_id, &call_subst);
          self.concretize_type(substituted)
        } else {
          let substituted = self.types.substitute(node.type_id, subst);
          self.concretize_type(substituted)
        };

        // Allocate the Call node with the correct return type
        return self.output_hir.alloc(HIRNode {
          kind: HIRKind::Call {
            callee: concrete_callee,
            type_args: vec![],
            args: new_args,
          },
          span: node.span,
          type_id: call_return_type,
        });
      },
      HIRKind::RecordInit {
        record_def,
        type_args,
        fields,
      } => {
        // Substitute type params in type_args
        let substituted_type_args: Vec<_> = type_args.iter().map(|ty| self.types.substitute(*ty, subst)).collect();
        let concrete_def = if !substituted_type_args.is_empty() {
          let key = InstanceKey::generic(*record_def, substituted_type_args);
          self.ensure_instantiated(&key)
        } else {
          *record_def
        };
        let new_fields: Vec<_> = fields
          .iter()
          .map(|(idx, val)| (*idx, self.substitute_hir(*val, subst)))
          .collect();
        HIRKind::RecordInit {
          record_def: concrete_def,
          type_args: vec![],
          fields: new_fields,
        }
      },
      HIRKind::MethodCall {
        receiver,
        method,
        type_args,
        args,
      } => {
        let new_receiver = receiver.map(|r| self.substitute_hir(r, subst));
        // Substitute type params in method's type_args
        let substituted_type_args: Vec<_> = type_args.iter().map(|ty| self.types.substitute(*ty, subst)).collect();
        let concrete_method = self.resolve_concrete_method(&new_receiver, *method, &substituted_type_args);
        let new_args: Vec<_> = args.iter().map(|a| self.substitute_hir(*a, subst)).collect();
        HIRKind::MethodCall {
          receiver: new_receiver,
          method: concrete_method,
          type_args: vec![],
          args: new_args,
        }
      },
      HIRKind::EnumVariant {
        enum_def,
        type_args,
        variant_tag,
        payload,
      } => {
        // Substitute type params in type_args
        let substituted_type_args: Vec<_> = type_args.iter().map(|ty| self.types.substitute(*ty, subst)).collect();
        if is_verbose() && !type_args.is_empty() {
          eprintln!(
            "[MONO] EnumVariant: type_args={:?}, substituted={:?}",
            type_args
              .iter()
              .map(|ty| self.types.get(ty).clone())
              .collect::<Vec<_>>(),
            substituted_type_args
              .iter()
              .map(|ty| self.types.get(ty).clone())
              .collect::<Vec<_>>()
          );
        }
        let concrete_def = if !substituted_type_args.is_empty() {
          let key = InstanceKey::generic(*enum_def, substituted_type_args);
          self.ensure_instantiated(&key)
        } else {
          *enum_def
        };
        let new_payload: Vec<_> = payload.iter().map(|p| self.substitute_hir(*p, subst)).collect();
        HIRKind::EnumVariant {
          enum_def: concrete_def,
          type_args: vec![],
          variant_tag: *variant_tag,
          payload: new_payload,
        }
      },
      // Recursively substitute in other node types
      _ => self.substitute_hir_kind_default(&node.kind, subst),
    };

    // Substitute the type as well
    let new_type = self.types.substitute(node.type_id, subst);
    let concretized_type = self.concretize_type(new_type);

    // Debug: check if we have Error types coming through
    if is_verbose() {
      if matches!(self.types.get(&concretized_type), Type::Error) {
        eprintln!(
          "[MONO] Warning: Error type in output HIR node. original={:?}, substituted={:?}, kind={:?}",
          self.types.get(&node.type_id),
          self.types.get(&new_type),
          new_kind
        );
      }
      // Also check for pointers to Error
      if let Type::Pointer { inner, .. } = self.types.get(&concretized_type) {
        if matches!(self.types.get(inner), Type::Error) {
          eprintln!(
            "[MONO] Warning: Pointer to Error in output HIR. original={:?}, substituted={:?}, concretized={:?}, kind={:?}",
            self.types.get(&node.type_id),
            self.types.get(&new_type),
            self.types.get(&concretized_type),
            new_kind
          );
        }
      }
    }

    self.output_hir.alloc(HIRNode {
      kind: new_kind,
      span: node.span,
      type_id: concretized_type,
    })
  }

  fn substitute_hir_kind_default(
    &mut self,
    kind: &HIRKind,
    subst: &Substitution,
  ) -> HIRKind {
    match kind {
      HIRKind::Literal(v) => HIRKind::Literal(v.clone()),
      HIRKind::Variable(def) => {
        // Remap variable references (e.g., parameters) to their new definitions
        let new_def = self.current_def_remap.get(def).copied().unwrap_or(*def);
        HIRKind::Variable(new_def)
      },
      HIRKind::Binary { operation, left, right } => {
        let new_left = self.substitute_hir(*left, subst);
        let new_right = self.substitute_hir(*right, subst);
        HIRKind::Binary {
          operation: operation.clone(),
          left: new_left,
          right: new_right,
        }
      },
      HIRKind::Unary { operation, operand } => {
        let new_operand = self.substitute_hir(*operand, subst);
        HIRKind::Unary {
          operation: operation.clone(),
          operand: new_operand,
        }
      },
      HIRKind::Cast { expression, target } => {
        let new_expr = self.substitute_hir(*expression, subst);
        let new_target = self.types.substitute(*target, subst);
        HIRKind::Cast {
          expression: new_expr,
          target: new_target,
        }
      },
      HIRKind::Reference { expression, mutable } => {
        let new_expr = self.substitute_hir(*expression, subst);
        HIRKind::Reference {
          expression: new_expr,
          mutable: *mutable,
        }
      },
      HIRKind::Dereference(expr) => {
        let new_expr = self.substitute_hir(*expr, subst);
        HIRKind::Dereference(new_expr)
      },
      HIRKind::BuiltinLoad { ty, ptr } => {
        let new_ptr = self.substitute_hir(*ptr, subst);
        let new_ty = self.types.substitute(*ty, subst);
        HIRKind::BuiltinLoad {
          ty: new_ty,
          ptr: new_ptr,
        }
      },
      HIRKind::BuiltinStore { ty, ptr, value } => {
        let new_ptr = self.substitute_hir(*ptr, subst);
        let new_value = self.substitute_hir(*value, subst);
        let new_ty = self.types.substitute(*ty, subst);
        HIRKind::BuiltinStore {
          ty: new_ty,
          ptr: new_ptr,
          value: new_value,
        }
      },
      HIRKind::Index { base, index } => {
        let new_base = self.substitute_hir(*base, subst);
        let new_index = self.substitute_hir(*index, subst);
        HIRKind::Index {
          base: new_base,
          index: new_index,
        }
      },
      HIRKind::VectorLiteral { elements } => {
        let new_elems: Vec<_> = elements.iter().map(|e| self.substitute_hir(*e, subst)).collect();
        HIRKind::VectorLiteral { elements: new_elems }
      },
      HIRKind::TypeOf(expr) => {
        let new_expr = self.substitute_hir(*expr, subst);
        HIRKind::TypeOf(new_expr)
      },
      HIRKind::SizeOf(ty) => {
        let new_ty = self.types.substitute(*ty, subst);
        HIRKind::SizeOf(new_ty)
      },
      HIRKind::FieldAccess { base, field_index } => {
        let new_base = self.substitute_hir(*base, subst);
        HIRKind::FieldAccess {
          base: new_base,
          field_index: *field_index,
        }
      },
      HIRKind::StaticAccess { def } => HIRKind::StaticAccess { def: *def },
      HIRKind::Let { name, value } => {
        let new_value = value.map(|v| self.substitute_hir(v, subst));
        HIRKind::Let {
          name: *name,
          value: new_value,
        }
      },
      HIRKind::Assign {
        target,
        value,
        operation,
      } => {
        let new_target = self.substitute_hir(*target, subst);
        let new_value = self.substitute_hir(*value, subst);
        HIRKind::Assign {
          target: new_target,
          value: new_value,
          operation: operation.clone(),
        }
      },
      HIRKind::Block { statements, expression } => {
        let new_stmts: Vec<_> = statements.iter().map(|s| self.substitute_hir(*s, subst)).collect();
        let new_expr = expression.map(|e| self.substitute_hir(e, subst));
        HIRKind::Block {
          statements: new_stmts,
          expression: new_expr,
        }
      },
      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => {
        let new_cond = self.substitute_hir(*condition, subst);
        let new_then = self.substitute_hir(*then_branch, subst);
        let new_else = else_branch.map(|e| self.substitute_hir(e, subst));
        HIRKind::If {
          condition: new_cond,
          then_branch: new_then,
          else_branch: new_else,
        }
      },
      HIRKind::Loop { condition, body } => {
        let new_cond = match condition {
          LoopKind::While { condition: c } => LoopKind::While {
            condition: self.substitute_hir(*c, subst),
          },
          LoopKind::For {
            init,
            condition,
            update,
          } => LoopKind::For {
            init: init.map(|i| self.substitute_hir(i, subst)),
            condition: condition.map(|c| self.substitute_hir(c, subst)),
            update: update.map(|u| self.substitute_hir(u, subst)),
          },
          LoopKind::Infinite => LoopKind::Infinite,
        };
        let new_body = self.substitute_hir(*body, subst);
        HIRKind::Loop {
          condition: new_cond,
          body: new_body,
        }
      },
      HIRKind::Break => HIRKind::Break,
      HIRKind::Continue => HIRKind::Continue,
      HIRKind::Return(expr) => {
        let new_expr = expr.map(|e| self.substitute_hir(e, subst));
        HIRKind::Return(new_expr)
      },
      HIRKind::ExpressionStatement(expr) => {
        let new_expr = self.substitute_hir(*expr, subst);
        HIRKind::ExpressionStatement(new_expr)
      },
      HIRKind::Error => HIRKind::Error,
      // These are handled in substitute_hir directly
      HIRKind::Call { .. } | HIRKind::RecordInit { .. } | HIRKind::MethodCall { .. } | HIRKind::EnumVariant { .. } => {
        unreachable!("should be handled in substitute_hir")
      },
    }
  }

  fn ensure_instantiated(
    &mut self,
    key: &InstanceKey,
  ) -> DefinitionId {
    if let Some(&id) = self.cache.get(key) {
      if is_verbose() {
        eprintln!("[MONO] ensure_instantiated: cache hit for concrete_id={:?}", id);
      }
      return id;
    }
    // Enqueue for processing (will be handled in fixpoint loop)
    self.enqueue(key.clone());
    // Create shell immediately so we have an ID to return
    let concrete_id = self.create_concrete_def_shell(key);
    self.cache.insert(key.clone(), concrete_id);
    self.reverse_cache.insert(concrete_id, key.clone());
    if is_verbose() {
      eprintln!(
        "[MONO] ensure_instantiated: added to reverse_cache: concrete_id={:?}",
        concrete_id
      );
    }
    concrete_id
  }

  fn resolve_concrete_method(
    &mut self,
    receiver: &Option<HIRId>,
    method_generic: DefinitionId,
    method_type_args: &[TypeId],
  ) -> DefinitionId {
    // Get owner args from receiver type, unwrapping pointers/references
    let owner_args = if let Some(recv_id) = receiver {
      let recv_ty = self.output_hir.get(*recv_id).type_id;
      if let Some((def, args)) = self.unwrap_to_instance_type_from_output(recv_ty) {
        if !args.is_empty() {
          // Type::Instance - use args directly
          if is_verbose() {
            eprintln!("[MONO] resolve_concrete_method: Type::Instance with args={:?}", args);
          }
          args
        } else {
          // Type::Record/Enum (already concrete) - look up original type args from reverse cache
          let result = self
            .reverse_cache
            .get(&def)
            .and_then(|key| match key {
              InstanceKey::Generic { args, .. } => Some(args.clone()),
              InstanceKey::Method { owner_args, .. } => Some(owner_args.clone()),
            })
            .unwrap_or_default();
          if is_verbose() {
            eprintln!(
              "[MONO] resolve_concrete_method: Type::Record/Enum def={:?}, reverse_cache lookup result={:?}",
              def, result
            );
          }
          result
        }
      } else {
        vec![]
      }
    } else {
      vec![]
    };

    self.resolve_concrete_method_with_args(method_generic, method_type_args, &owner_args)
  }

  /// Same as unwrap_to_instance_type but reads from output_hir types.
  fn unwrap_to_instance_type_from_output(
    &self,
    mut ty: TypeId,
  ) -> Option<(DefinitionId, Vec<TypeId>)> {
    loop {
      match self.types.get(&ty).clone() {
        Type::Reference { inner, .. } | Type::Pointer { inner, .. } => {
          ty = inner;
        },
        Type::Instance { generic, args } => {
          return Some((generic, args));
        },
        Type::Record(def_id) | Type::Enum(def_id) => {
          return Some((def_id, vec![]));
        },
        _ => return None,
      }
    }
  }

  /// Resolve a method call to its concrete instantiation, given the owner's type args.
  fn resolve_concrete_method_with_args(
    &mut self,
    method_generic: DefinitionId,
    method_type_args: &[TypeId],
    owner_args: &[TypeId],
  ) -> DefinitionId {
    // If no generic args, the method doesn't need instantiation
    if owner_args.is_empty() && method_type_args.is_empty() {
      return method_generic;
    }

    // Get owner_def from the method's definition
    let method_def = self.input_defs.get(&method_generic);
    let owner_def = match &method_def.kind {
      DefinitionKind::Method(md) => md.owner_type,
      _ => panic!("expected method, got {:?}", method_def.kind),
    };

    let key = InstanceKey::method(owner_def, owner_args.to_vec(), method_generic, method_type_args.to_vec());
    self.ensure_instantiated(&key)
  }

  /// Convert Type::Instance to Type::Record/Enum with concrete definition.
  fn concretize_type(
    &mut self,
    ty: TypeId,
  ) -> TypeId {
    match self.types.get(&ty).clone() {
      Type::Instance { generic, args } => {
        // Check if any args are Type::Param - if so, cannot concretize yet
        let has_param = args.iter().any(|ty| matches!(self.types.get(ty), Type::Param { .. }));

        if has_param {
          if is_verbose() {
            let generic_name = self
              .symbols
              .borrow()
              .get(&self.input_defs.get(&generic).name)
              .to_string();
            let args_debug: Vec<_> = args.iter().map(|ty| self.types.get(ty).clone()).collect();
            eprintln!(
              "[MONO] Warning: concretize_type called with Type::Instance containing Type::Param - returning as-is. generic={} (def {:?}), args={:?}",
              generic_name, generic, args_debug
            );
          }
          // Return the Type::Instance as-is - caller needs to handle this
          return ty;
        }

        let key = InstanceKey::generic(generic, args);
        let concrete_def = self.ensure_instantiated(&key);

        // Ensure reverse_cache entry exists for the concrete def
        if !self.reverse_cache.contains_key(&concrete_def) {
          self.reverse_cache.insert(concrete_def, key.clone());
        }

        let concrete_def_kind = &self.output_defs.get(&concrete_def).kind;
        match concrete_def_kind {
          DefinitionKind::Record(_) => self.types.record(concrete_def),
          DefinitionKind::Enum(_) => self.types.enum_type(concrete_def),
          _ => panic!("expected record or enum"),
        }
      },
      Type::Pointer { inner, mutable } => {
        let new_inner = self.concretize_type(inner);
        if new_inner == inner {
          ty
        } else {
          self.types.pointer(new_inner, mutable)
        }
      },
      Type::Reference { inner, mutable } => {
        let new_inner = self.concretize_type(inner);
        if new_inner == inner {
          ty
        } else {
          self.types.reference(new_inner, mutable)
        }
      },
      Type::Vector { element, size } => {
        let new_elem = self.concretize_type(element);
        if new_elem == element {
          ty
        } else {
          self.types.vector(new_elem, size)
        }
      },
      Type::Tuple(elems) => {
        let new_elems: Vec<_> = elems.iter().map(|e| self.concretize_type(*e)).collect();
        if new_elems == elems {
          ty
        } else {
          self.types.tuple(new_elems)
        }
      },
      Type::Function {
        params,
        ret,
        is_variadic,
      } => {
        let new_params: Vec<_> = params.iter().map(|p| self.concretize_type(*p)).collect();
        let new_ret = self.concretize_type(ret);
        if new_params == params && new_ret == ret {
          ty
        } else {
          self.types.function(new_params, new_ret, is_variadic)
        }
      },
      _ => ty,
    }
  }

  // === Name Mangling ===

  fn mangle_name(
    &self,
    key: &InstanceKey,
  ) -> String {
    match key {
      InstanceKey::Generic { def, args } => {
        let base = Self::escape(&self.get_def_name(def));
        let args_str = args.iter().map(|t| self.mangle_type(*t)).collect::<Vec<_>>().join("__");
        format!("{}__{}", base, args_str)
      },
      InstanceKey::Method {
        owner_def,
        owner_args,
        method_def,
        method_args,
      } => {
        let owner_name = Self::escape(&self.get_def_name(owner_def));
        let owner_args_str = owner_args
          .iter()
          .map(|t| self.mangle_type(*t))
          .collect::<Vec<_>>()
          .join("__");
        let method_name = Self::escape(&self.get_def_name(method_def));
        let method_args_str = if method_args.is_empty() {
          String::new()
        } else {
          format!(
            "__{}",
            method_args
              .iter()
              .map(|t| self.mangle_type(*t))
              .collect::<Vec<_>>()
              .join("__")
          )
        };
        format!("{}__{}__{}{}", owner_name, owner_args_str, method_name, method_args_str)
      },
    }
  }

  fn mangle_type(
    &self,
    ty: TypeId,
  ) -> String {
    match self.types.get(&ty) {
      Type::I8 => "i8".into(),
      Type::I16 => "i16".into(),
      Type::I32 => "i32".into(),
      Type::I64 => "i64".into(),
      Type::U8 => "u8".into(),
      Type::U16 => "u16".into(),
      Type::U32 => "u32".into(),
      Type::U64 => "u64".into(),
      Type::F32 => "f32".into(),
      Type::F64 => "f64".into(),
      Type::Boolean => "bool".into(),
      Type::Char => "char".into(),
      Type::String => "string".into(),
      Type::Void => "void".into(),
      Type::Never => "never".into(),
      Type::Pointer { inner, mutable } => {
        let prefix = if *mutable { "ptrmut" } else { "ptr" };
        format!("{}_{}", prefix, self.mangle_type(*inner))
      },
      Type::Reference { inner, mutable } => {
        let prefix = if *mutable { "refmut" } else { "ref" };
        format!("{}_{}", prefix, self.mangle_type(*inner))
      },
      Type::Vector { element, size } => match size {
        Some(n) => format!("arr{}_{}", n, self.mangle_type(*element)),
        None => format!("vec_{}", self.mangle_type(*element)),
      },
      Type::Instance { generic, args } => {
        let base = Self::escape(&self.get_def_name(generic));
        let args_str = args.iter().map(|a| self.mangle_type(*a)).collect::<Vec<_>>().join("__");
        format!("{}__{}", base, args_str)
      },
      Type::Record(def_id) | Type::Enum(def_id) => Self::escape(&self.get_def_name(def_id)),
      Type::Tuple(elems) => {
        let elems_str = elems.iter().map(|e| self.mangle_type(*e)).collect::<Vec<_>>().join("_");
        format!("tuple_{}", elems_str)
      },
      Type::Function { params, ret, .. } => {
        let params_str = params
          .iter()
          .map(|p| self.mangle_type(*p))
          .collect::<Vec<_>>()
          .join("_");
        let ret_str = self.mangle_type(*ret);
        format!("fn_{}_{}", params_str, ret_str)
      },
      Type::Param { owner, index } => format!("T{}__{}", index, owner.index()),
      Type::Infer => "infer".into(),
      Type::NullPtr => "null".into(),
      Type::Error => "error".into(),
    }
  }

  fn get_def_name(
    &self,
    def_id: &DefinitionId,
  ) -> String {
    let def = self.input_defs.get(def_id);
    let symbols = self.symbols.borrow();
    let name = symbols.get(&def.name).to_string();
    drop(symbols); // Explicitly release borrow
    name
  }

  fn escape(s: &str) -> String {
    s.replace('_', "_0")
  }

  /// Check if an owner (record/enum) definition is generic.
  fn is_owner_generic(
    &self,
    owner_id: DefinitionId,
  ) -> bool {
    // Try output_defs first (for concrete instantiations), then input_defs (for generic templates).
    // Concrete instantiations always have empty type_params (they're fully instantiated).
    let owner_def = self.output_defs.get(&owner_id);
    match &owner_def.kind {
      DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
      DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
      // Placeholder means it's being instantiated - check input_defs
      DefinitionKind::Placeholder => {
        let input_owner_def = self.input_defs.get(&owner_id);
        match &input_owner_def.kind {
          DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
          DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
          _ => false,
        }
      },
      _ => false,
    }
  }
}

#[cfg(debug_assertions)]
impl MonoOutput {
  /// Verify that no Type::Param or Type::Instance remains after monomorphization.
  ///
  /// Only verifies NON-GENERIC definitions, since generic definitions (like `Box<T>`)
  /// legitimately contain Type::Param in their fields/params. The real invariant
  /// (Invariante A) is enforced by panics in codegen if Type::Param reaches LIR/C.
  pub fn verify_no_generics(
    &self,
    types: &TypeStore,
  ) {
    let mut warnings = Vec::new();

    // Verify HIR nodes
    for (_, hir_id) in self.hir.nodes.iter() {
      self.check_type_is_concrete(hir_id.type_id, types, "HIR node", &mut warnings);
    }

    // Verify definitions - only check NON-GENERIC definitions
    for (_, def) in self.defs.iter() {
      match &def.kind {
        // Only verify non-generic functions
        DefinitionKind::Function(fd) if fd.type_params.is_empty() => {
          self.check_type_is_concrete(fd.return_type, types, "function return", &mut warnings);
          for param_id in &fd.params {
            let param_ty = self.defs.type_of(param_id);
            self.check_type_is_concrete(*param_ty, types, "function param", &mut warnings);
          }
        },

        // Only verify non-generic records
        DefinitionKind::Record(rd) if rd.type_params.is_empty() => {
          for field in &rd.fields {
            self.check_type_is_concrete(field.type_id, types, "record field", &mut warnings);
          }
        },

        // Only verify non-generic enums
        DefinitionKind::Enum(ed) if ed.type_params.is_empty() => {
          for variant in &ed.variants {
            for payload_ty in &variant.payload {
              self.check_type_is_concrete(*payload_ty, types, "enum payload", &mut warnings);
            }
          }
        },

        // Methods: verify if non-generic AND owner is non-generic
        DefinitionKind::Method(md) if md.type_params.is_empty() => {
          if !self.is_owner_generic(md.owner_type) {
            self.check_type_is_concrete(md.return_type, types, "method return", &mut warnings);
            for param_id in &md.params {
              let param_ty = self.defs.type_of(param_id);
              self.check_type_is_concrete(*param_ty, types, "method param", &mut warnings);
            }
          }
        },

        _ => {},
      }
    }

    // Log warnings if verbose mode (controlled by environment variable)
    if !warnings.is_empty() && is_verbose() {
      for warning in &warnings {
        eprintln!("[mono warning] {}", warning);
      }
    }
  }

  /// Check if an owner (record/enum) definition is generic.
  fn is_owner_generic(
    &self,
    owner_id: DefinitionId,
  ) -> bool {
    let owner_def = self.defs.get(&owner_id);
    match &owner_def.kind {
      DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
      DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
      _ => false,
    }
  }

  /// Check if a type is concrete (no Type::Param or Type::Instance).
  /// Collects warnings instead of panicking.
  fn check_type_is_concrete(
    &self,
    ty: TypeId,
    types: &TypeStore,
    context: &str,
    warnings: &mut Vec<String>,
  ) {
    match types.get(&ty) {
      Type::Param { owner, index } => {
        warnings.push(format!(
          "Type::Param({:?}, {}) found in {} post-mono - possible monomorphization bug",
          owner, index, context
        ));
      },
      Type::Instance { generic, args } => {
        warnings.push(format!(
          "Type::Instance({:?}, {:?}) found in {} post-mono - possible monomorphization bug",
          generic, args, context
        ));
      },
      Type::Pointer { inner, .. } => self.check_type_is_concrete(*inner, types, context, warnings),
      Type::Reference { inner, .. } => self.check_type_is_concrete(*inner, types, context, warnings),
      Type::Vector { element, .. } => self.check_type_is_concrete(*element, types, context, warnings),
      Type::Tuple(elems) => {
        for e in elems {
          self.check_type_is_concrete(*e, types, context, warnings);
        }
      },
      Type::Function { params, ret, .. } => {
        for p in params {
          self.check_type_is_concrete(*p, types, context, warnings);
        }
        self.check_type_is_concrete(*ret, types, context, warnings);
      },
      _ => {},
    }
  }
}
