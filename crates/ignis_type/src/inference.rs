use std::collections::HashMap;

use crate::span::Span;
use crate::symbol::SymbolId;
use crate::types::{InferVarId, Type, TypeId, TypeStore};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstraintReason {
  Assignment,
  Argument { param_idx: usize },
  Return,
  FieldAccess { field_name: SymbolId },
  Condition,
  MatchScrutinee,
  BinaryOp,
  VariableDeclaration,
}

#[derive(Debug, Clone)]
pub struct UnifyError {
  pub kind: UnifyErrorKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UnifyErrorKind {
  TypeMismatch {
    expected: TypeId,
    got: TypeId,
    reason: ConstraintReason,
  },
  MutabilityMismatch {
    expected_mutable: bool,
  },
  OccursCheckFailed {
    var: InferVarId,
    ty: TypeId,
  },
  TupleLengthMismatch {
    expected: usize,
    got: usize,
  },
  FunctionArityMismatch {
    expected: usize,
    got: usize,
  },
  UnresolvedInferenceVar {
    var: InferVarId,
  },
}

#[derive(Debug, Clone)]
pub struct InferCtx {
  next_var_id: u32,
  uf_parent: HashMap<InferVarId, InferVarId>,
  bindings: HashMap<InferVarId, TypeId>,
  var_origins: HashMap<InferVarId, Span>,
  var_constraints: HashMap<InferVarId, (TypeId, ConstraintReason, Span)>,
}

impl Default for InferCtx {
  fn default() -> Self {
    Self::new()
  }
}

impl InferCtx {
  pub fn new() -> Self {
    Self {
      next_var_id: 0,
      uf_parent: HashMap::new(),
      bindings: HashMap::new(),
      var_origins: HashMap::new(),
      var_constraints: HashMap::new(),
    }
  }

  pub fn fresh_var(
    &mut self,
    types: &mut TypeStore,
    origin_span: Span,
  ) -> TypeId {
    let id = InferVarId(self.next_var_id);
    self.next_var_id += 1;
    self.uf_parent.insert(id, id);
    self.var_origins.insert(id, origin_span);
    types.infer_var(id)
  }

  pub fn find_root(
    &self,
    var: InferVarId,
  ) -> InferVarId {
    let mut current = var;
    loop {
      let parent = self.uf_parent.get(&current).copied().unwrap_or(current);
      if parent == current {
        return current;
      }
      current = parent;
    }
  }

  pub fn union(
    &mut self,
    a: InferVarId,
    b: InferVarId,
  ) {
    let root_a = self.find_root(a);
    let root_b = self.find_root(b);
    if root_a != root_b {
      self.uf_parent.insert(root_a, root_b);
    }
  }

  pub fn get_binding(
    &self,
    var: InferVarId,
  ) -> Option<TypeId> {
    let root = self.find_root(var);
    self.bindings.get(&root).copied()
  }

  pub fn bind(
    &mut self,
    var: InferVarId,
    ty: TypeId,
    span: &Span,
    reason: ConstraintReason,
    types: &mut TypeStore,
  ) -> Result<(), UnifyError> {
    let root = self.find_root(var);

    if let Some(existing) = self.bindings.get(&root).copied() {
      return self.unify(existing, ty, span, reason, types);
    }

    if self.occurs_check(root, ty, types) {
      return Err(UnifyError {
        kind: UnifyErrorKind::OccursCheckFailed { var: root, ty },
        span: span.clone(),
      });
    }

    self.bindings.insert(root, ty);
    self.var_constraints.insert(root, (ty, reason, span.clone()));
    Ok(())
  }

  fn occurs_check(
    &self,
    var: InferVarId,
    ty: TypeId,
    types: &TypeStore,
  ) -> bool {
    match types.get(&ty) {
      Type::InferVar(id) => {
        let root = self.find_root(*id);
        self.find_root(var) == root
      },
      Type::Pointer { inner, .. } | Type::Reference { inner, .. } => self.occurs_check(var, *inner, types),
      Type::Tuple(elems) => elems.iter().any(|e| self.occurs_check(var, *e, types)),
      Type::Function { params, ret, .. } => {
        params.iter().any(|p| self.occurs_check(var, *p, types)) || self.occurs_check(var, *ret, types)
      },
      Type::Instance { args, .. } => args.iter().any(|a| self.occurs_check(var, *a, types)),
      _ => false,
    }
  }

  pub fn unify(
    &mut self,
    a: TypeId,
    b: TypeId,
    span: &Span,
    reason: ConstraintReason,
    types: &mut TypeStore,
  ) -> Result<(), UnifyError> {
    let a = self.resolve(a, types);
    let b = self.resolve(b, types);

    if a == b {
      return Ok(());
    }

    match (types.get(&a).clone(), types.get(&b).clone()) {
      (Type::InferVar(id_a), Type::InferVar(id_b)) => {
        self.union(id_a, id_b);
        Ok(())
      },
      (Type::InferVar(id), _) => self.bind(id, b, span, reason, types),
      (_, Type::InferVar(id)) => self.bind(id, a, span, reason, types),

      (Type::Pointer { inner: i1, mutable: m1 }, Type::Pointer { inner: i2, mutable: m2 }) => {
        if m1 != m2 {
          return Err(UnifyError {
            kind: UnifyErrorKind::MutabilityMismatch { expected_mutable: m1 },
            span: span.clone(),
          });
        }
        self.unify(i1, i2, span, reason, types)
      },

      (Type::Reference { inner: i1, mutable: m1 }, Type::Reference { inner: i2, mutable: m2 }) => {
        if m1 != m2 {
          return Err(UnifyError {
            kind: UnifyErrorKind::MutabilityMismatch { expected_mutable: m1 },
            span: span.clone(),
          });
        }
        self.unify(i1, i2, span, reason, types)
      },

      (Type::Tuple(elems_a), Type::Tuple(elems_b)) => {
        if elems_a.len() != elems_b.len() {
          return Err(UnifyError {
            kind: UnifyErrorKind::TupleLengthMismatch {
              expected: elems_a.len(),
              got: elems_b.len(),
            },
            span: span.clone(),
          });
        }
        for (ea, eb) in elems_a.iter().zip(elems_b.iter()) {
          self.unify(*ea, *eb, span, reason, types)?;
        }
        Ok(())
      },

      (
        Type::Function {
          params: p1,
          ret: r1,
          is_variadic: v1,
        },
        Type::Function {
          params: p2,
          ret: r2,
          is_variadic: v2,
        },
      ) => {
        if v1 != v2 || p1.len() != p2.len() {
          return Err(UnifyError {
            kind: UnifyErrorKind::FunctionArityMismatch {
              expected: p1.len(),
              got: p2.len(),
            },
            span: span.clone(),
          });
        }
        for (pa, pb) in p1.iter().zip(p2.iter()) {
          self.unify(*pa, *pb, span, reason, types)?;
        }
        self.unify(r1, r2, span, reason, types)
      },

      (Type::Instance { generic: g1, args: a1 }, Type::Instance { generic: g2, args: a2 }) => {
        if g1 != g2 || a1.len() != a2.len() {
          return Err(UnifyError {
            kind: UnifyErrorKind::TypeMismatch {
              expected: a,
              got: b,
              reason,
            },
            span: span.clone(),
          });
        }
        for (aa, ab) in a1.iter().zip(a2.iter()) {
          self.unify(*aa, *ab, span, reason, types)?;
        }
        Ok(())
      },

      (Type::Record(d1), Type::Record(d2)) if d1 == d2 => Ok(()),
      (Type::Enum(d1), Type::Enum(d2)) if d1 == d2 => Ok(()),

      (Type::Error, _) | (_, Type::Error) => Ok(()),
      (Type::Unknown, _) | (_, Type::Unknown) => Ok(()),
      (Type::Infer, _) | (_, Type::Infer) => Ok(()),

      _ => Err(UnifyError {
        kind: UnifyErrorKind::TypeMismatch {
          expected: a,
          got: b,
          reason,
        },
        span: span.clone(),
      }),
    }
  }

  pub fn resolve(
    &self,
    ty: TypeId,
    types: &mut TypeStore,
  ) -> TypeId {
    let ty_kind = types.get(&ty).clone();
    match ty_kind {
      Type::InferVar(id) => {
        let root = self.find_root(id);
        if let Some(binding) = self.bindings.get(&root).copied() {
          self.resolve(binding, types)
        } else {
          ty
        }
      },
      Type::Pointer { inner, mutable } => {
        let resolved_inner = self.resolve(inner, types);
        if resolved_inner == inner {
          ty
        } else {
          types.pointer(resolved_inner, mutable)
        }
      },
      Type::Reference { inner, mutable } => {
        let resolved_inner = self.resolve(inner, types);
        if resolved_inner == inner {
          ty
        } else {
          types.reference(resolved_inner, mutable)
        }
      },
      Type::Tuple(elems) => {
        let elems_clone = elems.clone();
        let resolved: Vec<_> = elems_clone.iter().map(|e| self.resolve(*e, types)).collect();
        if resolved.iter().zip(elems.iter()).all(|(r, e)| r == e) {
          ty
        } else {
          types.tuple(resolved)
        }
      },
      Type::Function {
        params,
        ret,
        is_variadic,
      } => {
        let params_clone = params.clone();
        let resolved_params: Vec<_> = params_clone.iter().map(|p| self.resolve(*p, types)).collect();
        let resolved_ret = self.resolve(ret, types);
        if resolved_params.iter().zip(params.iter()).all(|(r, p)| r == p) && resolved_ret == ret {
          ty
        } else {
          types.function(resolved_params, resolved_ret, is_variadic)
        }
      },
      Type::Instance { generic, args } => {
        let args_clone = args.clone();
        let resolved_args: Vec<_> = args_clone.iter().map(|a| self.resolve(*a, types)).collect();
        if resolved_args.iter().zip(args.iter()).all(|(r, a)| r == a) {
          ty
        } else {
          types.instance(generic, resolved_args)
        }
      },
      _ => ty,
    }
  }

  pub fn zonk(
    &self,
    ty: TypeId,
    types: &mut TypeStore,
  ) -> Result<TypeId, InferVarId> {
    let resolved = self.resolve(ty, types);
    if let Type::InferVar(id) = types.get(&resolved) {
      Err(*id)
    } else {
      Ok(resolved)
    }
  }

  pub fn get_origin(
    &self,
    var: InferVarId,
  ) -> Option<&Span> {
    let root = self.find_root(var);
    self.var_origins.get(&root)
  }

  pub fn get_constraint_info(
    &self,
    var: InferVarId,
  ) -> Option<&(TypeId, ConstraintReason, Span)> {
    let root = self.find_root(var);
    self.var_constraints.get(&root)
  }

  pub fn is_resolved(
    &self,
    var: InferVarId,
  ) -> bool {
    let root = self.find_root(var);
    self.bindings.contains_key(&root)
  }
}
