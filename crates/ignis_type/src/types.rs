use std::collections::HashMap;

use crate::{
  Id, Store,
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
  symbol::SymbolTable,
};

pub type TypeId = Id<Type>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  I8,
  I16,
  I32,
  I64,
  U8,
  U16,
  U32,
  U64,
  F32,
  F64,
  Boolean,
  Char,
  String,
  Void,
  Never,
  Infer,
  NullPtr,

  Pointer {
    inner: TypeId,
    mutable: bool,
  },
  Reference {
    inner: TypeId,
    mutable: bool,
  },
  Vector {
    element: TypeId,
    size: Option<usize>,
  },
  Tuple(Vec<TypeId>),

  Function {
    params: Vec<TypeId>,
    ret: TypeId,
    is_variadic: bool,
  },

  /// User-defined record type
  Record(DefinitionId),

  /// User-defined enum type
  Enum(DefinitionId),

  /// Type parameter (T, U, etc.) - only exists pre-monomorphization
  Param {
    /// The function/record/method that declares this type param
    owner: DefinitionId,
    /// Position in the type parameter list
    index: u32,
  },

  /// Instance of a generic type with concrete type arguments.
  /// Example: Vec<i32> = Instance { generic: Vec_def, args: [i32] }
  /// NOTE: Does not exist post-monomorphization (Invariant D)
  Instance {
    /// The generic record/enum definition
    generic: DefinitionId,
    /// Concrete type arguments
    args: Vec<TypeId>,
  },

  // TODO: Implement Inferenced(InferenceVariable)
  // Inferenced(InferenceVariable),
  Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PointerKey {
  inner: TypeId,
  mutable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ReferenceKey {
  inner: TypeId,
  mutable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct VectorKey {
  element: TypeId,
  size: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionKey {
  params: Vec<TypeId>,
  ret: TypeId,
  is_variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct InstanceKey {
  generic: DefinitionId,
  args: Vec<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ParamKey {
  owner: DefinitionId,
  index: u32,
}

#[derive(Debug, Clone)]
pub struct TypeStore {
  types: Store<Type>,
  primitives: HashMap<Type, TypeId>,
  pointers: HashMap<PointerKey, TypeId>,
  references: HashMap<ReferenceKey, TypeId>,
  vectors: HashMap<VectorKey, TypeId>,
  tuples: HashMap<Vec<TypeId>, TypeId>,
  functions: HashMap<FunctionKey, TypeId>,
  records: HashMap<DefinitionId, TypeId>,
  enums: HashMap<DefinitionId, TypeId>,
  instances: HashMap<InstanceKey, TypeId>,
  params: HashMap<ParamKey, TypeId>,
  null_ptr: TypeId,
}

impl Default for TypeStore {
  fn default() -> Self {
    Self::new()
  }
}

impl TypeStore {
  pub fn new() -> Self {
    let mut store = Self {
      types: Store::new(),
      primitives: HashMap::new(),
      pointers: HashMap::new(),
      references: HashMap::new(),
      vectors: HashMap::new(),
      tuples: HashMap::new(),
      functions: HashMap::new(),
      records: HashMap::new(),
      enums: HashMap::new(),
      instances: HashMap::new(),
      params: HashMap::new(),
      null_ptr: TypeId::new(0),
    };
    store.init_primitives();
    store.null_ptr = store.primitives[&Type::NullPtr];
    store
  }

  fn init_primitives(&mut self) {
    let primitives = [
      Type::I8,
      Type::I16,
      Type::I32,
      Type::I64,
      Type::U8,
      Type::U16,
      Type::U32,
      Type::U64,
      Type::F32,
      Type::F64,
      Type::Boolean,
      Type::Char,
      Type::String,
      Type::Void,
      Type::Never,
      Type::Infer,
      Type::NullPtr,
      Type::Error,
    ];

    for ty in primitives {
      let id = self.types.alloc(ty.clone());
      self.primitives.insert(ty, id);
    }
  }

  pub fn pointer(
    &mut self,
    inner: TypeId,
    mutable: bool,
  ) -> TypeId {
    let key = PointerKey { inner, mutable };
    if let Some(&id) = self.pointers.get(&key) {
      return id;
    }
    let id = self.types.alloc(Type::Pointer { inner, mutable });
    self.pointers.insert(key, id);
    id
  }

  pub fn reference(
    &mut self,
    inner: TypeId,
    mutable: bool,
  ) -> TypeId {
    let key = ReferenceKey { inner, mutable };
    if let Some(&id) = self.references.get(&key) {
      return id;
    }
    let id = self.types.alloc(Type::Reference { inner, mutable });
    self.references.insert(key, id);
    id
  }

  pub fn vector(
    &mut self,
    element: TypeId,
    size: Option<usize>,
  ) -> TypeId {
    let key = VectorKey { element, size };
    if let Some(&id) = self.vectors.get(&key) {
      return id;
    }
    let id = self.types.alloc(Type::Vector { element, size });
    self.vectors.insert(key, id);
    id
  }

  pub fn tuple(
    &mut self,
    elements: Vec<TypeId>,
  ) -> TypeId {
    if let Some(&id) = self.tuples.get(&elements) {
      return id;
    }
    let id = self.types.alloc(Type::Tuple(elements.clone()));
    self.tuples.insert(elements, id);
    id
  }

  pub fn function(
    &mut self,
    params: Vec<TypeId>,
    ret: TypeId,
    is_variadic: bool,
  ) -> TypeId {
    let key = FunctionKey {
      params: params.clone(),
      ret,
      is_variadic,
    };
    if let Some(&id) = self.functions.get(&key) {
      return id;
    }
    let id = self.types.alloc(Type::Function {
      params,
      ret,
      is_variadic,
    });
    self.functions.insert(key, id);
    id
  }

  /// Create or get a record type
  pub fn record(
    &mut self,
    def_id: DefinitionId,
  ) -> TypeId {
    if let Some(&id) = self.records.get(&def_id) {
      return id;
    }
    let id = self.types.alloc(Type::Record(def_id));
    self.records.insert(def_id, id);
    id
  }

  /// Create or get an enum type
  pub fn enum_type(
    &mut self,
    def_id: DefinitionId,
  ) -> TypeId {
    if let Some(&id) = self.enums.get(&def_id) {
      return id;
    }
    let id = self.types.alloc(Type::Enum(def_id));
    self.enums.insert(def_id, id);
    id
  }

  /// Create or get a type parameter
  pub fn param(
    &mut self,
    owner: DefinitionId,
    index: u32,
  ) -> TypeId {
    let key = ParamKey { owner, index };
    if let Some(&id) = self.params.get(&key) {
      return id;
    }
    let id = self.types.alloc(Type::Param { owner, index });
    self.params.insert(key, id);
    id
  }

  /// Create or get a generic type instance (e.g., Vec<i32>)
  pub fn instance(
    &mut self,
    generic: DefinitionId,
    args: Vec<TypeId>,
  ) -> TypeId {
    let key = InstanceKey {
      generic,
      args: args.clone(),
    };
    if let Some(&id) = self.instances.get(&key) {
      return id;
    }
    let id = self.types.alloc(Type::Instance { generic, args });
    self.instances.insert(key, id);
    id
  }

  #[inline]
  pub fn get(
    &self,
    id: &TypeId,
  ) -> &Type {
    self.types.get(id)
  }

  #[inline]
  pub fn i8(&self) -> TypeId {
    self.primitives[&Type::I8]
  }
  #[inline]
  pub fn i16(&self) -> TypeId {
    self.primitives[&Type::I16]
  }
  #[inline]
  pub fn i32(&self) -> TypeId {
    self.primitives[&Type::I32]
  }
  #[inline]
  pub fn i64(&self) -> TypeId {
    self.primitives[&Type::I64]
  }

  #[inline]
  pub fn u8(&self) -> TypeId {
    self.primitives[&Type::U8]
  }
  #[inline]
  pub fn u16(&self) -> TypeId {
    self.primitives[&Type::U16]
  }
  #[inline]
  pub fn u32(&self) -> TypeId {
    self.primitives[&Type::U32]
  }
  #[inline]
  pub fn u64(&self) -> TypeId {
    self.primitives[&Type::U64]
  }

  #[inline]
  pub fn f32(&self) -> TypeId {
    self.primitives[&Type::F32]
  }
  #[inline]
  pub fn f64(&self) -> TypeId {
    self.primitives[&Type::F64]
  }

  #[inline]
  pub fn boolean(&self) -> TypeId {
    self.primitives[&Type::Boolean]
  }
  #[inline]
  pub fn char(&self) -> TypeId {
    self.primitives[&Type::Char]
  }
  #[inline]
  pub fn string(&self) -> TypeId {
    self.primitives[&Type::String]
  }
  #[inline]
  pub fn void(&self) -> TypeId {
    self.primitives[&Type::Void]
  }
  #[inline]
  pub fn never(&self) -> TypeId {
    self.primitives[&Type::Never]
  }
  #[inline]
  pub fn infer(&self) -> TypeId {
    self.primitives[&Type::Infer]
  }
  #[inline]
  pub fn error(&self) -> TypeId {
    self.primitives[&Type::Error]
  }

  pub fn null_ptr(&self) -> TypeId {
    self.null_ptr
  }

  pub fn is_numeric(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(
      self.get(ty),
      Type::I8
        | Type::I16
        | Type::I32
        | Type::I64
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::F32
        | Type::F64
    )
  }

  pub fn is_integer(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(
      self.get(ty),
      Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::U8 | Type::U16 | Type::U32 | Type::U64
    )
  }

  pub fn is_pointer(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.get(ty), Type::Pointer { .. })
  }

  pub fn is_float(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.get(ty), Type::F32 | Type::F64)
  }

  pub fn is_signed(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(
      self.get(ty),
      Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::F32 | Type::F64
    )
  }

  pub fn is_unsigned(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.get(ty), Type::U8 | Type::U16 | Type::U32 | Type::U64)
  }

  /// Returns true if the type is an enum where all variants have no payload.
  /// This is used to determine if enum values can be compared with == and !=.
  pub fn is_unit_enum(
    &self,
    ty: &TypeId,
    defs: &DefinitionStore,
  ) -> bool {
    match self.get(ty) {
      Type::Enum(def_id) => {
        let def = defs.get(def_id);
        if let DefinitionKind::Enum(ed) = &def.kind {
          return ed.variants.iter().all(|v| v.payload.is_empty());
        }
        false
      },
      _ => false,
    }
  }

  pub fn types_equal(
    &self,
    a: &TypeId,
    b: &TypeId,
  ) -> bool {
    if a == b {
      return true;
    }

    if self.is_error(a) || self.is_error(b) {
      return true;
    }

    if self.is_infer(a) || self.is_infer(b) {
      return true;
    }

    false
  }

  #[inline]
  pub fn is_error(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.get(ty), Type::Error)
  }

  #[inline]
  pub fn is_infer(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.get(ty), Type::Infer)
  }

  #[inline]
  pub fn is_null_ptr(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.get(ty), Type::NullPtr)
  }

  /// Returns true if the type has Copy semantics (no ownership transfer on assignment).
  pub fn is_copy(
    &self,
    ty: &TypeId,
  ) -> bool {
    match self.get(ty) {
      Type::I8
      | Type::I16
      | Type::I32
      | Type::I64
      | Type::U8
      | Type::U16
      | Type::U32
      | Type::U64
      | Type::F32
      | Type::F64
      | Type::Boolean
      | Type::Char
      | Type::Void
      | Type::Never
      | Type::NullPtr
      | Type::Error => true,

      Type::Pointer { .. } | Type::Reference { .. } | Type::Function { .. } => true,

      Type::Vector { element, size: Some(_) } => self.is_copy(element),
      Type::Vector { size: None, .. } => false,

      Type::String | Type::Infer => false,

      Type::Tuple(elems) => elems.iter().all(|e| self.is_copy(e)),

      // Records and enums are Copy by default (for v0.2, no heap fields)
      Type::Record(_) | Type::Enum(_) => true,

      // Type parameters are conservatively non-Copy (resolved at monomorphization)
      Type::Param { .. } => false,

      // Generic instances need to check their concrete arguments
      Type::Instance { args, .. } => args.iter().all(|a| self.is_copy(a)),
    }
  }

  /// Returns true if the type owns heap resources that need dropping.
  pub fn is_owned(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.get(ty), Type::String | Type::Vector { size: None, .. } | Type::Infer)
  }

  #[inline]
  pub fn needs_drop(
    &self,
    ty: &TypeId,
  ) -> bool {
    self.is_owned(ty)
  }

  /// Returns true if the type contains any type parameters.
  ///
  /// This recursively checks compound types (pointers, references, vectors, tuples,
  /// functions, instances) for type parameters.
  pub fn contains_type_param(
    &self,
    ty: &TypeId,
  ) -> bool {
    match self.get(ty) {
      Type::Param { .. } => true,
      Type::Pointer { inner, .. } | Type::Reference { inner, .. } | Type::Vector { element: inner, .. } => {
        self.contains_type_param(inner)
      },
      Type::Tuple(elems) => elems.iter().any(|e| self.contains_type_param(e)),
      Type::Function { params, ret, .. } => {
        params.iter().any(|p| self.contains_type_param(p)) || self.contains_type_param(ret)
      },
      Type::Instance { args, .. } => args.iter().any(|a| self.contains_type_param(a)),
      _ => false,
    }
  }

  /// Substitute type parameters with concrete types according to the substitution map.
  ///
  /// This recursively traverses the type, replacing any `Type::Param` with its
  /// corresponding concrete type from the substitution. Compound types (pointers,
  /// references, vectors, tuples, functions, instances) are reconstructed with
  /// substituted inner types.
  pub fn substitute(
    &mut self,
    ty: TypeId,
    subst: &Substitution,
  ) -> TypeId {
    match self.get(&ty).clone() {
      Type::Param { owner, index } => {
        let result = subst.get(owner, index).unwrap_or(ty);
        if std::env::var("IGNIS_VERBOSE").is_ok() && result == ty && !subst.is_empty() {
          eprintln!(
            "[TYPES] substitute: Type::Param {{ owner: {:?}, index: {} }} NOT found in subst {:?}, returning as-is",
            owner, index, subst
          );
        }
        result
      },

      Type::Instance { generic, args } => {
        let new_args: Vec<_> = args.iter().map(|a| self.substitute(*a, subst)).collect();
        self.instance(generic, new_args)
      },

      Type::Pointer { inner, mutable } => {
        let new_inner = self.substitute(inner, subst);
        self.pointer(new_inner, mutable)
      },

      Type::Reference { inner, mutable } => {
        let new_inner = self.substitute(inner, subst);
        self.reference(new_inner, mutable)
      },

      Type::Vector { element, size } => {
        let new_elem = self.substitute(element, subst);
        self.vector(new_elem, size)
      },

      Type::Tuple(elems) => {
        let new_elems: Vec<_> = elems.iter().map(|e| self.substitute(*e, subst)).collect();
        self.tuple(new_elems)
      },

      Type::Function {
        params,
        ret,
        is_variadic,
      } => {
        let new_params: Vec<_> = params.iter().map(|p| self.substitute(*p, subst)).collect();
        let new_ret = self.substitute(ret, subst);
        self.function(new_params, new_ret, is_variadic)
      },

      // Nominal types (Record, Enum) and primitives don't need substitution
      _ => ty,
    }
  }

  /// Unify a pattern type against a concrete type for type inference.
  ///
  /// This walks both types in parallel, and when it finds a `Type::Param` in the
  /// pattern, it binds that parameter to the corresponding concrete type.
  /// Returns `true` if unification succeeded, `false` if the types are incompatible.
  ///
  /// Unlike full unification, this is asymmetric: only the pattern can contain
  /// type parameters, and they are bound to whatever is in the concrete type.
  pub fn unify_for_inference(
    &self,
    pattern: TypeId,
    concrete: TypeId,
    subst: &mut Substitution,
  ) -> bool {
    // Same type - trivially unifies
    if pattern == concrete {
      return true;
    }

    match (self.get(&pattern).clone(), self.get(&concrete).clone()) {
      // Type parameter: bind it to the concrete type
      (Type::Param { owner, index }, _) => {
        if let Some(existing) = subst.get(owner, index) {
          // Already bound - check consistency
          self.types_equal(&existing, &concrete)
        } else {
          // Bind the parameter
          subst.bind(owner, index, concrete);
          true
        }
      },

      // Instance types: must match generic def and unify args
      (Type::Instance { generic: g1, args: a1 }, Type::Instance { generic: g2, args: a2 }) => {
        if g1 != g2 || a1.len() != a2.len() {
          return false;
        }
        for (p, c) in a1.iter().zip(a2.iter()) {
          if !self.unify_for_inference(*p, *c, subst) {
            return false;
          }
        }
        true
      },

      // Pointer types
      (Type::Pointer { inner: p1, mutable: m1 }, Type::Pointer { inner: p2, mutable: m2 }) => {
        if m1 != m2 {
          return false;
        }
        self.unify_for_inference(p1, p2, subst)
      },

      // Reference types
      (Type::Reference { inner: i1, mutable: m1 }, Type::Reference { inner: i2, mutable: m2 }) => {
        if m1 != m2 {
          return false;
        }
        self.unify_for_inference(i1, i2, subst)
      },

      // Vector types
      (Type::Vector { element: e1, size: s1 }, Type::Vector { element: e2, size: s2 }) => {
        if s1 != s2 {
          return false;
        }
        self.unify_for_inference(e1, e2, subst)
      },

      // Tuple types
      (Type::Tuple(t1), Type::Tuple(t2)) => {
        if t1.len() != t2.len() {
          return false;
        }
        for (p, c) in t1.iter().zip(t2.iter()) {
          if !self.unify_for_inference(*p, *c, subst) {
            return false;
          }
        }
        true
      },

      // Function types
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
          return false;
        }
        for (p, c) in p1.iter().zip(p2.iter()) {
          if !self.unify_for_inference(*p, *c, subst) {
            return false;
          }
        }
        self.unify_for_inference(r1, r2, subst)
      },

      // Record/Enum: must match exactly (they're nominal)
      (Type::Record(d1), Type::Record(d2)) => d1 == d2,
      (Type::Enum(d1), Type::Enum(d2)) => d1 == d2,

      // Primitives: must match exactly
      _ => self.types_equal(&pattern, &concrete),
    }
  }
}

/// Mapping of type parameters to concrete types for monomorphization.
///
/// Type parameters are identified by their owner (the definition that declares them)
/// and their index in the type parameter list. This allows handling methods that
/// have both owner type parameters (from the containing record/enum) and their
/// own type parameters.
#[derive(Debug, Clone, Default)]
pub struct Substitution {
  bindings: HashMap<(DefinitionId, u32), TypeId>,
}

impl Substitution {
  pub fn new() -> Self {
    Self {
      bindings: HashMap::new(),
    }
  }

  /// Bind a type parameter to a concrete type.
  pub fn bind(
    &mut self,
    owner: DefinitionId,
    index: u32,
    ty: TypeId,
  ) {
    self.bindings.insert((owner, index), ty);
  }

  /// Get the concrete type bound to a type parameter, if any.
  pub fn get(
    &self,
    owner: DefinitionId,
    index: u32,
  ) -> Option<TypeId> {
    let result = self.bindings.get(&(owner, index)).copied();
    if std::env::var("IGNIS_VERBOSE").is_ok() && result.is_none() && !self.bindings.is_empty() {
      eprintln!(
        "[SUBST] get({:?}, {}) returned None. bindings keys: {:?}",
        owner,
        index,
        self.bindings.keys().collect::<Vec<_>>()
      );
    }
    result
  }

  /// Create a substitution for a generic function, record, or enum.
  ///
  /// Maps each type parameter (by index) to the corresponding concrete type argument.
  pub fn for_generic(
    owner: DefinitionId,
    args: &[TypeId],
  ) -> Self {
    let mut s = Self::new();
    for (i, ty) in args.iter().enumerate() {
      s.bind(owner, i as u32, *ty);
    }
    s
  }

  /// Create a substitution for a method call on a generic type.
  ///
  /// This binds both the owner's type parameters (from the record/enum)
  /// and the method's own type parameters.
  pub fn for_method(
    owner_def: DefinitionId,
    owner_args: &[TypeId],
    method_def: DefinitionId,
    method_args: &[TypeId],
  ) -> Self {
    let mut s = Self::new();
    for (i, ty) in owner_args.iter().enumerate() {
      s.bind(owner_def, i as u32, *ty);
    }
    for (i, ty) in method_args.iter().enumerate() {
      s.bind(method_def, i as u32, *ty);
    }
    s
  }

  /// Check if this substitution is empty (no bindings).
  pub fn is_empty(&self) -> bool {
    self.bindings.is_empty()
  }
}

/// Format a `TypeId` as its human-readable Ignis name (e.g. `i32`, `*mut i32`,
/// `record Point`).  Used by `@typeName<T>()` and the HIR pretty-printer.
pub fn format_type_name(
  type_id: &TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  let ty = types.get(type_id);

  match ty {
    Type::I8 => "i8".to_string(),
    Type::I16 => "i16".to_string(),
    Type::I32 => "i32".to_string(),
    Type::I64 => "i64".to_string(),
    Type::U8 => "u8".to_string(),
    Type::U16 => "u16".to_string(),
    Type::U32 => "u32".to_string(),
    Type::U64 => "u64".to_string(),
    Type::F32 => "f32".to_string(),
    Type::F64 => "f64".to_string(),
    Type::Boolean => "bool".to_string(),
    Type::Char => "char".to_string(),
    Type::String => "string".to_string(),
    Type::Void => "void".to_string(),
    Type::Never => "never".to_string(),
    Type::Infer => "infer".to_string(),
    Type::NullPtr => "null".to_string(),
    Type::Error => "error".to_string(),

    Type::Pointer { inner, mutable } => {
      if *mutable {
        format!("*mut {}", format_type_name(inner, types, defs, symbols))
      } else {
        format!("*{}", format_type_name(inner, types, defs, symbols))
      }
    },

    Type::Reference { inner, mutable } => {
      if *mutable {
        format!("&mut {}", format_type_name(inner, types, defs, symbols))
      } else {
        format!("&{}", format_type_name(inner, types, defs, symbols))
      }
    },

    Type::Vector { element, size } => {
      if let Some(s) = size {
        format!("[{}; {}]", format_type_name(element, types, defs, symbols), s)
      } else {
        format!("[{}]", format_type_name(element, types, defs, symbols))
      }
    },

    Type::Tuple(elements) => {
      let elem_strs: Vec<_> = elements
        .iter()
        .map(|e| format_type_name(e, types, defs, symbols))
        .collect();
      format!("({})", elem_strs.join(", "))
    },

    Type::Function {
      params,
      ret,
      is_variadic,
    } => {
      let param_strs: Vec<_> = params
        .iter()
        .map(|p| format_type_name(p, types, defs, symbols))
        .collect();
      let variadic = if *is_variadic { ", ..." } else { "" };
      format!(
        "fn({}{}) -> {}",
        param_strs.join(", "),
        variadic,
        format_type_name(ret, types, defs, symbols)
      )
    },

    Type::Record(def_id) => {
      let name = symbols.get(&defs.get(def_id).name);
      format!("record {}", name)
    },

    Type::Enum(def_id) => {
      let name = symbols.get(&defs.get(def_id).name);
      format!("enum {}", name)
    },

    Type::Param { owner, index } => {
      let owner_def = defs.get(owner);
      let type_params = match &owner_def.kind {
        DefinitionKind::Function(fd) => &fd.type_params,
        DefinitionKind::Record(rd) => &rd.type_params,
        DefinitionKind::Method(md) => &md.type_params,
        DefinitionKind::Enum(ed) => &ed.type_params,
        _ => return format!("T{}", index),
      };

      if let Some(param_def_id) = type_params.get(*index as usize) {
        let param_name = symbols.get(&defs.get(param_def_id).name);
        param_name.to_string()
      } else {
        format!("T{}", index)
      }
    },

    Type::Instance { generic, args } => {
      let name = symbols.get(&defs.get(generic).name);
      let arg_strs: Vec<_> = args.iter().map(|a| format_type_name(a, types, defs, symbols)).collect();
      format!("{}<{}>", name, arg_strs.join(", "))
    },
  }
}
