use std::collections::HashMap;

use crate::{Id, Store};

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
  Unknown,

  Pointer(TypeId),
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

  // TODO: Implement Inferenced(InferenceVariable)
  // Inferenced(InferenceVariable),
  Error,
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

#[derive(Debug, Clone)]
pub struct TypeStore {
  types: Store<Type>,
  primitives: HashMap<Type, TypeId>,
  pointers: HashMap<TypeId, TypeId>,
  references: HashMap<ReferenceKey, TypeId>,
  vectors: HashMap<VectorKey, TypeId>,
  tuples: HashMap<Vec<TypeId>, TypeId>,
  functions: HashMap<FunctionKey, TypeId>,
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
    };
    store.init_primitives();
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
      Type::Unknown,
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
  ) -> TypeId {
    if let Some(&id) = self.pointers.get(&inner) {
      return id;
    }
    let id = self.types.alloc(Type::Pointer(inner));
    self.pointers.insert(inner, id);
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
  pub fn unknown(&self) -> TypeId {
    self.primitives[&Type::Unknown]
  }
  #[inline]
  pub fn error(&self) -> TypeId {
    self.primitives[&Type::Error]
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

  pub fn types_equal(
    &self,
    a: &TypeId,
    b: &TypeId,
  ) -> bool {
    if a == b {
      return true;
    }

    if self.is_error(&a) || self.is_error(&b) {
      return true;
    }

    if self.is_unknown(&a) || self.is_unknown(&b) {
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
  pub fn is_unknown(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.get(ty), Type::Unknown)
  }
}
