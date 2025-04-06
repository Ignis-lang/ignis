use ignis_hir::HIRMetadataFlags;

#[derive(Debug, Clone, PartialEq)]
pub enum IRFlag {
  Abstract,
  Branch,
  Call,
  Condition,
  Constructor,
  Declaration,
  Enum,
  ExplicitReference,
  Exported,
  Extern,
  Field,
  Function,
  Imported,
  Intersection,
  Iterator,
  Jump,
  Label,
  Vector,
  Method,
  Mutable,
  Negate,
  NoTranspile,
  None,
  Parameter,
  Pointer,
  Public,
  RECURSIVE,
  Reference,
  Return,
  ReturnValue,
  Static,
  Std,
  Temporary,
  This,
  Union,
  Recursive,
  Moved,
  Optional,
  Property,
  Spread,
  Variable,
  Variadic,
  NamespaceMember,
  ExternMember,
  ObjectMember,
  Private,
  EnumMember,
  ComplexType,
}

pub type IRFlags = Vec<IRFlag>;

impl From<&HIRMetadataFlags> for IRFlag {
  fn from(flags: &HIRMetadataFlags) -> Self {
    match flags {
      HIRMetadataFlags::Parameter => IRFlag::Parameter,
      HIRMetadataFlags::Calleable => IRFlag::Call,
      HIRMetadataFlags::Abstract => IRFlag::Abstract,
      HIRMetadataFlags::Constructor => IRFlag::Constructor,
      HIRMetadataFlags::Function => IRFlag::Function,
      HIRMetadataFlags::Recursive => IRFlag::Recursive,
      HIRMetadataFlags::Exported => IRFlag::Exported,
      HIRMetadataFlags::Imported => IRFlag::Imported,
      HIRMetadataFlags::Extern => IRFlag::Extern,
      HIRMetadataFlags::Static => IRFlag::Static,
      HIRMetadataFlags::Public => IRFlag::Public,
      HIRMetadataFlags::Method => IRFlag::Method,
      HIRMetadataFlags::Declaration => IRFlag::Declaration,
      HIRMetadataFlags::EnumMember => IRFlag::EnumMember,
      HIRMetadataFlags::ExplicitReference => IRFlag::ExplicitReference,
      HIRMetadataFlags::Meta(_) => todo!(),
      HIRMetadataFlags::Moved => IRFlag::Moved,
      HIRMetadataFlags::Mutable => IRFlag::Mutable,
      HIRMetadataFlags::NamespaceMember => IRFlag::NamespaceMember,
      HIRMetadataFlags::ExternMember => IRFlag::ExternMember,
      HIRMetadataFlags::None => IRFlag::None,
      HIRMetadataFlags::ObjectMember => IRFlag::ObjectMember,
      HIRMetadataFlags::Optional => IRFlag::Optional,
      HIRMetadataFlags::Pointer => IRFlag::Pointer,
      HIRMetadataFlags::Private => IRFlag::Private,
      HIRMetadataFlags::Property => IRFlag::Property,
      HIRMetadataFlags::Reference => IRFlag::Reference,
      HIRMetadataFlags::Spread => IRFlag::Spread,
      HIRMetadataFlags::Variable => IRFlag::Variable,
      HIRMetadataFlags::Variadic => IRFlag::Variadic,
      HIRMetadataFlags::Complex => IRFlag::ComplexType,
    }
  }
}
