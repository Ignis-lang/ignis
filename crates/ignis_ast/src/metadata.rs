use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct ASTMetadata: u32 {
        const NONE              = 0;
        const CONSTANT          = 1 << 0;
        const CONSTRUCTOR       = 1 << 1;
        const DECLARATION       = 1 << 2;
        const ENUM_MEMBER       = 1 << 3;
        const EXPLICIT_REFERENCE= 1 << 4;
        const EXPORT            = 1 << 5;
        const EXTERN_MEMBER     = 1 << 6;
        const METHOD            = 1 << 8;
        const MUTABLE           = 1 << 9;
        const NAMESPACE_MEMBER  = 1 << 10;
        const OBJECT_MEMBER     = 1 << 11;
        const OPTIONAL          = 1 << 12;
        const PARAMETER         = 1 << 13;
        const POINTER           = 1 << 14;
        const PRIVATE           = 1 << 15;
        const PROPERTY          = 1 << 16;
        const PUBLIC            = 1 << 17;
        const REFERENCE         = 1 << 18;
        const SPREAD            = 1 << 19;
        const STATIC            = 1 << 20;
        const VARIABLE          = 1 << 21;
        const VARIADIC          = 1 << 22;
    }
}

#[macro_export]
macro_rules! set_flag {
  ($metadata:expr, $flag:expr) => {
    *$metadata |= $flag.bits();
  };
}

#[macro_export]
macro_rules! check_flag {
  ($metadata:expr, $flag:expr) => {
    $metadata & $flag.bits() == $flag.bits()
  };
}

#[macro_export]
macro_rules! remove_flag {
  ($metadata:expr, $flag:expr) => {
    *$metadata &= !$flag.bits();
  };
}

impl ASTMetadata {
  pub fn is_public(&self) -> bool {
    self.contains(ASTMetadata::PUBLIC)
  }

  pub fn is_private(&self) -> bool {
    self.contains(ASTMetadata::PRIVATE)
  }

  pub fn is_mutable(&self) -> bool {
    self.contains(ASTMetadata::MUTABLE)
  }

  pub fn is_static(&self) -> bool {
    self.contains(ASTMetadata::STATIC)
  }
}
