use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ASTMetadataFlags {
  Constructor,
  EnumMember,
  Export,
  Method,
  Mutable,
  NamespaceMember,
  Optional,
  Parameter,
  Pointer,
  Private,
  Property,
  Public,
  Reference,
  Static,
  Variable,
  Variadic,
  Spread,
  Meta(IgnisCompilerMeta),
  None,
}

impl std::fmt::Display for ASTMetadataFlags {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      ASTMetadataFlags::Constructor => write!(f, "Constructor"),
      ASTMetadataFlags::EnumMember => write!(f, "EnumMember"),
      ASTMetadataFlags::Export => write!(f, "Export"),
      ASTMetadataFlags::Method => write!(f, "Method"),
      ASTMetadataFlags::Mutable => write!(f, "Mutable"),
      ASTMetadataFlags::NamespaceMember => write!(f, "NamespaceMember"),
      ASTMetadataFlags::Optional => write!(f, "Optional"),
      ASTMetadataFlags::Parameter => write!(f, "Parameter"),
      ASTMetadataFlags::Pointer => write!(f, "Pointer"),
      ASTMetadataFlags::Private => write!(f, "Private"),
      ASTMetadataFlags::Property => write!(f, "Property"),
      ASTMetadataFlags::Public => write!(f, "Public"),
      ASTMetadataFlags::Reference => write!(f, "Reference"),
      ASTMetadataFlags::Static => write!(f, "Static"),
      ASTMetadataFlags::Variable => write!(f, "Variable"),
      ASTMetadataFlags::Variadic => write!(f, "Variadic"),
      ASTMetadataFlags::Meta(meta) => write!(f, "{:?}", meta),
      ASTMetadataFlags::None => write!(f, "None"),
      ASTMetadataFlags::Spread => write!(f, "Spread"),
    }
  }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ASTMetadata {
  flags: Vec<ASTMetadataFlags>,
}

impl ASTMetadata {
  pub fn to_json(&self) -> Value {
    let mut values: Vec<Value> = vec![];

    for flag in self.flags.iter() {
      values.push(json!(flag.to_string()));
    }

    json!(values)
  }
}

impl std::fmt::Display for ASTMetadata {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self
        .flags
        .iter()
        .map(|flag| flag.to_string())
        .collect::<Vec<String>>()
        .join(", ")
    )
  }
}

impl ASTMetadata {
  pub fn new(flags: Vec<ASTMetadataFlags>) -> Self {
    Self { flags }
  }

  pub fn any(
    &self,
    flag: ASTMetadataFlags,
  ) -> bool {
    self.flags.contains(&flag)
  }

  pub fn push(
    &mut self,
    flag: ASTMetadataFlags,
  ) {
    if !self.flags.contains(&flag) {
      self.flags.push(flag);
    }
  }

  pub fn push_all(
    &mut self,
    flags: Vec<ASTMetadataFlags>,
  ) {
    for flag in flags {
      self.push(flag);
    }
  }

  pub fn remove(
    &mut self,
    flag: ASTMetadataFlags,
  ) {
    self.flags.retain(|f| f != &flag);
  }

  pub fn reset(&mut self) {
    self.flags.clear();
  }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IgnisCompilerMeta {
  MutOnly,
  ToDo(Option<String>),
  Ignore,
  NotTranspile,
  Feature(String, String),
  Deprecated(String, String),
  PlatformSpecific(String, String),
  Experimental(String, String),
  Internal(String),
  Global(String),
  DisableLint,
  EnableLint,
  MainFunction,
  Optimize(u8),
  Panic(Option<String>),
  Copy,
  Clone,
  FFILink(String),
}
