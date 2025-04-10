use serde::Serialize;
use serde_json::{json, Value};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ASTMetadataFlags {
  Constructor,
  Declaration,
  EnumMember,
  ExplicitReference,
  Export,
  ExternMember,
  Inline,
  Meta(IgnisCompilerMeta),
  Method,
  Mutable,
  NamespaceMember,
  None,
  ObjectMember,
  Optional,
  Parameter,
  Pointer,
  Private,
  Property,
  Public,
  Reference,
  Spread,
  Static,
  Variable,
  Variadic,
}

impl std::fmt::Display for ASTMetadataFlags {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      ASTMetadataFlags::Constructor => write!(f, "Constructor"),
      ASTMetadataFlags::Declaration => write!(f, "Declaration"),
      ASTMetadataFlags::EnumMember => write!(f, "EnumMember"),
      ASTMetadataFlags::ExplicitReference => write!(f, "ExplicitReference"),
      ASTMetadataFlags::Export => write!(f, "Export"),
      ASTMetadataFlags::ExternMember => write!(f, "ExternMember"),
      ASTMetadataFlags::Inline => write!(f, "Inline"),
      ASTMetadataFlags::Meta(meta) => write!(f, "{:?}", meta),
      ASTMetadataFlags::Method => write!(f, "Method"),
      ASTMetadataFlags::Mutable => write!(f, "Mutable"),
      ASTMetadataFlags::NamespaceMember => write!(f, "NamespaceMember"),
      ASTMetadataFlags::None => write!(f, "None"),
      ASTMetadataFlags::ObjectMember => write!(f, "ObjectMember"),
      ASTMetadataFlags::Optional => write!(f, "Optional"),
      ASTMetadataFlags::Parameter => write!(f, "Parameter"),
      ASTMetadataFlags::Pointer => write!(f, "Pointer"),
      ASTMetadataFlags::Private => write!(f, "Private"),
      ASTMetadataFlags::Property => write!(f, "Property"),
      ASTMetadataFlags::Public => write!(f, "Public"),
      ASTMetadataFlags::Reference => write!(f, "Reference"),
      ASTMetadataFlags::Spread => write!(f, "Spread"),
      ASTMetadataFlags::Static => write!(f, "Static"),
      ASTMetadataFlags::Variable => write!(f, "Variable"),
      ASTMetadataFlags::Variadic => write!(f, "Variadic"),
    }
  }
}

#[derive(Debug, Clone, Default, PartialEq, Serialize)]
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

  pub fn is(
    &self,
    flag: ASTMetadataFlags,
  ) -> bool {
    self.flags.contains(&flag)
  }

  pub fn get(&self) -> Vec<ASTMetadataFlags> {
    self.flags.clone()
  }
}

#[derive(Debug, Clone, PartialEq, Serialize, Eq)]
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

impl std::fmt::Display for IgnisCompilerMeta {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      IgnisCompilerMeta::MutOnly => write!(f, "mut_only"),
      IgnisCompilerMeta::ToDo(value) => write!(f, "todo({})", value.as_ref().unwrap()),
      IgnisCompilerMeta::Ignore => write!(f, "ignore"),
      IgnisCompilerMeta::NotTranspile => write!(f, "not_transpile"),
      IgnisCompilerMeta::Feature(feature, value) => write!(f, "feature({}, {})", feature, value),
      IgnisCompilerMeta::Deprecated(feature, value) => write!(f, "deprecated({}, {})", feature, value),
      IgnisCompilerMeta::PlatformSpecific(feature, value) => write!(f, "platform_specific({}, {})", feature, value),
      IgnisCompilerMeta::Experimental(feature, value) => write!(f, "experimental({}, {})", feature, value),
      IgnisCompilerMeta::Internal(value) => write!(f, "internal({})", value),
      IgnisCompilerMeta::Global(value) => write!(f, "global({})", value),
      IgnisCompilerMeta::DisableLint => write!(f, "disable_lint"),
      IgnisCompilerMeta::Clone => write!(f, "clone"),
      IgnisCompilerMeta::Copy => write!(f, "copy"),
      IgnisCompilerMeta::FFILink(value) => write!(f, "ffi_link({})", value),
      IgnisCompilerMeta::EnableLint => write!(f, "enable_lint"),
      IgnisCompilerMeta::MainFunction => write!(f, "main_function"),
      IgnisCompilerMeta::Optimize(value) => write!(f, "optimize({})", value),
      IgnisCompilerMeta::Panic(value) => write!(f, "panic({})", value.as_ref().unwrap()),
    }
  }
}
