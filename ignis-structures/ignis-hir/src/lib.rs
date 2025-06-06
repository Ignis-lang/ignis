pub mod hir_assign;
pub mod hir_binary;
pub mod hir_block;
pub mod hir_call;
pub mod hir_cast;
pub mod hir_comment;
pub mod hir_const;
pub mod hir_enum;
pub mod hir_extern;
pub mod hir_for;
pub mod hir_for_of;
pub mod hir_function;
pub mod hir_function_instance;
pub mod hir_get;
pub mod hir_grouping;
pub mod hir_if;
pub mod hir_import;
pub mod hir_literal;
pub mod hir_logical;
pub mod hir_member_access;
pub mod hir_meta;
pub mod hir_method;
pub mod hir_method_call;
pub mod hir_namespace;
pub mod hir_object;
pub mod hir_record;
pub mod hir_return;
pub mod hir_set;
pub mod hir_spread;
pub mod hir_ternary;
pub mod hir_this;
pub mod hir_type;
pub mod hir_unary;
pub mod hir_value;
pub mod hir_variable;
pub mod hir_vector;
pub mod hir_vector_access;
pub mod hir_while;

use std::fmt::{self, Display, Formatter};

use hir_assign::HIRAssign;
use hir_block::HIRBlock;
use hir_call::HIRCall;
use hir_cast::HIRCast;
use hir_comment::HIRComment;
use hir_const::HIRConstant;
use hir_enum::HIREnum;
use hir_extern::HIRExtern;
use hir_for::HIRFor;
use hir_for_of::HIRForOf;
use hir_function::HIRFunction;
use hir_function_instance::HIRFunctionInstance;
use hir_grouping::HIRGrouping;
use hir_if::HIRIf;
use hir_import::HIRImport;
use hir_literal::HIRLiteral;
use hir_binary::HIRBinary;
use hir_logical::HIRLogical;
use hir_member_access::HIRMemberAccess;
use hir_meta::HIRMeta;
use hir_method::HIRMethod;
use hir_method_call::HIRMethodCall;
use hir_namespace::HIRNamespace;
use hir_object::HIRObjectLiteral;
use hir_record::HIRRecord;
use hir_return::HIRReturn;
use hir_spread::HIRSpread;
use hir_ternary::HIRTernary;
use hir_this::HIRThis;
use hir_type::HIRType;
use hir_variable::HIRVariable;
use hir_vector::HIRVector;
use hir_vector_access::HIRVectorAccess;
use hir_while::HIRWhile;
use ignis_ast::metadata::{ASTMetadataFlags, IgnisCompilerMeta};
use ignis_data_type::DataType;
use ignis_token::{token::Token, token_types::TokenType};
use serde::Serialize;
use hir_unary::HIRUnary;

#[derive(Debug, Clone, Serialize)]
pub enum HIRInstructionType {
  Add,
  Sub,
  Mul,
  Div,
  GreaterEqual,
  Greater,
  LessEqual,
  Less,
  Equal,
  NotEqual,
  And,
  Or,
  Not,
  Assign,
  AssignAdd,
  AssignSub,
  Mod,
  Concatenate,
  Increment,
  Decrement,
}

impl HIRInstructionType {
  pub fn from_token_kind(kind: &TokenType) -> Self {
    match kind {
      TokenType::Plus => HIRInstructionType::Add,
      TokenType::Minus => HIRInstructionType::Sub,
      TokenType::Asterisk => HIRInstructionType::Mul,
      TokenType::Slash => HIRInstructionType::Div,
      TokenType::GreaterEqual => HIRInstructionType::GreaterEqual,
      TokenType::Greater => HIRInstructionType::Greater,
      TokenType::LessEqual => HIRInstructionType::LessEqual,
      TokenType::Less => HIRInstructionType::Less,
      TokenType::Mod => HIRInstructionType::Mod,
      TokenType::EqualEqual => HIRInstructionType::Equal,
      TokenType::BangEqual => HIRInstructionType::NotEqual,
      TokenType::And => HIRInstructionType::And,
      TokenType::Or => HIRInstructionType::Or,
      TokenType::Bang => HIRInstructionType::Not,
      TokenType::Equal => HIRInstructionType::Assign,
      TokenType::Increment => HIRInstructionType::Increment,
      TokenType::Decrement => HIRInstructionType::Decrement,
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, Clone, Serialize)]
pub enum HIRInstruction {
  Literal(HIRLiteral),
  Binary(HIRBinary),
  Logical(HIRLogical),
  Unary(HIRUnary),
  Grouping(HIRGrouping),
  Function(HIRFunction),
  Block(HIRBlock),
  Variable(HIRVariable),
  Ternary(HIRTernary),
  If(HIRIf),
  Constant(HIRConstant),
  Call(HIRCall),
  MethodCall(HIRMethodCall),
  FunctionInstance(HIRFunctionInstance),
  While(HIRWhile),
  Vector(HIRVector),
  VectorAccess(HIRVectorAccess),
  For(HIRFor),
  ForOf(HIRForOf),
  Cast(HIRCast),
  Comment(HIRComment),
  Record(HIRRecord),
  Method(HIRMethod),
  Object(HIRObjectLiteral),
  Return(HIRReturn),
  This(HIRThis),
  MemberAccess(HIRMemberAccess),
  Assign(HIRAssign),
  Extern(HIRExtern),
  Namespace(HIRNamespace),
  Spread(HIRSpread),
  Import(HIRImport),
  Meta(HIRMeta),
  Type(HIRType),
  Enum(HIREnum),
  Break(Token),
  Continue(Token),
}

impl HIRInstruction {
  pub fn get_metadata(&self) -> &HIRMetadata {
    match self {
      HIRInstruction::Variable(v) => &v.metadata,
      HIRInstruction::Function(f) => &f.metadata,
      HIRInstruction::Record(r) => &r.metadata,
      HIRInstruction::Method(m) => &m.metadata,
      HIRInstruction::Enum(e) => &e.metadata,
      _ => todo!("{self:#?}"),
    }
  }

  pub fn get_name(&self) -> &Token {
    match self {
      HIRInstruction::Variable(v) => &v.name,
      HIRInstruction::Function(f) => &f.name,
      HIRInstruction::Record(r) => &r.name,
      HIRInstruction::Method(m) => &m.name,
      HIRInstruction::Enum(e) => &e.name,
      HIRInstruction::This(t) => &t.token,
      HIRInstruction::Namespace(n) => &n.name,
      HIRInstruction::Extern(e) => &e.name,
      _ => todo!("{self:#?}"),
    }
  }

  pub fn extract_data_type(&self) -> DataType {
    match self {
      HIRInstruction::Literal(l) => l.value.clone().into(),
      HIRInstruction::Constant(c) => c.data_type.clone(),
      HIRInstruction::Variable(v) => v.data_type.clone(),
      HIRInstruction::Function(f) => f.return_type.clone(),
      HIRInstruction::Binary(b) => b.data_type.clone(),
      HIRInstruction::Unary(u) => u.data_type.clone(),
      HIRInstruction::Logical(_) => DataType::Boolean,
      HIRInstruction::Cast(cast) => cast.target_type.clone(),
      HIRInstruction::Call(c) => c.return_type.clone(),
      HIRInstruction::Return(r) => r.data_type.clone(),
      HIRInstruction::Vector(array) => array.data_type.clone(),
      HIRInstruction::Enum(e) => e.data_type.clone(),
      HIRInstruction::Record(r) => r.data_type.clone(),
      HIRInstruction::Object(object) => object.data_type.clone(),
      HIRInstruction::VectorAccess(array) => match &array.data_type {
        DataType::Vector(t, ..) => *t.clone(),
        _ => DataType::Unknown,
      },
      HIRInstruction::This(this) => this.data_type.clone(),
      HIRInstruction::Type(t) => t.value.as_ref().clone(),
      HIRInstruction::MemberAccess(member) => member.member.extract_data_type(),
      HIRInstruction::Enum(enum_) => enum_.data_type.clone(),
      HIRInstruction::MethodCall(call) => call.return_type.clone(),
      HIRInstruction::Extern(_) => DataType::Null,
      HIRInstruction::Namespace(_) => DataType::Null,
      _ => DataType::Unknown,
    }
  }

  pub fn as_literal(&self) -> Option<&HIRLiteral> {
    match self {
      HIRInstruction::Literal(literal) => Some(literal),
      _ => None,
    }
  }

  pub fn push_flag(
    &mut self,
    flag: HIRMetadataFlags,
  ) {
    match self {
      HIRInstruction::Function(function) => function.metadata.push(flag),
      HIRInstruction::Record(record) => record.metadata.push(flag),
      HIRInstruction::Method(method) => method.metadata.push(flag),
      HIRInstruction::Enum(enum_) => enum_.metadata.push(flag),
      HIRInstruction::Variable(variable) => variable.metadata.push(flag),
      HIRInstruction::Extern(extern_) => extern_.metadata.push(flag),
      HIRInstruction::Namespace(namespace) => namespace.metadata.push(flag),
      HIRInstruction::Type(type_) => type_.metadata.push(flag),
      HIRInstruction::MemberAccess(member_access) => member_access.metadata.push(flag),
      HIRInstruction::MethodCall(method_call) => method_call.metadata.push(flag),
      HIRInstruction::Constant(constant) => constant.metadata.push(flag),
      _ => todo!("{self:#?}"),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum HIRMetadataFlags {
  Abstract,
  Calleable,
  Constructor,
  Declaration,
  EnumMember,
  ExplicitReference,
  Exported,
  Extern,
  Function,
  Inline,
  Imported,
  Meta(IgnisCompilerMeta),
  Method,
  Moved,
  Mutable,
  NamespaceMember,
  ExternMember,
  None,
  ObjectMember,
  Optional,
  Parameter,
  Pointer,
  Private,
  Property,
  Public,
  Recursive,
  Reference,
  Spread,
  Static,
  Variable,
  Variadic,
  Complex,
}

impl Display for HIRMetadataFlags {
  fn fmt(
    &self,
    f: &mut Formatter,
  ) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        HIRMetadataFlags::Parameter => "parameter",
        HIRMetadataFlags::Function => "function",
        HIRMetadataFlags::Calleable => "callable",
        HIRMetadataFlags::Recursive => "recursive",
        HIRMetadataFlags::Exported => "exported",
        HIRMetadataFlags::Inline => "inline",
        HIRMetadataFlags::Imported => "imported",
        HIRMetadataFlags::Extern => "extern",
        HIRMetadataFlags::Static => "static",
        HIRMetadataFlags::Public => "public",
        HIRMetadataFlags::Constructor => "constructor",
        HIRMetadataFlags::Method => "method",
        HIRMetadataFlags::Reference => "reference",
        HIRMetadataFlags::Optional => "optional",
        HIRMetadataFlags::Private => "private",
        HIRMetadataFlags::Property => "property",
        HIRMetadataFlags::Mutable => "mutable",
        HIRMetadataFlags::Pointer => "pointer",
        HIRMetadataFlags::Meta(_) => "meta",
        HIRMetadataFlags::EnumMember => "enum_member",
        HIRMetadataFlags::NamespaceMember => "namespace_member",
        HIRMetadataFlags::ExternMember => "extern_member",
        HIRMetadataFlags::None => "none",
        HIRMetadataFlags::Variable => "variable",
        HIRMetadataFlags::Variadic => "variadic",
        HIRMetadataFlags::Spread => "spread",
        HIRMetadataFlags::Declaration => "declaration",
        HIRMetadataFlags::Moved => "moved",
        HIRMetadataFlags::ExplicitReference => "explicit_reference",
        HIRMetadataFlags::ObjectMember => "object_member",
        HIRMetadataFlags::Abstract => "abstract",
        HIRMetadataFlags::Complex => "complex",
      }
    )
  }
}

impl From<&ASTMetadataFlags> for HIRMetadataFlags {
  fn from(value: &ASTMetadataFlags) -> Self {
    match value {
      ASTMetadataFlags::Constructor => HIRMetadataFlags::Constructor,
      ASTMetadataFlags::Declaration => HIRMetadataFlags::Declaration,
      ASTMetadataFlags::EnumMember => HIRMetadataFlags::EnumMember,
      ASTMetadataFlags::ExplicitReference => HIRMetadataFlags::ExplicitReference,
      ASTMetadataFlags::Export => HIRMetadataFlags::Exported,
      ASTMetadataFlags::ExternMember => HIRMetadataFlags::ExternMember,
      ASTMetadataFlags::Inline => HIRMetadataFlags::Inline,
      ASTMetadataFlags::Meta(meta) => HIRMetadataFlags::Meta(meta.clone()),
      ASTMetadataFlags::Method => HIRMetadataFlags::Method,
      ASTMetadataFlags::Mutable => HIRMetadataFlags::Mutable,
      ASTMetadataFlags::NamespaceMember => HIRMetadataFlags::NamespaceMember,
      ASTMetadataFlags::None => HIRMetadataFlags::None,
      ASTMetadataFlags::ObjectMember => HIRMetadataFlags::ObjectMember,
      ASTMetadataFlags::Optional => HIRMetadataFlags::Optional,
      ASTMetadataFlags::Parameter => HIRMetadataFlags::Parameter,
      ASTMetadataFlags::Pointer => HIRMetadataFlags::Pointer,
      ASTMetadataFlags::Private => HIRMetadataFlags::Private,
      ASTMetadataFlags::Property => HIRMetadataFlags::Property,
      ASTMetadataFlags::Public => HIRMetadataFlags::Public,
      ASTMetadataFlags::Reference => HIRMetadataFlags::Reference,
      ASTMetadataFlags::Spread => HIRMetadataFlags::Spread,
      ASTMetadataFlags::Static => HIRMetadataFlags::Static,
      ASTMetadataFlags::Variable => HIRMetadataFlags::Variable,
      ASTMetadataFlags::Variadic => HIRMetadataFlags::Variadic,
    }
  }
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct HIRMetadata {
  pub flags: Vec<HIRMetadataFlags>,
  pub complex_type: Option<Box<HIRInstruction>>,
}

impl HIRMetadata {
  pub fn new(
    flags: Vec<HIRMetadataFlags>,
    complex_type: Option<Box<HIRInstruction>>,
  ) -> Self {
    Self { flags, complex_type }
  }

  pub fn is(
    &self,
    flag: HIRMetadataFlags,
  ) -> bool {
    self.flags.contains(&flag)
  }

  pub fn remove_flag(
    &mut self,
    flag: HIRMetadataFlags,
  ) {
    if let Some(index) = self.flags.iter().position(|f| f == &flag) {
      self.flags.remove(index);
    }
  }

  pub fn push(
    &mut self,
    flag: HIRMetadataFlags,
  ) {
    if !self.flags.contains(&flag) {
      self.flags.push(flag);
    }
  }

  pub fn push_all(
    &mut self,
    flags: Vec<HIRMetadataFlags>,
  ) {
    for flag in flags {
      self.push(flag);
    }
  }
}
