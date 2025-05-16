use ignis_data_type::DataType;

use super::{
  ir_flags::IRFlags,
  ir_operation::{IROperation, IROperationValue},
};

#[derive(Debug, Clone)]
pub enum IRProgramInstruction {
  Function(IRFunction),
  Global(IRInstruction),
  Import(IRImport),
  Struct(IRStruct),
  Method(IRFunction),
  Type(IRTypeDefinition),
  Enum(IREnum),
}

pub struct IRNamespace {
  pub name: String,
  pub members: Vec<IRProgramInstruction>,
  pub flags: IRFlags,
}

#[derive(Debug, Clone)]
pub struct IRInstruction {
  pub op: IROperation,
  pub dest: String,
  pub type_: DataType,
  pub left: IROperationValue,
  pub right: IROperationValue,
  pub flags: IRFlags,
  // pub ffi_data: Vec<IgnisFFIOptions>,
}

#[derive(Debug, Clone)]
pub struct IRFunction {
  pub name: String,
  pub body: Vec<IRInstruction>,
  pub return_type: DataType,
  pub flags: IRFlags,
  pub parameters: Vec<IRInstruction>,
}

#[derive(Debug, Clone)]
pub struct IRStruct {
  pub name: String,
  pub fields: Vec<IRInstruction>,
  pub flags: IRFlags,
  pub generics: Vec<DataType>,
}

#[derive(Debug, Clone)]
pub struct IRImport {
  pub modules: Vec<(String, Option<String>)>,
  pub path: String,
  pub flags: IRFlags,
  pub extra_path: Option<String>,
}

#[derive(Debug, Clone)]
pub struct IRTypeDefinition {
  pub name: String,
  pub type_: DataType,
  pub flags: IRFlags,
}

#[derive(Debug, Clone)]
pub enum IREnumValue {
  Simple {
    name: String,
    discriminant: i32,
  },
  Complex {
    name: String,
    discriminant: i32,
    payload: Vec<DataType>,
  },
}

#[derive(Debug, Clone)]
pub struct IREnum {
  pub name: String,
  pub values: Vec<IREnumValue>,
  pub flags: IRFlags,
}
