use core::fmt;
use std::fmt::{Display, Formatter};

use ignis_data_type::{value::IgnisLiteralValue, DataType};
use ignis_hir::HIRInstructionType;

use crate::backend::ir::ir_flags::IRFlag;

use super::{ir::IRInstruction, ir_flags::IRFlags};

#[derive(Debug, Clone)]
pub enum IROperationValue {
  Register {
    name: String,
    type_: DataType,
    flags: IRFlags,
  },
  Constant {
    value: IgnisLiteralValue,
    type_: DataType,
    flags: IRFlags,
  },
  Vector {
    type_: DataType,
    values: Vec<IROperationValue>,
    length: usize,
  },
  Arguments {
    values: Vec<IROperationValue>,
  },
  FieldAccess {
    base: String,
    field: String,
    type_: DataType,
    flags: IRFlags,
    base_type: DataType,
  },
  Label(String),
  Function {
    name: String,
    parameters: Vec<DataType>,
    return_type: DataType,
    flags: IRFlags,
  },
  Field {
    name: String,
    type_: DataType,
    value: Box<IROperationValue>,
  },
  ObjectLiteral {
    type_: DataType,
    properties: Vec<(String, IROperationValue)>,
    flags: IRFlags,
  },
  None,
}

impl IROperationValue {
  pub fn get_name(&self) -> String {
    match self {
      IROperationValue::Register { name, .. } => name.clone(),
      IROperationValue::FieldAccess { field, .. } => field.clone(),
      _ => String::new(),
    }
  }

  pub fn get_base(&self) -> String {
    match self {
      IROperationValue::FieldAccess { base, .. } => base.clone(),
      _ => String::new(),
    }
  }

  pub fn get_flags(&self) -> IRFlags {
    match self {
      IROperationValue::Register { flags, .. } => flags.to_vec(),
      IROperationValue::Constant { flags, .. } => flags.to_vec(),
      IROperationValue::FieldAccess { flags, .. } => flags.to_vec(),
      _ => vec![],
    }
  }

  pub fn get_type(&self) -> DataType {
    match self {
      IROperationValue::Register { type_, .. } => type_.clone(),
      IROperationValue::Constant { type_, .. } => type_.clone(),
      IROperationValue::Vector { type_, .. } => type_.clone(),
      IROperationValue::FieldAccess { type_, .. } => type_.clone(),
      IROperationValue::Function { return_type, .. } => return_type.clone(),
      _ => DataType::Void,
    }
  }
}

impl Display for IROperationValue {
  fn fmt(
    &self,
    f: &mut Formatter,
  ) -> fmt::Result {
    match self {
      IROperationValue::Register { name, flags, .. } => {
        let mut modified = String::new();

        if flags.contains(&IRFlag::ExplicitReference) {
          modified.push('&');
        }

        if flags.contains(&IRFlag::Mutable) {
          modified.push_str("mut ");
        }

        write!(f, "{}{}", modified, name)
      },
      IROperationValue::Constant { value, .. } => {
        write!(f, "{}", value)
      },
      IROperationValue::Label(name) => {
        write!(f, "{}", name)
      },
      IROperationValue::Vector { values, .. } => {
        let values = values
          .iter()
          .map(|value| format!("{}", value))
          .collect::<Vec<String>>()
          .join(", ");

        write!(f, "[{}]", values)
      },
      IROperationValue::Arguments { values } => {
        let values = values
          .iter()
          .map(|value| format!("{}", value))
          .collect::<Vec<String>>()
          .join(", ");

        write!(f, "{}", values)
      },
      IROperationValue::None => {
        write!(f, "")
      },
      IROperationValue::FieldAccess { base, field, .. } => {
        write!(f, "{}->{}", base, field)
      },
      IROperationValue::Function { name, .. } => {
        write!(f, "*{}", name)
      },
      IROperationValue::ObjectLiteral { properties, .. } => {
        let properties = properties
          .iter()
          .map(|(name, value)| format!("{}: {}", name, value))
          .collect::<Vec<String>>()
          .join(", ");

        write!(f, "{{ {} }}", properties)
      },
      IROperationValue::Field { name, value, .. } => {
        write!(f, "{}: {}", name, value)
      },
    }
  }
}

impl Into<DataType> for IROperationValue {
  fn into(self) -> DataType {
    match self {
      IROperationValue::Register { type_, .. } => type_,
      IROperationValue::Constant { type_, .. } => type_,
      IROperationValue::Vector { type_, .. } => type_,
      IROperationValue::Arguments { .. } => DataType::Void,
      IROperationValue::FieldAccess { type_, .. } => type_,
      IROperationValue::Label(_) => DataType::Void,
      IROperationValue::None => DataType::Void,
      IROperationValue::Function {
        parameters,
        return_type,
        ..
      } => DataType::Function(parameters.into_iter().map(|x| x.into()).collect(), return_type.into()),
      IROperationValue::ObjectLiteral { type_, .. } => type_,
      IROperationValue::Field { type_, .. } => type_,
    }
  }
}

#[derive(Debug, Clone)]
pub enum IROperation {
  Constant,
  Add,
  Subtract,
  Multiply,
  Divide,
  Return,
  Assign,
  Declare,
  Call,
  Parameter,
  GreaterEqual,
  Greater,
  LessEqual,
  Less,
  Equal,
  NotEqual,
  And,
  Or,
  Not,
  AssignAdd,
  AssignSub,
  Mod,
  Concatenate,
  Increment,
  Decrement,
  Label,
  If,
  Goto,
  Allocate,
  VectorAccess,
  StartBlock,
  EndBlock,
  Cast,
  This,
  Callable,
  MethodCall,
}

impl From<&HIRInstructionType> for IROperation {
  fn from(instruction_type: &HIRInstructionType) -> Self {
    match instruction_type {
      HIRInstructionType::Add => IROperation::Add,
      HIRInstructionType::Sub => IROperation::Subtract,
      HIRInstructionType::Mul => IROperation::Multiply,
      HIRInstructionType::Div => IROperation::Divide,
      HIRInstructionType::Equal => IROperation::Equal,
      HIRInstructionType::Assign => IROperation::Assign,
      HIRInstructionType::GreaterEqual => IROperation::GreaterEqual,
      HIRInstructionType::Greater => IROperation::Greater,
      HIRInstructionType::LessEqual => IROperation::LessEqual,
      HIRInstructionType::Less => IROperation::Less,
      HIRInstructionType::NotEqual => IROperation::NotEqual,
      HIRInstructionType::And => IROperation::And,
      HIRInstructionType::Or => IROperation::Or,
      HIRInstructionType::Not => IROperation::Not,
      HIRInstructionType::AssignAdd => IROperation::AssignAdd,
      HIRInstructionType::AssignSub => IROperation::AssignSub,
      HIRInstructionType::Mod => IROperation::Mod,
      HIRInstructionType::Concatenate => IROperation::Concatenate,
      HIRInstructionType::Increment => IROperation::Increment,
      HIRInstructionType::Decrement => IROperation::Decrement,
    }
  }
}
