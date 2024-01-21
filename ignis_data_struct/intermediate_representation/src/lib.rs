pub mod analyzer_value;
pub mod assign;
pub mod binary;
pub mod block;
pub mod call;
pub mod class;
pub mod class_instance;
pub mod function;
pub mod import;
pub mod instruction_type;
pub mod ir_array;
pub mod ir_array_access;
pub mod ir_break;
pub mod ir_continue;
pub mod ir_for;
pub mod ir_for_in;
pub mod ir_get;
pub mod ir_if;
pub mod ir_method_call;
pub mod ir_return;
pub mod ir_set;
pub mod ir_while;
pub mod literal;
pub mod logical;
pub mod ternary;
pub mod unary;
pub mod variable;

use ir_array_access::IRArrayAccess;
use ir_for::IRFor;
use ir_method_call::IRMethodCall;
use serde_json::Value;

use self::{
  binary::IRBinary, block::IRBlock, literal::IRLiteral, unary::IRUnary, variable::IRVariable,
  logical::IRLogical, ir_if::IRIf, ir_while::IRWhile, function::IRFunction, call::IRCall,
  class::IRClass, assign::IRAssign, ir_return::IRReturn, ternary::IRTernary, ir_for_in::IRForIn,
  ir_array::IRArray, import::IRImport, ir_break::IRBreak, ir_continue::IRContinue, ir_get::IRGet,
  ir_set::IRSet, class_instance::IRClassInstance,
};

#[derive(Debug, Clone)]
pub enum IRInstruction {
  Binary(IRBinary),
  Block(IRBlock),
  Literal(IRLiteral),
  Unary(IRUnary),
  Variable(IRVariable),
  Logical(IRLogical),
  If(IRIf),
  While(IRWhile),
  Function(IRFunction),
  Call(IRCall),
  Return(IRReturn),
  Assign(IRAssign),
  Class(IRClass),
  Ternary(IRTernary),
  For(IRFor),
  ForIn(IRForIn),
  Array(IRArray),
  ArrayAccess(IRArrayAccess),
  Import(IRImport),
  Break(IRBreak),
  Continue(IRContinue),
  Get(IRGet),
  ClassInstance(IRClassInstance),
  Set(IRSet),
  MethodCall(IRMethodCall),
}

pub trait IRInstructionTrait {
  fn to_json(&self) -> Value;
}

impl IRInstructionTrait for IRInstruction {
  fn to_json(&self) -> Value {
    match self {
      IRInstruction::Binary(instruction) => instruction.to_json(),
      IRInstruction::Block(instruction) => instruction.to_json(),
      IRInstruction::Literal(instruction) => instruction.to_json(),
      IRInstruction::Unary(instruction) => instruction.to_json(),
      IRInstruction::Variable(instruction) => instruction.to_json(),
      IRInstruction::Logical(instruction) => instruction.to_json(),
      IRInstruction::If(instruction) => instruction.to_json(),
      IRInstruction::While(instruction) => instruction.to_json(),
      IRInstruction::Function(instruction) => instruction.to_json(),
      IRInstruction::Call(instruction) => instruction.to_json(),
      IRInstruction::Return(instruction) => instruction.to_json(),
      IRInstruction::Assign(instruction) => instruction.to_json(),
      IRInstruction::Class(instruction) => instruction.to_json(),
      IRInstruction::Get(instruction) => instruction.to_json(),
      IRInstruction::Ternary(instruction) => instruction.to_json(),
      IRInstruction::ForIn(instruction) => instruction.to_json(),
      IRInstruction::Array(instruction) => instruction.to_json(),
      IRInstruction::Import(instruction) => instruction.to_json(),
      IRInstruction::Break(instruction) => instruction.to_json(),
      IRInstruction::Continue(instruction) => instruction.to_json(),
      IRInstruction::ClassInstance(instruction) => instruction.to_json(),
      IRInstruction::Set(set) => set.to_json(),
      IRInstruction::For(_for) => _for.to_json(),
      IRInstruction::ArrayAccess(array) => array.to_json(),
      IRInstruction::MethodCall(call) => call.to_json(),
    }
  }
}
