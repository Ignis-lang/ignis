pub mod analyzer_value;
pub mod assign;
pub mod binary;
pub mod block;
pub mod call;
pub mod class;
pub mod function;
pub mod import;
pub mod instruction_type;
pub mod ir_array;
pub mod ir_break;
pub mod ir_continue;
pub mod ir_for_in;
pub mod ir_if;
pub mod ir_println;
pub mod ir_return;
pub mod ir_while;
pub mod literal;
pub mod logical;
pub mod ternary;
pub mod unary;
pub mod variable;

use serde_json::json;

use self::{
  binary::IRBinary, block::IRBlock, literal::IRLiteral, unary::IRUnary, variable::IRVariable,
  logical::IRLogical, ir_if::IRIf, ir_while::IRWhile, function::IRFunction, call::IRCall,
  class::IRClass, assign::IRAssign, ir_return::IRReturn, ternary::IRTernary, ir_for_in::IRForIn,
  ir_array::IRArray, import::IRImport, ir_break::IRBreak, ir_continue::IRContinue,
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
  ForIn(IRForIn),
  Array(IRArray),
  Import(IRImport),
  Break(IRBreak),
  Continue(IRContinue),
}

impl IRInstruction {
  pub fn to_json(&self) -> serde_json::Value {
    match self {
      IRInstruction::Binary(binary) => binary.to_json(),
      IRInstruction::Block(block) => block.to_json(),
      IRInstruction::Literal(literal) => literal.to_json(),
      IRInstruction::Unary(unary) => unary.to_json(),
      IRInstruction::Variable(variable) => variable.to_json(),
      IRInstruction::Logical(logical) => logical.to_json(),
      IRInstruction::If(ir_if) => ir_if.to_json(),
      IRInstruction::While(ir_while) => ir_while.to_json(),
      IRInstruction::Function(function) => function.to_json(),
      IRInstruction::Call(call) => call.to_json(),
      IRInstruction::Return(re) => re.to_json(),
      IRInstruction::Assign(assign) => assign.to_json(),
      IRInstruction::Class(class) => class.to_json(),
      IRInstruction::Ternary(ternary) => ternary.to_json(),
      IRInstruction::ForIn(for_in) => for_in.to_json(),
      IRInstruction::Array(array) => array.to_json(),
      IRInstruction::Import(import) => import.to_json(),
      IRInstruction::Break(ir_break) => ir_break.to_json(),
      IRInstruction::Continue(cont) => cont.to_json(),
    }
  }
}
