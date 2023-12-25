pub mod assign;
pub mod binary;
pub mod block;
pub mod call;
pub mod class;
pub mod function;
pub mod import;
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
