use ignis_type::span::Span;

use crate::{BlockId, Instr, Operand};

/// A basic block: a sequence of instructions ending with a terminator.
#[derive(Debug, Clone)]
pub struct Block {
  /// Unique label for this block (for debugging).
  pub label: String,
  /// Instructions in this block (executed sequentially).
  pub instructions: Vec<Instr>,
  /// How this block exits (branch, return, etc.).
  pub terminator: Terminator,
  /// Source span covering this block.
  pub span: Span,
}

impl Block {
  pub fn new(label: String) -> Self {
    Self {
      label,
      instructions: Vec::new(),
      terminator: Terminator::Unreachable,
      span: Span::default(),
    }
  }
}

/// Block terminator: how control exits a basic block.
#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
  /// Unconditional jump to a target block.
  Goto(BlockId),

  /// Conditional branch: if condition is true, go to then_block, else else_block.
  Branch {
    condition: Operand,
    then_block: BlockId,
    else_block: BlockId,
  },

  /// Return from function with optional value.
  Return(Option<Operand>),

  /// Unreachable code (after diverging expressions or unset).
  Unreachable,
}
