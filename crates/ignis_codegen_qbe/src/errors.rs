use std::fmt;

#[derive(Debug, Clone)]
pub enum QbeError {
  UnsupportedInstruction {
    function: String,
    block: String,
    instruction_index: usize,
    instruction: String,
  },
  UnsupportedType {
    context: String,
    type_repr: String,
  },
  UnsupportedOperand {
    context: String,
    operand: String,
  },
}

impl QbeError {
  pub fn unsupported_instruction(
    function: String,
    block: String,
    instruction_index: usize,
    instruction: String,
  ) -> Self {
    Self::UnsupportedInstruction {
      function,
      block,
      instruction_index,
      instruction,
    }
  }

  pub fn unsupported_type(
    context: impl Into<String>,
    type_repr: impl Into<String>,
  ) -> Self {
    Self::UnsupportedType {
      context: context.into(),
      type_repr: type_repr.into(),
    }
  }

  pub fn unsupported_operand(
    context: impl Into<String>,
    operand: impl Into<String>,
  ) -> Self {
    Self::UnsupportedOperand {
      context: context.into(),
      operand: operand.into(),
    }
  }
}

impl fmt::Display for QbeError {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      QbeError::UnsupportedInstruction {
        function,
        block,
        instruction_index,
        instruction,
      } => {
        write!(
          f,
          "QBE0001 Unsupported LIR instruction: {} (function={}, block={}, instr={})",
          instruction,
          function,
          block,
          instruction_index
        )
      },
      QbeError::UnsupportedType { context, type_repr } => {
        write!(f, "QBE0002 Unsupported type in {}: {}", context, type_repr)
      },
      QbeError::UnsupportedOperand { context, operand } => {
        write!(f, "QBE0003 Unsupported operand in {}: {}", context, operand)
      },
    }
  }
}

impl std::error::Error for QbeError {}
