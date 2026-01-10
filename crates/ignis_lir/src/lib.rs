pub mod block;
pub mod display;
pub mod instr;
pub mod lowering;
pub mod operand;
pub mod program;
pub mod verify;

use ignis_type::Id;

pub use block::{Block, Terminator};
pub use instr::Instr;
pub use operand::{ConstValue, Operand};
pub use program::{FunctionLir, LirProgram, LocalData, TempData};
pub use verify::{VerifyError, verify_lir};

/// Unique identifier for a temporary value within a function.
pub type TempId = Id<TempData>;

/// Unique identifier for a local variable slot within a function.
pub type LocalId = Id<LocalData>;

/// Unique identifier for a basic block within a function.
pub type BlockId = Id<Block>;
