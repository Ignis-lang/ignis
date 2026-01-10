use std::collections::HashMap;

use ignis_type::{Store, definition::DefinitionId, span::Span, types::TypeId};

use crate::{Block, BlockId, LocalId, TempId};

/// Top-level LIR program containing all functions.
#[derive(Debug, Clone)]
pub struct LirProgram {
  /// All functions in the program, keyed by their definition ID.
  pub functions: HashMap<DefinitionId, FunctionLir>,
  /// Global constant initializers (for module-level constants).
  pub global_inits: HashMap<DefinitionId, crate::Operand>,
  /// Entry point function (main).
  pub entry_point: Option<DefinitionId>,
}

impl LirProgram {
  pub fn new() -> Self {
    Self {
      functions: HashMap::new(),
      global_inits: HashMap::new(),
      entry_point: None,
    }
  }
}

impl Default for LirProgram {
  fn default() -> Self {
    Self::new()
  }
}

/// A single function in LIR form.
#[derive(Debug, Clone)]
pub struct FunctionLir {
  /// The definition ID from the analyzer.
  pub def_id: DefinitionId,
  /// Parameter definitions (in order).
  pub params: Vec<DefinitionId>,
  /// Return type.
  pub return_type: TypeId,
  /// Local variable slots (stack-allocated).
  pub locals: Store<LocalData>,
  /// Temporary values (SSA-like).
  pub temps: Store<TempData>,
  /// Basic blocks forming the CFG.
  pub blocks: Store<Block>,
  /// Entry block ID.
  pub entry_block: BlockId,
  /// Whether this is an extern declaration (no body).
  pub is_extern: bool,
  /// Whether this function is variadic.
  pub is_variadic: bool,
  /// Source span for error reporting.
  pub span: Span,
}

/// Metadata for a local variable slot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalData {
  /// Original definition from HIR (for debugging/mapping).
  pub def_id: Option<DefinitionId>,
  /// Type of the local slot.
  pub ty: TypeId,
  /// Whether this local is mutable.
  pub mutable: bool,
  /// Debug name (for pretty printing).
  pub name: Option<String>,
}

/// Metadata for a temporary value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TempData {
  /// Type of the temporary value.
  pub ty: TypeId,
  /// Source span (for error reporting).
  pub span: Span,
}

impl FunctionLir {
  pub fn local_type(
    &self,
    local: LocalId,
  ) -> TypeId {
    self.locals.get(&local).ty
  }

  pub fn temp_type(
    &self,
    temp: TempId,
  ) -> TypeId {
    self.temps.get(&temp).ty
  }
}
