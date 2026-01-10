use ignis_type::{Store, definition::DefinitionId, span::Span, types::TypeId};

use crate::{Block, BlockId, FunctionLir, Instr, LocalData, LocalId, TempData, TempId, Terminator};

/// Builder for constructing a single function's LIR.
pub struct FunctionBuilder {
  def_id: DefinitionId,
  params: Vec<DefinitionId>,
  return_type: TypeId,
  is_extern: bool,
  is_variadic: bool,
  span: Span,

  locals: Store<LocalData>,
  temps: Store<TempData>,
  blocks: Store<Block>,

  /// Currently active block being built.
  current_block: Option<BlockId>,

  /// Counter for generating unique block labels.
  block_counter: u32,
}

impl FunctionBuilder {
  pub fn new(
    def_id: DefinitionId,
    params: Vec<DefinitionId>,
    return_type: TypeId,
    is_extern: bool,
    is_variadic: bool,
    span: Span,
  ) -> Self {
    let mut builder = Self {
      def_id,
      params,
      return_type,
      is_extern,
      is_variadic,
      span,
      locals: Store::new(),
      temps: Store::new(),
      blocks: Store::new(),
      current_block: None,
      block_counter: 0,
    };

    // Create entry block
    let entry = builder.create_block("entry");
    builder.switch_to_block(entry);

    builder
  }

  /// Create a new basic block with a label.
  pub fn create_block(
    &mut self,
    prefix: &str,
  ) -> BlockId {
    let label = format!("{}_{}", prefix, self.block_counter);
    self.block_counter += 1;
    self.blocks.alloc(Block::new(label))
  }

  /// Switch to building a different block.
  pub fn switch_to_block(
    &mut self,
    block: BlockId,
  ) {
    self.current_block = Some(block);
  }

  /// Get the current block ID.
  pub fn current_block(&self) -> BlockId {
    self.current_block.expect("no current block")
  }

  /// Allocate a new local variable slot.
  pub fn alloc_local(
    &mut self,
    data: LocalData,
  ) -> LocalId {
    self.locals.alloc(data)
  }

  /// Allocate a new temporary.
  pub fn alloc_temp(
    &mut self,
    ty: TypeId,
    span: Span,
  ) -> TempId {
    self.temps.alloc(TempData { ty, span })
  }

  /// Register a parameter temp (already has an implicit ID).
  pub fn register_param_temp(
    &mut self,
    ty: TypeId,
    span: Span,
  ) {
    // This allocates the temp in order, so params get t0, t1, t2, ...
    self.temps.alloc(TempData { ty, span });
  }

  /// Get the type of a temporary.
  pub fn temp_type(
    &self,
    temp: TempId,
  ) -> TypeId {
    self.temps.get(&temp).ty
  }

  /// Get the type of a local.
  pub fn local_type(
    &self,
    local: LocalId,
  ) -> TypeId {
    self.locals.get(&local).ty
  }

  /// Get the return type.
  pub fn return_type(&self) -> TypeId {
    self.return_type
  }

  /// Emit an instruction to the current block.
  pub fn emit(
    &mut self,
    instr: Instr,
  ) {
    let block = self.blocks.get_mut(&self.current_block());
    block.instructions.push(instr);
  }

  /// Set the terminator for the current block.
  pub fn terminate(
    &mut self,
    term: Terminator,
  ) {
    let block = self.blocks.get_mut(&self.current_block());
    block.terminator = term;
  }

  /// Check if the current block is terminated.
  pub fn is_terminated(&self) -> bool {
    let block = self.blocks.get(&self.current_block());
    !matches!(block.terminator, Terminator::Unreachable)
  }

  /// Finish building and return the completed function.
  pub fn finish(self) -> FunctionLir {
    FunctionLir {
      def_id: self.def_id,
      params: self.params,
      return_type: self.return_type,
      locals: self.locals,
      temps: self.temps,
      blocks: self.blocks,
      entry_block: BlockId::new(0), // Entry is always first
      is_extern: self.is_extern,
      is_variadic: self.is_variadic,
      span: self.span,
    }
  }
}
