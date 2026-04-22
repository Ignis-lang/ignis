use std::fmt;

use ignis_analyzer::{AnalyzerOutput, SemanticArtifacts};
use ignis_config::{IgnisConfig, TargetBackend};
use ignis_diagnostics::diagnostic_report::{Diagnostic, Severity};
use ignis_hir::{DropSchedules, HIR};
use ignis_lir::verify::VerifyResult;
use ignis_lir::LirProgram;
use ignis_type::definition::{DefinitionId, DefinitionStore};
use ignis_type::module::ModuleId;
use ignis_type::types::TypeStore;

use crate::backend::BackendRequest;
use crate::context::CompilationContext;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StageError {
  MissingRootModule { root_id: ModuleId },
  MissingParsedModule { module_id: ModuleId },
  AnalysisFailed,
  AnalysisDiagnostics { error_count: usize },
  UnsupportedBackend { backend: TargetBackend },
  MissingEntryDefinition { definition_id: DefinitionId },
  PostAnalysisErrors { error_count: usize },
  LirVerificationFailed { error_count: usize },
  BackendRequestRequiresHeaderInput,
  BackendRequestRequiresLoweredInput,
  MissingBackendRootModule { root_id: ModuleId },
  MissingBackendDefinition { definition_id: DefinitionId },
}

impl fmt::Display for StageError {
  fn fmt(
    &self,
    formatter: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      Self::MissingRootModule { root_id } => write!(formatter, "parsed stage is missing root module {:?}", root_id),
      Self::MissingParsedModule { module_id } => {
        write!(formatter, "parsed stage is missing parsed data for module {:?}", module_id)
      },
      Self::AnalysisFailed => write!(formatter, "analyzer stage failed before producing a verified artifact"),
      Self::AnalysisDiagnostics { error_count } => {
        write!(formatter, "analyzer stage contains {error_count} error diagnostics")
      },
      Self::UnsupportedBackend { backend } => {
        write!(formatter, "backend '{backend:?}' is not implemented")
      },
      Self::MissingEntryDefinition { definition_id } => {
        write!(
          formatter,
          "analyzed stage references missing entry definition {:?}",
          definition_id
        )
      },
      Self::PostAnalysisErrors { error_count } => {
        write!(formatter, "checked stage contains {error_count} error diagnostics")
      },
      Self::LirVerificationFailed { error_count } => {
        write!(formatter, "lir stage contains {error_count} verification errors")
      },
      Self::BackendRequestRequiresHeaderInput => {
        write!(formatter, "backend request requires header-only input")
      },
      Self::BackendRequestRequiresLoweredInput => {
        write!(formatter, "backend request requires lowered input")
      },
      Self::MissingBackendRootModule { root_id } => {
        write!(formatter, "backend input is missing artifacts for root module {:?}", root_id)
      },
      Self::MissingBackendDefinition { definition_id } => {
        write!(formatter, "backend input references missing definition {:?}", definition_id)
      },
    }
  }
}

pub struct ParsedStage {
  pub ctx: CompilationContext,
  pub root_id: ModuleId,
}

impl ParsedStage {
  pub fn new(
    ctx: CompilationContext,
    root_id: ModuleId,
  ) -> Self {
    Self { ctx, root_id }
  }

  pub fn verify(&self) -> Result<(), StageError> {
    if self.ctx.module_graph.root != Some(self.root_id) {
      return Err(StageError::MissingRootModule { root_id: self.root_id });
    }

    if self
      .ctx
      .module_graph
      .modules
      .iter()
      .any(|(module_id, _)| !self.ctx.parsed_modules.contains_key(&module_id))
    {
      let missing_module_id = self
        .ctx
        .module_graph
        .modules
        .iter()
        .find_map(|(module_id, _)| (!self.ctx.parsed_modules.contains_key(&module_id)).then_some(module_id))
        .expect("missing module id must exist when any() succeeds");

      return Err(StageError::MissingParsedModule {
        module_id: missing_module_id,
      });
    }

    Ok(())
  }

  pub fn analyze(
    mut self,
    config: &IgnisConfig,
  ) -> Result<AnalyzedStage, StageError> {
    self.verify()?;

    let (output, has_errors) = self
      .ctx
      .compile_collect_all(self.root_id, config)
      .map_err(|_| StageError::AnalysisFailed)?;
    let stage = AnalyzedStage::from_output(self.ctx, self.root_id, output, has_errors)?;

    stage.verify()?;
    Ok(stage)
  }
}

pub struct AnalyzedStage {
  pub ctx: CompilationContext,
  pub root_id: ModuleId,
  pub semantic: SemanticArtifacts,
  pub hir: HIR,
}

impl AnalyzedStage {
  pub fn from_output(
    ctx: CompilationContext,
    root_id: ModuleId,
    output: AnalyzerOutput,
    has_errors: bool,
  ) -> Result<Self, StageError> {
    let (semantic, hir) = output.into_parts();

    if has_errors {
      let error_count = semantic
        .diagnostics
        .iter()
        .filter(|diagnostic| matches!(diagnostic.severity, Severity::Error))
        .count();

      return Err(StageError::AnalysisDiagnostics { error_count });
    }

    Ok(Self {
      ctx,
      root_id,
      semantic,
      hir,
    })
  }

  pub fn verify(&self) -> Result<(), StageError> {
    if self.ctx.module_graph.root != Some(self.root_id) {
      return Err(StageError::MissingRootModule { root_id: self.root_id });
    }

    if let Some(definition_id) = self.hir.entry_point
      && !has_definition(&self.semantic.defs, definition_id)
    {
      return Err(StageError::MissingEntryDefinition { definition_id });
    }

    Ok(())
  }
}

pub struct CheckedStage {
  pub ctx: CompilationContext,
  pub root_id: ModuleId,
  pub semantic: SemanticArtifacts,
  pub hir: HIR,
  pub types: TypeStore,
  pub mono_output: ignis_analyzer::mono::MonoOutput,
  pub drop_schedules: DropSchedules,
  pub diagnostics: Vec<Diagnostic>,
}

impl CheckedStage {
  pub fn new(
    analyzed_stage: AnalyzedStage,
    types: TypeStore,
    mono_output: ignis_analyzer::mono::MonoOutput,
    drop_schedules: DropSchedules,
    diagnostics: Vec<Diagnostic>,
  ) -> Self {
    Self {
      ctx: analyzed_stage.ctx,
      root_id: analyzed_stage.root_id,
      semantic: analyzed_stage.semantic,
      hir: analyzed_stage.hir,
      types,
      mono_output,
      drop_schedules,
      diagnostics,
    }
  }

  pub fn verify(&self) -> Result<(), StageError> {
    if self.ctx.module_graph.root != Some(self.root_id) {
      return Err(StageError::MissingRootModule { root_id: self.root_id });
    }

    let error_count = self
      .diagnostics
      .iter()
      .filter(|diagnostic| matches!(diagnostic.severity, Severity::Error))
      .count();

    if error_count > 0 {
      return Err(StageError::PostAnalysisErrors { error_count });
    }

    Ok(())
  }
}

pub struct LirStage {
  pub ctx: CompilationContext,
  pub root_id: ModuleId,
  pub semantic: SemanticArtifacts,
  pub hir: HIR,
  pub types: TypeStore,
  pub mono_output: ignis_analyzer::mono::MonoOutput,
  pub drop_schedules: DropSchedules,
  pub diagnostics: Vec<Diagnostic>,
  pub program: LirProgram,
  pub verification: VerifyResult,
}

impl LirStage {
  pub fn new(
    checked_stage: CheckedStage,
    program: LirProgram,
    verification: VerifyResult,
  ) -> Self {
    Self {
      ctx: checked_stage.ctx,
      root_id: checked_stage.root_id,
      semantic: checked_stage.semantic,
      hir: checked_stage.hir,
      types: checked_stage.types,
      mono_output: checked_stage.mono_output,
      drop_schedules: checked_stage.drop_schedules,
      diagnostics: checked_stage.diagnostics,
      program,
      verification,
    }
  }

  pub fn verify(&self) -> Result<(), StageError> {
    if self.ctx.module_graph.root != Some(self.root_id) {
      return Err(StageError::MissingRootModule { root_id: self.root_id });
    }

    match &self.verification {
      Ok(()) => Ok(()),
      Err(errors) => Err(StageError::LirVerificationFailed {
        error_count: errors.len(),
      }),
    }
  }
}

pub enum BackendInput<'a> {
  Header {
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
  },
  Lowered {
    root_id: ModuleId,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    program: &'a LirProgram,
  },
}

impl<'a> BackendInput<'a> {
  pub fn from_lir(stage: &'a LirStage) -> Self {
    Self::Lowered {
      root_id: stage.root_id,
      types: &stage.types,
      defs: &stage.mono_output.defs,
      program: &stage.program,
    }
  }

  pub fn verify(&self) -> Result<(), StageError> {
    match self {
      Self::Header { types, defs } => Self::verify_header_artifacts(types, defs),
      Self::Lowered {
        root_id,
        types,
        defs,
        program,
      } => {
        Self::verify_header_artifacts(types, defs)?;
        Self::verify_lowered_root_module(defs, *root_id)?;
        Self::verify_lowered_artifacts(defs, program)
      },
    }
  }

  pub fn verify_for(
    &self,
    request: &BackendRequest<'_>,
  ) -> Result<(), StageError> {
    match (self, request) {
      (Self::Header { types, defs }, BackendRequest::Header(_)) => Self::verify_header_artifacts(types, defs),
      (Self::Header { .. }, BackendRequest::Lowered(_)) => Err(StageError::BackendRequestRequiresLoweredInput),
      (
        Self::Lowered {
          root_id,
          types,
          defs,
          program,
        },
        BackendRequest::Lowered(_),
      ) => {
        Self::verify_header_artifacts(types, defs)?;
        Self::verify_lowered_root_module(defs, *root_id)?;
        Self::verify_lowered_artifacts(defs, program)
      },
      (Self::Lowered { .. }, BackendRequest::Header(_)) => Err(StageError::BackendRequestRequiresHeaderInput),
    }
  }

  fn verify_header_artifacts(
    types: &TypeStore,
    defs: &DefinitionStore,
  ) -> Result<(), StageError> {
    let _ = types;
    let _ = defs;

    Ok(())
  }

  fn verify_lowered_root_module(
    defs: &DefinitionStore,
    root_id: ModuleId,
  ) -> Result<(), StageError> {
    if defs.iter().any(|(_, definition)| definition.owner_module == root_id) {
      return Ok(());
    }

    Err(StageError::MissingBackendRootModule { root_id })
  }

  fn verify_lowered_artifacts(
    defs: &DefinitionStore,
    program: &LirProgram,
  ) -> Result<(), StageError> {
    for definition_id in program.functions.keys().chain(program.global_inits.keys()) {
      if !has_definition(defs, *definition_id) {
        return Err(StageError::MissingBackendDefinition {
          definition_id: *definition_id,
        });
      }
    }

    Ok(())
  }
}

fn has_definition(
  defs: &DefinitionStore,
  definition_id: DefinitionId,
) -> bool {
  defs.iter().any(|(candidate_id, _)| candidate_id == definition_id)
}

#[cfg(test)]
mod tests {
  use std::collections::HashMap;
  use std::rc::Rc;

  use super::*;

  use ignis_analyzer::AnalyzerOutput;
  use ignis_config::IgnisConfig;
  use ignis_diagnostics::diagnostic_report::{Diagnostic, Severity};
  use ignis_hir::{DropSchedules, HIR};
  use ignis_lir::{LirProgram, VerifyError};
  use ignis_type::definition::{Definition, DefinitionKind, DefinitionStore, Visibility};
  use ignis_type::file::SourceMap;
  use ignis_type::module::{Module, ModulePath};
  use ignis_type::namespace::NamespaceStore;
  use ignis_type::span::Span;
  use ignis_type::symbol::SymbolTable;
  use ignis_type::types::TypeStore;
  use ignis_type::Store;

  use crate::context::{CompilationContext, ParsedModule};
  use crate::backend::{BackendRequest, HeaderBackendRequest, LoweredBackendRequest};

  fn parsed_stage_fixture() -> ParsedStage {
    let config = IgnisConfig::default();
    let mut ctx = CompilationContext::new(&config);
    let file_id = ctx
      .source_map
      .add_virtual("stage-test", "function main(): void {}".to_string());
    let module_id = ctx
      .module_graph
      .modules
      .alloc(Module::new(file_id, ModulePath::Project("stage_test.ign".into())));

    ctx.module_graph.root = Some(module_id);
    ctx.parsed_modules.insert(
      module_id,
      ParsedModule {
        file_id,
        nodes: Store::new(),
        roots: Vec::new(),
        import_paths: Vec::new(),
      },
    );

    ParsedStage::new(ctx, module_id)
  }

  fn analyzed_stage_fixture() -> AnalyzedStage {
    let parsed_stage = parsed_stage_fixture();

    AnalyzedStage::from_output(
      parsed_stage.ctx,
      parsed_stage.root_id,
      AnalyzerOutput {
        types: TypeStore::new(),
        defs: DefinitionStore::new(),
        namespaces: NamespaceStore::new(),
        hir: HIR::new(),
        diagnostics: Vec::new(),
        symbols: Rc::new(std::cell::RefCell::new(SymbolTable::new())),
        node_defs: HashMap::new(),
        node_types: HashMap::new(),
        node_spans: HashMap::new(),
        resolved_calls: HashMap::new(),
        import_item_defs: HashMap::new(),
        import_module_files: HashMap::new(),
        extension_methods: HashMap::new(),
      },
      false,
    )
    .expect("analyzed stage fixture should succeed")
  }

  fn backend_header_input_fixture<'a>(
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
  ) -> BackendInput<'a> {
    BackendInput::Header { types, defs }
  }

  fn backend_lowered_input_fixture<'a>(
    root_id: ModuleId,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    program: &'a LirProgram,
  ) -> BackendInput<'a> {
    BackendInput::Lowered {
      root_id,
      types,
      defs,
      program,
    }
  }

  fn alloc_placeholder_definition(
    defs: &mut DefinitionStore,
    owner_module: ModuleId,
  ) -> DefinitionId {
    defs.alloc(Definition {
      kind: DefinitionKind::Placeholder,
      name: Default::default(),
      span: Span::default(),
      name_span: Span::default(),
      visibility: Visibility::Private,
      owner_module,
      owner_namespace: None,
      doc: None,
    })
  }

  fn valid_lowered_backend_fixture(types: &TypeStore) -> (DefinitionStore, LirProgram, ModuleId) {
    let root_id = ModuleId::new(11);
    let mut defs = DefinitionStore::new();
    let function_id = alloc_placeholder_definition(&mut defs, root_id);
    let global_id = alloc_placeholder_definition(&mut defs, root_id);
    let mut program = LirProgram::new();

    program.entry_point = Some(function_id);
    program.functions.insert(
      function_id,
      ignis_lir::FunctionLir {
        def_id: function_id,
        params: Vec::new(),
        return_type: types.void(),
        locals: Store::new(),
        temps: Store::new(),
        blocks: Store::new(),
        entry_block: ignis_lir::BlockId::new(0),
        is_extern: false,
        is_variadic: false,
        inline_mode: Default::default(),
        span: Span::default(),
      },
    );
    program
      .global_inits
      .insert(global_id, ignis_lir::Operand::Const(ignis_lir::ConstValue::Int(0, types.i32())));

    (defs, program, root_id)
  }

  fn lowered_fixture_with_missing_root_module(types: &TypeStore) -> (DefinitionStore, LirProgram, ModuleId) {
    let wrong_root_id = ModuleId::new(77);
    let actual_root_id = ModuleId::new(11);
    let mut defs = DefinitionStore::new();
    let function_id = alloc_placeholder_definition(&mut defs, actual_root_id);
    let mut program = LirProgram::new();

    program.entry_point = Some(function_id);
    program.functions.insert(
      function_id,
      ignis_lir::FunctionLir {
        def_id: function_id,
        params: Vec::new(),
        return_type: types.void(),
        locals: Store::new(),
        temps: Store::new(),
        blocks: Store::new(),
        entry_block: ignis_lir::BlockId::new(0),
        is_extern: false,
        is_variadic: false,
        inline_mode: Default::default(),
        span: Span::default(),
      },
    );

    (defs, program, wrong_root_id)
  }

  fn lowered_fixture_with_missing_global_init(
    types: &TypeStore
  ) -> (DefinitionStore, LirProgram, ModuleId, DefinitionId) {
    let (defs, mut program, root_id) = valid_lowered_backend_fixture(types);
    let missing_global_id = DefinitionId::new(99);

    program.global_inits.insert(
      missing_global_id,
      ignis_lir::Operand::Const(ignis_lir::ConstValue::Int(1, types.i32())),
    );

    (defs, program, root_id, missing_global_id)
  }

  #[test]
  fn parsed_stage_verifies_registered_root_and_parsed_modules() {
    let stage = parsed_stage_fixture();

    assert_eq!(stage.verify(), Ok(()));
  }

  #[test]
  fn parsed_stage_rejects_missing_parsed_module() {
    let mut stage = parsed_stage_fixture();
    stage.ctx.parsed_modules.clear();

    assert_eq!(
      stage.verify(),
      Err(StageError::MissingParsedModule {
        module_id: stage.root_id,
      })
    );
  }

  #[test]
  fn analyzed_stage_rejects_missing_entry_definition() {
    let mut stage = analyzed_stage_fixture();
    stage.hir.entry_point = Some(ignis_type::definition::DefinitionId::new(99));

    assert_eq!(
      stage.verify(),
      Err(StageError::MissingEntryDefinition {
        definition_id: ignis_type::definition::DefinitionId::new(99),
      })
    );
  }

  #[test]
  fn analyzed_stage_surfaces_structured_analysis_diagnostics() {
    let parsed_stage = parsed_stage_fixture();
    let file_id = SourceMap::new().add_virtual("analyzer-stage", String::new());
    let output = AnalyzerOutput {
      types: TypeStore::new(),
      defs: DefinitionStore::new(),
      namespaces: NamespaceStore::new(),
      hir: HIR::new(),
      diagnostics: vec![Diagnostic::new(
        Severity::Error,
        "analysis error".to_string(),
        "E-analyze".to_string(),
        Span::empty_at(file_id, Default::default()),
      )],
      symbols: Rc::new(std::cell::RefCell::new(SymbolTable::new())),
      node_defs: HashMap::new(),
      node_types: HashMap::new(),
      node_spans: HashMap::new(),
      resolved_calls: HashMap::new(),
      import_item_defs: HashMap::new(),
      import_module_files: HashMap::new(),
      extension_methods: HashMap::new(),
    };

    let result = AnalyzedStage::from_output(parsed_stage.ctx, parsed_stage.root_id, output, true);

    assert!(matches!(result, Err(StageError::AnalysisDiagnostics { error_count: 1 })));
  }

  #[test]
  fn analyzed_stage_splits_semantic_artifacts_from_hir() {
    let stage = analyzed_stage_fixture();

    assert!(stage.semantic.diagnostics.is_empty());
    assert_eq!(stage.hir.entry_point, None);
  }

  #[test]
  fn checked_stage_rejects_error_diagnostics() {
    let analyzed_stage = analyzed_stage_fixture();
    let file_id = SourceMap::new().add_virtual("check-stage", String::new());
    let diagnostics = vec![Diagnostic::new(
      Severity::Error,
      "error".to_string(),
      "E-stage".to_string(),
      Span::empty_at(file_id, Default::default()),
    )];

    let stage = CheckedStage::new(
      analyzed_stage,
      TypeStore::new(),
      ignis_analyzer::mono::MonoOutput {
        hir: HIR::new(),
        defs: DefinitionStore::new(),
        instance_map: HashMap::new(),
      },
      DropSchedules::default(),
      diagnostics,
    );

    assert_eq!(stage.verify(), Err(StageError::PostAnalysisErrors { error_count: 1 }));
  }

  #[test]
  fn lir_stage_rejects_failed_verification() {
    let checked_stage = CheckedStage::new(
      analyzed_stage_fixture(),
      TypeStore::new(),
      ignis_analyzer::mono::MonoOutput {
        hir: HIR::new(),
        defs: DefinitionStore::new(),
        instance_map: HashMap::new(),
      },
      DropSchedules::default(),
      Vec::new(),
    );

    let stage = LirStage::new(
      checked_stage,
      LirProgram::new(),
      Err(vec![VerifyError::MissingTerminator {
        function: "main".to_string(),
        block: "entry".to_string(),
      }]),
    );

    assert_eq!(stage.verify(), Err(StageError::LirVerificationFailed { error_count: 1 }));
  }

  #[test]
  fn backend_input_accepts_header_requests_without_lir() {
    let types = TypeStore::new();
    let defs = DefinitionStore::new();
    let symbols = SymbolTable::new();
    let namespaces = NamespaceStore::new();
    let input = backend_header_input_fixture(&types, &defs);

    assert_eq!(
      input.verify_for(&BackendRequest::Header(HeaderBackendRequest::EmitStdHeader {
        namespaces: &namespaces,
        symbols: &symbols,
      })),
      Ok(())
    );
  }

  #[test]
  fn backend_input_rejects_lowered_requests_without_lowered_artifacts() {
    let types = TypeStore::new();
    let defs = DefinitionStore::new();
    let symbols = SymbolTable::new();
    let namespaces = NamespaceStore::new();
    let input = backend_header_input_fixture(&types, &defs);

    assert_eq!(
      input.verify_for(&BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      })),
      Err(StageError::BackendRequestRequiresLoweredInput)
    );
  }

  #[test]
  fn backend_input_rejects_lowered_requests_with_missing_function_definition() {
    let types = TypeStore::new();
    let mut defs = DefinitionStore::new();
    let mut program = LirProgram::new();
    let symbols = SymbolTable::new();
    let namespaces = NamespaceStore::new();
    let root_id = ModuleId::new(7);
    let root_def_id = alloc_placeholder_definition(&mut defs, root_id);
    let missing_function_id = DefinitionId::new(root_def_id.index() + 1);

    program.entry_point = Some(missing_function_id);
    program.functions.insert(
      missing_function_id,
      ignis_lir::FunctionLir {
        def_id: missing_function_id,
        params: Vec::new(),
        return_type: types.void(),
        locals: Store::new(),
        temps: Store::new(),
        blocks: Store::new(),
        entry_block: ignis_lir::BlockId::new(0),
        is_extern: false,
        is_variadic: false,
        inline_mode: Default::default(),
        span: Span::default(),
      },
    );

    let input = backend_lowered_input_fixture(root_id, &types, &defs, &program);

    assert_eq!(
      input.verify_for(&BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      })),
      Err(StageError::MissingBackendDefinition {
        definition_id: missing_function_id,
      })
    );
  }

  #[test]
  fn backend_input_rejects_lowered_requests_with_missing_root_module_artifacts() {
    let types = TypeStore::new();
    let symbols = SymbolTable::new();
    let namespaces = NamespaceStore::new();
    let (defs, program, root_id) = lowered_fixture_with_missing_root_module(&types);
    let input = backend_lowered_input_fixture(root_id, &types, &defs, &program);

    assert_eq!(
      input.verify_for(&BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      })),
      Err(StageError::MissingBackendRootModule { root_id })
    );
  }

  #[test]
  fn backend_input_accepts_valid_lowered_requests_with_registered_artifacts() {
    let types = TypeStore::new();
    let symbols = SymbolTable::new();
    let namespaces = NamespaceStore::new();
    let (defs, program, root_id) = valid_lowered_backend_fixture(&types);
    let input = backend_lowered_input_fixture(root_id, &types, &defs, &program);

    assert_eq!(
      input.verify_for(&BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      })),
      Ok(())
    );
  }

  #[test]
  fn backend_input_rejects_header_requests_with_lowered_input() {
    let types = TypeStore::new();
    let symbols = SymbolTable::new();
    let namespaces = NamespaceStore::new();
    let (defs, program, root_id) = valid_lowered_backend_fixture(&types);
    let input = backend_lowered_input_fixture(root_id, &types, &defs, &program);

    assert_eq!(
      input.verify_for(&BackendRequest::Header(HeaderBackendRequest::EmitStdHeader {
        namespaces: &namespaces,
        symbols: &symbols,
      })),
      Err(StageError::BackendRequestRequiresHeaderInput)
    );
  }

  #[test]
  fn backend_input_rejects_lowered_requests_with_missing_global_init_definition() {
    let types = TypeStore::new();
    let symbols = SymbolTable::new();
    let namespaces = NamespaceStore::new();
    let (defs, program, root_id, missing_global_id) = lowered_fixture_with_missing_global_init(&types);
    let input = backend_lowered_input_fixture(root_id, &types, &defs, &program);

    assert_eq!(
      input.verify_for(&BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      })),
      Err(StageError::MissingBackendDefinition {
        definition_id: missing_global_id,
      })
    );
  }
}
