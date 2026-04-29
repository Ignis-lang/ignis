mod bind;
mod checks;
mod const_eval;
mod lint;
mod lower;
mod resolve;
mod typecheck;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use ignis_ast::{ASTNode, NodeId};
use ignis_hir::HIR;
use ignis_type::definition::{DefinitionId, DefinitionStore};
use ignis_type::namespace::NamespaceStore;
use ignis_type::symbol::{SymbolId, SymbolTable};
use ignis_type::types::{TypeId, TypeStore};
use ignis_type::Store as ASTStore;

use crate::directive_scheduler::{
  DirectiveExecutionReport, DirectiveSchedulePlan, DirectiveScheduler, NoopDirectiveReanalysisHook,
  NoopDirectiveStageExecutor,
};
use crate::{Analyzer, SemanticArtifacts, build_node_spans};

const DIRECTIVE_SCHEDULER_CYCLE_LIMIT: usize = 8;

pub(crate) fn run_semantic_passes(
  analyzer: &mut Analyzer<'_>,
  roots: &[NodeId],
) {
  bind::run(analyzer, roots);
  resolve::run(analyzer, roots);
  typecheck::run(analyzer, roots);
  const_eval::run(analyzer, roots);
  checks::run(analyzer, roots);
  lint::run(analyzer, roots);
}

#[cfg(test)]
pub(crate) fn run_semantic_phases(
  analyzer: &mut Analyzer<'_>,
  ast: &ASTStore<ASTNode>,
  roots: &[NodeId],
  symbols: Rc<RefCell<SymbolTable>>,
) -> SemanticArtifacts {
  run_semantic_passes(analyzer, roots);
  build_semantic_artifacts(analyzer, ast, symbols)
}

pub(crate) fn run_lowering_phase(
  analyzer: &mut Analyzer<'_>,
  roots: &[NodeId],
) -> HIR {
  lower::run(analyzer, roots)
}

pub(crate) fn run_directive_scheduling_phase(analyzer: &mut Analyzer<'_>) -> DirectiveExecutionReport {
  let plan = DirectiveSchedulePlan::from_registry(&analyzer.directive_registry);
  let mut scheduler = DirectiveScheduler::new(DIRECTIVE_SCHEDULER_CYCLE_LIMIT);
  let mut executor = NoopDirectiveStageExecutor;
  let mut reanalysis_hook = NoopDirectiveReanalysisHook;
  let report = scheduler.run(plan, &mut executor, &mut reanalysis_hook);

  if let Some(diagnostic) = report.failure.as_ref().and_then(|failure| failure.as_diagnostic()) {
    analyzer.add_diagnostic(diagnostic);
  }

  report
}

pub(crate) fn build_semantic_artifacts(
  analyzer: &Analyzer<'_>,
  ast: &ASTStore<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
) -> SemanticArtifacts {
  let node_spans = build_node_spans(ast, &analyzer.node_defs, &analyzer.node_types);

  SemanticArtifacts {
    types: analyzer.types.clone(),
    defs: analyzer.defs.clone(),
    namespaces: analyzer.namespaces.clone(),
    diagnostics: analyzer.diagnostics.clone(),
    symbols,
    node_defs: analyzer.node_defs.clone(),
    node_types: analyzer.node_types.clone(),
    node_spans,
    resolved_calls: analyzer.resolved_calls.clone(),
    import_item_defs: analyzer.import_item_defs.clone(),
    import_module_files: analyzer.import_module_files.clone(),
    extension_methods: analyzer.extension_methods.clone(),
    directive_registry: analyzer.directive_registry.clone(),
    directive_execution_report: analyzer.directive_execution_report.clone(),
  }
}

pub(crate) fn build_shared_semantic_artifacts(
  analyzer: &Analyzer<'_>,
  ast: &ASTStore<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
  shared_types: &TypeStore,
  shared_defs: &DefinitionStore,
  shared_namespaces: &NamespaceStore,
  shared_extension_methods: &HashMap<TypeId, HashMap<SymbolId, Vec<DefinitionId>>>,
) -> SemanticArtifacts {
  let node_spans = build_node_spans(ast, &analyzer.node_defs, &analyzer.node_types);

  SemanticArtifacts {
    types: shared_types.clone(),
    defs: shared_defs.clone(),
    namespaces: shared_namespaces.clone(),
    diagnostics: analyzer.diagnostics.clone(),
    symbols,
    node_defs: analyzer.node_defs.clone(),
    node_types: analyzer.node_types.clone(),
    node_spans,
    resolved_calls: analyzer.resolved_calls.clone(),
    import_item_defs: analyzer.import_item_defs.clone(),
    import_module_files: analyzer.import_module_files.clone(),
    extension_methods: shared_extension_methods.clone(),
    directive_registry: analyzer.directive_registry.clone(),
    directive_execution_report: analyzer.directive_execution_report.clone(),
  }
}
