macro_rules! with_for_of_scope {
  ($self:expr, $node_id:expr, $for_of:expr, $body:block) => {{
    $self.scopes.push(ScopeKind::Loop);

    if let Some(def_id) = $self.for_of_binding_defs.get($node_id).cloned() {
      let _ = $self.scopes.define(&$for_of.binding.name, &def_id, false);
    }

    $body

    $self.scopes.pop();
  }};
}

mod binder;
pub mod borrowck_hir;
pub mod capture;
mod checks;
mod const_eval;
pub mod directive_registry;
pub mod directive_scheduler;
pub mod dump;
pub mod escape;
pub mod imports;
mod lint;
mod lowering;
pub mod modules;
pub mod mono;
pub mod ownership_hir;
mod phases;
mod resolver;
mod scope;
mod typeck;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;

use ignis_ast::{ASTNode, NodeId, statements::ASTStatement, type_::IgnisTypeSyntax};
use ignis_type::{
  inference::InferCtx,
  symbol::{SymbolId, SymbolTable},
  Store as ASTStore,
};
use ignis_type::types::{TypeId, TypeStore};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, SymbolEntry, Visibility};
use ignis_type::lint::{LintId, LintLevel};
use ignis_type::module::ModuleId;
use ignis_type::namespace::{NamespaceId, NamespaceStore};
use ignis_hir::{HIR, HIRId};
use ignis_diagnostics::diagnostic_report::Diagnostic;

use imports::ExportTable;
use directive_registry::DirectiveRegistry;
use directive_scheduler::DirectiveExecutionReport;

pub use ignis_hir::{DropSchedules, ExitKey};
pub use borrowck_hir::HirBorrowChecker;
pub use ownership_hir::HirOwnershipChecker;
pub use resolver::ResolvedPath;
pub use scope::{ScopeTree, ScopeId, ScopeKind};

/// Context passed through type checking for function-scoped information.
#[derive(Clone, Default)]
pub struct TypecheckContext {
  pub expected_return: Option<TypeId>,
}

impl TypecheckContext {
  pub fn new() -> Self {
    Self { expected_return: None }
  }

  pub fn with_return(ret: TypeId) -> Self {
    Self {
      expected_return: Some(ret),
    }
  }
}

/// Context for bidirectional type inference.
#[derive(Clone, Default)]
pub struct InferContext {
  pub expected: Option<TypeId>,
}

impl InferContext {
  pub fn none() -> Self {
    Self { expected: None }
  }

  pub fn expecting(ty: TypeId) -> Self {
    Self { expected: Some(ty) }
  }
}

/// Whether the piped LHS value is prepended or replaces a `_` placeholder.
#[derive(Debug, Clone, Copy)]
pub enum PipeArgInsertion {
  Prepend,
  ReplaceAt(usize),
}

/// How a pipe expression `lhs |> rhs` desugars. Set by typeck, read by lowering.
#[derive(Debug, Clone)]
pub enum PipeResolution {
  /// `x |> f` or `x |> f(a, b)` — callee is a known function definition.
  DirectCall {
    def_id: DefinitionId,
    extra_args: Vec<NodeId>,
    type_args: Vec<TypeId>,
    insertion: PipeArgInsertion,
  },
  /// `x |> closureVar` or `x |> (n: i32): i32 -> n * 2` — callee is a closure/function-typed value.
  ClosureCall {
    callee_node: NodeId,
    extra_args: Vec<NodeId>,
    insertion: PipeArgInsertion,
  },
  /// `x |> obj.method(a, b)` or `x |> obj.method` — instance method call with receiver.
  MethodCall {
    receiver_node: NodeId,
    method_id: DefinitionId,
    extra_args: Vec<NodeId>,
    type_args: Vec<TypeId>,
    self_mutable: bool,
    insertion: PipeArgInsertion,
  },
}

pub struct Analyzer<'a> {
  ast: &'a ASTStore<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
  types: TypeStore,
  defs: DefinitionStore,
  namespaces: NamespaceStore,
  scopes: ScopeTree,
  infer_ctx: InferCtx,
  scope_infer_vars: HashMap<crate::scope::ScopeId, Vec<(DefinitionId, ignis_type::types::InferVarId)>>,
  node_defs: HashMap<NodeId, DefinitionId>,
  node_types: HashMap<NodeId, TypeId>,
  diagnostics: Vec<Diagnostic>,
  export_table: ExportTable,
  module_for_path: HashMap<String, ModuleId>,
  implicit_imports: Vec<ModuleId>,
  path_to_file: HashMap<String, ignis_type::file::FileId>,
  current_module: ModuleId,
  current_namespace: Option<NamespaceId>,
  in_callee_context: bool,
  conditional_let_context_depth: usize,
  resolving_type_aliases: HashSet<DefinitionId>,
  type_alias_syntax: HashMap<DefinitionId, IgnisTypeSyntax>,
  lowering_counter: u32,
  for_of_binding_defs: HashMap<NodeId, DefinitionId>,
  resolved_calls: HashMap<NodeId, DefinitionId>,

  import_item_defs: HashMap<ignis_type::span::Span, DefinitionId>,
  import_module_files: HashMap<ignis_type::span::Span, ignis_type::file::FileId>,
  referenced_defs: HashSet<DefinitionId>,
  mutated_defs: HashSet<DefinitionId>,
  imported_defs: HashMap<DefinitionId, ignis_type::span::Span>,
  lint_overrides: Vec<(LintId, LintLevel)>,
  extension_methods: HashMap<TypeId, HashMap<SymbolId, Vec<DefinitionId>>>,
  trait_default_bodies: HashMap<DefinitionId, NodeId>,
  trait_default_clones: HashMap<DefinitionId, NodeId>,
  directive_registry: DirectiveRegistry,
  directive_execution_report: DirectiveExecutionReport,

  /// Lambda parameter definitions created by the resolver, keyed by lambda expression NodeId.
  /// The typechecker reuses these definitions (updating their types) instead of creating new ones.
  lambda_param_defs: HashMap<NodeId, Vec<DefinitionId>>,

  /// Stack of per-lambda capture override maps, built during HIR lowering.
  /// Pushed when entering `lower_lambda_to_hir`, popped into the Closure node.
  capture_override_stack: Vec<HashMap<DefinitionId, ignis_hir::CaptureMode>>,

  /// Pipe operator resolutions. Set by typeck, read by lowering.
  pipe_resolutions: HashMap<NodeId, PipeResolution>,

  /// Ambient LHS type for deep-placeholder pipe typechecking.
  /// Pushed when entering ambient-path pipe typecheck, read by PipePlaceholder arm.
  pipe_lhs_type_stack: Vec<TypeId>,

  /// Ambient LHS HIR for deep-placeholder pipe lowering.
  /// Pushed when entering ambient-path pipe lowering, read by PipePlaceholder arm.
  pipe_lhs_hir_stack: Vec<HIRId>,

  /// Return type context while lowering callable bodies.
  /// Used by `expr!` lowering to synthesize early returns with the callable return type.
  lowering_return_type_stack: Vec<TypeId>,

  /// When true, the resolver suppresses diagnostics for undeclared variables and identifiers.
  /// Used when resolving method bodies for `node_defs` population only.
  resolve_suppress_errors: bool,
}

pub struct AnalyzerOutput {
  pub types: TypeStore,
  pub defs: DefinitionStore,
  pub namespaces: NamespaceStore,
  pub hir: HIR,
  pub diagnostics: Vec<Diagnostic>,
  pub symbols: Rc<RefCell<SymbolTable>>,

  /// Maps AST nodes to their resolved definitions.
  /// Useful for "go to definition" in LSP.
  pub node_defs: HashMap<NodeId, DefinitionId>,

  /// Maps AST nodes to their inferred types.
  /// Useful for "hover" in LSP.
  pub node_types: HashMap<NodeId, TypeId>,

  /// Maps AST nodes to their source spans.
  /// Useful for finding which node is at a given cursor position.
  pub node_spans: HashMap<NodeId, ignis_type::span::Span>,

  /// Maps Call nodes to their resolved overload.
  /// Used when hovering over an overloaded function call.
  pub resolved_calls: HashMap<NodeId, DefinitionId>,

  /// Maps import item spans to their resolved definitions.
  /// Used for hover on import statements.
  pub import_item_defs: HashMap<ignis_type::span::Span, DefinitionId>,

  /// Maps import path string spans to the FileId of the imported module.
  /// Used for Go to Definition on import path strings.
  pub import_module_files: HashMap<ignis_type::span::Span, ignis_type::file::FileId>,

  /// Extension methods indexed by target type.
  /// Used by LSP to provide dot-completion for primitive types.
  pub extension_methods:
    HashMap<TypeId, HashMap<ignis_type::symbol::SymbolId, Vec<ignis_type::definition::DefinitionId>>>,
  pub directive_registry: DirectiveRegistry,
  pub directive_execution_report: DirectiveExecutionReport,
}

pub struct SemanticArtifacts {
  pub types: TypeStore,
  pub defs: DefinitionStore,
  pub namespaces: NamespaceStore,
  pub diagnostics: Vec<Diagnostic>,
  pub symbols: Rc<RefCell<SymbolTable>>,
  pub node_defs: HashMap<NodeId, DefinitionId>,
  pub node_types: HashMap<NodeId, TypeId>,
  pub node_spans: HashMap<NodeId, ignis_type::span::Span>,
  pub resolved_calls: HashMap<NodeId, DefinitionId>,
  pub import_item_defs: HashMap<ignis_type::span::Span, DefinitionId>,
  pub import_module_files: HashMap<ignis_type::span::Span, ignis_type::file::FileId>,
  pub extension_methods:
    HashMap<TypeId, HashMap<ignis_type::symbol::SymbolId, Vec<ignis_type::definition::DefinitionId>>>,
  pub directive_registry: DirectiveRegistry,
  pub directive_execution_report: DirectiveExecutionReport,
}

impl SemanticArtifacts {
  pub fn collect_exports(&self) -> imports::ModuleExportData {
    let mut exports = HashMap::new();

    for (def_id, def) in self.defs.iter() {
      if def.visibility == Visibility::Public {
        exports.insert(def.name, def_id);
      }
    }

    imports::ModuleExportData { exports }
  }
}

impl AnalyzerOutput {
  pub fn effective_implemented_traits_for_owner(
    &self,
    owner_def_id: DefinitionId,
  ) -> Vec<DefinitionId> {
    let mut traits = match &self.defs.get(&owner_def_id).kind {
      DefinitionKind::Record(record) => record.implemented_traits.clone(),
      DefinitionKind::Enum(enum_def) => enum_def.implemented_traits.clone(),
      _ => return Vec::new(),
    };

    for (trait_def_id, _) in self
      .directive_registry
      .generated_implemented_traits_for_owner(owner_def_id)
    {
      if !traits.contains(&trait_def_id) {
        traits.push(trait_def_id);
      }
    }

    traits
  }

  pub fn into_parts(self) -> (SemanticArtifacts, HIR) {
    let hir = self.hir;
    let semantic = SemanticArtifacts {
      types: self.types,
      defs: self.defs,
      namespaces: self.namespaces,
      diagnostics: self.diagnostics,
      symbols: self.symbols,
      node_defs: self.node_defs,
      node_types: self.node_types,
      node_spans: self.node_spans,
      resolved_calls: self.resolved_calls,
      import_item_defs: self.import_item_defs,
      import_module_files: self.import_module_files,
      extension_methods: self.extension_methods,
      directive_registry: self.directive_registry,
      directive_execution_report: self.directive_execution_report,
    };

    (semantic, hir)
  }

  pub fn from_semantic_artifacts(
    semantic: SemanticArtifacts,
    hir: HIR,
  ) -> Self {
    Self {
      types: semantic.types,
      defs: semantic.defs,
      namespaces: semantic.namespaces,
      hir,
      diagnostics: semantic.diagnostics,
      symbols: semantic.symbols,
      node_defs: semantic.node_defs,
      node_types: semantic.node_types,
      node_spans: semantic.node_spans,
      resolved_calls: semantic.resolved_calls,
      import_item_defs: semantic.import_item_defs,
      import_module_files: semantic.import_module_files,
      extension_methods: semantic.extension_methods,
      directive_registry: semantic.directive_registry,
      directive_execution_report: semantic.directive_execution_report,
    }
  }

  pub fn collect_exports(&self) -> imports::ModuleExportData {
    SemanticArtifacts {
      types: self.types.clone(),
      defs: self.defs.clone(),
      namespaces: self.namespaces.clone(),
      diagnostics: self.diagnostics.clone(),
      symbols: self.symbols.clone(),
      node_defs: self.node_defs.clone(),
      node_types: self.node_types.clone(),
      node_spans: self.node_spans.clone(),
      resolved_calls: self.resolved_calls.clone(),
      import_item_defs: self.import_item_defs.clone(),
      import_module_files: self.import_module_files.clone(),
      extension_methods: self.extension_methods.clone(),
      directive_registry: self.directive_registry.clone(),
      directive_execution_report: self.directive_execution_report.clone(),
    }
    .collect_exports()
  }
}

impl<'a> Analyzer<'a> {
  pub fn new(
    ast: &'a ASTStore<ASTNode>,
    symbols: Rc<RefCell<SymbolTable>>,
    current_module: ModuleId,
  ) -> Self {
    Self {
      ast,
      symbols,
      types: TypeStore::new(),
      defs: DefinitionStore::new(),
      namespaces: NamespaceStore::new(),
      scopes: ScopeTree::new(),
      infer_ctx: InferCtx::new(),
      scope_infer_vars: HashMap::new(),
      node_defs: HashMap::new(),
      node_types: HashMap::new(),
      diagnostics: Vec::new(),
      export_table: HashMap::new(),
      module_for_path: HashMap::new(),
      implicit_imports: Vec::new(),
      path_to_file: HashMap::new(),
      current_module,
      current_namespace: None,
      in_callee_context: false,
      conditional_let_context_depth: 0,
      resolving_type_aliases: HashSet::new(),
      type_alias_syntax: HashMap::new(),
      lowering_counter: 0,
      for_of_binding_defs: HashMap::new(),
      resolved_calls: HashMap::new(),
      import_item_defs: HashMap::new(),
      import_module_files: HashMap::new(),
      referenced_defs: HashSet::new(),
      mutated_defs: HashSet::new(),
      imported_defs: HashMap::new(),
      lint_overrides: Vec::new(),
      extension_methods: HashMap::new(),
      trait_default_bodies: HashMap::new(),
      trait_default_clones: HashMap::new(),
      directive_registry: DirectiveRegistry::default(),
      directive_execution_report: DirectiveExecutionReport::default(),
      lambda_param_defs: HashMap::new(),
      capture_override_stack: Vec::new(),
      pipe_resolutions: HashMap::new(),
      pipe_lhs_type_stack: Vec::new(),
      pipe_lhs_hir_stack: Vec::new(),
      lowering_return_type_stack: Vec::new(),
      resolve_suppress_errors: false,
    }
  }

  pub fn analyze(
    ast: &ASTStore<ASTNode>,
    roots: &[NodeId],
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> AnalyzerOutput {
    Self::analyze_with_imports(ast, roots, symbols, &HashMap::new(), &HashMap::new())
  }

  pub fn analyze_staged(
    ast: &ASTStore<ASTNode>,
    roots: &[NodeId],
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> AnalyzerOutput {
    let (semantic, hir) = Self::analyze_to_artifacts(ast, roots, symbols);
    AnalyzerOutput::from_semantic_artifacts(semantic, hir)
  }

  pub fn analyze_to_artifacts(
    ast: &ASTStore<ASTNode>,
    roots: &[NodeId],
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> (SemanticArtifacts, HIR) {
    let symbols_clone = symbols.clone();
    let mut analyzer = Analyzer::new(ast, symbols, ModuleId::new(0));

    phases::run_semantic_passes(&mut analyzer, roots);
    analyzer.directive_execution_report = phases::run_directive_scheduling_phase(&mut analyzer);

    let mut hir = phases::run_lowering_phase(&mut analyzer, roots);

    let closure_diags = capture::populate_closure_captures(
      &mut hir,
      &mut analyzer.defs,
      &mut analyzer.types,
      &mut analyzer.symbols.borrow_mut(),
    );
    analyzer.diagnostics.extend(closure_diags);

    let semantic = phases::build_semantic_artifacts(&analyzer, ast, symbols_clone);

    (semantic, hir)
  }

  /// Analyze with imports from other modules
  pub fn analyze_with_imports(
    ast: &ASTStore<ASTNode>,
    roots: &[NodeId],
    symbols: Rc<RefCell<SymbolTable>>,
    export_table: &ExportTable,
    module_for_path: &HashMap<String, ModuleId>,
  ) -> AnalyzerOutput {
    let symbols_clone = symbols.clone();
    // Use a dummy ModuleId for standalone analysis
    let mut analyzer = Analyzer::new(ast, symbols, ModuleId::new(0));
    analyzer.export_table = export_table.clone();
    analyzer.module_for_path = module_for_path.clone();

    phases::run_semantic_passes(&mut analyzer, roots);
    analyzer.directive_execution_report = phases::run_directive_scheduling_phase(&mut analyzer);

    let mut hir = phases::run_lowering_phase(&mut analyzer, roots);

    let closure_diags = capture::populate_closure_captures(
      &mut hir,
      &mut analyzer.defs,
      &mut analyzer.types,
      &mut analyzer.symbols.borrow_mut(),
    );
    analyzer.diagnostics.extend(closure_diags);

    let semantic = phases::build_semantic_artifacts(&analyzer, ast, symbols_clone);

    AnalyzerOutput::from_semantic_artifacts(semantic, hir)
  }

  /// Analyze with shared stores, enabling cross-module compilation.
  #[allow(clippy::too_many_arguments)]
  pub fn analyze_with_shared_stores(
    ast: &ASTStore<ASTNode>,
    roots: &[NodeId],
    symbols: Rc<RefCell<SymbolTable>>,
    export_table: &ExportTable,
    module_for_path: &HashMap<String, ModuleId>,
    path_to_file: &HashMap<String, ignis_type::file::FileId>,
    shared_types: &mut TypeStore,
    shared_defs: &mut DefinitionStore,
    shared_namespaces: &mut NamespaceStore,
    shared_extension_methods: &mut HashMap<TypeId, HashMap<SymbolId, Vec<DefinitionId>>>,
    current_module: ModuleId,
    implicit_imports: Vec<ModuleId>,
  ) -> AnalyzerOutput {
    let symbols_clone = symbols.clone();

    let mut analyzer = Analyzer {
      ast,
      symbols,
      types: std::mem::replace(shared_types, TypeStore::new()),
      defs: std::mem::replace(shared_defs, DefinitionStore::new()),
      namespaces: std::mem::replace(shared_namespaces, NamespaceStore::new()),
      scopes: ScopeTree::new(),
      infer_ctx: InferCtx::new(),
      scope_infer_vars: HashMap::new(),
      node_defs: HashMap::new(),
      node_types: HashMap::new(),
      diagnostics: Vec::new(),
      export_table: export_table.clone(),
      module_for_path: module_for_path.clone(),
      implicit_imports,
      path_to_file: path_to_file.clone(),
      current_module,
      current_namespace: None,
      in_callee_context: false,
      conditional_let_context_depth: 0,
      resolving_type_aliases: HashSet::new(),
      type_alias_syntax: HashMap::new(),
      lowering_counter: 0,
      for_of_binding_defs: HashMap::new(),
      resolved_calls: HashMap::new(),
      import_item_defs: HashMap::new(),
      import_module_files: HashMap::new(),
      referenced_defs: HashSet::new(),
      mutated_defs: HashSet::new(),
      imported_defs: HashMap::new(),
      lint_overrides: Vec::new(),
      extension_methods: std::mem::take(shared_extension_methods),
      trait_default_bodies: HashMap::new(),
      trait_default_clones: HashMap::new(),
      directive_registry: DirectiveRegistry::default(),
      directive_execution_report: DirectiveExecutionReport::default(),
      lambda_param_defs: HashMap::new(),
      capture_override_stack: Vec::new(),
      pipe_resolutions: HashMap::new(),
      pipe_lhs_type_stack: Vec::new(),
      pipe_lhs_hir_stack: Vec::new(),
      lowering_return_type_stack: Vec::new(),
      resolve_suppress_errors: false,
    };

    phases::run_semantic_passes(&mut analyzer, roots);
    analyzer.directive_execution_report = phases::run_directive_scheduling_phase(&mut analyzer);
    let mut hir = phases::run_lowering_phase(&mut analyzer, roots);

    let closure_diags = capture::populate_closure_captures(
      &mut hir,
      &mut analyzer.defs,
      &mut analyzer.types,
      &mut analyzer.symbols.borrow_mut(),
    );
    analyzer.diagnostics.extend(closure_diags);

    *shared_types = std::mem::replace(&mut analyzer.types, TypeStore::new());
    *shared_defs = std::mem::replace(&mut analyzer.defs, DefinitionStore::new());
    *shared_namespaces = std::mem::replace(&mut analyzer.namespaces, NamespaceStore::new());
    *shared_extension_methods = std::mem::take(&mut analyzer.extension_methods);

    let semantic = phases::build_shared_semantic_artifacts(
      &analyzer,
      ast,
      symbols_clone,
      shared_types,
      shared_defs,
      shared_namespaces,
      shared_extension_methods,
    );

    AnalyzerOutput::from_semantic_artifacts(semantic, hir)
  }

  fn add_diagnostic(
    &mut self,
    diagnostic: Diagnostic,
  ) {
    self.diagnostics.push(diagnostic);
  }

  fn type_of(
    &self,
    def_id: &DefinitionId,
  ) -> &TypeId {
    self.defs.type_of(def_id)
  }

  fn lookup_type(
    &self,
    node_id: &NodeId,
  ) -> Option<&TypeId> {
    self.node_types.get(node_id)
  }

  fn set_type(
    &mut self,
    node_id: &NodeId,
    type_id: &TypeId,
  ) {
    self.node_types.insert(*node_id, *type_id);
  }

  fn lookup_def(
    &self,
    node_id: &NodeId,
  ) -> Option<&DefinitionId> {
    self.node_defs.get(node_id)
  }

  fn set_def(
    &mut self,
    node_id: &NodeId,
    def_id: &DefinitionId,
  ) {
    self.node_defs.insert(*node_id, *def_id);
  }

  fn set_import_item_def(
    &mut self,
    span: &ignis_type::span::Span,
    def_id: &DefinitionId,
  ) {
    self.import_item_defs.insert(span.clone(), *def_id);
  }

  fn lookup_import_item_def(
    &self,
    span: &ignis_type::span::Span,
  ) -> Option<&DefinitionId> {
    self.import_item_defs.get(span)
  }

  fn lookup_resolved_call(
    &self,
    node_id: &NodeId,
  ) -> Option<&DefinitionId> {
    self.resolved_calls.get(node_id)
  }

  fn set_resolved_call(
    &mut self,
    node_id: &NodeId,
    def_id: DefinitionId,
  ) {
    self.resolved_calls.insert(*node_id, def_id);
  }

  fn mark_referenced(
    &mut self,
    def_id: DefinitionId,
  ) {
    self.referenced_defs.insert(def_id);
  }

  fn mark_mutated(
    &mut self,
    def_id: DefinitionId,
  ) {
    self.mutated_defs.insert(def_id);
  }

  fn effective_lint_level(
    &self,
    lint: LintId,
  ) -> LintLevel {
    for (id, level) in self.lint_overrides.iter().rev() {
      if *id == lint {
        return *level;
      }
    }

    LintLevel::Warn
  }

  fn push_lint_overrides(
    &mut self,
    overrides: &[(LintId, LintLevel)],
  ) {
    self.lint_overrides.extend_from_slice(overrides);
  }

  #[allow(dead_code)]
  fn pop_lint_overrides(
    &mut self,
    count: usize,
  ) {
    let new_len = self.lint_overrides.len().saturating_sub(count);
    self.lint_overrides.truncate(new_len);
  }

  fn define_decl_in_current_scope(
    &mut self,
    node_id: &NodeId,
  ) -> Option<DefinitionId> {
    let def_id = match self.lookup_def(node_id) {
      Some(id) => *id,
      None => {
        return None;
      },
    };
    let name = self.defs.get(&def_id).name;

    // Only functions are overloadable - records, types, etc. are not
    let is_overloadable = matches!(
      self.defs.get(&def_id).kind,
      DefinitionKind::Function(_) | DefinitionKind::Method(_)
    );

    let current_scope_id = *self.scopes.current();
    let scope = self.scopes.get_scope(&current_scope_id);

    if let Some(existing_entry) = scope.symbols.get(&name) {
      match existing_entry {
        SymbolEntry::Overload(group) => {
          if group.contains(&def_id) {
            return Some(def_id);
          }
          // Add this def_id to the existing overload group
          let scope_mut = self.scopes.get_scope_mut(&current_scope_id);
          if let Some(SymbolEntry::Overload(group)) = scope_mut.symbols.get_mut(&name) {
            group.push(def_id);
          }
          return Some(def_id);
        },
        SymbolEntry::Single(existing_def_id) => {
          if *existing_def_id == def_id {
            return Some(def_id);
          }
          // If both are functions, convert single to overload group
          if is_overloadable
            && let DefinitionKind::Function(_) = self.defs.get(&def_id).kind
            && let DefinitionKind::Function(_) = self.defs.get(existing_def_id).kind
          {
            let old_id = *existing_def_id;
            let scope_mut = self.scopes.get_scope_mut(&current_scope_id);
            scope_mut
              .symbols
              .insert(name, SymbolEntry::Overload(vec![old_id, def_id]));
            return Some(def_id);
          }
          // Otherwise, don't add duplicate
          return Some(def_id);
        },
      }
    }

    let _ = self.scopes.define(&name, &def_id, is_overloadable);

    Some(def_id)
  }

  fn define_function_params_in_scope(
    &mut self,
    def_id: &DefinitionId,
  ) {
    let params = match &self.defs.get(def_id).kind {
      DefinitionKind::Function(func_def) => func_def.params.clone(),
      DefinitionKind::Method(method_def) => method_def.params.clone(),
      _ => Vec::new(),
    };

    for param_id in &params {
      let name = &self.defs.get(param_id).name;
      let _ = self.scopes.define(name, param_id, false);
    }
  }

  fn get_symbol_name(
    &self,
    symbol_id: &ignis_type::symbol::SymbolId,
  ) -> String {
    self.symbols.borrow().get(symbol_id).to_string()
  }

  fn is_builtin_name(
    &self,
    symbol_id: &ignis_type::symbol::SymbolId,
  ) -> bool {
    let symbols = self.symbols.borrow();
    let name = symbols.get(symbol_id);

    ignis_type::at_items::is_builtin(name)
  }

  fn node_span(
    &self,
    node_id: &NodeId,
  ) -> &ignis_type::span::Span {
    self.ast.get(node_id).span()
  }

  /// Reinitializes the scope tree for a new analysis phase.
  /// Must call `process_imports()` at the end to re-inject imported symbols.
  fn reset_scopes(
    &mut self,
    roots: &[NodeId],
  ) {
    self.scopes = ScopeTree::new();

    for root in roots {
      self.define_root(root);
    }

    self.process_imports(roots);
  }

  fn process_imports(
    &mut self,
    roots: &[NodeId],
  ) {
    let export_table = self.export_table.clone();
    let module_for_path = self.module_for_path.clone();
    self.import_phase(roots, &export_table, &module_for_path);
    self.process_implicit_imports();
  }

  fn define_root(
    &mut self,
    node_id: &NodeId,
  ) {
    let node = self.ast.get(node_id);
    if let ASTNode::Statement(stmt) = node {
      self.define_root_statement(node_id, stmt);
    }
  }

  fn define_root_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
  ) {
    match stmt {
      ASTStatement::Function(_)
      | ASTStatement::Variable(_)
      | ASTStatement::Constant(_)
      | ASTStatement::TypeAlias(_)
      | ASTStatement::Record(_)
      | ASTStatement::Enum(_)
      | ASTStatement::Trait(_) => {
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Extern(extern_stmt) => {
        for item in &extern_stmt.items {
          self.define_root(item);
        }
      },
      ASTStatement::Namespace(_) => {
        // Register the namespace itself in scope (it has its own Definition now)
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Export(export_stmt) => match export_stmt {
        ignis_ast::statements::ASTExport::Declaration { decl, .. } => {
          self.define_root(decl);
        },
        ignis_ast::statements::ASTExport::Name { .. } | ignis_ast::statements::ASTExport::ReExportFrom { .. } => {},
      },
      _ => {},
    }
  }
}

/// Build a mapping from NodeId to Span for all nodes that have definitions or types.
/// This is used by the LSP to find which node is at a given cursor position.
fn build_node_spans(
  ast: &ASTStore<ASTNode>,
  node_defs: &HashMap<NodeId, DefinitionId>,
  node_types: &HashMap<NodeId, TypeId>,
) -> HashMap<NodeId, ignis_type::span::Span> {
  let mut spans = HashMap::new();

  // Add spans for all nodes with definitions
  for node_id in node_defs.keys() {
    if let Some(span) = get_node_span(ast, node_id) {
      spans.insert(*node_id, span);
    }
  }

  // Add spans for all nodes with types (that aren't already added)
  for node_id in node_types.keys() {
    if !spans.contains_key(node_id)
      && let Some(span) = get_node_span(ast, node_id)
    {
      spans.insert(*node_id, span);
    }
  }

  spans
}

/// Get the span of an AST node, if it exists in the store.
fn get_node_span(
  ast: &ASTStore<ASTNode>,
  node_id: &NodeId,
) -> Option<ignis_type::span::Span> {
  // Try to get the node from the store
  // The store might panic if the node_id is invalid, so we check first
  if (node_id.index() as usize) < ast.len() {
    Some(ast.get(node_id).span().clone())
  } else {
    None
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use ignis_hir::display::print_hir;
  use ignis_hir::HIR;
  use ignis_parser::{IgnisLexer, IgnisParser};
  use ignis_type::file::SourceMap;
  use ignis_type::span::Span;

  fn semantic_artifacts_fixture() -> SemanticArtifacts {
    let file_id = SourceMap::new().add_virtual("semantic-artifacts", String::new());
    let span = Span::empty_at(file_id, Default::default());
    let mut symbols = SymbolTable::new();
    let public_name = symbols.get_or_intern("publicValue");
    let private_name = symbols.get_or_intern("privateValue");

    let types = TypeStore::new();
    let i32_type = types.i32();
    let mut defs = DefinitionStore::new();
    let public_def_id = defs.alloc(ignis_type::definition::Definition {
      kind: DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
        type_id: i32_type,
        mutable: false,
      }),
      name: public_name,
      span: span.clone(),
      name_span: span.clone(),
      visibility: Visibility::Public,
      owner_module: ModuleId::new(0),
      owner_namespace: None,
      doc: None,
    });
    let private_def_id = defs.alloc(ignis_type::definition::Definition {
      kind: DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
        type_id: i32_type,
        mutable: false,
      }),
      name: private_name,
      span: span.clone(),
      name_span: span.clone(),
      visibility: Visibility::Private,
      owner_module: ModuleId::new(0),
      owner_namespace: None,
      doc: None,
    });

    let mut node_defs = HashMap::new();
    node_defs.insert(NodeId::new(1), public_def_id);

    let mut node_types = HashMap::new();
    node_types.insert(NodeId::new(2), i32_type);

    let mut node_spans = HashMap::new();
    node_spans.insert(NodeId::new(1), Span::empty_at(file_id, Default::default()));

    let mut import_item_defs = HashMap::new();
    import_item_defs.insert(Span::empty_at(file_id, Default::default()), private_def_id);

    SemanticArtifacts {
      types,
      defs,
      namespaces: NamespaceStore::new(),
      diagnostics: Vec::new(),
      symbols: Rc::new(RefCell::new(symbols)),
      node_defs,
      node_types,
      node_spans,
      resolved_calls: HashMap::new(),
      import_item_defs,
      import_module_files: HashMap::new(),
      extension_methods: HashMap::new(),
      directive_registry: DirectiveRegistry::default(),
      directive_execution_report: DirectiveExecutionReport::default(),
    }
  }

  fn analyze_via_legacy_sequence(src: &str) -> (SemanticArtifacts, HIR) {
    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file("legacy.ign", src.to_string());

    let mut lexer = IgnisLexer::new(file_id, source_map.get(&file_id).text.as_str());
    lexer.scan_tokens();
    assert!(lexer.diagnostics.is_empty(), "Lexer errors: {:?}", lexer.diagnostics);

    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
    let (nodes, roots) = parser.parse().expect("Parse failed");

    let mut analyzer = Analyzer::new(&nodes, symbols.clone(), ModuleId::new(0));
    analyzer.bind_phase(&roots);
    analyzer.resolve_phase(&roots);
    analyzer.typecheck_phase(&roots);
    analyzer.const_eval_phase(&roots);
    analyzer.extra_checks_phase(&roots);
    analyzer.lint_phase(&roots);
    analyzer.directive_execution_report = crate::phases::run_directive_scheduling_phase(&mut analyzer);

    let mut hir = analyzer.lower_to_hir(&roots);
    let closure_diags = capture::populate_closure_captures(
      &mut hir,
      &mut analyzer.defs,
      &mut analyzer.types,
      &mut analyzer.symbols.borrow_mut(),
    );
    analyzer.diagnostics.extend(closure_diags);

    let node_spans = build_node_spans(&nodes, &analyzer.node_defs, &analyzer.node_types);
    let semantic = SemanticArtifacts {
      types: analyzer.types,
      defs: analyzer.defs,
      namespaces: analyzer.namespaces,
      diagnostics: analyzer.diagnostics,
      symbols,
      node_defs: analyzer.node_defs,
      node_types: analyzer.node_types,
      node_spans,
      resolved_calls: analyzer.resolved_calls,
      import_item_defs: analyzer.import_item_defs,
      import_module_files: analyzer.import_module_files,
      extension_methods: analyzer.extension_methods,
      directive_registry: analyzer.directive_registry,
      directive_execution_report: analyzer.directive_execution_report,
    };

    (semantic, hir)
  }

  fn analyze_via_extracted_sequence(src: &str) -> (SemanticArtifacts, HIR) {
    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file("extracted.ign", src.to_string());

    let mut lexer = IgnisLexer::new(file_id, source_map.get(&file_id).text.as_str());
    lexer.scan_tokens();
    assert!(lexer.diagnostics.is_empty(), "Lexer errors: {:?}", lexer.diagnostics);

    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
    let (nodes, roots) = parser.parse().expect("Parse failed");

    let mut analyzer = Analyzer::new(&nodes, symbols.clone(), ModuleId::new(0));
    let semantic = crate::phases::run_semantic_phases(&mut analyzer, &nodes, &roots, symbols);
    analyzer.directive_execution_report = crate::phases::run_directive_scheduling_phase(&mut analyzer);
    let hir = crate::phases::run_lowering_phase(&mut analyzer, &roots);

    (semantic, hir)
  }

  #[test]
  fn semantic_artifacts_collect_only_public_exports() {
    let semantic = semantic_artifacts_fixture();

    let exports = semantic.collect_exports();
    let symbol_table = semantic.symbols.borrow();
    let exported_names: Vec<_> = exports
      .exports
      .keys()
      .map(|symbol_id| symbol_table.get(symbol_id).to_string())
      .collect();

    assert_eq!(exported_names, vec!["publicValue".to_string()]);
  }

  #[test]
  fn analyzer_output_from_semantic_artifacts_preserves_semantic_maps() {
    let semantic = semantic_artifacts_fixture();
    let expected_node_defs = semantic.node_defs.clone();
    let expected_node_types = semantic.node_types.clone();
    let expected_node_spans = semantic.node_spans.clone();
    let expected_import_item_defs = semantic.import_item_defs.clone();
    let hir = HIR::new();

    let output = AnalyzerOutput::from_semantic_artifacts(semantic, hir);

    assert_eq!(output.node_defs, expected_node_defs);
    assert_eq!(output.node_types, expected_node_types);
    assert_eq!(output.node_spans, expected_node_spans);
    assert_eq!(output.import_item_defs, expected_import_item_defs);
  }

  #[test]
  fn extracted_semantic_phase_sequence_matches_legacy_semantic_results() {
    let src = r#"
      export function add(a: i32, b: i32): i32 {
        let sum = a + b;
        return sum;
      }
    "#;

    let (legacy_semantic, _) = analyze_via_legacy_sequence(src);
    let (extracted_semantic, _) = analyze_via_extracted_sequence(src);

    assert_eq!(extracted_semantic.diagnostics.len(), legacy_semantic.diagnostics.len());
    assert_eq!(extracted_semantic.node_defs, legacy_semantic.node_defs);
    assert_eq!(extracted_semantic.node_types, legacy_semantic.node_types);
    assert_eq!(
      extracted_semantic.collect_exports().exports,
      legacy_semantic.collect_exports().exports
    );
  }

  #[test]
  fn extracted_lowering_phase_matches_legacy_hir_output() {
    let src = r#"
      function main(): i32 {
        let left = 20;
        let right = 22;
        return left + right;
      }
    "#;

    let (legacy_semantic, legacy_hir) = analyze_via_legacy_sequence(src);
    let (extracted_semantic, extracted_hir) = analyze_via_extracted_sequence(src);
    let legacy_symbols = legacy_semantic.symbols.borrow();
    let extracted_symbols = extracted_semantic.symbols.borrow();

    assert_eq!(
      print_hir(
        &extracted_hir,
        &extracted_semantic.types,
        &extracted_semantic.defs,
        &extracted_symbols
      ),
      print_hir(&legacy_hir, &legacy_semantic.types, &legacy_semantic.defs, &legacy_symbols)
    );
  }
}
