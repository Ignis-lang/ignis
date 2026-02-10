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
mod checks;
mod const_eval;
pub mod dump;
pub mod imports;
mod lint;
mod lowering;
pub mod modules;
pub mod mono;
pub mod ownership_hir;
mod resolver;
mod scope;
mod typeck;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;

use ignis_ast::{ASTNode, NodeId, statements::ASTStatement, type_::IgnisTypeSyntax};
use ignis_type::{
  compilation_context::CompilationContext,
  symbol::{SymbolId, SymbolTable},
  Store as ASTStore,
};
use ignis_type::types::{TypeId, TypeStore};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, SymbolEntry, Visibility};
use ignis_type::lint::{LintId, LintLevel};
use ignis_type::module::ModuleId;
use ignis_type::namespace::{NamespaceId, NamespaceStore};
use ignis_hir::HIR;
use ignis_diagnostics::diagnostic_report::Diagnostic;

use imports::ExportTable;

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

pub struct Analyzer<'a> {
  ast: &'a ASTStore<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
  types: TypeStore,
  defs: DefinitionStore,
  namespaces: NamespaceStore,
  scopes: ScopeTree,
  node_defs: HashMap<NodeId, DefinitionId>,
  node_types: HashMap<NodeId, TypeId>,
  diagnostics: Vec<Diagnostic>,
  export_table: ExportTable,
  module_for_path: HashMap<String, ModuleId>,
  path_to_file: HashMap<String, ignis_type::file::FileId>,
  current_module: ModuleId,
  current_namespace: Option<NamespaceId>,
  in_callee_context: bool,
  resolving_type_aliases: HashSet<DefinitionId>,
  type_alias_syntax: HashMap<DefinitionId, IgnisTypeSyntax>,
  lowering_counter: u32,
  for_of_binding_defs: HashMap<NodeId, DefinitionId>,
  resolved_calls: HashMap<NodeId, DefinitionId>,

  import_item_defs: HashMap<ignis_type::span::Span, DefinitionId>,
  import_module_files: HashMap<ignis_type::span::Span, ignis_type::file::FileId>,
  compilation_ctx: Option<CompilationContext>,
  referenced_defs: HashSet<DefinitionId>,
  imported_defs: HashMap<DefinitionId, ignis_type::span::Span>,
  lint_overrides: Vec<(LintId, LintLevel)>,
  extension_methods: HashMap<TypeId, HashMap<SymbolId, Vec<DefinitionId>>>,
  trait_default_bodies: HashMap<DefinitionId, NodeId>,
  trait_default_clones: HashMap<DefinitionId, NodeId>,
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
}

impl AnalyzerOutput {
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
      node_defs: HashMap::new(),
      node_types: HashMap::new(),
      diagnostics: Vec::new(),
      export_table: HashMap::new(),
      module_for_path: HashMap::new(),
      path_to_file: HashMap::new(),
      current_module,
      current_namespace: None,
      in_callee_context: false,
      resolving_type_aliases: HashSet::new(),
      type_alias_syntax: HashMap::new(),
      lowering_counter: 0,
      for_of_binding_defs: HashMap::new(),
      resolved_calls: HashMap::new(),
      import_item_defs: HashMap::new(),
      import_module_files: HashMap::new(),
      compilation_ctx: Some(CompilationContext::default()),
      referenced_defs: HashSet::new(),
      imported_defs: HashMap::new(),
      lint_overrides: Vec::new(),
      extension_methods: HashMap::new(),
      trait_default_bodies: HashMap::new(),
      trait_default_clones: HashMap::new(),
    }
  }

  pub fn analyze(
    ast: &ASTStore<ASTNode>,
    roots: &Vec<NodeId>,
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> AnalyzerOutput {
    Self::analyze_with_imports(ast, roots, symbols, &HashMap::new(), &HashMap::new())
  }

  /// Analyze with imports from other modules
  pub fn analyze_with_imports(
    ast: &ASTStore<ASTNode>,
    roots: &Vec<NodeId>,
    symbols: Rc<RefCell<SymbolTable>>,
    export_table: &ExportTable,
    module_for_path: &HashMap<String, ModuleId>,
  ) -> AnalyzerOutput {
    let symbols_clone = symbols.clone();
    // Use a dummy ModuleId for standalone analysis
    let mut analyzer = Analyzer::new(ast, symbols, ModuleId::new(0));
    analyzer.export_table = export_table.clone();
    analyzer.module_for_path = module_for_path.clone();

    analyzer.bind_phase(roots);
    analyzer.resolve_phase(roots);
    analyzer.typecheck_phase(roots);
    analyzer.const_eval_phase(roots);
    analyzer.extra_checks_phase(roots);
    analyzer.lint_phase(roots);

    let hir = analyzer.lower_to_hir(roots);

    // Build node_spans by looking up spans from AST for all nodes in node_defs and node_types
    let node_spans = build_node_spans(ast, &analyzer.node_defs, &analyzer.node_types);

    AnalyzerOutput {
      types: analyzer.types,
      defs: analyzer.defs,
      namespaces: analyzer.namespaces,
      hir,
      diagnostics: analyzer.diagnostics,
      symbols: symbols_clone,
      node_defs: analyzer.node_defs,
      node_types: analyzer.node_types,
      node_spans,
      resolved_calls: analyzer.resolved_calls,
      import_item_defs: analyzer.import_item_defs,
      import_module_files: analyzer.import_module_files,
      extension_methods: analyzer.extension_methods,
    }
  }

  /// Analyze with shared stores, enabling cross-module compilation.
  #[allow(clippy::too_many_arguments)]
  pub fn analyze_with_shared_stores(
    ast: &ASTStore<ASTNode>,
    roots: &Vec<NodeId>,
    symbols: Rc<RefCell<SymbolTable>>,
    export_table: &ExportTable,
    module_for_path: &HashMap<String, ModuleId>,
    path_to_file: &HashMap<String, ignis_type::file::FileId>,
    shared_types: &mut TypeStore,
    shared_defs: &mut DefinitionStore,
    shared_namespaces: &mut NamespaceStore,
    shared_extension_methods: &mut HashMap<TypeId, HashMap<SymbolId, Vec<DefinitionId>>>,
    current_module: ModuleId,
  ) -> AnalyzerOutput {
    let symbols_clone = symbols.clone();

    let mut analyzer = Analyzer {
      ast,
      symbols,
      types: std::mem::replace(shared_types, TypeStore::new()),
      defs: std::mem::replace(shared_defs, DefinitionStore::new()),
      namespaces: std::mem::replace(shared_namespaces, NamespaceStore::new()),
      scopes: ScopeTree::new(),
      node_defs: HashMap::new(),
      node_types: HashMap::new(),
      diagnostics: Vec::new(),
      export_table: export_table.clone(),
      module_for_path: module_for_path.clone(),
      path_to_file: path_to_file.clone(),
      current_module,
      current_namespace: None,
      in_callee_context: false,
      resolving_type_aliases: HashSet::new(),
      type_alias_syntax: HashMap::new(),
      lowering_counter: 0,
      for_of_binding_defs: HashMap::new(),
      resolved_calls: HashMap::new(),
      import_item_defs: HashMap::new(),
      import_module_files: HashMap::new(),
      compilation_ctx: Some(CompilationContext::default()),
      referenced_defs: HashSet::new(),
      imported_defs: HashMap::new(),
      lint_overrides: Vec::new(),
      extension_methods: std::mem::take(shared_extension_methods),
      trait_default_bodies: HashMap::new(),
      trait_default_clones: HashMap::new(),
    };

    analyzer.bind_phase(roots);
    analyzer.resolve_phase(roots);
    analyzer.typecheck_phase(roots);
    analyzer.const_eval_phase(roots);
    analyzer.extra_checks_phase(roots);
    analyzer.lint_phase(roots);
    let hir = analyzer.lower_to_hir(roots);

    *shared_types = std::mem::replace(&mut analyzer.types, TypeStore::new());
    *shared_defs = std::mem::replace(&mut analyzer.defs, DefinitionStore::new());
    *shared_namespaces = std::mem::replace(&mut analyzer.namespaces, NamespaceStore::new());
    *shared_extension_methods = std::mem::take(&mut analyzer.extension_methods);

    // Build node_spans by looking up spans from AST for all nodes in node_defs and node_types
    let node_spans = build_node_spans(ast, &analyzer.node_defs, &analyzer.node_types);

    AnalyzerOutput {
      types: shared_types.clone(),
      defs: shared_defs.clone(),
      namespaces: shared_namespaces.clone(),
      hir,
      diagnostics: analyzer.diagnostics,
      symbols: symbols_clone,
      node_defs: analyzer.node_defs,
      node_types: analyzer.node_types,
      node_spans,
      resolved_calls: analyzer.resolved_calls,
      import_item_defs: analyzer.import_item_defs,
      import_module_files: analyzer.import_module_files,
      extension_methods: shared_extension_methods.clone(),
    }
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

    matches!(name, "typeOf" | "sizeOf" | "alignOf" | "maxOf" | "minOf")
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
        ignis_ast::statements::ASTExport::Name { .. } => {},
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
