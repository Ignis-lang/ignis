macro_rules! with_for_of_scope {
  ($self:expr, $node_id:expr, $for_of:expr, $body:block) => {{
    $self.scopes.push(ScopeKind::Loop);

    if let Some(def_id) = $self.for_of_binding_defs.get($node_id).cloned() {
      let _ = $self.scopes.define(&$for_of.binding.name, &def_id);
    }

    $body

    $self.scopes.pop();
  }};
}

mod binder;
mod borrowck;
mod checks;
mod const_eval;
pub mod dump;
pub mod imports;
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
use ignis_type::{symbol::SymbolTable, Store as ASTStore};
use ignis_type::types::{TypeId, TypeStore};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, Visibility};
use ignis_type::module::ModuleId;
use ignis_type::namespace::{NamespaceId, NamespaceStore};
use ignis_hir::HIR;
use ignis_diagnostics::diagnostic_report::Diagnostic;

use imports::ExportTable;

pub use ignis_hir::{DropSchedules, ExitKey};
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

#[derive(Debug, Clone)]
pub struct RuntimeBuiltins {
  pub buf_len: DefinitionId,
  pub buf_at: DefinitionId,
  pub buf_at_const: DefinitionId,
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
  current_module: ModuleId,
  current_namespace: Option<NamespaceId>,
  in_callee_context: bool,
  resolving_type_aliases: HashSet<DefinitionId>,
  type_alias_syntax: HashMap<DefinitionId, IgnisTypeSyntax>,
  lowering_counter: u32,
  for_of_binding_defs: HashMap<NodeId, DefinitionId>,
  runtime: Option<RuntimeBuiltins>,
}

pub struct AnalyzerOutput {
  pub types: TypeStore,
  pub defs: DefinitionStore,
  pub namespaces: NamespaceStore,
  pub hir: HIR,
  pub diagnostics: Vec<Diagnostic>,
  pub symbols: Rc<RefCell<SymbolTable>>,
}

impl AnalyzerOutput {
  pub fn collect_exports(&self) -> imports::ModuleExportData {
    let mut exports = HashMap::new();

    for (def_id, def) in self.defs.iter() {
      if def.visibility == Visibility::Public {
        exports.insert(def.name.clone(), def_id);
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
    let mut analyzer = Self {
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
      current_module,
      current_namespace: None,
      in_callee_context: false,
      resolving_type_aliases: HashSet::new(),
      type_alias_syntax: HashMap::new(),
      lowering_counter: 0,
      for_of_binding_defs: HashMap::new(),
      runtime: None,
    };
    analyzer.runtime = Some(analyzer.register_runtime_builtins());
    analyzer
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
    analyzer.borrowcheck_phase(roots);
    analyzer.const_eval_phase(roots);
    analyzer.extra_checks_phase(roots);
    let hir = analyzer.lower_to_hir(roots);

    AnalyzerOutput {
      types: analyzer.types,
      defs: analyzer.defs,
      namespaces: analyzer.namespaces,
      hir,
      diagnostics: analyzer.diagnostics,
      symbols: symbols_clone,
    }
  }

  /// Analyze with shared stores, enabling cross-module compilation.
  pub fn analyze_with_shared_stores(
    ast: &ASTStore<ASTNode>,
    roots: &Vec<NodeId>,
    symbols: Rc<RefCell<SymbolTable>>,
    export_table: &ExportTable,
    module_for_path: &HashMap<String, ModuleId>,
    shared_types: &mut TypeStore,
    shared_defs: &mut DefinitionStore,
    shared_namespaces: &mut NamespaceStore,
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
      current_module,
      current_namespace: None,
      in_callee_context: false,
      resolving_type_aliases: HashSet::new(),
      type_alias_syntax: HashMap::new(),
      lowering_counter: 0,
      for_of_binding_defs: HashMap::new(),
      runtime: None,
    };
    analyzer.runtime = Some(analyzer.register_runtime_builtins());

    analyzer.bind_phase(roots);
    analyzer.resolve_phase(roots);
    analyzer.typecheck_phase(roots);
    analyzer.borrowcheck_phase(roots);
    analyzer.const_eval_phase(roots);
    analyzer.extra_checks_phase(roots);
    let hir = analyzer.lower_to_hir(roots);

    *shared_types = std::mem::replace(&mut analyzer.types, TypeStore::new());
    *shared_defs = std::mem::replace(&mut analyzer.defs, DefinitionStore::new());
    *shared_namespaces = std::mem::replace(&mut analyzer.namespaces, NamespaceStore::new());

    AnalyzerOutput {
      types: shared_types.clone(),
      defs: shared_defs.clone(),
      namespaces: shared_namespaces.clone(),
      hir,
      diagnostics: analyzer.diagnostics,
      symbols: symbols_clone,
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
    self.node_types.get(&node_id)
  }

  fn set_type(
    &mut self,
    node_id: &NodeId,
    type_id: &TypeId,
  ) {
    self.node_types.insert(node_id.clone(), type_id.clone());
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
    self.node_defs.insert(node_id.clone(), def_id.clone());
  }

  fn define_decl_in_current_scope(
    &mut self,
    node_id: &NodeId,
  ) -> Option<DefinitionId> {
    let def_id = self.lookup_def(node_id)?.clone();
    let name = self.defs.get(&def_id).name.clone();
    let _ = self.scopes.define(&name, &def_id);

    Some(def_id)
  }

  fn define_function_params_in_scope(
    &mut self,
    def_id: &DefinitionId,
  ) {
    let params = match &self.defs.get(&def_id).kind {
      DefinitionKind::Function(func_def) => func_def.params.clone(),
      DefinitionKind::Method(method_def) => method_def.params.clone(),
      _ => Vec::new(),
    };

    for param_id in &params {
      let name = &self.defs.get(param_id).name;
      let _ = self.scopes.define(name, param_id);
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
    name == "typeOf" || name == "sizeOf" || name == "__builtin_read" || name == "__builtin_write"
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
    let node = self.ast.get(&node_id);
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
      | ASTStatement::Enum(_) => {
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
          self.define_root(&decl);
        },
        ignis_ast::statements::ASTExport::Name { .. } => {},
      },
      _ => {},
    }
  }

  fn register_runtime_builtins(&mut self) -> RuntimeBuiltins {
    let span = ignis_type::span::Span::default();
    let buffer_ptr_type = self.types.vector(self.types.unknown(), None);
    let u64_type = self.types.u64();
    let void_ptr_type = self.types.pointer(self.types.void());

    let buf_len = self.create_extern_builtin("ignis_buf_len", vec![("buf", buffer_ptr_type)], u64_type, span.clone());
    let buf_at = self.create_extern_builtin(
      "ignis_buf_at",
      vec![("buf", buffer_ptr_type), ("idx", u64_type)],
      void_ptr_type,
      span.clone(),
    );
    let buf_at_const = self.create_extern_builtin(
      "ignis_buf_at_const",
      vec![("buf", buffer_ptr_type), ("idx", u64_type)],
      void_ptr_type,
      span,
    );

    RuntimeBuiltins {
      buf_len,
      buf_at,
      buf_at_const,
    }
  }

  fn create_extern_builtin(
    &mut self,
    name: &str,
    params: Vec<(&str, TypeId)>,
    return_type: TypeId,
    span: ignis_type::span::Span,
  ) -> DefinitionId {
    use ignis_type::definition::{Definition, FunctionDefinition, ParameterDefinition};

    let fn_name = self.symbols.borrow_mut().intern(name);
    let param_defs: Vec<DefinitionId> = params
      .iter()
      .map(|(pname, ptype)| {
        let pname_sym = self.symbols.borrow_mut().intern(pname);
        let def = Definition {
          kind: DefinitionKind::Parameter(ParameterDefinition {
            type_id: *ptype,
            mutable: false,
          }),
          name: pname_sym,
          span: span.clone(),
          visibility: Visibility::Private,
          owner_module: self.current_module,
          owner_namespace: None,
        };
        self.defs.alloc(def)
      })
      .collect();

    let def = Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: Vec::new(),
        params: param_defs,
        return_type,
        is_extern: true,
        is_variadic: false,
      }),
      name: fn_name,
      span,
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
    };

    self.defs.alloc(def)
  }
}
