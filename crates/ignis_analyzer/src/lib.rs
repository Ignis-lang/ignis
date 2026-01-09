mod binder;
mod borrowck;
mod checks;
mod const_eval;
pub mod dump;
mod lowering;
mod resolver;
mod scope;
mod typeck;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use ignis_ast::{ASTNode, NodeId, statements::ASTStatement};
use ignis_type::{symbol::SymbolTable, Store as ASTStore};
use ignis_type::types::{TypeId, TypeStore};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore};
use ignis_hir::HIR;
use ignis_diagnostics::diagnostic_report::Diagnostic;

pub use scope::{ScopeTree, ScopeId, ScopeKind};

pub struct Analyzer<'a> {
  ast: &'a ASTStore<ASTNode>,
  symbols: Rc<RefCell<SymbolTable>>,
  types: TypeStore,
  defs: DefinitionStore,
  scopes: ScopeTree,
  node_defs: HashMap<NodeId, DefinitionId>,
  node_types: HashMap<NodeId, TypeId>,
  diagnostics: Vec<Diagnostic>,
  current_function_return_type: Option<TypeId>,
}

pub struct AnalyzerOutput {
  pub types: TypeStore,
  pub defs: DefinitionStore,
  pub hir: HIR,
  pub diagnostics: Vec<Diagnostic>,
  pub symbols: Rc<RefCell<SymbolTable>>,
}

impl<'a> Analyzer<'a> {
  pub fn new(
    ast: &'a ASTStore<ASTNode>,
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> Self {
    Self {
      ast,
      symbols,
      types: TypeStore::new(),
      defs: DefinitionStore::new(),
      scopes: ScopeTree::new(),
      node_defs: HashMap::new(),
      node_types: HashMap::new(),
      diagnostics: Vec::new(),
      current_function_return_type: None,
    }
  }

  pub fn analyze(
    ast: &ASTStore<ASTNode>,
    roots: &Vec<NodeId>,
    symbols: Rc<RefCell<SymbolTable>>,
  ) -> AnalyzerOutput {
    let symbols_clone = symbols.clone();
    let mut analyzer = Analyzer::new(ast, symbols);

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

  fn node_span(
    &self,
    node_id: &NodeId,
  ) -> &ignis_type::span::Span {
    self.ast.get(node_id).span()
  }

  fn reset_scopes(
    &mut self,
    roots: &[NodeId],
  ) {
    self.scopes = ScopeTree::new();

    for root in roots {
      self.define_root(root);
    }
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
      ASTStatement::Function(_) | ASTStatement::Variable(_) | ASTStatement::Constant(_) => {
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Extern(extern_stmt) => {
        self.define_root(&extern_stmt.item);
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
}
