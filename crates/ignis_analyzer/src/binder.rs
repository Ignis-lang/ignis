use crate::{Analyzer, ScopeKind};
use ignis_ast::{
  metadata::ASTMetadata,
  statements::{const_statement::ASTConstant, function::ASTFunction, variable::ASTVariable, ASTStatement},
  ASTNode, NodeId,
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::definition::{
  ConstantDefinition, Definition, DefinitionKind, FunctionDefinition, NamespaceDefinition, ParameterDefinition,
  VariableDefinition, Visibility,
};

impl<'a> Analyzer<'a> {
  pub fn bind_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    for root in roots {
      self.bind_node(root, ScopeKind::Global);
    }
  }

  fn bind_node(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
  ) {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Statement(stmt) => self.bind_statement(&node_id, stmt, scope_kind),
      ASTNode::Expression(_) => {},
    }
  }

  fn bind_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    scope_kind: ScopeKind,
  ) {
    match stmt {
      ASTStatement::Function(func) => self.bind_function(node_id, func, scope_kind),
      ASTStatement::Variable(var) => self.bind_variable(node_id, var),
      ASTStatement::Constant(const_) => self.bind_constant(node_id, const_),
      ASTStatement::Block(block) => self.bind_block(block),
      ASTStatement::If(if_stmt) => self.bind_if(if_stmt),
      ASTStatement::While(while_stmt) => self.bind_while(while_stmt),
      ASTStatement::For(for_stmt) => self.bind_for(for_stmt),
      ASTStatement::Extern(extern_stmt) => {
        self.bind_extern(extern_stmt);
      },
      ASTStatement::Namespace(ns_stmt) => {
        self.bind_namespace(node_id, ns_stmt);
      },
      ASTStatement::Export(export_stmt) => self.bind_export(export_stmt, scope_kind),
      ASTStatement::Expression(_) => {},
      _ => {},
    }
  }

  fn bind_function(
    &mut self,
    node_id: &NodeId,
    func: &ASTFunction,
    _scope_kind: ScopeKind,
  ) {
    let span = &func.signature.span;
    let is_extern = func.signature.metadata.contains(ASTMetadata::EXTERN_MEMBER);
    let is_variadic = func
      .signature
      .parameters
      .last()
      .map(|param| param.metadata.contains(ASTMetadata::VARIADIC))
      .unwrap_or(false);

    let mut param_defs = Vec::new();
    for param in &func.signature.parameters {
      let def = Definition {
        kind: DefinitionKind::Parameter(ParameterDefinition {
          type_id: self.types.error(),
          mutable: param.metadata.is_mutable(),
        }),
        name: param.name.clone(),
        span: param.span.clone(),
        visibility: Visibility::Private,
        owner_module: self.current_module,
        owner_namespace: None,
      };
      let def_id = self.defs.alloc(def);
      param_defs.push(def_id);
    }

    let func_def = FunctionDefinition {
      params: param_defs.clone(),
      return_type: self.types.error(),
      is_extern,
      is_variadic,
    };

    let def = Definition {
      kind: DefinitionKind::Function(func_def),
      name: func.signature.name.clone(),
      span: span.clone(),
      visibility: if func.signature.metadata.is_public() {
        Visibility::Public
      } else {
        Visibility::Private
      },
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let def_id = &self.defs.alloc(def);
    self.set_def(node_id, def_id);

    if let Err(existing) = self.scopes.define(&func.signature.name, &def_id) {
      let existing_def = self.defs.get(&existing);
      let symbol = self.get_symbol_name(&existing_def.name);

      self.add_diagnostic(
        DiagnosticMessage::FunctionAlreadyDefined {
          name: symbol,
          span: span.clone(),
          previous_span: existing_def.span.clone(),
        }
        .report(),
      );
    }

    if let Some(body_id) = &func.body {
      self.scopes.push(ScopeKind::Function);

      for param_id in &param_defs {
        let param_def = self.defs.get(param_id);
        let param_span = param_def.span.clone();
        let name = param_def.name.clone();
        if let Err(existing) = self.scopes.define(&name, &param_id) {
          let existing_def = self.defs.get(&existing);
          let symbol = self.get_symbol_name(&existing_def.name);

          self.add_diagnostic(
            DiagnosticMessage::ParameterAlreadyDefined {
              name: symbol,
              span: param_span,
              previous_span: existing_def.span.clone(),
            }
            .report(),
          );
        }
      }

      self.bind_node(&body_id, ScopeKind::Function);
      self.scopes.pop();
    }
  }

  fn bind_variable(
    &mut self,
    node_id: &NodeId,
    var: &ASTVariable,
  ) {
    let span = &var.span;

    let var_def = VariableDefinition {
      type_id: self.types.error(),
      mutable: var.metadata.is_mutable(),
    };

    let def = Definition {
      kind: DefinitionKind::Variable(var_def),
      name: var.name.clone(),
      span: span.clone(),
      visibility: if var.metadata.is_public() {
        Visibility::Public
      } else {
        Visibility::Private
      },
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let def_id = &self.defs.alloc(def);
    self.set_def(node_id, def_id);

    if let Err(existing) = &self.scopes.define(&var.name, def_id) {
      let existing_def = self.defs.get(existing);
      let symbol = self.get_symbol_name(&existing_def.name);

      self.add_diagnostic(
        DiagnosticMessage::VariableAlreadyDefined {
          name: symbol,
          span: span.clone(),
          previous_span: existing_def.span.clone(),
        }
        .report(),
      );
    }

    if let Some(value_id) = &var.value {
      self.bind_node(value_id, ScopeKind::Block);
    }
  }

  fn bind_constant(
    &mut self,
    node_id: &NodeId,
    const_: &ASTConstant,
  ) {
    let span = const_.span.clone();

    let const_def = ConstantDefinition {
      type_id: self.types.error(),
      value: None,
    };

    let def = Definition {
      kind: DefinitionKind::Constant(const_def),
      name: const_.name.clone(),
      span: span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);

    if let Err(existing) = self.scopes.define(&const_.name, &def_id) {
      let existing_def = self.defs.get(&existing);
      let symbol = self.get_symbol_name(&existing_def.name);
      self.add_diagnostic(
        DiagnosticMessage::ConstantAlreadyDefined {
          name: symbol,
          span: span.clone(),
          previous_span: existing_def.span.clone(),
        }
        .report(),
      );
    }

    // Only bind value if it exists (not for extern const)
    if let Some(value_id) = &const_.value {
      self.bind_node(value_id, ScopeKind::Block);
    }
  }

  fn bind_block(
    &mut self,
    block: &ignis_ast::statements::ASTBlock,
  ) {
    self.scopes.push(ScopeKind::Block);

    for stmt_id in &block.statements {
      self.bind_node(stmt_id, ScopeKind::Block);
    }

    self.scopes.pop();
  }

  fn bind_if(
    &mut self,
    if_stmt: &ignis_ast::statements::ASTIf,
  ) {
    self.bind_node(&if_stmt.condition, ScopeKind::Block);
    self.bind_node(&if_stmt.then_block, ScopeKind::Block);

    if let Some(else_branch) = &if_stmt.else_block {
      self.bind_node(else_branch, ScopeKind::Block);
    }
  }

  fn bind_while(
    &mut self,
    while_stmt: &ignis_ast::statements::ASTWhile,
  ) {
    self.scopes.push(ScopeKind::Loop);

    self.bind_node(&while_stmt.condition, ScopeKind::Loop);
    self.bind_node(&while_stmt.body, ScopeKind::Loop);

    self.scopes.pop();
  }

  fn bind_for(
    &mut self,
    for_stmt: &ignis_ast::statements::ASTFor,
  ) {
    self.scopes.push(ScopeKind::Loop);

    self.bind_node(&for_stmt.initializer, ScopeKind::Loop);
    self.bind_node(&for_stmt.condition, ScopeKind::Loop);
    self.bind_node(&for_stmt.increment, ScopeKind::Loop);
    self.bind_node(&for_stmt.body, ScopeKind::Loop);

    self.scopes.pop();
  }

  fn bind_export(
    &mut self,
    export_stmt: &ignis_ast::statements::ASTExport,
    scope_kind: ScopeKind,
  ) {
    match export_stmt {
      ignis_ast::statements::ASTExport::Declaration { decl, .. } => {
        self.bind_node(decl, scope_kind);

        if let Some(def_id) = self.lookup_def(decl).cloned() {
          self.defs.get_mut(&def_id).visibility = Visibility::Public;
        }
      },
      ignis_ast::statements::ASTExport::Name { name, .. } => {
        if let Some(def_id) = self.scopes.lookup(name).cloned() {
          self.defs.get_mut(&def_id).visibility = Visibility::Public;
        }
      },
    }
  }

  fn mark_extern(
    &mut self,
    node_id: &NodeId,
  ) {
    let node = self.ast.get(node_id);

    if let ASTNode::Statement(ASTStatement::Function(_)) = node {
      if let Some(def_id) = self.lookup_def(node_id).cloned() {
        if let DefinitionKind::Function(func_def) = &mut self.defs.get_mut(&def_id).kind {
          func_def.is_extern = true;
        }
      }
    }
  }

  fn bind_extern(
    &mut self,
    extern_stmt: &ignis_ast::statements::extern_statement::ASTExtern,
  ) {
    let ns_id = self.namespaces.get_or_create(&extern_stmt.path, true);
    let prev_ns = self.current_namespace;
    self.current_namespace = Some(ns_id);

    let scope_kind = ScopeKind::Namespace(ns_id);
    self.scopes.push(scope_kind);

    for item in &extern_stmt.items {
      self.bind_node(item, scope_kind);
      self.mark_extern(item);

      if let Some(def_id) = self.lookup_def(item).cloned() {
        let def = self.defs.get(&def_id);
        self.namespaces.define(ns_id, def.name.clone(), def_id);
      }
    }

    self.scopes.pop();
    self.current_namespace = prev_ns;
  }

  fn bind_namespace(
    &mut self,
    node_id: &NodeId,
    ns_stmt: &ignis_ast::statements::namespace_statement::ASTNamespace,
  ) {
    let ns_id = self.namespaces.get_or_create(&ns_stmt.path, false);
    let ns_name = *ns_stmt.path.last().expect("namespace path cannot be empty");
    let ns_def = Definition {
      kind: DefinitionKind::Namespace(NamespaceDefinition {
        namespace_id: ns_id,
        is_extern: false,
      }),
      name: ns_name,
      span: ns_stmt.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let def_id = self.defs.alloc(ns_def);
    self.set_def(node_id, &def_id);
    let _ = self.scopes.define(&ns_name, &def_id);

    let prev_ns = self.current_namespace;
    self.current_namespace = Some(ns_id);

    let scope_kind = ScopeKind::Namespace(ns_id);
    self.scopes.push(scope_kind);

    for item in &ns_stmt.items {
      self.bind_node(item, scope_kind);

      if let Some(def_id) = self.lookup_def(item).cloned() {
        let def = self.defs.get(&def_id);
        self.namespaces.define(ns_id, def.name.clone(), def_id);
      }
    }

    self.scopes.pop();
    self.current_namespace = prev_ns;
  }
}
