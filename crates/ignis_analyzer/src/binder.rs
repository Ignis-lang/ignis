use std::collections::HashMap;

use crate::{Analyzer, ScopeKind};
use ignis_ast::{
  generics::ASTGenericParams,
  metadata::ASTMetadata,
  statements::{
    const_statement::ASTConstant,
    enum_::{ASTEnum, ASTEnumItem},
    function::ASTFunction,
    record::{ASTMethod, ASTRecord, ASTRecordItem},
    type_alias::ASTTypeAlias,
    variable::ASTVariable,
    ASTStatement,
  },
  ASTNode, NodeId,
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::definition::{
  ConstantDefinition, Definition, DefinitionId, DefinitionKind, EnumDefinition, EnumVariantDef, FunctionDefinition,
  MethodDefinition, NamespaceDefinition, ParameterDefinition, RecordDefinition, RecordFieldDef, TypeAliasDefinition,
  TypeParamDefinition, VariableDefinition, Visibility,
};

impl<'a> Analyzer<'a> {
  /// Two-pass binding phase:
  /// - Pass 1: Predeclare type definitions (TypeAlias, Record, Enum) so they can reference each other
  /// - Pass 2: Complete all bindings including type definition details
  pub fn bind_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    // Pass 1: Predeclare type definitions
    for root in roots {
      self.bind_predecl(root, ScopeKind::Global);
    }

    // Pass 2: Complete bindings
    for root in roots {
      self.bind_complete(root, ScopeKind::Global);
    }
  }

  /// Pass 1: Predeclare type definitions so they can be referenced before fully defined
  fn bind_predecl(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
  ) {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Statement(stmt) => self.bind_predecl_statement(node_id, stmt, scope_kind),
      ASTNode::Expression(_) => {},
    }
  }

  fn bind_predecl_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    scope_kind: ScopeKind,
  ) {
    match stmt {
      ASTStatement::TypeAlias(ta) => self.bind_type_alias_predecl(node_id, ta),
      ASTStatement::Record(rec) => self.bind_record_predecl(node_id, rec),
      ASTStatement::Enum(en) => self.bind_enum_predecl(node_id, en),
      ASTStatement::Namespace(ns_stmt) => {
        // Recursively predeclare types in namespace
        self.bind_namespace_predecl(node_id, ns_stmt);
      },
      ASTStatement::Extern(extern_stmt) => {
        // Recursively predeclare types in extern block
        for item in &extern_stmt.items {
          self.bind_predecl(item, scope_kind);
        }
      },
      ASTStatement::Export(export_stmt) => {
        if let ignis_ast::statements::ASTExport::Declaration { decl, .. } = export_stmt {
          self.bind_predecl(decl, scope_kind);
        }
      },
      _ => {},
    }
  }

  /// Pass 2: Complete all bindings
  fn bind_complete(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
  ) {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Statement(stmt) => self.bind_complete_statement(node_id, stmt, scope_kind),
      ASTNode::Expression(_) => {},
    }
  }

  fn bind_complete_statement(
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
      ASTStatement::ForOf(for_of_stmt) => self.bind_for_of(node_id, for_of_stmt),
      ASTStatement::TypeAlias(ta) => self.bind_type_alias_complete(node_id, ta),
      ASTStatement::Record(rec) => self.bind_record_complete(node_id, rec),
      ASTStatement::Enum(en) => self.bind_enum_complete(node_id, en),
      ASTStatement::Extern(extern_stmt) => {
        self.bind_extern(extern_stmt);
      },
      ASTStatement::Namespace(ns_stmt) => {
        self.bind_namespace_complete(node_id, ns_stmt);
      },
      ASTStatement::Export(export_stmt) => self.bind_export(export_stmt, scope_kind),
      ASTStatement::Expression(_) => {},
      _ => {},
    }
  }

  // ========================================================================
  // Type Alias Binding
  // ========================================================================

  fn bind_type_alias_predecl(
    &mut self,
    node_id: &NodeId,
    ta: &ASTTypeAlias,
  ) {
    let def = Definition {
      kind: DefinitionKind::TypeAlias(TypeAliasDefinition {
        target: self.types.error(), // Placeholder until complete pass
      }),
      name: ta.name,
      span: ta.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);
    self.type_alias_syntax.insert(def_id.clone(), ta.target.clone());

    if let Err(existing) = self.scopes.define(&ta.name, &def_id) {
      let existing_def = self.defs.get(&existing);
      let symbol = self.get_symbol_name(&existing_def.name);
      self.add_diagnostic(
        DiagnosticMessage::TypeAlreadyDefined {
          name: symbol,
          span: ta.span.clone(),
          previous_span: existing_def.span.clone(),
        }
        .report(),
      );
    }
  }

  fn bind_type_alias_complete(
    &mut self,
    _node_id: &NodeId,
    _ta: &ASTTypeAlias,
  ) {
    // Target resolution done in typecheck phase
  }

  // ========================================================================
  // Record Binding
  // ========================================================================

  fn bind_record_predecl(
    &mut self,
    node_id: &NodeId,
    rec: &ASTRecord,
  ) {
    // Allocate the definition first so we can use its ID as owner for type params
    let def = Definition {
      kind: DefinitionKind::Record(RecordDefinition {
        type_params: Vec::new(),     // Will be populated after binding type params
        type_id: self.types.error(), // Placeholder
        fields: Vec::new(),
        instance_methods: HashMap::new(),
        static_methods: HashMap::new(),
        static_fields: HashMap::new(),
      }),
      name: rec.name,
      span: rec.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);

    // Bind type params now that we have the owner def_id
    let type_param_defs = self.bind_type_params(rec.type_params.as_ref(), def_id);
    self.pop_type_params_scope(rec.type_params.as_ref());

    // Update the record definition with type params
    if let DefinitionKind::Record(rd) = &mut self.defs.get_mut(&def_id).kind {
      rd.type_params = type_param_defs;
    }

    if let Err(existing) = self.scopes.define(&rec.name, &def_id) {
      let existing_def = self.defs.get(&existing);
      let symbol = self.get_symbol_name(&existing_def.name);
      self.add_diagnostic(
        DiagnosticMessage::TypeAlreadyDefined {
          name: symbol,
          span: rec.span.clone(),
          previous_span: existing_def.span.clone(),
        }
        .report(),
      );
    }
  }

  fn bind_record_complete(
    &mut self,
    node_id: &NodeId,
    rec: &ASTRecord,
  ) {
    let Some(record_def_id) = self.lookup_def(node_id).cloned() else {
      return;
    };

    // Create the Type::Record and update the definition's type_id
    let type_id = self.types.record(record_def_id);

    // Re-establish generic scope for type params visibility within methods/fields
    // We need to register the type params in scope again for this pass
    self.push_type_params_scope(record_def_id);

    let mut fields = Vec::new();
    let mut instance_methods = HashMap::new();
    let mut static_methods = HashMap::new();
    let mut static_fields = HashMap::new();
    let mut field_index = 0u32;

    for item in &rec.items {
      match item {
        ASTRecordItem::Field(field) => {
          if field.is_static() {
            // Static field - create as constant
            // Static fields require initializer (checked in extra_checks)
            let const_def = Definition {
              kind: DefinitionKind::Constant(ConstantDefinition {
                type_id: self.types.error(), // Resolved in typeck
                value: None,
                owner_type: Some(record_def_id),
              }),
              name: field.name,
              span: field.span.clone(),
              visibility: Visibility::Public,
              owner_module: self.current_module,
              owner_namespace: self.current_namespace,
            };
            let const_def_id = self.defs.alloc(const_def);
            static_fields.insert(field.name, const_def_id);
          } else {
            // Instance field
            fields.push(RecordFieldDef {
              name: field.name,
              type_id: self.types.error(), // Resolved in typeck
              index: field_index,
            });
            field_index += 1;
          }
        },
        ASTRecordItem::Method(method) => {
          let is_static = method.is_static();
          let method_def_id = self.bind_method(method, record_def_id, is_static);

          if is_static {
            static_methods.insert(method.name, method_def_id);
          } else {
            instance_methods.insert(method.name, method_def_id);
          }
        },
      }
    }

    // Pop the generic scope
    self.pop_type_params_scope_if_generic(record_def_id);

    // Update the record definition
    if let DefinitionKind::Record(rd) = &mut self.defs.get_mut(&record_def_id).kind {
      rd.type_id = type_id;
      rd.fields = fields;
      rd.instance_methods = instance_methods;
      rd.static_methods = static_methods;
      rd.static_fields = static_fields;
    }
  }

  // ========================================================================
  // Enum Binding
  // ========================================================================

  fn bind_enum_predecl(
    &mut self,
    node_id: &NodeId,
    en: &ASTEnum,
  ) {
    // Allocate the definition first so we can use its ID as owner for type params
    let def = Definition {
      kind: DefinitionKind::Enum(EnumDefinition {
        type_params: Vec::new(),     // Will be populated after binding type params
        type_id: self.types.error(), // Placeholder
        variants: Vec::new(),
        variants_by_name: HashMap::new(),
        tag_type: self.types.u32(),
        static_methods: HashMap::new(),
        static_fields: HashMap::new(),
      }),
      name: en.name,
      span: en.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);

    // Bind type params now that we have the owner def_id
    let type_param_defs = self.bind_type_params(en.type_params.as_ref(), def_id);
    self.pop_type_params_scope(en.type_params.as_ref());

    // Update the enum definition with type params
    if let DefinitionKind::Enum(ed) = &mut self.defs.get_mut(&def_id).kind {
      ed.type_params = type_param_defs;
    }

    if let Err(existing) = self.scopes.define(&en.name, &def_id) {
      let existing_def = self.defs.get(&existing);
      let symbol = self.get_symbol_name(&existing_def.name);
      self.add_diagnostic(
        DiagnosticMessage::TypeAlreadyDefined {
          name: symbol,
          span: en.span.clone(),
          previous_span: existing_def.span.clone(),
        }
        .report(),
      );
    }
  }

  fn bind_enum_complete(
    &mut self,
    node_id: &NodeId,
    en: &ASTEnum,
  ) {
    let Some(enum_def_id) = self.lookup_def(node_id).cloned() else {
      return;
    };

    // Create the Type::Enum and update the definition's type_id
    let type_id = self.types.enum_type(enum_def_id);

    // Re-establish generic scope for type params visibility within variants/methods
    self.push_type_params_scope(enum_def_id);

    let mut variants = Vec::new();
    let mut variants_by_name = HashMap::new();
    let mut static_methods = HashMap::new();
    let mut static_fields = HashMap::new();
    let mut tag_value = 0u32;

    for item in &en.items {
      match item {
        ASTEnumItem::Variant(variant) => {
          // Payload types resolved in typeck phase
          let payload: Vec<_> = variant.payload.iter().map(|_| self.types.error()).collect();

          variants.push(EnumVariantDef {
            name: variant.name,
            payload,
            tag_value,
          });
          variants_by_name.insert(variant.name, tag_value);
          tag_value += 1;
        },
        ASTEnumItem::Method(method) => {
          // In enum, all methods are implicitly static
          let method_def_id = self.bind_method(method, enum_def_id, true);
          static_methods.insert(method.name, method_def_id);
        },
        ASTEnumItem::Field(field) => {
          // In enum, all fields are implicitly static
          // Fields require initializer (checked in extra_checks)
          let const_def = Definition {
            kind: DefinitionKind::Constant(ConstantDefinition {
              type_id: self.types.error(), // Resolved in typeck
              value: None,
              owner_type: Some(enum_def_id),
            }),
            name: field.name,
            span: field.span.clone(),
            visibility: Visibility::Public,
            owner_module: self.current_module,
            owner_namespace: self.current_namespace,
          };
          let const_def_id = self.defs.alloc(const_def);
          static_fields.insert(field.name, const_def_id);
        },
      }
    }

    // Pop the generic scope
    self.pop_type_params_scope_if_generic(enum_def_id);

    // Update the enum definition
    if let DefinitionKind::Enum(ed) = &mut self.defs.get_mut(&enum_def_id).kind {
      ed.type_id = type_id;
      ed.variants = variants;
      ed.variants_by_name = variants_by_name;
      ed.static_methods = static_methods;
      ed.static_fields = static_fields;
    }
  }

  // ========================================================================
  // Method Binding (shared by Record and Enum)
  // ========================================================================

  fn bind_method(
    &mut self,
    method: &ASTMethod,
    owner: DefinitionId,
    is_static: bool,
  ) -> DefinitionId {
    // Create parameter definitions
    let mut param_defs = Vec::new();
    for param in &method.parameters {
      let param_def = Definition {
        kind: DefinitionKind::Parameter(ParameterDefinition {
          type_id: self.types.error(), // Resolved in typeck
          mutable: param.metadata.is_mutable(),
        }),
        name: param.name,
        span: param.span.clone(),
        visibility: Visibility::Private,
        owner_module: self.current_module,
        owner_namespace: None,
      };
      let param_def_id = self.defs.alloc(param_def);
      param_defs.push(param_def_id);
    }

    let method_def = Definition {
      kind: DefinitionKind::Method(MethodDefinition {
        owner_type: owner,
        type_params: Vec::new(), // Will be populated after binding type params
        params: param_defs.clone(),
        return_type: self.types.error(), // Resolved in typeck
        is_static,
      }),
      name: method.name,
      span: method.span.clone(),
      visibility: Visibility::Public,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let method_def_id = self.defs.alloc(method_def);

    // Bind method's own type params (in addition to owner's type params already in scope)
    let type_param_defs = self.bind_type_params(method.type_params.as_ref(), method_def_id);

    // Update the method definition with type params
    if let DefinitionKind::Method(md) = &mut self.defs.get_mut(&method_def_id).kind {
      md.type_params = type_param_defs;
    }

    // Push function scope for parameters
    self.scopes.push(ScopeKind::Function);

    for param_id in &param_defs {
      let param_def = self.defs.get(param_id);
      let _ = self.scopes.define(&param_def.name, param_id);
    }

    self.bind_complete(&method.body, ScopeKind::Function);

    self.scopes.pop(); // Pop function scope

    // Pop method's generic scope (does nothing if no type params)
    self.pop_type_params_scope(method.type_params.as_ref());

    method_def_id
  }

  // ========================================================================
  // Namespace Predecl/Complete (for recursive type support in namespaces)
  // ========================================================================

  fn bind_namespace_predecl(
    &mut self,
    node_id: &NodeId,
    ns_stmt: &ignis_ast::statements::namespace_statement::ASTNamespace,
  ) {
    let ns_id = self.namespaces.get_or_create(&ns_stmt.path, false);
    let ns_name = *ns_stmt.path.last().expect("namespace path cannot be empty");

    // Check if namespace definition already exists (from a previous predecl)
    if self.lookup_def(node_id).is_none() {
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
    }

    let prev_ns = self.current_namespace;
    self.current_namespace = Some(ns_id);

    let scope_kind = ScopeKind::Namespace(ns_id);
    self.scopes.push(scope_kind);

    // Predeclare types in namespace
    for item in &ns_stmt.items {
      self.bind_predecl(item, scope_kind);

      // Register in namespace for lookup
      if let Some(def_id) = self.lookup_def(item).cloned() {
        let def = self.defs.get(&def_id);
        self.namespaces.define(ns_id, def.name, def_id);
      }
    }

    self.scopes.pop();
    self.current_namespace = prev_ns;
  }

  fn bind_namespace_complete(
    &mut self,
    _node_id: &NodeId,
    ns_stmt: &ignis_ast::statements::namespace_statement::ASTNamespace,
  ) {
    let ns_id = self.namespaces.get_or_create(&ns_stmt.path, false);

    let prev_ns = self.current_namespace;
    self.current_namespace = Some(ns_id);

    let scope_kind = ScopeKind::Namespace(ns_id);
    self.scopes.push(scope_kind);

    // Re-register predeclared definitions in scope
    for item in &ns_stmt.items {
      if let Some(def_id) = self.lookup_def(item).cloned() {
        let def = self.defs.get(&def_id);
        let _ = self.scopes.define(&def.name, &def_id);
      }
    }

    for item in &ns_stmt.items {
      self.bind_complete(item, scope_kind);

      if let Some(def_id) = self.lookup_def(item).cloned() {
        let def = self.defs.get(&def_id);
        self.namespaces.define(ns_id, def.name, def_id);
      }
    }

    self.scopes.pop();
    self.current_namespace = prev_ns;
  }

  // ========================================================================
  // Function Binding
  // ========================================================================

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

    // Create function definition first (without type params)
    let func_def = FunctionDefinition {
      type_params: Vec::new(), // Will be populated after binding type params
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

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);

    // Bind type params now that we have the owner def_id
    let type_param_defs = self.bind_type_params(func.signature.type_params.as_ref(), def_id);

    // Update the function definition with type params
    if let DefinitionKind::Function(fd) = &mut self.defs.get_mut(&def_id).kind {
      fd.type_params = type_param_defs;
    }

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
      // Note: generic scope is already pushed by bind_type_params if there are type params
      // We still need the function scope for parameters
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

      self.bind_complete(&body_id, ScopeKind::Function);
      self.scopes.pop(); // Pop function scope
    }

    // Pop the generic scope (does nothing if no type params)
    self.pop_type_params_scope(func.signature.type_params.as_ref());
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
      self.bind_complete(value_id, ScopeKind::Block);
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
      owner_type: None,
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
      self.bind_complete(value_id, ScopeKind::Block);
    }
  }

  fn bind_block(
    &mut self,
    block: &ignis_ast::statements::ASTBlock,
  ) {
    self.scopes.push(ScopeKind::Block);

    for stmt_id in &block.statements {
      self.bind_complete(stmt_id, ScopeKind::Block);
    }

    self.scopes.pop();
  }

  fn bind_if(
    &mut self,
    if_stmt: &ignis_ast::statements::ASTIf,
  ) {
    self.bind_complete(&if_stmt.condition, ScopeKind::Block);
    self.bind_complete(&if_stmt.then_block, ScopeKind::Block);

    if let Some(else_branch) = &if_stmt.else_block {
      self.bind_complete(else_branch, ScopeKind::Block);
    }
  }

  fn bind_while(
    &mut self,
    while_stmt: &ignis_ast::statements::ASTWhile,
  ) {
    self.scopes.push(ScopeKind::Loop);

    self.bind_complete(&while_stmt.condition, ScopeKind::Loop);
    self.bind_complete(&while_stmt.body, ScopeKind::Loop);

    self.scopes.pop();
  }

  fn bind_for(
    &mut self,
    for_stmt: &ignis_ast::statements::ASTFor,
  ) {
    self.scopes.push(ScopeKind::Loop);

    self.bind_complete(&for_stmt.initializer, ScopeKind::Loop);
    self.bind_complete(&for_stmt.condition, ScopeKind::Loop);
    self.bind_complete(&for_stmt.increment, ScopeKind::Loop);
    self.bind_complete(&for_stmt.body, ScopeKind::Loop);

    self.scopes.pop();
  }

  fn bind_for_of(
    &mut self,
    node_id: &NodeId,
    for_of: &ignis_ast::statements::ASTForOf,
  ) {
    self.scopes.push(ScopeKind::Loop);

    // Type is unknown here; typeck will resolve it based on the iterable's element type.
    let var_def = VariableDefinition {
      type_id: self.types.unknown(),
      mutable: false,
    };

    let def = Definition {
      kind: DefinitionKind::Variable(var_def),
      name: for_of.binding.name.clone(),
      span: for_of.binding.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
    };

    let def_id = self.defs.alloc(def);
    let _ = self.scopes.define(&for_of.binding.name, &def_id);
    self.for_of_binding_defs.insert(node_id.clone(), def_id);

    self.bind_complete(&for_of.iter, ScopeKind::Loop);
    self.bind_complete(&for_of.body, ScopeKind::Loop);

    self.scopes.pop();
  }

  fn bind_export(
    &mut self,
    export_stmt: &ignis_ast::statements::ASTExport,
    scope_kind: ScopeKind,
  ) {
    match export_stmt {
      ignis_ast::statements::ASTExport::Declaration { decl, .. } => {
        self.bind_complete(decl, scope_kind);

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
      self.bind_complete(item, scope_kind);
      self.mark_extern(item);

      if let Some(def_id) = self.lookup_def(item).cloned() {
        let def = self.defs.get(&def_id);
        self.namespaces.define(ns_id, def.name, def_id);
      }
    }

    self.scopes.pop();
    self.current_namespace = prev_ns;
  }

  // ========================================================================
  // Type Parameter Binding
  // ========================================================================

  /// Binds type parameters for a generic definition.
  ///
  /// Creates `DefinitionKind::TypeParam` definitions for each type parameter,
  /// pushes a `ScopeKind::Generic` scope, and registers the type params in that scope.
  ///
  /// Returns the list of type param definition IDs. The caller is responsible
  /// for popping the generic scope after processing the generic item's body.
  fn bind_type_params(
    &mut self,
    type_params: Option<&ASTGenericParams>,
    owner: DefinitionId,
  ) -> Vec<DefinitionId> {
    let Some(params) = type_params else {
      return Vec::new();
    };

    if params.is_empty() {
      return Vec::new();
    }

    // Push generic scope for type parameter visibility
    self.scopes.push(ScopeKind::Generic);

    let mut type_param_defs = Vec::with_capacity(params.len());

    for (index, param) in params.params.iter().enumerate() {
      let def = Definition {
        kind: DefinitionKind::TypeParam(TypeParamDefinition {
          index: index as u32,
          owner,
        }),
        name: param.name,
        span: param.span.clone(),
        visibility: Visibility::Private,
        owner_module: self.current_module,
        owner_namespace: self.current_namespace,
      };

      let def_id = self.defs.alloc(def);
      type_param_defs.push(def_id);

      // Register in generic scope so `T` resolves to this definition
      if let Err(existing) = self.scopes.define(&param.name, &def_id) {
        let existing_def = self.defs.get(&existing);
        let symbol = self.get_symbol_name(&existing_def.name);
        self.add_diagnostic(
          DiagnosticMessage::TypeParamAlreadyDefined {
            name: symbol,
            span: param.span.clone(),
            previous_span: existing_def.span.clone(),
          }
          .report(),
        );
      }
    }

    type_param_defs
  }

  /// Pops the generic scope if type params were bound.
  /// Call this after processing the body of a generic item.
  fn pop_type_params_scope(
    &mut self,
    type_params: Option<&ASTGenericParams>,
  ) {
    if let Some(params) = type_params {
      if !params.is_empty() {
        self.scopes.pop();
      }
    }
  }

  /// Pushes a generic scope and registers already-bound type params for an owner definition.
  /// Used in the complete pass when we need to re-establish the type param scope.
  fn push_type_params_scope(
    &mut self,
    owner_def_id: DefinitionId,
  ) {
    let type_params = match &self.defs.get(&owner_def_id).kind {
      DefinitionKind::Record(rd) => rd.type_params.clone(),
      DefinitionKind::Enum(ed) => ed.type_params.clone(),
      DefinitionKind::Function(fd) => fd.type_params.clone(),
      DefinitionKind::Method(md) => md.type_params.clone(),
      _ => Vec::new(),
    };

    if type_params.is_empty() {
      return;
    }

    self.scopes.push(ScopeKind::Generic);

    for param_id in &type_params {
      let name = self.defs.get(param_id).name;
      let _ = self.scopes.define(&name, param_id);
    }
  }

  /// Pops the generic scope if the owner definition has type params.
  fn pop_type_params_scope_if_generic(
    &mut self,
    owner_def_id: DefinitionId,
  ) {
    let has_type_params = match &self.defs.get(&owner_def_id).kind {
      DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
      DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
      DefinitionKind::Function(fd) => !fd.type_params.is_empty(),
      DefinitionKind::Method(md) => !md.type_params.is_empty(),
      _ => false,
    };

    if has_type_params {
      self.scopes.pop();
    }
  }
}
