use std::collections::HashMap;

use crate::{Analyzer, ScopeKind};
use ignis_ast::{
  attribute::ASTAttribute,
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
use ignis_type::{
  attribute::{FieldAttr, FunctionAttr, RecordAttr},
  definition::{
    ConstantDefinition, Definition, DefinitionId, DefinitionKind, EnumDefinition, EnumVariantDef, FieldDefinition,
    FunctionDefinition, MethodDefinition, NamespaceDefinition, ParameterDefinition, RecordDefinition, RecordFieldDef,
    SymbolEntry, TypeAliasDefinition, TypeParamDefinition, VariableDefinition, VariantDefinition, Visibility,
  },
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
        self.bind_extern(node_id, extern_stmt);
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
        type_params: Vec::new(),
        target: self.types.error(),
      }),
      name: ta.name,
      span: ta.span.clone(),
      name_span: ta.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: ta.doc.clone(),
    };

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);
    self.type_alias_syntax.insert(def_id, ta.target.clone());

    let type_param_defs = self.bind_type_params(ta.type_params.as_ref(), def_id);
    self.pop_type_params_scope(ta.type_params.as_ref());

    if let DefinitionKind::TypeAlias(tad) = &mut self.defs.get_mut(&def_id).kind {
      tad.type_params = type_param_defs;
    }

    if let Err(existing) = self.scopes.define(&ta.name, &def_id, false) {
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
        attrs: vec![],
      }),
      name: rec.name,
      span: rec.span.clone(),
      name_span: rec.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: rec.doc.clone(),
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

    if let Err(existing) = self.scopes.define(&rec.name, &def_id, false) {
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

    let record_attrs = self.bind_record_attrs(&rec.attrs);

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
              name_span: field.name_span.clone(),
              visibility: Visibility::Public,
              owner_module: self.current_module,
              owner_namespace: self.current_namespace,
              doc: field.doc.clone(),
            };
            let const_def_id = self.defs.alloc(const_def);
            self.set_import_item_def(&field.name_span, &const_def_id);
            static_fields.insert(field.name, const_def_id);
          } else {
            // Fields are private by default unless explicitly marked public
            let visibility = if field.metadata.is_public() {
              Visibility::Public
            } else {
              Visibility::Private
            };

            let field_def = Definition {
              kind: DefinitionKind::Field(FieldDefinition {
                type_id: self.types.error(), // Resolved in typeck
                owner_type: record_def_id,
                index: field_index,
              }),
              name: field.name,
              span: field.span.clone(),
              name_span: field.name_span.clone(),
              visibility,
              owner_module: self.current_module,
              owner_namespace: self.current_namespace,
              doc: field.doc.clone(),
            };
            let field_def_id = self.defs.alloc(field_def);
            self.set_import_item_def(&field.name_span, &field_def_id);
            let field_attrs = self.bind_field_attrs(&field.attrs);
            fields.push(RecordFieldDef {
              name: field.name,
              type_id: self.types.error(), // Resolved in typeck
              index: field_index,
              span: field.span.clone(),
              def_id: field_def_id,
              attrs: field_attrs,
            });
            field_index += 1;
          }
        },
        ASTRecordItem::Method(method) => {
          let is_static = method.is_static();
          let method_def_id = self.bind_method(method, record_def_id, is_static);

          let target_map = if is_static {
            &mut static_methods
          } else {
            &mut instance_methods
          };

          match target_map.get_mut(&method.name) {
            Some(SymbolEntry::Overload(group)) => group.push(method_def_id),
            Some(SymbolEntry::Single(existing)) => {
              let existing = *existing;
              target_map.insert(method.name, SymbolEntry::Overload(vec![existing, method_def_id]));
            },
            None => {
              target_map.insert(method.name, SymbolEntry::Single(method_def_id));
            },
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
      rd.attrs = record_attrs;
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
        attrs: vec![],
      }),
      name: en.name,
      span: en.span.clone(),
      name_span: en.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: en.doc.clone(),
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

    if let Err(existing) = self.scopes.define(&en.name, &def_id, false) {
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

    let enum_attrs = self.bind_record_attrs(&en.attrs);

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

          let variant_def = Definition {
            kind: DefinitionKind::Variant(VariantDefinition {
              payload: payload.clone(),
              owner_enum: enum_def_id,
              tag_value,
            }),
            name: variant.name,
            span: variant.span.clone(),
            name_span: variant.name_span.clone(),
            visibility: Visibility::Public,
            owner_module: self.current_module,
            owner_namespace: self.current_namespace,
            doc: variant.doc.clone(),
          };
          let variant_def_id = self.defs.alloc(variant_def);

          self.set_import_item_def(&variant.name_span, &variant_def_id);

          variants.push(EnumVariantDef {
            name: variant.name,
            payload,
            tag_value,
            span: variant.span.clone(),
            def_id: variant_def_id,
          });
          variants_by_name.insert(variant.name, tag_value);
          tag_value += 1;
        },
        ASTEnumItem::Method(method) => {
          // In enum, all methods are implicitly static
          let method_def_id = self.bind_method(method, enum_def_id, true);

          match static_methods.get_mut(&method.name) {
            Some(SymbolEntry::Overload(group)) => group.push(method_def_id),
            Some(SymbolEntry::Single(existing)) => {
              let existing = *existing;
              static_methods.insert(method.name, SymbolEntry::Overload(vec![existing, method_def_id]));
            },
            None => {
              static_methods.insert(method.name, SymbolEntry::Single(method_def_id));
            },
          }
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
            name_span: field.name_span.clone(),
            visibility: Visibility::Public,
            owner_module: self.current_module,
            owner_namespace: self.current_namespace,
            doc: None,
          };
          let const_def_id = self.defs.alloc(const_def);
          self.set_import_item_def(&field.name_span, &const_def_id);
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
      ed.attrs = enum_attrs;
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
        name_span: param.span.clone(),
        visibility: Visibility::Private,
        owner_module: self.current_module,
        owner_namespace: None,
        doc: None,
      };
      let param_def_id = self.defs.alloc(param_def);
      param_defs.push(param_def_id);
    }

    // Methods are private by default unless explicitly marked public
    let visibility = if method.metadata.is_public() {
      Visibility::Public
    } else {
      Visibility::Private
    };

    let method_attrs = self.bind_function_attrs(&method.attrs);

    let method_def = Definition {
      kind: DefinitionKind::Method(MethodDefinition {
        owner_type: owner,
        type_params: Vec::new(), // Will be populated after binding type params
        params: param_defs.clone(),
        return_type: self.types.error(), // Resolved in typeck
        is_static,
        self_mutable: method.self_param.unwrap_or(false),
        inline_mode: method.inline_mode,
        attrs: method_attrs,
      }),
      name: method.name,
      span: method.span.clone(),
      name_span: method.name_span.clone(),
      visibility,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: method.doc.clone(),
    };

    let method_def_id = self.defs.alloc(method_def);
    self.set_import_item_def(&method.name_span, &method_def_id);

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
      let _ = self.scopes.define(&param_def.name, param_id, false);
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
        name_span: ns_stmt.span.clone(),
        visibility: Visibility::Private,
        owner_module: self.current_module,
        owner_namespace: self.current_namespace,
        doc: ns_stmt.doc.clone(),
      };

      let def_id = self.defs.alloc(ns_def);
      self.set_def(node_id, &def_id);
      let _ = self.scopes.define(&ns_name, &def_id, false);
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
        let is_overloadable = matches!(def.kind, DefinitionKind::Function(_) | DefinitionKind::Method(_));
        self.namespaces.define(ns_id, def.name, def_id, is_overloadable);
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
        let is_overloadable = matches!(def.kind, DefinitionKind::Function(_) | DefinitionKind::Method(_));
        let _ = self.scopes.define(&def.name, &def_id, is_overloadable);
      }
    }

    for item in &ns_stmt.items {
      self.bind_complete(item, scope_kind);

      if let Some(def_id) = self.lookup_def(item).cloned() {
        let def = self.defs.get(&def_id);
        let is_overloadable = matches!(def.kind, DefinitionKind::Function(_) | DefinitionKind::Method(_));
        self.namespaces.define(ns_id, def.name, def_id, is_overloadable);
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
        name: param.name,
        span: param.span.clone(),
        name_span: param.span.clone(),
        visibility: Visibility::Private,
        owner_module: self.current_module,
        owner_namespace: None,
        doc: None,
      };
      let def_id = self.defs.alloc(def);
      param_defs.push(def_id);
    }

    let attrs = self.bind_function_attrs(&func.signature.attrs);

    // Create function definition first (without type params)
    let func_def = FunctionDefinition {
      type_params: Vec::new(), // Will be populated after binding type params
      params: param_defs.clone(),
      return_type: self.types.error(),
      is_extern,
      is_variadic,
      inline_mode: func.signature.inline_mode,
      attrs,
    };

    let def = Definition {
      kind: DefinitionKind::Function(func_def),
      name: func.signature.name,
      span: span.clone(),
      name_span: func.signature.name_span.clone(),
      visibility: if func.signature.metadata.is_public() {
        Visibility::Public
      } else {
        Visibility::Private
      },
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: func.signature.doc.clone(),
    };

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);
    self.set_import_item_def(&func.signature.name_span, &def_id);

    // Bind type params now that we have the owner def_id
    let type_param_defs = self.bind_type_params(func.signature.type_params.as_ref(), def_id);

    // Update the function definition with type params
    if let DefinitionKind::Function(fd) = &mut self.defs.get_mut(&def_id).kind {
      fd.type_params = type_param_defs;
    }

    let main_symbol = self.symbols.borrow_mut().intern("main");
    let is_main = func.signature.name == main_symbol;

    match self.scopes.define(&func.signature.name, &def_id, true) {
      Ok(()) => {},
      Err(existing) => {
        let existing_def = self.defs.get(&existing);

        if !matches!(existing_def.kind, DefinitionKind::Function(_)) {
          let symbol = self.get_symbol_name(&existing_def.name);
          self.add_diagnostic(
            DiagnosticMessage::FunctionAlreadyDefined {
              name: symbol,
              span: span.clone(),
              previous_span: existing_def.span.clone(),
            }
            .report(),
          );
        } else {
          self.scopes.promote_to_overload(&func.signature.name, &def_id);
        }
      },
    }

    if is_main {
      if let Some(SymbolEntry::Overload(group)) = self.scopes.lookup(&func.signature.name) {
        if group.len() > 1 {
          self.add_diagnostic(DiagnosticMessage::MainFunctionCannotBeOverloaded { span: span.clone() }.report());
        }
      }
    }

    if let Some(body_id) = &func.body {
      // Note: generic scope is already pushed by bind_type_params if there are type params
      // We still need the function scope for parameters
      self.scopes.push(ScopeKind::Function);

      for param_id in &param_defs {
        let param_def = self.defs.get(param_id);
        let param_span = param_def.span.clone();
        let name = param_def.name;
        if let Err(existing) = self.scopes.define(&name, param_id, false) {
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

      self.bind_complete(body_id, ScopeKind::Function);
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
      name: var.name,
      span: span.clone(),
      name_span: span.clone(),
      visibility: if var.metadata.is_public() {
        Visibility::Public
      } else {
        Visibility::Private
      },
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: None,
    };

    let def_id = &self.defs.alloc(def);
    self.set_def(node_id, def_id);

    if let Err(existing) = &self.scopes.define(&var.name, def_id, false) {
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
      name: const_.name,
      span: span.clone(),
      name_span: span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: None,
    };

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);

    if let Err(existing) = self.scopes.define(&const_.name, &def_id, false) {
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

    // Type is implicit here; typeck will resolve it based on the iterable's element type.
    let var_def = VariableDefinition {
      type_id: self.types.infer(),
      mutable: false,
    };

    let def = Definition {
      kind: DefinitionKind::Variable(var_def),
      name: for_of.binding.name,
      span: for_of.binding.span.clone(),
      name_span: for_of.binding.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: None,
    };

    let def_id = self.defs.alloc(def);
    let _ = self.scopes.define(&for_of.binding.name, &def_id, false);
    self.for_of_binding_defs.insert(*node_id, def_id);

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
        if let Some(entry) = self.scopes.lookup(name) {
          match entry {
            SymbolEntry::Single(def_id) => {
              self.defs.get_mut(def_id).visibility = Visibility::Public;
            },
            SymbolEntry::Overload(group) => {
              for def_id in group {
                self.defs.get_mut(def_id).visibility = Visibility::Public;
              }
            },
          }
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
    node_id: &NodeId,
    extern_stmt: &ignis_ast::statements::extern_statement::ASTExtern,
  ) {
    let ns_id = self.namespaces.get_or_create(&extern_stmt.path, true);
    let ns_name = *extern_stmt.path.last().expect("extern path cannot be empty");

    if self.lookup_def(node_id).is_none() {
      let ns_def = Definition {
        kind: DefinitionKind::Namespace(NamespaceDefinition {
          namespace_id: ns_id,
          is_extern: true,
        }),
        name: ns_name,
        span: extern_stmt.span.clone(),
        name_span: extern_stmt.span.clone(),
        visibility: Visibility::Public,
        owner_module: self.current_module,
        owner_namespace: self.current_namespace,
        doc: extern_stmt.doc.clone(),
      };

      let def_id = self.defs.alloc(ns_def);
      self.set_def(node_id, &def_id);
      let _ = self.scopes.define(&ns_name, &def_id, false);
    }

    let prev_ns = self.current_namespace;
    self.current_namespace = Some(ns_id);

    let scope_kind = ScopeKind::Namespace(ns_id);
    self.scopes.push(scope_kind);

    for item in &extern_stmt.items {
      self.bind_complete(item, scope_kind);
      self.mark_extern(item);

      if let Some(def_id) = self.lookup_def(item).cloned() {
        let def = self.defs.get(&def_id);
        let is_overloadable = matches!(def.kind, DefinitionKind::Function(_) | DefinitionKind::Method(_));
        self.namespaces.define(ns_id, def.name, def_id, is_overloadable);
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
        name_span: param.span.clone(),
        visibility: Visibility::Private,
        owner_module: self.current_module,
        owner_namespace: self.current_namespace,
        doc: None,
      };

      let def_id = self.defs.alloc(def);
      type_param_defs.push(def_id);

      // Register in generic scope so `T` resolves to this definition
      if let Err(existing) = self.scopes.define(&param.name, &def_id, false) {
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
      let _ = self.scopes.define(&name, param_id, false);
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

  // ========================================================================
  // Attribute Binding
  // ========================================================================

  fn bind_record_attrs(
    &mut self,
    ast_attrs: &[ASTAttribute],
  ) -> Vec<RecordAttr> {
    let mut attrs = Vec::new();

    for attr in ast_attrs {
      let name = self.get_symbol_name(&attr.name);

      match name.as_str() {
        "packed" => {
          if !attr.args.is_empty() {
            self.add_diagnostic(
              DiagnosticMessage::AttributeArgCount {
                attr: name,
                expected: 0,
                got: attr.args.len(),
                span: attr.span.clone(),
              }
              .report(),
            );
          } else {
            attrs.push(RecordAttr::Packed);
          }
        },
        "aligned" => {
          if attr.args.len() != 1 {
            self.add_diagnostic(
              DiagnosticMessage::AttributeArgCount {
                attr: name,
                expected: 1,
                got: attr.args.len(),
                span: attr.span.clone(),
              }
              .report(),
            );
          } else if let Some(n) = self.extract_int_arg(&name, &attr.args[0]) {
            if !n.is_power_of_two() {
              self.add_diagnostic(
                DiagnosticMessage::AlignmentNotPowerOfTwo {
                  value: n,
                  span: attr.span.clone(),
                }
                .report(),
              );
            } else {
              attrs.push(RecordAttr::Aligned(n));
            }
          }
        },
        "allow" | "warn" | "deny" => {},
        _ => {
          self.add_diagnostic(
            DiagnosticMessage::UnknownAttribute {
              name,
              target: "record".to_string(),
              span: attr.span.clone(),
            }
            .report(),
          );
        },
      }
    }

    attrs
  }

  fn bind_field_attrs(
    &mut self,
    ast_attrs: &[ASTAttribute],
  ) -> Vec<FieldAttr> {
    let mut attrs = Vec::new();

    for attr in ast_attrs {
      let name = self.get_symbol_name(&attr.name);

      match name.as_str() {
        "aligned" => {
          if attr.args.len() != 1 {
            self.add_diagnostic(
              DiagnosticMessage::AttributeArgCount {
                attr: name,
                expected: 1,
                got: attr.args.len(),
                span: attr.span.clone(),
              }
              .report(),
            );
          } else if let Some(n) = self.extract_int_arg(&name, &attr.args[0]) {
            if !n.is_power_of_two() {
              self.add_diagnostic(
                DiagnosticMessage::AlignmentNotPowerOfTwo {
                  value: n,
                  span: attr.span.clone(),
                }
                .report(),
              );
            } else {
              attrs.push(FieldAttr::Aligned(n));
            }
          }
        },
        "allow" | "warn" | "deny" => {},
        _ => {
          self.add_diagnostic(
            DiagnosticMessage::UnknownAttribute {
              name,
              target: "field".to_string(),
              span: attr.span.clone(),
            }
            .report(),
          );
        },
      }
    }

    attrs
  }

  fn bind_function_attrs(
    &mut self,
    ast_attrs: &[ASTAttribute],
  ) -> Vec<FunctionAttr> {
    let mut attrs = Vec::new();

    for attr in ast_attrs {
      let name = self.get_symbol_name(&attr.name);

      match name.as_str() {
        "externName" => {
          if attr.args.len() != 1 {
            self.add_diagnostic(
              DiagnosticMessage::AttributeArgCount {
                attr: name,
                expected: 1,
                got: attr.args.len(),
                span: attr.span.clone(),
              }
              .report(),
            );
          } else if let Some(s) = self.extract_string_arg(&name, &attr.args[0]) {
            attrs.push(FunctionAttr::ExternName(s));
          }
        },
        "cold" => {
          if !attr.args.is_empty() {
            self.add_diagnostic(
              DiagnosticMessage::AttributeArgCount {
                attr: name,
                expected: 0,
                got: attr.args.len(),
                span: attr.span.clone(),
              }
              .report(),
            );
          } else {
            attrs.push(FunctionAttr::Cold);
          }
        },
        "deprecated" => {
          if attr.args.len() > 1 {
            self.add_diagnostic(
              DiagnosticMessage::AttributeArgCount {
                attr: name,
                expected: 1,
                got: attr.args.len(),
                span: attr.span.clone(),
              }
              .report(),
            );
          } else if attr.args.is_empty() {
            attrs.push(FunctionAttr::Deprecated(None));
          } else if let Some(s) = self.extract_string_arg(&name, &attr.args[0]) {
            attrs.push(FunctionAttr::Deprecated(Some(s)));
          }
        },
        "allow" | "warn" | "deny" => {},
        _ => {
          self.add_diagnostic(
            DiagnosticMessage::UnknownAttribute {
              name,
              target: "function".to_string(),
              span: attr.span.clone(),
            }
            .report(),
          );
        },
      }
    }

    attrs
  }

  fn extract_int_arg(
    &mut self,
    attr_name: &str,
    arg: &ignis_ast::attribute::ASTAttributeArg,
  ) -> Option<u64> {
    match arg {
      ignis_ast::attribute::ASTAttributeArg::IntLiteral(v, _) => {
        if *v < 0 {
          self.add_diagnostic(
            DiagnosticMessage::AttributeExpectedInt {
              attr: attr_name.to_string(),
              span: arg.span().clone(),
            }
            .report(),
          );
          None
        } else {
          Some(*v as u64)
        }
      },
      ignis_ast::attribute::ASTAttributeArg::StringLiteral(_, span)
      | ignis_ast::attribute::ASTAttributeArg::Identifier(_, span) => {
        self.add_diagnostic(
          DiagnosticMessage::AttributeExpectedInt {
            attr: attr_name.to_string(),
            span: span.clone(),
          }
          .report(),
        );
        None
      },
    }
  }

  fn extract_string_arg(
    &mut self,
    attr_name: &str,
    arg: &ignis_ast::attribute::ASTAttributeArg,
  ) -> Option<String> {
    match arg {
      ignis_ast::attribute::ASTAttributeArg::StringLiteral(s, _) => Some(s.clone()),
      ignis_ast::attribute::ASTAttributeArg::IntLiteral(_, span)
      | ignis_ast::attribute::ASTAttributeArg::Identifier(_, span) => {
        self.add_diagnostic(
          DiagnosticMessage::AttributeExpectedString {
            attr: attr_name.to_string(),
            span: span.clone(),
          }
          .report(),
        );
        None
      },
    }
  }

  pub fn bind_lint_directives(
    &mut self,
    ast_attrs: &[ignis_ast::attribute::ASTAttribute],
  ) -> Vec<(ignis_type::lint::LintId, ignis_type::lint::LintLevel)> {
    use ignis_type::lint::{LintId, LintLevel};

    let mut directives = Vec::new();

    for attr in ast_attrs {
      let name = self.get_symbol_name(&attr.name);

      let level = match name.as_str() {
        "allow" => LintLevel::Allow,
        "warn" => LintLevel::Warn,
        "deny" => LintLevel::Deny,
        _ => continue,
      };

      if attr.args.len() != 1 {
        self.add_diagnostic(
          DiagnosticMessage::AttributeArgCount {
            attr: name,
            expected: 1,
            got: attr.args.len(),
            span: attr.span.clone(),
          }
          .report(),
        );
        continue;
      }

      match &attr.args[0] {
        ignis_ast::attribute::ASTAttributeArg::Identifier(sym, span) => {
          let lint_name = self.get_symbol_name(sym);

          match lint_name.as_str() {
            "unused_variable" => directives.push((LintId::UnusedVariable, level)),
            "unused_import" => directives.push((LintId::UnusedImport, level)),
            "deprecated" => directives.push((LintId::Deprecated, level)),
            _ => {
              self.add_diagnostic(
                DiagnosticMessage::UnknownLint {
                  name: lint_name,
                  span: span.clone(),
                }
                .report(),
              );
            },
          }
        },
        other => {
          self.add_diagnostic(
            DiagnosticMessage::AttributeExpectedIdentifier {
              attr: name,
              span: other.span().clone(),
            }
            .report(),
          );
        },
      }
    }

    directives
  }
}
