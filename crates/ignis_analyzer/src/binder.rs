use std::collections::HashMap;

use crate::{Analyzer, ScopeKind, directive_registry::DirectiveUse};
use ignis_ast::{
  attribute::ASTAttribute,
  expressions::{ASTExpression, lambda::LambdaBody},
  generics::ASTGenericParams,
  metadata::ASTMetadata,
  statements::{
    const_statement::ASTConstant,
    enum_::{ASTEnum, ASTEnumItem},
    function::ASTFunction,
    import_statement::ImportItemKind,
    record::{ASTMethod, ASTRecord, ASTRecordItem},
    trait_declaration::ASTTrait,
    type_alias::ASTTypeAlias,
    variable::ASTVariable,
    ASTStatement,
  },
  ASTNode, NodeId,
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::symbol::SymbolId;
use ignis_type::{
  attribute::{
    DirectiveCapability, DirectiveEffect, DirectiveMetadata as FunctionAttrDirectiveMetadata, DirectivePhase,
    DirectiveTarget, FieldAttr, FunctionAttr, NamespaceAttr, ParamAttr, RecordAttr,
  },
  definition::{
    ConstantDefinition, Definition, DefinitionId, DefinitionKind, DirectiveDefId, DirectiveDefinition,
    DirectiveProvenance, EnumDefinition, EnumVariantDef, FieldDefinition, FunctionDefinition, LangTraitSet,
    MethodDefinition, NamespaceDefinition, ParameterDefinition, RecordDefinition, RecordFieldDef, SymbolEntry,
    TraitDefinition, TraitMethodEntry, TryCapability, TypeAliasDefinition, TypeParamDefinition, VariableDefinition,
    VariantDefinition, Visibility,
  },
};

enum DirectiveUseResolution {
  Matched {
    function_def_id: DefinitionId,
    directive_id: DirectiveDefId,
    provenance: DirectiveProvenance,
  },
  WrongTarget {
    function_def_id: DefinitionId,
    name: String,
    expected: DirectiveTarget,
    actual: DirectiveTarget,
    span: ignis_type::span::Span,
  },
}

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

    self.process_imports(roots);

    // Pass 2: Complete bindings
    for root in roots {
      self.bind_complete(root, ScopeKind::Global);
    }
  }

  pub fn bind_generated_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    for root in roots {
      self.bind_predecl(root, ScopeKind::Global);
    }

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
    let node = self.ast_node(node_id).clone();

    match &node {
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
      ASTStatement::Trait(tr) => self.bind_trait_predecl(node_id, tr),
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
      ASTStatement::Export(ignis_ast::statements::ASTExport::Declaration { decl, .. }) => {
        self.bind_predecl(decl, scope_kind);
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
    let node = self.ast_node(node_id).clone();

    match &node {
      ASTNode::Statement(stmt) => self.bind_complete_statement(node_id, stmt, scope_kind),
      ASTNode::Expression(expr) => self.bind_expression_lambdas(expr),
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
      ASTStatement::LetElse(let_else) => self.bind_let_else(let_else),
      ASTStatement::Constant(const_) => self.bind_constant(node_id, const_),
      ASTStatement::Block(block) => self.bind_block(block),
      ASTStatement::If(if_stmt) => self.bind_if(if_stmt),
      ASTStatement::While(while_stmt) => self.bind_while(while_stmt),
      ASTStatement::For(for_stmt) => self.bind_for(for_stmt),
      ASTStatement::ForOf(for_of_stmt) => self.bind_for_of(node_id, for_of_stmt),
      ASTStatement::TypeAlias(ta) => self.bind_type_alias_complete(node_id, ta),
      ASTStatement::Record(rec) => self.bind_record_complete(node_id, rec),
      ASTStatement::Enum(en) => self.bind_enum_complete(node_id, en),
      ASTStatement::Trait(tr) => self.bind_trait_complete(node_id, tr),
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

  /// Recursively bind declarations inside lambda bodies so they get `DefinitionId`s.
  fn bind_expression_lambdas(
    &mut self,
    expr: &ASTExpression,
  ) {
    match expr {
      ASTExpression::Lambda(lambda) => match &lambda.body {
        LambdaBody::Block(block_id) => self.bind_complete(block_id, ScopeKind::Block),
        LambdaBody::Expression(expr_id) => self.bind_complete(expr_id, ScopeKind::Block),
      },

      ASTExpression::Assignment(a) => {
        self.bind_complete(&a.value, ScopeKind::Block);
      },
      ASTExpression::Binary(b) => {
        self.bind_complete(&b.left, ScopeKind::Block);
        self.bind_complete(&b.right, ScopeKind::Block);
      },
      ASTExpression::Ternary(t) => {
        self.bind_complete(&t.condition, ScopeKind::Block);
        self.bind_complete(&t.then_expr, ScopeKind::Block);
        self.bind_complete(&t.else_expr, ScopeKind::Block);
      },
      ASTExpression::Cast(c) => {
        self.bind_complete(&c.expression, ScopeKind::Block);
      },
      ASTExpression::Call(c) => {
        self.bind_complete(&c.callee, ScopeKind::Block);
        for arg in &c.arguments {
          self.bind_complete(arg, ScopeKind::Block);
        }
      },
      ASTExpression::Dereference(d) => {
        self.bind_complete(&d.inner, ScopeKind::Block);
      },
      ASTExpression::Grouped(g) => {
        self.bind_complete(&g.expression, ScopeKind::Block);
      },
      ASTExpression::Reference(r) => {
        self.bind_complete(&r.inner, ScopeKind::Block);
      },
      ASTExpression::Unary(u) => {
        self.bind_complete(&u.operand, ScopeKind::Block);
      },
      ASTExpression::Match(m) => {
        self.bind_complete(&m.scrutinee, ScopeKind::Block);
        for arm in &m.arms {
          if let Some(g) = &arm.guard {
            self.bind_complete(g, ScopeKind::Block);
          }
          self.bind_complete(&arm.body, ScopeKind::Block);
        }
      },
      ASTExpression::Vector(v) => {
        for elem in &v.items {
          self.bind_complete(elem, ScopeKind::Block);
        }
      },
      ASTExpression::VectorAccess(va) => {
        self.bind_complete(&va.name, ScopeKind::Block);
        self.bind_complete(&va.index, ScopeKind::Block);
      },
      ASTExpression::MemberAccess(ma) => {
        self.bind_complete(&ma.object, ScopeKind::Block);
      },
      ASTExpression::RecordInit(ri) => {
        for field in &ri.fields {
          self.bind_complete(&field.value, ScopeKind::Block);
        }
      },
      ASTExpression::BuiltinCall(bc) => {
        for arg in &bc.args {
          self.bind_complete(arg, ScopeKind::Block);
        }
      },
      ASTExpression::PostfixIncrement { expr, .. } | ASTExpression::PostfixDecrement { expr, .. } => {
        self.bind_complete(expr, ScopeKind::Block);
      },
      ASTExpression::Try { expr, .. } => {
        self.bind_complete(expr, ScopeKind::Block);
      },
      ASTExpression::LetCondition(lc) => {
        self.bind_complete(&lc.value, ScopeKind::Block);
      },

      ASTExpression::CaptureOverride(co) => {
        self.bind_complete(&co.inner, ScopeKind::Block);
      },

      ASTExpression::Pipe { lhs, rhs, .. } => {
        self.bind_complete(lhs, ScopeKind::Block);
        self.bind_complete(rhs, ScopeKind::Block);
      },

      // Leaf expressions (no sub-expressions that could contain lambdas)
      ASTExpression::Literal(_)
      | ASTExpression::Variable(_)
      | ASTExpression::Path(_)
      | ASTExpression::PipePlaceholder { .. } => {},
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

    let type_param_defs = self.bind_type_params(ta.type_params.as_ref(), def_id, false);
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
        lang_traits: LangTraitSet::default(),
        implemented_traits: Vec::new(),
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
    let type_param_defs = self.bind_type_params(rec.type_params.as_ref(), def_id, false);
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

    self.refresh_type_param_bounds(rec.type_params.as_ref(), record_def_id);

    let type_name = self.get_symbol_name(&rec.name);
    let (record_attrs, lang_traits, implemented_traits) =
      self.bind_record_attrs(node_id, &rec.attrs, &type_name, DirectiveTarget::Record);

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
      rd.lang_traits = lang_traits;
      rd.implemented_traits = implemented_traits;
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
        instance_methods: HashMap::new(),
        static_methods: HashMap::new(),
        static_fields: HashMap::new(),
        attrs: vec![],
        lang_traits: LangTraitSet::default(),
        implemented_traits: Vec::new(),
        try_capable: None,
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
    let type_param_defs = self.bind_type_params(en.type_params.as_ref(), def_id, false);
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

    self.refresh_type_param_bounds(en.type_params.as_ref(), enum_def_id);

    let type_name = self.get_symbol_name(&en.name);
    let (enum_attrs, lang_traits, implemented_traits) =
      self.bind_record_attrs(node_id, &en.attrs, &type_name, DirectiveTarget::Enum);

    // Create the Type::Enum and update the definition's type_id
    let type_id = self.types.enum_type(enum_def_id);

    // Re-establish generic scope for type params visibility within variants/methods
    self.push_type_params_scope(enum_def_id);

    let mut variants = Vec::new();
    let mut variants_by_name = HashMap::new();
    let mut instance_methods = HashMap::new();
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
          let is_static = method.is_static() || !method.has_self();
          let method_def_id = self.bind_method(method, enum_def_id, is_static);

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

    // Check for @lang(try) attribute
    let try_capable = if enum_attrs.iter().any(|a| matches!(a, RecordAttr::LangTry)) {
      if variants.len() != 2 {
        self.add_diagnostic(
          DiagnosticMessage::LangTryRequiresTwoVariants {
            name: type_name.clone(),
            count: variants.len(),
            span: en.span.clone(),
          }
          .report(),
        );
        None
      } else {
        Some(TryCapability {
          ok_variant: 0,
          err_variant: 1,
        })
      }
    } else {
      None
    };

    // Update the enum definition
    if let DefinitionKind::Enum(ed) = &mut self.defs.get_mut(&enum_def_id).kind {
      ed.type_id = type_id;
      ed.variants = variants;
      ed.variants_by_name = variants_by_name;
      ed.instance_methods = instance_methods;
      ed.static_methods = static_methods;
      ed.static_fields = static_fields;
      ed.attrs = enum_attrs;
      ed.lang_traits = lang_traits;
      ed.implemented_traits = implemented_traits;
      ed.try_capable = try_capable;
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
      let param_attrs = self.bind_param_attrs(&param.attrs);
      let param_def = Definition {
        kind: DefinitionKind::Parameter(ParameterDefinition {
          type_id: self.types.error(), // Resolved in typeck
          mutable: param.metadata.is_mutable(),
          attrs: param_attrs,
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

    let method_attrs = self.bind_function_attrs(None, &method.attrs, "method", false);

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
    let type_param_defs = self.bind_type_params(method.type_params.as_ref(), method_def_id, true);

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
  // Trait Binding
  // ========================================================================

  fn bind_trait_predecl(
    &mut self,
    node_id: &NodeId,
    tr: &ASTTrait,
  ) {
    let def = Definition {
      kind: DefinitionKind::Trait(TraitDefinition {
        type_params: Vec::new(),
        methods: Vec::new(),
        type_id: self.types.error(),
      }),
      name: tr.name,
      span: tr.span.clone(),
      name_span: tr.span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: tr.doc.clone(),
    };

    let def_id = self.defs.alloc(def);
    self.set_def(node_id, &def_id);

    let type_param_defs = self.bind_type_params(tr.type_params.as_ref(), def_id, false);
    self.pop_type_params_scope(tr.type_params.as_ref());

    if let DefinitionKind::Trait(td) = &mut self.defs.get_mut(&def_id).kind {
      td.type_params = type_param_defs;
    }

    if let Err(existing) = self.scopes.define(&tr.name, &def_id, false) {
      let existing_def = self.defs.get(&existing);
      let symbol = self.get_symbol_name(&existing_def.name);
      self.add_diagnostic(
        DiagnosticMessage::TypeAlreadyDefined {
          name: symbol,
          span: tr.span.clone(),
          previous_span: existing_def.span.clone(),
        }
        .report(),
      );
    }
  }

  fn bind_trait_complete(
    &mut self,
    node_id: &NodeId,
    tr: &ASTTrait,
  ) {
    let Some(trait_def_id) = self.lookup_def(node_id).cloned() else {
      return;
    };

    self.refresh_type_param_bounds(tr.type_params.as_ref(), trait_def_id);

    let type_id = self.types.error();

    self.push_type_params_scope(trait_def_id);

    let mut trait_methods = Vec::new();

    for method in &tr.methods {
      if method.self_param.is_none() {
        let method_name = self.get_symbol_name(&method.name);
        self.add_diagnostic(
          DiagnosticMessage::TraitStaticMethodNotAllowed {
            method_name,
            span: method.span.clone(),
          }
          .report(),
        );
        continue;
      }

      let method_def_id = self.bind_trait_method(method, trait_def_id);

      trait_methods.push(TraitMethodEntry {
        name: method.name,
        method_def_id,
        has_default: method.body.is_some(),
      });
    }

    self.pop_type_params_scope_if_generic(trait_def_id);

    if let DefinitionKind::Trait(td) = &mut self.defs.get_mut(&trait_def_id).kind {
      td.type_id = type_id;
      td.methods = trait_methods;
    }
  }

  fn bind_trait_method(
    &mut self,
    method: &ignis_ast::statements::trait_declaration::ASTTraitMethod,
    owner: DefinitionId,
  ) -> DefinitionId {
    let mut param_defs = Vec::new();
    for param in &method.parameters {
      let param_attrs = self.bind_param_attrs(&param.attrs);
      let param_def = Definition {
        kind: DefinitionKind::Parameter(ParameterDefinition {
          type_id: self.types.error(),
          mutable: param.metadata.is_mutable(),
          attrs: param_attrs,
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

    let method_attrs = self.bind_function_attrs(None, &method.attrs, "method", false);

    let method_def = Definition {
      kind: DefinitionKind::Method(MethodDefinition {
        owner_type: owner,
        type_params: Vec::new(),
        params: param_defs.clone(),
        return_type: self.types.error(),
        is_static: false,
        self_mutable: method.self_param.unwrap_or(false),
        inline_mode: method.inline_mode,
        attrs: method_attrs,
      }),
      name: method.name,
      span: method.span.clone(),
      name_span: method.name_span.clone(),
      visibility: Visibility::Public,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: method.doc.clone(),
    };

    let method_def_id = self.defs.alloc(method_def);
    self.set_import_item_def(&method.name_span, &method_def_id);

    let type_param_defs = self.bind_type_params(method.type_params.as_ref(), method_def_id, true);

    if let DefinitionKind::Method(md) = &mut self.defs.get_mut(&method_def_id).kind {
      md.type_params = type_param_defs;
    }

    if let Some(body_id) = &method.body {
      self.scopes.push(ScopeKind::Function);

      for param_id in &param_defs {
        let param_def = self.defs.get(param_id);
        let _ = self.scopes.define(&param_def.name, param_id, false);
      }

      self.bind_complete(body_id, ScopeKind::Function);
      self.scopes.pop();
    }

    self.pop_type_params_scope(method.type_params.as_ref());

    method_def_id
  }

  // ========================================================================
  // Namespace Predecl/Complete (for recursive type support in namespaces)
  // ========================================================================

  /// Builds the full namespace path by prepending the current parent namespace's
  /// path to the AST-level path. For `namespace Memory { ... }` nested inside
  /// `namespace LibC { ... }`, this turns `[Memory]` into `[LibC, Memory]` so
  /// that `get_or_create` registers the correct parent-child relationship.
  fn build_full_namespace_path(
    &self,
    ast_path: &[SymbolId],
  ) -> Vec<SymbolId> {
    if let Some(parent_ns_id) = self.current_namespace {
      let mut full = self.namespaces.full_path(parent_ns_id);
      full.extend_from_slice(ast_path);
      full
    } else {
      ast_path.to_vec()
    }
  }

  fn bind_namespace_predecl(
    &mut self,
    node_id: &NodeId,
    ns_stmt: &ignis_ast::statements::namespace_statement::ASTNamespace,
  ) {
    let full_path = self.build_full_namespace_path(&ns_stmt.path);
    let ns_id = self.namespaces.get_or_create(&full_path, false);
    let ns_name = *ns_stmt.path.last().expect("namespace path cannot be empty");

    // Check if namespace definition already exists (from a previous predecl)
    if self.lookup_def(node_id).is_none() {
      let ns_def = Definition {
        kind: DefinitionKind::Namespace(NamespaceDefinition {
          namespace_id: ns_id,
          is_extern: false,
          attrs: Vec::new(),
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
    let full_path = self.build_full_namespace_path(&ns_stmt.path);
    let ns_id = self.namespaces.get_or_create(&full_path, false);

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
      let param_attrs = self.bind_param_attrs(&param.attrs);
      let def = Definition {
        kind: DefinitionKind::Parameter(ParameterDefinition {
          type_id: self.types.error(),
          mutable: param.metadata.is_mutable(),
          attrs: param_attrs,
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

    let attrs = self.bind_function_attrs(Some(node_id), &func.signature.attrs, "function", true);

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
    let type_param_defs = self.bind_type_params(func.signature.type_params.as_ref(), def_id, true);

    // Update the function definition with type params
    if let DefinitionKind::Function(fd) = &mut self.defs.get_mut(&def_id).kind {
      fd.type_params = type_param_defs;
    }

    self.register_function_directives(def_id, func);

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

    if is_main
      && let Some(SymbolEntry::Overload(group)) = self.scopes.lookup(&func.signature.name)
      && group.len() > 1
    {
      self.add_diagnostic(DiagnosticMessage::MainFunctionCannotBeOverloaded { span: span.clone() }.report());
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
      name_span: var.name_span.clone(),
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

  fn bind_let_else(
    &mut self,
    let_else: &ignis_ast::statements::ASTLetElse,
  ) {
    self.bind_complete(&let_else.value, ScopeKind::Block);
    self.bind_complete(&let_else.else_block, ScopeKind::Block);
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
          self.mark_exported_definition(def_id);
        }
      },
      ignis_ast::statements::ASTExport::Name { name, .. } => {
        for def_id in self.exported_definition_ids(name) {
          self.mark_exported_definition(def_id);
        }
      },
      ignis_ast::statements::ASTExport::ReExportFrom { items, .. } => {
        for item in items {
          let ImportItemKind::Named(name) = &item.kind else {
            continue;
          };

          for def_id in self.exported_definition_ids(name) {
            self.mark_exported_definition(def_id);
          }
        }
      },
    }
  }

  fn exported_definition_ids(
    &self,
    name: &SymbolId,
  ) -> Vec<DefinitionId> {
    match self.scopes.lookup(name) {
      Some(SymbolEntry::Single(def_id)) => vec![*def_id],
      Some(SymbolEntry::Overload(group)) => group.clone(),
      None => Vec::new(),
    }
  }

  fn mark_exported_definition(
    &mut self,
    def_id: DefinitionId,
  ) {
    let def = self.defs.get_mut(&def_id);
    def.visibility = Visibility::Public;

    if def.owner_module != self.current_module {
      self.reexported_defs.insert(def.name, def_id);
    }
  }

  fn mark_extern(
    &mut self,
    node_id: &NodeId,
  ) {
    let node = self.ast.get(node_id);

    if let ASTNode::Statement(ASTStatement::Function(_)) = node
      && let Some(def_id) = self.lookup_def(node_id).cloned()
      && let DefinitionKind::Function(func_def) = &mut self.defs.get_mut(&def_id).kind
    {
      func_def.is_extern = true;
    }
  }

  fn bind_extern(
    &mut self,
    node_id: &NodeId,
    extern_stmt: &ignis_ast::statements::extern_statement::ASTExtern,
  ) {
    let full_path = self.build_full_namespace_path(&extern_stmt.path);
    let ns_id = self.namespaces.get_or_create(&full_path, true);
    let ns_name = *extern_stmt.path.last().expect("extern path cannot be empty");
    let namespace_attrs = self.bind_namespace_attrs(&extern_stmt.attrs, "extern namespace");

    if self.lookup_def(node_id).is_none() {
      let ns_def = Definition {
        kind: DefinitionKind::Namespace(NamespaceDefinition {
          namespace_id: ns_id,
          is_extern: true,
          attrs: namespace_attrs,
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
    resolve_bounds: bool,
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
      let bounds = if resolve_bounds {
        self.bind_type_param_bounds(param)
      } else {
        Vec::new()
      };

      let def = Definition {
        kind: DefinitionKind::TypeParam(TypeParamDefinition {
          index: index as u32,
          owner,
          bounds,
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

  fn refresh_type_param_bounds(
    &mut self,
    type_params: Option<&ASTGenericParams>,
    owner: DefinitionId,
  ) {
    let Some(params) = type_params else {
      return;
    };

    let param_defs = match &self.defs.get(&owner).kind {
      DefinitionKind::TypeAlias(td) => td.type_params.clone(),
      DefinitionKind::Record(rd) => rd.type_params.clone(),
      DefinitionKind::Enum(ed) => ed.type_params.clone(),
      DefinitionKind::Trait(td) => td.type_params.clone(),
      DefinitionKind::Function(fd) => fd.type_params.clone(),
      DefinitionKind::Method(md) => md.type_params.clone(),
      _ => return,
    };

    for (param, param_def_id) in params.params.iter().zip(param_defs.iter()) {
      let bounds = self.bind_type_param_bounds(param);
      if let DefinitionKind::TypeParam(tp) = &mut self.defs.get_mut(param_def_id).kind {
        tp.bounds = bounds;
      }
    }
  }

  fn bind_type_param_bounds(
    &mut self,
    param: &ignis_ast::generics::ASTGenericParam,
  ) -> Vec<DefinitionId> {
    let mut bound_defs = Vec::with_capacity(param.bounds.len());

    for bound in &param.bounds {
      let bound_name = bound
        .segments
        .iter()
        .map(|segment| self.get_symbol_name(segment))
        .collect::<Vec<_>>()
        .join("::");

      let Some(def_id) = self.resolve_generic_bound(bound) else {
        self.add_diagnostic(
          DiagnosticMessage::UnknownTraitInGenericBound {
            name: bound_name,
            span: bound.span.clone(),
          }
          .report(),
        );
        continue;
      };

      if !matches!(self.defs.get(&def_id).kind, DefinitionKind::Trait(_)) {
        self.add_diagnostic(
          DiagnosticMessage::GenericBoundMustBeTrait {
            name: bound_name,
            span: bound.span.clone(),
          }
          .report(),
        );
        continue;
      }

      bound_defs.push(def_id);
    }

    bound_defs
  }

  fn resolve_generic_bound(
    &self,
    bound: &ignis_ast::generics::ASTGenericBound,
  ) -> Option<DefinitionId> {
    let first = bound.segments.first()?;
    let mut current_def = self
      .scopes
      .lookup_def(first)
      .cloned()
      .or_else(|| self.lookup_trait_def_by_name(first))?;

    for segment in bound.segments.iter().skip(1) {
      let DefinitionKind::Namespace(ns_def) = &self.defs.get(&current_def).kind else {
        return None;
      };

      let entry = self.namespaces.lookup_def(ns_def.namespace_id, segment)?;
      current_def = *entry.as_single()?;
    }

    Some(current_def)
  }

  fn lookup_trait_def_by_name(
    &self,
    symbol: &ignis_type::symbol::SymbolId,
  ) -> Option<DefinitionId> {
    self
      .defs
      .iter()
      .filter_map(|(def_id, def)| {
        if def.name == *symbol && matches!(def.kind, DefinitionKind::Trait(_)) {
          Some(def_id)
        } else {
          None
        }
      })
      .last()
  }

  /// Pops the generic scope if type params were bound.
  /// Call this after processing the body of a generic item.
  fn pop_type_params_scope(
    &mut self,
    type_params: Option<&ASTGenericParams>,
  ) {
    if let Some(params) = type_params
      && !params.is_empty()
    {
      self.scopes.pop();
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
      DefinitionKind::Trait(td) => td.type_params.clone(),
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
      DefinitionKind::Trait(td) => !td.type_params.is_empty(),
      _ => false,
    };

    if has_type_params {
      self.scopes.pop();
    }
  }

  // ========================================================================
  // Attribute Binding
  // ========================================================================

  fn bind_namespace_attrs(
    &mut self,
    ast_attrs: &[ASTAttribute],
    target: &str,
  ) -> Vec<NamespaceAttr> {
    let mut attrs = Vec::new();

    for attr in ast_attrs {
      let name = self.get_symbol_name(&attr.name);

      match name.as_str() {
        "lang" => {
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

          let hook_name = match &attr.args[0] {
            ignis_ast::attribute::ASTAttributeArg::Identifier(symbol_id, _) => Some(self.get_symbol_name(symbol_id)),
            ignis_ast::attribute::ASTAttributeArg::StringLiteral(value, _) => Some(value.clone()),
            ignis_ast::attribute::ASTAttributeArg::IntLiteral(_, span) => {
              self.add_diagnostic(
                DiagnosticMessage::AttributeExpectedIdentifier {
                  attr: "lang".to_string(),
                  span: span.clone(),
                }
                .report(),
              );
              None
            },
          };

          let Some(hook_name) = hook_name else {
            continue;
          };

          if !matches!(hook_name.as_str(), "string_runtime" | "vector_runtime" | "weak_runtime") {
            self.add_diagnostic(
              DiagnosticMessage::UnknownAttribute {
                name: format!("lang({})", hook_name),
                target: target.to_string(),
                span: attr.span.clone(),
              }
              .report(),
            );
            continue;
          }

          attrs.push(NamespaceAttr::LangHook(hook_name));
        },
        _ => {
          self.add_diagnostic(
            DiagnosticMessage::UnknownAttribute {
              name,
              target: target.to_string(),
              span: attr.span.clone(),
            }
            .report(),
          );
        },
      }
    }

    attrs
  }

  fn bind_record_attrs(
    &mut self,
    target_node: &NodeId,
    ast_attrs: &[ASTAttribute],
    type_name: &str,
    directive_target: DirectiveTarget,
  ) -> (Vec<RecordAttr>, LangTraitSet, Vec<DefinitionId>) {
    let mut attrs = Vec::new();
    let mut lang_traits = LangTraitSet::default();
    let mut implemented_traits = Vec::new();

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
        "implements" => {
          let user_traits = self.bind_implements_attr(attr, &mut lang_traits, type_name);
          implemented_traits.extend(user_traits);
        },
        "lang" => {
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
          } else if let Some(arg_name) = self.extract_ident_arg(&attr.args[0]) {
            if arg_name == "try" {
              attrs.push(RecordAttr::LangTry);
            } else {
              self.add_diagnostic(
                DiagnosticMessage::UnknownAttribute {
                  name: format!("lang({})", arg_name),
                  target: "record".to_string(),
                  span: attr.span.clone(),
                }
                .report(),
              );
            }
          }
        },
        name if ignis_type::at_items::is_lint_level_directive(name) => {},
        _ => {
          if self.collect_directive_use(target_node, attr, directive_target.clone()) {
            continue;
          }

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

    (attrs, lang_traits, implemented_traits)
  }

  fn bind_implements_attr(
    &mut self,
    attr: &ASTAttribute,
    lang_traits: &mut LangTraitSet,
    type_name: &str,
  ) -> Vec<DefinitionId> {
    let mut user_traits = Vec::new();

    if attr.args.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::AttributeArgCount {
          attr: "implements".to_string(),
          expected: 1,
          got: 0,
          span: attr.span.clone(),
        }
        .report(),
      );
      return user_traits;
    }

    for arg in &attr.args {
      match arg {
        ignis_ast::attribute::ASTAttributeArg::Identifier(sym, span) => {
          let trait_name = self.get_symbol_name(sym);

          match trait_name.as_str() {
            "Drop" => lang_traits.drop = true,
            "Clone" => lang_traits.clone = true,
            "Copy" => lang_traits.copy = true,
            _ => {
              if let Some(SymbolEntry::Single(def_id)) = self.scopes.lookup(sym) {
                let def_id = *def_id;
                if matches!(self.defs.get(&def_id).kind, DefinitionKind::Trait(_)) {
                  user_traits.push(def_id);
                } else {
                  self.add_diagnostic(
                    DiagnosticMessage::UnknownTraitInImplements {
                      name: trait_name,
                      span: span.clone(),
                    }
                    .report(),
                  );
                }
              } else if let Some(def_id) = self.lookup_trait_def_by_name(sym) {
                user_traits.push(def_id);
              } else {
                self.add_diagnostic(
                  DiagnosticMessage::UnknownTraitInImplements {
                    name: trait_name,
                    span: span.clone(),
                  }
                  .report(),
                );
              }
            },
          }
        },
        other => {
          self.add_diagnostic(
            DiagnosticMessage::AttributeExpectedIdentifier {
              attr: "implements".to_string(),
              span: other.span().clone(),
            }
            .report(),
          );
        },
      }
    }

    if lang_traits.drop && lang_traits.copy {
      self.add_diagnostic(
        DiagnosticMessage::LangTraitDropCopyConflict {
          type_name: type_name.to_string(),
          span: attr.span.clone(),
        }
        .report(),
      );
    }

    user_traits
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
        name if ignis_type::at_items::is_lint_level_directive(name) => {},
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
    target_node: Option<&NodeId>,
    ast_attrs: &[ASTAttribute],
    target: &str,
    allow_test: bool,
  ) -> Vec<FunctionAttr> {
    let mut attrs = Vec::new();

    for attr in ast_attrs {
      let name = self.get_symbol_name(&attr.name);

      if name == "directive" {
        if let Some(metadata) = self.bind_directive_function_attr(attr, target) {
          attrs.push(FunctionAttr::Directive(metadata));
        }

        continue;
      }

      if ignis_type::at_items::is_lint_level_directive(&name) {
        continue;
      }

      if self.is_legacy_function_attr(&name) {
        if let Some(attr) = self.bind_legacy_function_attr(attr, &name, target, allow_test) {
          attrs.push(attr);
        }

        continue;
      }

      if target_node.is_some_and(|node_id| self.collect_directive_use(node_id, attr, DirectiveTarget::Function)) {
        continue;
      }

      self.add_diagnostic(
        DiagnosticMessage::UnknownAttribute {
          name,
          target: target.to_string(),
          span: attr.span.clone(),
        }
        .report(),
      );
    }

    attrs
  }

  fn is_legacy_function_attr(
    &self,
    name: &str,
  ) -> bool {
    matches!(name, "test" | "externName" | "cold" | "deprecated" | "extension")
  }

  fn collect_directive_use(
    &mut self,
    target_node: &NodeId,
    attr: &ASTAttribute,
    target: DirectiveTarget,
  ) -> bool {
    let Some(resolution) = self.resolve_directive_use(attr, target) else {
      return false;
    };

    match resolution {
      DirectiveUseResolution::Matched {
        function_def_id,
        directive_id,
        provenance,
      } => {
        self.mark_referenced(function_def_id);

        self.directive_registry.uses.push(DirectiveUse {
          target_node: *target_node,
          directive: directive_id,
          span: attr.span.clone(),
          provenance,
        });
      },
      DirectiveUseResolution::WrongTarget {
        function_def_id,
        name,
        expected,
        actual,
        span,
      } => {
        self.mark_referenced(function_def_id);
        self.add_diagnostic(
          DiagnosticMessage::DirectiveTargetMismatch {
            name,
            expected: self.directive_target_name(&expected).to_string(),
            actual: self.directive_target_name(&actual).to_string(),
            span,
          }
          .report(),
        );
      },
    }

    true
  }

  fn resolve_directive_use(
    &mut self,
    attr: &ASTAttribute,
    target: DirectiveTarget,
  ) -> Option<DirectiveUseResolution> {
    let entry = if attr.path.len() == 1 {
      self.scopes.lookup(&attr.name).cloned()
    } else {
      let namespace_id = self.namespaces.lookup(&attr.path[..attr.path.len() - 1])?;
      self.namespaces.lookup_def(namespace_id, &attr.name).cloned()
    }?;

    self.resolve_directive_entry(attr, &entry, target)
  }

  fn resolve_directive_entry(
    &mut self,
    attr: &ASTAttribute,
    entry: &SymbolEntry,
    target: DirectiveTarget,
  ) -> Option<DirectiveUseResolution> {
    let def_ids: &[DefinitionId] = match entry {
      SymbolEntry::Single(def_id) => std::slice::from_ref(def_id),
      SymbolEntry::Overload(def_ids) => def_ids.as_slice(),
    };

    let attr_name = self.format_attribute_path(attr);
    let mut wrong_target: Option<DirectiveUseResolution> = None;

    for def_id in def_ids {
      let Some((directive_id, directive)) = self.directive_definition_for_function(*def_id) else {
        continue;
      };

      if directive.target == target {
        return Some(DirectiveUseResolution::Matched {
          function_def_id: *def_id,
          directive_id,
          provenance: directive.provenance.clone(),
        });
      }

      wrong_target.get_or_insert_with(|| DirectiveUseResolution::WrongTarget {
        function_def_id: *def_id,
        name: attr_name.clone(),
        expected: directive.target,
        actual: target.clone(),
        span: attr.span.clone(),
      });
    }

    wrong_target
  }

  fn directive_definition_for_function(
    &mut self,
    function_def_id: DefinitionId,
  ) -> Option<(DirectiveDefId, DirectiveDefinition)> {
    if let Some((directive_id, directive)) = self.directive_registry.definition_for_function(function_def_id) {
      return Some((directive_id, directive.clone()));
    }

    let directive_attr = match &self.defs.get(&function_def_id).kind {
      DefinitionKind::Function(function) => function.attrs.iter().find_map(|attr| match attr {
        FunctionAttr::Directive(metadata) => Some(metadata.clone()),
        _ => None,
      })?,
      _ => return None,
    };

    let definition = self.defs.get(&function_def_id);
    let directive_id = self.directive_registry.register_definition(DirectiveDefinition {
      id: DirectiveDefId::new(0),
      function_def_id,
      name: definition.name,
      target: directive_attr.target,
      phase: directive_attr.phase,
      effect: directive_attr.effect,
      group: directive_attr.group,
      capabilities: directive_attr.capabilities,
      provenance: DirectiveProvenance {
        origin_attr_span: definition.span.clone(),
      },
    });

    self
      .directive_registry
      .defs
      .iter()
      .find(|directive| directive.id == directive_id)
      .cloned()
      .map(|directive| (directive_id, directive))
  }

  fn bind_legacy_function_attr(
    &mut self,
    attr: &ASTAttribute,
    name: &str,
    target: &str,
    allow_test: bool,
  ) -> Option<FunctionAttr> {
    match name {
      "test" => {
        if !allow_test {
          self.add_diagnostic(
            DiagnosticMessage::UnknownAttribute {
              name: name.to_string(),
              target: target.to_string(),
              span: attr.span.clone(),
            }
            .report(),
          );
          return None;
        }

        if !attr.args.is_empty() {
          self.add_diagnostic(
            DiagnosticMessage::AttributeArgCount {
              attr: name.to_string(),
              expected: 0,
              got: attr.args.len(),
              span: attr.span.clone(),
            }
            .report(),
          );
          return None;
        }

        Some(FunctionAttr::Test)
      },
      "externName" => {
        if attr.args.len() != 1 {
          self.add_diagnostic(
            DiagnosticMessage::AttributeArgCount {
              attr: name.to_string(),
              expected: 1,
              got: attr.args.len(),
              span: attr.span.clone(),
            }
            .report(),
          );
          return None;
        }

        self
          .extract_string_arg(name, &attr.args[0])
          .map(FunctionAttr::ExternName)
      },
      "cold" => {
        if !attr.args.is_empty() {
          self.add_diagnostic(
            DiagnosticMessage::AttributeArgCount {
              attr: name.to_string(),
              expected: 0,
              got: attr.args.len(),
              span: attr.span.clone(),
            }
            .report(),
          );
          return None;
        }

        Some(FunctionAttr::Cold)
      },
      "deprecated" => {
        if attr.args.len() > 1 {
          self.add_diagnostic(
            DiagnosticMessage::AttributeArgCount {
              attr: name.to_string(),
              expected: 1,
              got: attr.args.len(),
              span: attr.span.clone(),
            }
            .report(),
          );
          return None;
        }

        if attr.args.is_empty() {
          Some(FunctionAttr::Deprecated(None))
        } else {
          self
            .extract_string_arg(name, &attr.args[0])
            .map(|message| FunctionAttr::Deprecated(Some(message)))
        }
      },
      "extension" => self.bind_extension_attr(attr, name),
      _ => {
        self.add_diagnostic(
          DiagnosticMessage::UnknownAttribute {
            name: name.to_string(),
            target: target.to_string(),
            span: attr.span.clone(),
          }
          .report(),
        );
        None
      },
    }
  }

  fn bind_extension_attr(
    &mut self,
    attr: &ASTAttribute,
    attr_name: &str,
  ) -> Option<FunctionAttr> {
    if attr.args.is_empty() || attr.args.len() > 2 {
      self.add_diagnostic(
        DiagnosticMessage::AttributeArgCount {
          attr: attr_name.to_string(),
          expected: 1,
          got: attr.args.len(),
          span: attr.span.clone(),
        }
        .report(),
      );
      return None;
    }

    let type_name = match &attr.args[0] {
      ignis_ast::attribute::ASTAttributeArg::StringLiteral(s, _) => Some(s.clone()),
      ignis_ast::attribute::ASTAttributeArg::Identifier(sym, _) => Some(self.symbols.borrow().get(sym).to_string()),
      ignis_ast::attribute::ASTAttributeArg::IntLiteral(_, span) => {
        self.add_diagnostic(
          DiagnosticMessage::AttributeExpectedString {
            attr: attr_name.to_string(),
            span: span.clone(),
          }
          .report(),
        );
        None
      },
    }?;

    let mutable = attr.args.get(1).is_some_and(|arg| {
      matches!(arg, ignis_ast::attribute::ASTAttributeArg::Identifier(sym, _)
        if self.symbols.borrow().get(sym) == "mut")
    });

    Some(FunctionAttr::Extension { type_name, mutable })
  }

  fn bind_directive_function_attr(
    &mut self,
    attr: &ASTAttribute,
    target: &str,
  ) -> Option<FunctionAttrDirectiveMetadata> {
    if target != "function" {
      self.add_diagnostic(
        DiagnosticMessage::UnknownAttribute {
          name: self.get_symbol_name(&attr.name),
          target: target.to_string(),
          span: attr.span.clone(),
        }
        .report(),
      );
      return None;
    }

    self.bind_directive_attr(attr)
  }

  fn bind_param_attrs(
    &mut self,
    ast_attrs: &[ASTAttribute],
  ) -> Vec<ParamAttr> {
    let mut attrs = Vec::new();

    for attr in ast_attrs {
      let name = self.get_symbol_name(&attr.name);

      match name.as_str() {
        "takes" => {
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
            attrs.push(ParamAttr::Takes);
          }
        },
        "noescape" => {
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
            attrs.push(ParamAttr::NoEscape);
          }
        },
        _ => {
          self.add_diagnostic(
            DiagnosticMessage::UnknownAttribute {
              name,
              target: "parameter".to_string(),
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

  fn extract_ident_arg(
    &mut self,
    arg: &ignis_ast::attribute::ASTAttributeArg,
  ) -> Option<String> {
    match arg {
      ignis_ast::attribute::ASTAttributeArg::Identifier(sym, _) => Some(self.get_symbol_name(sym).clone()),
      ignis_ast::attribute::ASTAttributeArg::IntLiteral(_, span)
      | ignis_ast::attribute::ASTAttributeArg::StringLiteral(_, span) => {
        self.add_diagnostic(
          DiagnosticMessage::AttributeExpectedIdentifier {
            attr: "lang".to_string(),
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

  fn bind_directive_attr(
    &mut self,
    attr: &ASTAttribute,
  ) -> Option<FunctionAttrDirectiveMetadata> {
    if !attr.args.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::AttributeArgCount {
          attr: "directive".to_string(),
          expected: 0,
          got: attr.args.len(),
          span: attr.span.clone(),
        }
        .report(),
      );
      return None;
    }

    let mut target = None;
    let mut phase = None;
    let mut effect = None;
    let mut group = None;
    let mut capabilities = Vec::new();
    let mut seen_keys = std::collections::HashSet::new();
    let mut has_error = false;

    for named_arg in &attr.named_args {
      let key = self.get_symbol_name(&named_arg.name).clone();

      if !seen_keys.insert(key.clone()) {
        has_error = true;
        self.add_diagnostic(
          DiagnosticMessage::CompileError {
            message: format!("duplicate @directive metadata '{}'", key),
            span: named_arg.span.clone(),
          }
          .report(),
        );
        continue;
      }

      match key.as_str() {
        "target" => {
          target = self.parse_directive_target(&named_arg.value);
          has_error |= target.is_none();
        },
        "phase" => {
          phase = self.parse_directive_phase(&named_arg.value);
          has_error |= phase.is_none();
        },
        "effect" => {
          effect = self.parse_directive_effect(&named_arg.value);
          has_error |= effect.is_none();
        },
        "group" => {
          group = self.extract_attr_name_value("directive", &named_arg.value);
          has_error |= group.is_none();
        },
        "capabilities" => match self.parse_directive_capability(&named_arg.value) {
          Some(capability) => capabilities.push(capability),
          None => has_error = true,
        },
        _ => {
          has_error = true;
          self.add_diagnostic(
            DiagnosticMessage::UnknownDirectiveMetadata {
              name: key,
              span: named_arg.span.clone(),
            }
            .report(),
          );
        },
      }
    }

    let target = target?;
    let phase = phase?;
    let effect = effect?;

    if matches!(phase, DirectivePhase::Check) && !matches!(effect, DirectiveEffect::Diagnose) {
      has_error = true;
      self.add_diagnostic(
        DiagnosticMessage::InvalidDirectivePhaseEffect {
          phase: self.directive_phase_name(&phase).to_string(),
          effect: self.directive_effect_name(&effect).to_string(),
          span: attr.span.clone(),
        }
        .report(),
      );
    }

    if has_error {
      return None;
    }

    Some(FunctionAttrDirectiveMetadata {
      target,
      phase,
      effect,
      group,
      capabilities,
    })
  }

  fn register_function_directives(
    &mut self,
    function_def_id: DefinitionId,
    func: &ASTFunction,
  ) {
    let directive_attr_spans: Vec<_> = func
      .signature
      .attrs
      .iter()
      .filter(|attr| self.get_symbol_name(&attr.name) == "directive")
      .map(|attr| attr.span.clone())
      .collect();

    let def = self.defs.get(&function_def_id);
    let name = def.name;

    let directive_attrs: Vec<_> = match &def.kind {
      DefinitionKind::Function(function) => function
        .attrs
        .iter()
        .filter_map(|attr| match attr {
          FunctionAttr::Directive(metadata) => Some(metadata.clone()),
          _ => None,
        })
        .collect(),
      _ => return,
    };

    for (index, metadata) in directive_attrs.into_iter().enumerate() {
      let origin_attr_span = directive_attr_spans
        .get(index)
        .cloned()
        .unwrap_or_else(|| func.signature.span.clone());

      self.directive_registry.register_definition(DirectiveDefinition {
        id: DirectiveDefId::new(0),
        function_def_id,
        name,
        target: metadata.target,
        phase: metadata.phase,
        effect: metadata.effect,
        group: metadata.group,
        capabilities: metadata.capabilities,
        provenance: DirectiveProvenance { origin_attr_span },
      });
    }
  }

  fn parse_directive_target(
    &mut self,
    arg: &ignis_ast::attribute::ASTAttributeArg,
  ) -> Option<DirectiveTarget> {
    let value = self.extract_attr_name_value("directive", arg)?;

    match value.as_str() {
      "record" => Some(DirectiveTarget::Record),
      "enum" => Some(DirectiveTarget::Enum),
      "function" => Some(DirectiveTarget::Function),
      _ => {
        self.add_diagnostic(
          DiagnosticMessage::UnknownDirectiveMetadata {
            name: format!("target={}", value),
            span: arg.span().clone(),
          }
          .report(),
        );
        None
      },
    }
  }

  fn parse_directive_phase(
    &mut self,
    arg: &ignis_ast::attribute::ASTAttributeArg,
  ) -> Option<DirectivePhase> {
    let value = self.extract_attr_name_value("directive", arg)?;

    match value.as_str() {
      "check" => Some(DirectivePhase::Check),
      "expand" => Some(DirectivePhase::Expand),
      "collect" => Some(DirectivePhase::Collect),
      "finalize" => Some(DirectivePhase::Finalize),
      "transform" => Some(DirectivePhase::Transform),
      _ => {
        self.add_diagnostic(
          DiagnosticMessage::UnknownDirectiveMetadata {
            name: format!("phase={}", value),
            span: arg.span().clone(),
          }
          .report(),
        );
        None
      },
    }
  }

  fn parse_directive_effect(
    &mut self,
    arg: &ignis_ast::attribute::ASTAttributeArg,
  ) -> Option<DirectiveEffect> {
    let value = self.extract_attr_name_value("directive", arg)?;

    match value.as_str() {
      "diagnose" => Some(DirectiveEffect::Diagnose),
      "emit" => Some(DirectiveEffect::Emit),
      "collect" => Some(DirectiveEffect::Collect),
      "transform" => Some(DirectiveEffect::Transform),
      _ => {
        self.add_diagnostic(
          DiagnosticMessage::UnknownDirectiveMetadata {
            name: format!("effect={}", value),
            span: arg.span().clone(),
          }
          .report(),
        );
        None
      },
    }
  }

  fn parse_directive_capability(
    &mut self,
    arg: &ignis_ast::attribute::ASTAttributeArg,
  ) -> Option<DirectiveCapability> {
    let value = self.extract_attr_name_value("directive", arg)?;

    match value.as_str() {
      "diagnostics" => Some(DirectiveCapability::Diagnostics),
      "filesystem" => Some(DirectiveCapability::FileSystem),
      "network" => Some(DirectiveCapability::Network),
      "process" => Some(DirectiveCapability::Process),
      "ffi" => Some(DirectiveCapability::Ffi),
      "clock" => Some(DirectiveCapability::Clock),
      _ => {
        self.add_diagnostic(
          DiagnosticMessage::UnknownDirectiveMetadata {
            name: format!("capabilities={}", value),
            span: arg.span().clone(),
          }
          .report(),
        );
        None
      },
    }
  }

  fn extract_attr_name_value(
    &mut self,
    attr_name: &str,
    arg: &ignis_ast::attribute::ASTAttributeArg,
  ) -> Option<String> {
    match arg {
      ignis_ast::attribute::ASTAttributeArg::Identifier(sym, _) => Some(self.get_symbol_name(sym).clone()),
      ignis_ast::attribute::ASTAttributeArg::StringLiteral(value, _) => Some(value.clone()),
      ignis_ast::attribute::ASTAttributeArg::IntLiteral(_, span) => {
        self.add_diagnostic(
          DiagnosticMessage::AttributeExpectedIdentifier {
            attr: attr_name.to_string(),
            span: span.clone(),
          }
          .report(),
        );
        None
      },
    }
  }

  fn directive_phase_name(
    &self,
    phase: &DirectivePhase,
  ) -> &'static str {
    match phase {
      DirectivePhase::Check => "check",
      DirectivePhase::Expand => "expand",
      DirectivePhase::Collect => "collect",
      DirectivePhase::Finalize => "finalize",
      DirectivePhase::Transform => "transform",
    }
  }

  fn directive_effect_name(
    &self,
    effect: &DirectiveEffect,
  ) -> &'static str {
    match effect {
      DirectiveEffect::Diagnose => "diagnose",
      DirectiveEffect::Emit => "emit",
      DirectiveEffect::Collect => "collect",
      DirectiveEffect::Transform => "transform",
    }
  }

  fn directive_target_name(
    &self,
    target: &DirectiveTarget,
  ) -> &'static str {
    match target {
      DirectiveTarget::Record => "record",
      DirectiveTarget::Enum => "enum",
      DirectiveTarget::Function => "function",
    }
  }

  fn format_attribute_path(
    &self,
    attr: &ASTAttribute,
  ) -> String {
    attr
      .path
      .iter()
      .map(|segment| self.get_symbol_name(segment))
      .collect::<Vec<_>>()
      .join("::")
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
            "unused_mut" => directives.push((LintId::UnusedMut, level)),
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
