use crate::{Analyzer, ScopeKind};
use ignis_ast::{
  expressions::{ASTExpression, ASTPathSegment},
  statements::ASTStatement,
  ASTNode, NodeId,
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::definition::{DefinitionId, SymbolEntry};

/// Result of resolving a qualified path expression.
///
/// Enum variants are not stored as separate definitions, so we need to distinguish
/// between a path that resolves to a definition (variable, function, constant, type)
/// and a path that resolves to an enum variant.
#[derive(Debug, Clone)]
pub enum ResolvedPath {
  /// Path resolves to a definition (variable, function, constant, namespace, type).
  Entry(SymbolEntry),

  /// Path resolves to an enum variant.
  EnumVariant { enum_def: DefinitionId, variant_index: u32 },
}

impl<'a> Analyzer<'a> {
  pub fn resolve_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    self.reset_scopes(roots);

    for root in roots {
      self.resolve_node(root, ScopeKind::Global);
    }
  }

  fn resolve_node(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
  ) {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Statement(stmt) => self.resolve_statement(node_id, stmt, scope_kind),
      ASTNode::Expression(expr) => self.resolve_expression(Some(node_id), expr, scope_kind),
    }
  }

  fn resolve_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    scope_kind: ScopeKind,
  ) {
    match stmt {
      ASTStatement::Variable(var) => {
        if let Some(value_id) = &var.value {
          self.resolve_node(value_id, scope_kind);
        }
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Function(func) => {
        let def_id = self.define_decl_in_current_scope(node_id);
        let mut pushed_generic = false;

        if let Some(def_id) = &def_id
          && let ignis_type::definition::DefinitionKind::Function(func_def) = &self.defs.get(def_id).kind
          && !func_def.type_params.is_empty()
        {
          self.scopes.push(ScopeKind::Generic);
          pushed_generic = true;

          for param_id in &func_def.type_params {
            let name = self.defs.get(param_id).name;
            let _ = self.scopes.define(&name, param_id, false);
          }
        }

        self.scopes.push(ScopeKind::Function);
        if let Some(def_id) = &def_id {
          self.define_function_params_in_scope(def_id);
        }
        if let Some(body_id) = &func.body {
          self.resolve_node(body_id, ScopeKind::Function);
        }
        self.scopes.pop();

        if pushed_generic {
          self.scopes.pop();
        }
      },
      ASTStatement::Constant(const_) => {
        // Only resolve value if it exists (not for extern const)
        if let Some(value_id) = &const_.value {
          self.resolve_node(value_id, scope_kind);
        }
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Block(block) => {
        self.scopes.push(ScopeKind::Block);
        for stmt_id in &block.statements {
          self.resolve_node(stmt_id, ScopeKind::Block);
        }
        self.scopes.pop();
      },
      ASTStatement::If(if_stmt) => {
        self.resolve_node(&if_stmt.condition, scope_kind);
        self.resolve_node(&if_stmt.then_block, ScopeKind::Block);
        if let Some(else_branch) = &if_stmt.else_block {
          self.resolve_node(else_branch, ScopeKind::Block);
        }
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.resolve_node(&while_stmt.condition, ScopeKind::Loop);
        self.resolve_node(&while_stmt.body, ScopeKind::Loop);

        self.scopes.pop();
      },
      ASTStatement::For(for_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.resolve_node(&for_stmt.initializer, ScopeKind::Loop);
        self.resolve_node(&for_stmt.condition, ScopeKind::Loop);
        self.resolve_node(&for_stmt.increment, ScopeKind::Loop);
        self.resolve_node(&for_stmt.body, ScopeKind::Loop);

        self.scopes.pop();
      },
      ASTStatement::ForOf(for_of) => {
        with_for_of_scope!(self, node_id, for_of, {
          self.resolve_node(&for_of.iter, ScopeKind::Loop);
          self.resolve_node(&for_of.body, ScopeKind::Loop);
        });
      },
      ASTStatement::Return(ret) => {
        if let Some(value) = &ret.expression {
          self.resolve_node(value, scope_kind);
        }
      },
      ASTStatement::Expression(expr) => {
        self.resolve_expression(None, expr, scope_kind);
      },
      ASTStatement::Extern(extern_stmt) => {
        for item in &extern_stmt.items {
          self.resolve_node(item, scope_kind);
        }
      },
      ASTStatement::Namespace(ns_stmt) => {
        for item in &ns_stmt.items {
          self.resolve_node(item, scope_kind);
        }
      },
      ASTStatement::Export(export_stmt) => match export_stmt {
        ignis_ast::statements::ASTExport::Declaration { decl, .. } => {
          self.resolve_node(decl, scope_kind);
        },
        ignis_ast::statements::ASTExport::Name { name, span } => {
          if let Some(def_id) = self.scopes.lookup_def(name).cloned() {
            self.mark_referenced(def_id);
          } else {
            let symbol = self.get_symbol_name(name);
            self.add_diagnostic(
              DiagnosticMessage::UndeclaredIdentifier {
                name: symbol,
                span: span.clone(),
              }
              .report(),
            );
          }
        },
      },
      _ => {},
    }
  }

  fn resolve_expression(
    &mut self,
    node_id: Option<&NodeId>,
    expr: &ASTExpression,
    scope_kind: ScopeKind,
  ) {
    match expr {
      ASTExpression::Variable(var_expr) => {
        if self.is_builtin_name(&var_expr.name) {
          return;
        }

        let symbol_entry = self.scopes.lookup(&var_expr.name);
        match symbol_entry {
          Some(SymbolEntry::Single(def_id)) => {
            if let Some(nid) = node_id {
              let def_id = *def_id;
              self.set_def(nid, &def_id);
              self.mark_referenced(def_id);
            }
          },
          Some(SymbolEntry::Overload(_)) => {},
          None => {
            self.add_diagnostic(
              DiagnosticMessage::UndeclaredVariable {
                name: self.get_symbol_name(&var_expr.name),
                span: var_expr.span.clone(),
              }
              .report(),
            );
          },
        }
      },
      ASTExpression::Call(call_expr) => {
        let prev = self.in_callee_context;
        self.in_callee_context = true;
        self.resolve_node(&call_expr.callee, scope_kind);
        self.in_callee_context = prev;

        for arg in &call_expr.arguments {
          self.resolve_node(arg, scope_kind);
        }
      },
      ASTExpression::Binary(binary) => {
        self.resolve_node(&binary.left, scope_kind);
        self.resolve_node(&binary.right, scope_kind);
      },
      ASTExpression::Ternary(ternary) => {
        self.resolve_node(&ternary.condition, scope_kind);
        self.resolve_node(&ternary.then_expr, scope_kind);
        self.resolve_node(&ternary.else_expr, scope_kind);
      },
      ASTExpression::Unary(unary) => {
        self.resolve_node(&unary.operand, scope_kind);
      },
      ASTExpression::Assignment(assign) => {
        self.resolve_node(&assign.target, scope_kind);
        self.resolve_node(&assign.value, scope_kind);
      },
      ASTExpression::Cast(cast) => {
        self.resolve_node(&cast.expression, scope_kind);
      },
      ASTExpression::Reference(ref_) => {
        self.resolve_node(&ref_.inner, scope_kind);
      },
      ASTExpression::Dereference(deref) => {
        self.resolve_node(&deref.inner, scope_kind);
      },
      ASTExpression::VectorAccess(access) => {
        self.resolve_node(&access.name, scope_kind);
        self.resolve_node(&access.index, scope_kind);
      },
      ASTExpression::Grouped(grouped) => {
        self.resolve_node(&grouped.expression, scope_kind);
      },
      ASTExpression::Vector(vector) => {
        for elem in &vector.items {
          self.resolve_node(elem, scope_kind);
        }
      },
      ASTExpression::Path(path) => {
        // Skip resolution for builtin type constructors (e.g., Rc::new)
        if !path.segments.is_empty() && self.is_builtin_name(&path.segments[0].name) {
          return;
        }

        let full_path = || {
          path
            .segments
            .iter()
            .map(|s| self.get_symbol_name(&s.name))
            .collect::<Vec<_>>()
            .join("::")
        };

        match self.resolve_qualified_path(&path.segments) {
          Some(ResolvedPath::Entry(entry)) => {
            use ignis_type::definition::DefinitionKind;

            match entry {
              SymbolEntry::Single(def_id) => {
                let def = self.defs.get(&def_id);
                match &def.kind {
                  DefinitionKind::Constant(_) => {
                    if let Some(nid) = node_id {
                      self.set_def(nid, &def_id);
                    }
                  },
                  DefinitionKind::Function(_) | DefinitionKind::Method(_) => {
                    if self.in_callee_context {
                      if let Some(nid) = node_id {
                        self.set_def(nid, &def_id);
                      }
                    } else {
                      self.add_diagnostic(
                        DiagnosticMessage::FunctionPathNotAsCallee {
                          name: full_path(),
                          span: path.span.clone(),
                        }
                        .report(),
                      );
                    }
                  },
                  _ => {
                    self.add_diagnostic(
                      DiagnosticMessage::UnsupportedPathExpression {
                        name: full_path(),
                        span: path.span.clone(),
                      }
                      .report(),
                    );
                  },
                }
              },
              SymbolEntry::Overload(_) => {
                if !self.in_callee_context {
                  self.add_diagnostic(
                    DiagnosticMessage::OverloadGroupAsValue {
                      name: full_path(),
                      span: path.span.clone(),
                    }
                    .report(),
                  );
                }
                // For overloads, we don't set_def here as it's ambiguous.
                // Typecheck will resolve it.
              },
            }
          },
          Some(ResolvedPath::EnumVariant { .. }) => {
            // Enum variants are valid path expressions, no def to set
          },
          None => {
            // Emit more specific errors for enum-related path failures
            let diagnostic = self.diagnose_path_failure(&path.segments, &path.span, full_path);
            self.add_diagnostic(diagnostic.report());
          },
        }
      },
      ASTExpression::Literal(_) => {},
      ASTExpression::PostfixIncrement { expr, .. } => {
        self.resolve_node(expr, scope_kind);
      },
      ASTExpression::PostfixDecrement { expr, .. } => {
        self.resolve_node(expr, scope_kind);
      },
      ASTExpression::MemberAccess(ma) => {
        self.resolve_node(&ma.object, scope_kind);
      },
      ASTExpression::RecordInit(ri) => {
        for field in &ri.fields {
          self.resolve_node(&field.value, scope_kind);
        }
      },
      ASTExpression::BuiltinCall(bc) => {
        for arg_id in &bc.args {
          self.resolve_node(arg_id, scope_kind);
        }
      },
    }
  }

  /// Resolve a qualified path (`Math::add`) or simple name (`add`).
  ///
  /// Returns `ResolvedPath::Def` for definitions and `ResolvedPath::EnumVariant`
  /// for enum variants (which are not stored as separate definitions).
  pub fn resolve_qualified_path(
    &self,
    segments: &[ASTPathSegment],
  ) -> Option<ResolvedPath> {
    use ignis_type::definition::DefinitionKind;

    if segments.is_empty() {
      return None;
    }

    if segments.len() == 1 {
      return self.scopes.lookup(&segments[0].name).cloned().map(ResolvedPath::Entry);
    }

    let ns_path = &segments[..segments.len() - 1];
    let def_name = &segments[segments.len() - 1].name;

    // Check for imported namespace in scope
    if let Some(def_id) = self.scopes.lookup_def(&ns_path[0].name) {
      let def = self.defs.get(def_id);

      match &def.kind {
        DefinitionKind::Namespace(ns_def) => {
          let mut current_ns = ns_def.namespace_id;

          for segment in &ns_path[1..] {
            current_ns = self.namespaces.lookup_child(current_ns, &segment.name)?;
          }

          return self
            .namespaces
            .lookup_def(current_ns, def_name)
            .cloned()
            .map(ResolvedPath::Entry);
        },

        // Handle Record::member (static methods/fields)
        DefinitionKind::Record(rd) if ns_path.len() == 1 => {
          if let Some(entry) = rd.static_methods.get(def_name) {
            return Some(ResolvedPath::Entry(entry.clone()));
          }
          if let Some(&field_id) = rd.static_fields.get(def_name) {
            return Some(ResolvedPath::Entry(SymbolEntry::Single(field_id)));
          }
          return None;
        },

        // Handle Enum::Variant or Enum::member (static methods/fields)
        DefinitionKind::Enum(ed) if ns_path.len() == 1 => {
          // Check static methods first
          if let Some(entry) = ed.static_methods.get(def_name) {
            return Some(ResolvedPath::Entry(entry.clone()));
          }
          // Check static fields
          if let Some(&field_id) = ed.static_fields.get(def_name) {
            return Some(ResolvedPath::Entry(SymbolEntry::Single(field_id)));
          }
          // Check enum variants
          if let Some(&tag) = ed.variants_by_name.get(def_name) {
            return Some(ResolvedPath::EnumVariant {
              enum_def: *def_id,
              variant_index: tag,
            });
          }
          return None;
        },

        _ => {},
      }
    }

    // Namespace defined in current module
    let ns_syms: Vec<_> = ns_path.iter().map(|s| s.name).collect();
    let ns_id = self.namespaces.lookup(&ns_syms)?;
    self
      .namespaces
      .lookup_def(ns_id, def_name)
      .cloned()
      .map(ResolvedPath::Entry)
  }

  /// Diagnose why a path resolution failed and return a specific error.
  fn diagnose_path_failure<F: Fn() -> String>(
    &self,
    segments: &[ASTPathSegment],
    span: &ignis_type::span::Span,
    full_path: F,
  ) -> DiagnosticMessage {
    use ignis_type::definition::DefinitionKind;

    // For 2-segment paths like Foo::Bar, check if first segment is an enum
    if segments.len() == 2
      && let Some(def_id) = self.scopes.lookup_def(&segments[0].name)
    {
      let def = self.defs.get(def_id);
      let first_name = self.get_symbol_name(&segments[0].name);
      let second_name = self.get_symbol_name(&segments[1].name);

      match &def.kind {
        DefinitionKind::Enum(_) => {
          // Enum exists but variant not found
          return DiagnosticMessage::EnumVariantNotFound {
            enum_name: first_name,
            variant: second_name,
            span: span.clone(),
          };
        },
        DefinitionKind::Record(_) | DefinitionKind::Namespace(_) => {
          // Fall through to generic error - member not found
        },
        _ => {
          // Not an enum/record/namespace, but user tried to access member
          return DiagnosticMessage::EnumNotFound {
            name: first_name,
            span: span.clone(),
          };
        },
      }
    }

    // Default to generic undeclared identifier
    DiagnosticMessage::UndeclaredIdentifier {
      name: full_path(),
      span: span.clone(),
    }
  }
}
