use crate::{Analyzer, PipeArgInsertion, PipeResolution, ResolvedPath, ScopeKind};
use ignis_ast::{ASTNode, NodeId, statements::ASTStatement, expressions::ASTExpression, pattern::ASTPattern};
use ignis_ast::expressions::binary::ASTBinaryOperator;
use ignis_ast::expressions::builtin_call::ASTBuiltinCall;
use ignis_ast::expressions::assignment::ASTAssignmentOperator;
use ignis_ast::expressions::member_access::ASTAccessOp;
use ignis_ast::expressions::unary::UnaryOperator;
use ignis_ast::statements::record::{ASTMethod, ASTRecord, ASTRecordItem};
use ignis_ast::statements::enum_::{ASTEnum, ASTEnumItem};
use ignis_ast::statements::for_of::ASTForOf;
use ignis_hir::{
  HIR, HIRNode, HIRKind, HIRId, HIRMatchArm, HIRPattern,
  operation::{BinaryOperation, UnaryOperation},
  statement::LoopKind,
};
use ignis_type::definition::{Definition, DefinitionId, DefinitionKind, SymbolEntry, VariableDefinition, Visibility};
use ignis_type::span::Span;
use ignis_type::symbol::SymbolId;
use ignis_type::types::{Substitution, Type, TypeId};

struct ForOfContext<'a> {
  for_of: &'a ASTForOf,
  synth_id: u32,
  element_type: TypeId,
  iter_hir: HIRId,
  is_by_ref: bool,
  is_mut_ref: bool,
  binding_def_id: DefinitionId,
  span: &'a Span,
}

impl<'a> Analyzer<'a> {
  pub fn lower_to_hir(
    &mut self,
    roots: &[NodeId],
  ) -> HIR {
    let mut hir = HIR::new();

    self.lowering_return_type_stack.clear();

    for root in roots {
      self.lower_node_to_hir(root, &mut hir, ScopeKind::Global);
    }

    // Lower trait default method bodies per-implementing record.
    // Each cloned method gets its own freshly lowered HIR body with correct `self` type.
    let clones: Vec<_> = self.trait_default_clones.iter().map(|(&k, &v)| (k, v)).collect();
    for (cloned_id, body_node) in clones {
      self.enter_type_params_scope_for_method(&cloned_id);
      self.scopes.push(ScopeKind::Function);
      self.define_function_params_in_scope(&cloned_id);

      let has_return_context = if let Some(return_type) = self.callable_return_type(&cloned_id) {
        self.lowering_return_type_stack.push(return_type);
        true
      } else {
        false
      };

      let body_hir_id = self.lower_node_to_hir(&body_node, &mut hir, ScopeKind::Function);

      if has_return_context {
        self.lowering_return_type_stack.pop();
      }

      self.scopes.pop();
      self.exit_type_params_scope_for_method(&cloned_id);

      hir.function_bodies.insert(cloned_id, body_hir_id);
      hir.items.push(cloned_id);
    }

    hir
  }

  fn callable_return_type(
    &self,
    def_id: &DefinitionId,
  ) -> Option<TypeId> {
    match &self.defs.get(def_id).kind {
      DefinitionKind::Function(fd) => Some(fd.return_type),
      DefinitionKind::Method(md) => Some(md.return_type),
      _ => None,
    }
  }

  fn current_lowering_return_type(&self) -> Option<TypeId> {
    self.lowering_return_type_stack.last().copied()
  }

  fn lower_node_to_hir(
    &mut self,
    node_id: &NodeId,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let node = self.ast.get(node_id).clone();

    match node {
      ASTNode::Statement(stmt) => self.lower_statement_to_hir(node_id, &stmt, hir, scope_kind),
      ASTNode::Expression(expr) => self.lower_expression_to_hir(node_id, &expr, hir, scope_kind),
    }
  }

  fn lower_statement_to_hir(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    match stmt {
      ASTStatement::Variable(var) => {
        let def_id = match self.lookup_def(node_id).cloned() {
          Some(id) => id,
          None => match self.scopes.lookup_def(&var.name).cloned() {
            Some(id) => id,
            None => {
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span: var.span.clone(),
                type_id: self.types.error(),
              });
            },
          },
        };
        let init = var
          .value
          .as_ref()
          .map(|value_id| self.lower_node_to_hir(value_id, hir, scope_kind));

        if let Some(init_id) = init {
          hir.variables_inits.insert(def_id, init_id);
        }

        self.scopes.define(&var.name, &def_id, false).ok();

        let hir_node = HIRNode {
          kind: HIRKind::Let {
            name: def_id,
            value: init,
          },
          span: var.span.clone(),
          type_id: self.types.void(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Function(func) => {
        let def_id = match self.lookup_def(node_id).cloned() {
          Some(id) => id,
          None => match self.scopes.lookup_def(&func.signature.name).cloned() {
            Some(id) => id,
            None => {
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span: func.signature.span.clone(),
                type_id: self.types.error(),
              });
            },
          },
        };

        if let Some(body_id) = func.body {
          // First, add type params to scope so type resolution works
          self.enter_type_params_scope_for_function(&def_id);
          self.scopes.push(ScopeKind::Function);

          self.define_function_params_in_scope(&def_id);

          let has_return_context = if let Some(return_type) = self.callable_return_type(&def_id) {
            self.lowering_return_type_stack.push(return_type);
            true
          } else {
            false
          };

          let body_hir_id = self.lower_node_to_hir(&body_id, hir, ScopeKind::Function);

          if has_return_context {
            self.lowering_return_type_stack.pop();
          }

          self.scopes.pop();
          self.exit_type_params_scope_for_function(&def_id);
          hir.function_bodies.insert(def_id, body_hir_id);
        }

        let hir_node = HIRNode {
          kind: HIRKind::Block {
            statements: Vec::new(),
            expression: None,
          },
          span: func.signature.span.clone(),
          type_id: self.types.void(),
        };

        let node_id = hir.alloc(hir_node);
        hir.items.push(def_id);

        let main_symbol = { self.symbols.borrow_mut().get_or_intern("main") };
        if func.signature.name == main_symbol {
          hir.entry_point = Some(def_id);
        }

        node_id
      },
      ASTStatement::Constant(const_) => {
        let def_id = match self.lookup_def(node_id).cloned() {
          Some(id) => id,
          None => match self.scopes.lookup_def(&const_.name).cloned() {
            Some(id) => id,
            None => {
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span: const_.span.clone(),
                type_id: self.types.error(),
              });
            },
          },
        };

        let init_hir = const_.value.as_ref().map(|value_id| {
          let init = self.lower_node_to_hir(value_id, hir, scope_kind);
          hir.variables_inits.insert(def_id, init);
          init
        });

        let hir_node = HIRNode {
          kind: HIRKind::Let {
            name: def_id,
            value: init_hir,
          },
          span: const_.span.clone(),
          type_id: self.types.void(),
        };

        let node_id = hir.alloc(hir_node);
        hir.items.push(def_id);

        node_id
      },
      ASTStatement::Block(block) => {
        self.scopes.push(ScopeKind::Block);

        let stmt_hir_ids: Vec<_> = block
          .statements
          .iter()
          .map(|stmt_id| self.lower_node_to_hir(stmt_id, hir, ScopeKind::Block))
          .collect();

        self.scopes.pop();

        let block_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.void());

        let hir_node = HIRNode {
          kind: HIRKind::Block {
            statements: stmt_hir_ids,
            expression: None,
          },
          span: block.span.clone(),
          type_id: block_type,
        };

        hir.alloc(hir_node)
      },
      ASTStatement::If(if_stmt) => {
        let condition_id = self.lower_node_to_hir(&if_stmt.condition, hir, scope_kind);
        let then_id = self.lower_node_to_hir(&if_stmt.then_block, hir, ScopeKind::Block);

        let else_id = if_stmt
          .else_block
          .as_ref()
          .map(|else_branch| self.lower_node_to_hir(else_branch, hir, ScopeKind::Block));

        let if_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.void());

        let hir_node = HIRNode {
          kind: HIRKind::If {
            condition: condition_id,
            then_branch: then_id,
            else_branch: else_id,
          },
          span: if_stmt.span.clone(),
          type_id: if_type,
        };

        hir.alloc(hir_node)
      },
      ASTStatement::LetElse(let_else) => {
        let value = self.lower_node_to_hir(&let_else.value, hir, scope_kind);
        let else_block = self.lower_node_to_hir(&let_else.else_block, hir, ScopeKind::Block);
        let pattern = if let_else.binding_type.is_some() {
          self
            .lower_shorthand_let_else_pattern(&let_else.pattern, value, hir)
            .unwrap_or_else(|| self.lower_pattern(&let_else.pattern, None, None))
        } else {
          self.lower_pattern(&let_else.pattern, None, None)
        };

        hir.alloc(HIRNode {
          kind: HIRKind::LetElse {
            pattern,
            value,
            else_block,
          },
          span: let_else.span.clone(),
          type_id: self.types.void(),
        })
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);
        let condition_id = self.lower_node_to_hir(&while_stmt.condition, hir, ScopeKind::Loop);
        let body_id = self.lower_node_to_hir(&while_stmt.body, hir, ScopeKind::Loop);
        self.scopes.pop();

        let hir_node = HIRNode {
          kind: HIRKind::Loop {
            condition: LoopKind::While {
              condition: condition_id,
            },
            body: body_id,
          },
          span: while_stmt.span.clone(),
          type_id: self.types.void(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::For(for_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        let init = Some(self.lower_node_to_hir(&for_stmt.initializer, hir, ScopeKind::Loop));
        let condition = Some(self.lower_node_to_hir(&for_stmt.condition, hir, ScopeKind::Loop));
        let update = Some(self.lower_node_to_hir(&for_stmt.increment, hir, ScopeKind::Loop));
        let body_id = self.lower_node_to_hir(&for_stmt.body, hir, ScopeKind::Loop);

        self.scopes.pop();

        let hir_node = HIRNode {
          kind: HIRKind::Loop {
            condition: LoopKind::For {
              init,
              condition,
              update,
            },
            body: body_id,
          },
          span: for_stmt.span.clone(),
          type_id: self.types.void(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Return(ret) => {
        let value = ret
          .expression
          .as_ref()
          .map(|value_id| self.lower_node_to_hir(value_id, hir, scope_kind));

        let hir_node = HIRNode {
          kind: HIRKind::Return(value),
          span: ret.span.clone(),
          type_id: self.types.never(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Break(brk) => {
        let hir_node = HIRNode {
          kind: HIRKind::Break,
          span: brk.span.clone(),
          type_id: self.types.never(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Continue(cont) => {
        let hir_node = HIRNode {
          kind: HIRKind::Continue,
          span: cont.span.clone(),
          type_id: self.types.never(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Expression(expr) => {
        let expr_hir_id = self.lower_expression_to_hir(node_id, expr, hir, scope_kind);
        let expr_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.void());

        hir.alloc(HIRNode {
          kind: HIRKind::ExpressionStatement(expr_hir_id),
          span: expr.span().clone(),
          type_id: expr_type,
        })
      },
      ASTStatement::Extern(ext) => {
        // Lower all items in extern block
        let mut hir_ids = Vec::new();
        for item in &ext.items {
          hir_ids.push(self.lower_node_to_hir(item, hir, scope_kind));
        }
        // Return a block containing all extern items
        hir.alloc(HIRNode {
          kind: HIRKind::Block {
            statements: hir_ids,
            expression: None,
          },
          span: ext.span.clone(),
          type_id: self.types.void(),
        })
      },
      ASTStatement::Namespace(ns) => {
        // Push a scope containing the namespace's definitions so that
        // record/enum lowering inside the namespace can find their own
        // DefinitionIds (e.g., PathBuf inside `namespace Path { ... }`).
        let ns_defs = self.collect_namespace_defs(&ns.path);

        self.scopes.push(ScopeKind::Block);
        for (sym, def_id) in &ns_defs {
          let _ = self.scopes.define(sym, def_id, true);
        }

        let mut hir_ids = Vec::new();
        for item in &ns.items {
          hir_ids.push(self.lower_node_to_hir(item, hir, scope_kind));
        }

        self.scopes.pop();

        hir.alloc(HIRNode {
          kind: HIRKind::Block {
            statements: hir_ids,
            expression: None,
          },
          span: ns.span.clone(),
          type_id: self.types.void(),
        })
      },
      ASTStatement::Export(exp) => match exp {
        ignis_ast::statements::ASTExport::Declaration { decl, .. } => self.lower_node_to_hir(decl, hir, scope_kind),
        ignis_ast::statements::ASTExport::Name { .. } | ignis_ast::statements::ASTExport::ReExportFrom { .. } => hir
          .alloc(HIRNode {
            kind: HIRKind::Block {
              statements: Vec::new(),
              expression: None,
            },
            span: exp.span().clone(),
            type_id: self.types.void(),
          }),
      },
      ASTStatement::Record(record) => {
        // Lower record methods to HIR
        self.lower_record_methods(record, hir, scope_kind)
      },
      ASTStatement::Enum(enum_) => {
        // Lower enum methods to HIR
        self.lower_enum_methods(enum_, hir, scope_kind)
      },
      ASTStatement::Trait(tr) => self.lower_trait_methods(tr, hir, scope_kind),
      ASTStatement::ForOf(for_of) => self.lower_for_of(node_id, for_of, hir),
      ASTStatement::Defer(d) => {
        let expr_id = self.lower_node_to_hir(&d.expression, hir, scope_kind);

        hir.alloc(HIRNode {
          kind: HIRKind::Defer { body: expr_id },
          span: d.span.clone(),
          type_id: self.types.void(),
        })
      },
      _ => hir.alloc(HIRNode {
        kind: HIRKind::Block {
          statements: Vec::new(),
          expression: None,
        },
        span: Span::default(),
        type_id: self.types.void(),
      }),
    }
  }

  /// Find the matching method definition for an AST method from a SymbolEntry.
  ///
  /// For `SymbolEntry::Single`, returns the single definition.
  /// For `SymbolEntry::Overload`, matches by parameter count first, then by span if needed.
  pub(crate) fn find_method_def_for_ast_method(
    &self,
    entry: &SymbolEntry,
    ast_method: &ASTMethod,
  ) -> Option<DefinitionId> {
    match entry {
      SymbolEntry::Single(def_id) => Some(*def_id),

      SymbolEntry::Overload(group) => {
        let ast_param_count = ast_method.parameters.len();
        let has_self = ast_method.self_param.is_some();

        // First pass: find candidates with matching parameter count.
        // After typechecking, md.params includes the injected `self` parameter
        // as the first element, but ast_method.parameters does not include self.
        let mut candidates: Vec<DefinitionId> = Vec::new();
        for &def_id in group {
          let def = self.defs.get(&def_id);
          if let DefinitionKind::Method(md) = &def.kind {
            let expected = if has_self && !md.is_static {
              ast_param_count + 1
            } else {
              ast_param_count
            };
            if md.params.len() == expected {
              candidates.push(def_id);
            }
          }
        }

        match candidates.len() {
          0 => None,
          1 => Some(candidates[0]),
          _ => {
            // Multiple candidates with same param count - match by span
            for def_id in &candidates {
              let def = self.defs.get(def_id);
              if def.span == ast_method.span {
                return Some(*def_id);
              }
            }
            None
          },
        }
      },
    }
  }

  /// Lower record methods to HIR.
  /// Iterates through all methods in the record and lowers their bodies.
  fn lower_record_methods(
    &mut self,
    record: &ASTRecord,
    hir: &mut HIR,
    _scope_kind: ScopeKind,
  ) -> HIRId {
    for item in &record.items {
      if let ASTRecordItem::Method(method) = item {
        // Find the method's DefinitionId by looking it up in the record definition
        let record_def_id = self.scopes.lookup_def(&record.name).cloned();

        let Some(record_def_id) = record_def_id else {
          continue;
        };

        let method_def_id = {
          let def = self.defs.get(&record_def_id);
          let DefinitionKind::Record(rd) = &def.kind else {
            continue;
          };

          // Look up method entry (may be Single or Overload)
          let entry = if method.is_static() {
            rd.static_methods.get(&method.name)
          } else {
            rd.instance_methods.get(&method.name)
          };

          entry.and_then(|e| self.find_method_def_for_ast_method(e, method))
        };

        let Some(method_def_id) = method_def_id else {
          continue;
        };

        // Lower the method body
        // First, add owner/method type params to scope so type resolution works
        self.enter_type_params_scope_for_method(&method_def_id);
        self.scopes.push(ScopeKind::Function);
        self.define_function_params_in_scope(&method_def_id);

        let has_return_context = if let Some(return_type) = self.callable_return_type(&method_def_id) {
          self.lowering_return_type_stack.push(return_type);
          true
        } else {
          false
        };

        let body_hir_id = self.lower_node_to_hir(&method.body, hir, ScopeKind::Function);

        if has_return_context {
          self.lowering_return_type_stack.pop();
        }

        self.scopes.pop();
        self.exit_type_params_scope_for_method(&method_def_id);

        // Register the method body
        hir.function_bodies.insert(method_def_id, body_hir_id);
        hir.items.push(method_def_id);
      }
    }

    // Return an empty block as the record statement result
    hir.alloc(HIRNode {
      kind: HIRKind::Block {
        statements: Vec::new(),
        expression: None,
      },
      span: record.span.clone(),
      type_id: self.types.void(),
    })
  }

  /// Lower enum methods to HIR.
  /// Iterates through all methods in the enum and lowers their bodies.
  fn lower_enum_methods(
    &mut self,
    enum_: &ASTEnum,
    hir: &mut HIR,
    _scope_kind: ScopeKind,
  ) -> HIRId {
    for item in &enum_.items {
      if let ASTEnumItem::Method(method) = item {
        // Find the method's DefinitionId by looking it up in the enum definition
        let enum_def_id = self.scopes.lookup_def(&enum_.name).cloned();

        let Some(enum_def_id) = enum_def_id else {
          continue;
        };

        let method_def_id = {
          let def = self.defs.get(&enum_def_id);
          let DefinitionKind::Enum(ed) = &def.kind else {
            continue;
          };

          let is_static = method.is_static() || !method.has_self();
          let entry = if is_static {
            ed.static_methods.get(&method.name)
          } else {
            ed.instance_methods.get(&method.name)
          };

          entry.and_then(|e| self.find_method_def_for_ast_method(e, method))
        };

        let Some(method_def_id) = method_def_id else {
          continue;
        };

        // Lower the method body
        // First, add owner/method type params to scope so type resolution works
        self.enter_type_params_scope_for_method(&method_def_id);
        self.scopes.push(ScopeKind::Function);
        self.define_function_params_in_scope(&method_def_id);

        let has_return_context = if let Some(return_type) = self.callable_return_type(&method_def_id) {
          self.lowering_return_type_stack.push(return_type);
          true
        } else {
          false
        };

        let body_hir_id = self.lower_node_to_hir(&method.body, hir, ScopeKind::Function);

        if has_return_context {
          self.lowering_return_type_stack.pop();
        }

        self.scopes.pop();
        self.exit_type_params_scope_for_method(&method_def_id);

        // Register the method body
        hir.function_bodies.insert(method_def_id, body_hir_id);
        hir.items.push(method_def_id);
      }
    }

    // Return an empty block as the enum statement result
    hir.alloc(HIRNode {
      kind: HIRKind::Block {
        statements: Vec::new(),
        expression: None,
      },
      span: enum_.span.clone(),
      type_id: self.types.void(),
    })
  }

  /// Trait declarations produce no HIR. Default method bodies are lowered
  /// per-implementing record in the post-lowering step (via `trait_default_clones`).
  fn lower_trait_methods(
    &mut self,
    tr: &ignis_ast::statements::ASTTrait,
    hir: &mut HIR,
    _scope_kind: ScopeKind,
  ) -> HIRId {
    hir.alloc(HIRNode {
      kind: HIRKind::Block {
        statements: Vec::new(),
        expression: None,
      },
      span: tr.span.clone(),
      type_id: self.types.void(),
    })
  }

  fn lower_expression_to_hir(
    &mut self,
    node_id: &NodeId,
    expr: &ASTExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    match expr {
      ASTExpression::Literal(lit) => {
        let lit_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| match &lit.value {
          ignis_type::value::IgnisLiteralValue::Int8(_) => self.types.i8(),
          ignis_type::value::IgnisLiteralValue::Int16(_) => self.types.i16(),
          ignis_type::value::IgnisLiteralValue::Int32(_) => self.types.i32(),
          ignis_type::value::IgnisLiteralValue::Int64(_) => self.types.i64(),
          ignis_type::value::IgnisLiteralValue::UnsignedInt8(_) => self.types.u8(),
          ignis_type::value::IgnisLiteralValue::UnsignedInt16(_) => self.types.u16(),
          ignis_type::value::IgnisLiteralValue::UnsignedInt32(_) => self.types.u32(),
          ignis_type::value::IgnisLiteralValue::UnsignedInt64(_) => self.types.u64(),
          ignis_type::value::IgnisLiteralValue::Float32(_) => self.types.f32(),
          ignis_type::value::IgnisLiteralValue::Float64(_) => self.types.f64(),
          ignis_type::value::IgnisLiteralValue::Boolean(_) => self.types.boolean(),
          ignis_type::value::IgnisLiteralValue::Char(_) => self.types.char(),
          ignis_type::value::IgnisLiteralValue::String(_) => self.types.str(),
          ignis_type::value::IgnisLiteralValue::Atom(_) => self.types.atom(),
          ignis_type::value::IgnisLiteralValue::Hex(_) => self.types.u32(),
          ignis_type::value::IgnisLiteralValue::Binary(_) => self.types.u8(),
          ignis_type::value::IgnisLiteralValue::Null => self.types.null_ptr(),
        });

        let hir_node = HIRNode {
          kind: HIRKind::Literal(lit.value.clone()),
          span: lit.span.clone(),
          type_id: lit_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Variable(var) => {
        let def_id = if let Some(def_id) = self.lookup_def(node_id).cloned() {
          def_id
        } else if let Some(def_id) = self.scopes.lookup_def(&var.name).cloned() {
          def_id
        } else {
          return hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: var.span.clone(),
            type_id: self.types.error(),
          });
        };

        // Handle type parameters specially - they don't have a type_of in the usual sense
        // but they are used in expressions like sizeOf(T)
        let var_type = match &self.defs.get(&def_id).kind {
          DefinitionKind::TypeParam(tp) => self.types.param(tp.owner, tp.index),
          _ => *self.type_of(&def_id),
        };

        let hir_node = HIRNode {
          kind: HIRKind::Variable(def_id),
          span: var.span.clone(),
          type_id: var_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Call(call) => {
        // Check for builtins BEFORE normal scope lookup
        let callee_node = self.ast.get(&call.callee);

        if let ASTNode::Expression(ASTExpression::Variable(var)) = callee_node {
          let name = self.symbols.borrow().get(&var.name).to_string();

          match name.as_str() {
            "typeOf" => return self.lower_typeof_builtin(call, hir, scope_kind),
            "sizeOf" => return self.lower_sizeof_builtin(call, hir),
            "alignOf" => return self.lower_alignof_builtin(call, hir),
            "maxOf" => return self.lower_maxof_builtin(call, hir),
            "minOf" => return self.lower_minof_builtin(call, hir),
            _ => {},
          }
        }

        if let ASTNode::Expression(ASTExpression::MemberAccess(ma)) = callee_node {
          return self.lower_method_call(node_id, ma, call, hir, scope_kind);
        }

        if let ASTNode::Expression(ASTExpression::Path(path)) = callee_node
          && let Some(result) = self.try_lower_path_call(node_id, path, call, hir, scope_kind)
        {
          return result;
        }

        // Get the callee def_id by looking up the variable name
        let callee_def_id = if let Some(def_id) = self.lookup_resolved_call(node_id).cloned() {
          def_id
        } else {
          match callee_node {
            ASTNode::Expression(ASTExpression::Variable(var)) => match self.scopes.lookup_def(&var.name) {
              Some(def_id) => *def_id,
              None => {
                let span = self.node_span(&call.callee).clone();
                return hir.alloc(HIRNode {
                  kind: HIRKind::Error,
                  span,
                  type_id: self.types.error(),
                });
              },
            },
            ASTNode::Expression(ASTExpression::Path(path)) => match self.resolve_qualified_path(&path.segments) {
              Some(ResolvedPath::Entry(SymbolEntry::Single(def_id))) => def_id,
              Some(ResolvedPath::Entry(SymbolEntry::Overload(_))) => {
                // Should have been resolved in typecheck
                let span = self.node_span(&call.callee).clone();
                return hir.alloc(HIRNode {
                  kind: HIRKind::Error,
                  span,
                  type_id: self.types.error(),
                });
              },
              Some(ResolvedPath::EnumVariant { .. }) => {
                // Handled by try_lower_path_call above
                let span = self.node_span(&call.callee).clone();
                return hir.alloc(HIRNode {
                  kind: HIRKind::Error,
                  span,
                  type_id: self.types.error(),
                });
              },
              None => {
                let span = self.node_span(&call.callee).clone();
                return hir.alloc(HIRNode {
                  kind: HIRKind::Error,
                  span,
                  type_id: self.types.error(),
                });
              },
            },
            _ => {
              let span = self.node_span(&call.callee).clone();
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span,
                type_id: self.types.error(),
              });
            },
          }
        };

        let is_closure_call = matches!(
          &self.defs.get(&callee_def_id).kind,
          DefinitionKind::Variable(_) | DefinitionKind::Parameter(_) | DefinitionKind::Constant(_)
        ) && matches!(
          self.types.get(self.type_of(&callee_def_id)),
          ignis_type::types::Type::Function { .. }
        );

        if is_closure_call {
          let callee_ty = *self.type_of(&callee_def_id);
          let callee_hir = hir.alloc(HIRNode {
            kind: HIRKind::Variable(callee_def_id),
            span: call.span.clone(),
            type_id: callee_ty,
          });
          let args_hir: Vec<_> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          let return_type = if let ignis_type::types::Type::Function { ret, .. } = self.types.get(&callee_ty).clone() {
            ret
          } else {
            self.types.error()
          };

          return hir.alloc(HIRNode {
            kind: HIRKind::CallClosure {
              callee: callee_hir,
              args: args_hir,
            },
            span: call.span.clone(),
            type_id: return_type,
          });
        }

        let callee_type = self.get_definition_type(&callee_def_id);
        let callee_name = self
          .symbols
          .borrow()
          .get(&self.defs.get(&callee_def_id).name)
          .to_string();

        let args_hir: Vec<_> = if callee_name == "deallocate" {
          self.lower_deallocate_args(call, hir, scope_kind)
        } else {
          call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect()
        };

        let return_type = if let ignis_type::types::Type::Function { ret, .. } = self.types.get(&callee_type).clone() {
          ret
        } else {
          self.types.error()
        };

        // Resolve type arguments: explicit first, then infer if needed
        let type_args = self.resolve_or_infer_call_type_args(&callee_def_id, call, &args_hir, hir);

        let hir_node = HIRNode {
          kind: HIRKind::Call {
            callee: callee_def_id,
            type_args,
            args: args_hir,
          },
          span: call.span.clone(),
          type_id: return_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Binary(binary) => {
        let left_id = self.lower_node_to_hir(&binary.left, hir, scope_kind);
        let right_id = self.lower_node_to_hir(&binary.right, hir, scope_kind);
        let binary_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

        let hir_node = HIRNode {
          kind: HIRKind::Binary {
            operation: convert_binary_op(&binary.operator),
            left: left_id,
            right: right_id,
          },
          span: binary.span.clone(),
          type_id: binary_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Ternary(ternary) => {
        let condition_id = self.lower_node_to_hir(&ternary.condition, hir, scope_kind);
        let then_id = self.lower_node_to_hir(&ternary.then_expr, hir, scope_kind);
        let else_id = self.lower_node_to_hir(&ternary.else_expr, hir, scope_kind);
        let ternary_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

        let hir_node = HIRNode {
          kind: HIRKind::If {
            condition: condition_id,
            then_branch: then_id,
            else_branch: Some(else_id),
          },
          span: ternary.span.clone(),
          type_id: ternary_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Unary(unary) => {
        let operand_id = self.lower_node_to_hir(&unary.operand, hir, scope_kind);
        let unary_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

        let hir_node = HIRNode {
          kind: HIRKind::Unary {
            operation: convert_unary_op(&unary.operator),
            operand: operand_id,
          },
          span: unary.span.clone(),
          type_id: unary_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Assignment(assign) => {
        let target_id = self.lower_node_to_hir(&assign.target, hir, scope_kind);
        let value_id = self.lower_node_to_hir(&assign.value, hir, scope_kind);
        let assign_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

        let op = if assign.operator == ASTAssignmentOperator::Assign {
          None
        } else {
          Some(convert_assignment_op(&assign.operator))
        };

        let hir_node = HIRNode {
          kind: HIRKind::Assign {
            target: target_id,
            value: value_id,
            operation: op,
          },
          span: assign.span.clone(),
          type_id: assign_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Cast(cast) => {
        let expr_id = self.lower_node_to_hir(&cast.expression, hir, scope_kind);
        let target_type = self.resolve_type_syntax(&cast.target_type);

        let hir_node = HIRNode {
          kind: HIRKind::Cast {
            expression: expr_id,
            target: target_type,
          },
          span: cast.span.clone(),
          type_id: target_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Reference(ref_) => {
        let expr_id = self.lower_node_to_hir(&ref_.inner, hir, scope_kind);
        let expr_type = self
          .lookup_type(&ref_.inner)
          .cloned()
          .unwrap_or_else(|| self.types.error());
        let ref_type = self.types.reference(expr_type, ref_.mutable);

        let hir_node = HIRNode {
          kind: HIRKind::Reference {
            expression: expr_id,
            mutable: ref_.mutable,
          },
          span: ref_.span.clone(),
          type_id: ref_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Dereference(deref) => {
        let expr_id = self.lower_node_to_hir(&deref.inner, hir, scope_kind);
        let expr_type = self
          .lookup_type(&deref.inner)
          .cloned()
          .unwrap_or_else(|| self.types.error());

        let deref_type = match self.types.get(&expr_type).clone() {
          ignis_type::types::Type::Pointer { inner, .. } => inner,
          ignis_type::types::Type::Reference { inner, .. } => inner,
          _ => self.types.error(),
        };

        let hir_node = HIRNode {
          kind: HIRKind::Dereference(expr_id),
          span: deref.span.clone(),
          type_id: deref_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::VectorAccess(access) => {
        let base_id = self.lower_node_to_hir(&access.name, hir, scope_kind);
        let index_id = self.lower_node_to_hir(&access.index, hir, scope_kind);
        let base_type = self
          .lookup_type(&access.name)
          .cloned()
          .unwrap_or_else(|| self.types.error());

        let elem_type = match self.types.get(&base_type).clone() {
          ignis_type::types::Type::Vector { element, .. } => element,
          ignis_type::types::Type::Pointer { inner, .. } => inner,
          _ => self.types.error(),
        };

        let hir_node = HIRNode {
          kind: HIRKind::Index {
            base: base_id,
            index: index_id,
          },
          span: access.span.clone(),
          type_id: elem_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Grouped(grouped) => self.lower_node_to_hir(&grouped.expression, hir, scope_kind),
      ASTExpression::Vector(vector) => {
        let elem_hirs: Vec<_> = vector
          .items
          .iter()
          .map(|elem| self.lower_node_to_hir(elem, hir, scope_kind))
          .collect();

        let vec_type = self.types.vector(
          vector
            .items
            .first()
            .and_then(|e| self.lookup_type(e).cloned())
            .unwrap_or_else(|| self.types.error()),
          vector.items.len(),
        );

        let hir_node = HIRNode {
          kind: HIRKind::VectorLiteral { elements: elem_hirs },
          span: vector.span.clone(),
          type_id: vec_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Path(path) => {
        match self.resolve_qualified_path(&path.segments) {
          Some(ResolvedPath::Entry(SymbolEntry::Single(def_id))) => {
            let def = self.defs.get(&def_id);
            if let DefinitionKind::Constant(c) = &def.kind {
              let type_id = c.type_id;
              if c.owner_type.is_some() {
                return hir.alloc(HIRNode {
                  kind: HIRKind::StaticAccess { def: def_id },
                  span: path.span.clone(),
                  type_id,
                });
              }

              return hir.alloc(HIRNode {
                kind: HIRKind::Variable(def_id),
                span: path.span.clone(),
                type_id,
              });
            }

            // Function/Method paths handled by try_lower_path_call
            hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span: path.span.clone(),
              type_id: self.types.error(),
            })
          },

          Some(ResolvedPath::Entry(SymbolEntry::Overload(_))) => {
            // Should have been resolved in typecheck
            hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span: path.span.clone(),
              type_id: self.types.error(),
            })
          },

          Some(ResolvedPath::EnumVariant {
            enum_def,
            variant_index,
          }) => {
            let ed = match &self.defs.get(&enum_def).kind {
              DefinitionKind::Enum(ed) => ed.clone(),
              _ => unreachable!("ResolvedPath::EnumVariant should reference an enum"),
            };

            let variant = &ed.variants[variant_index as usize];

            if !variant.payload.is_empty() {
              // Error emitted in typeck
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span: path.span.clone(),
                type_id: self.types.error(),
              });
            }

            // Extract type_args from result type if it's a generic instance
            let result_type = self.lookup_type(node_id).cloned().unwrap_or(ed.type_id);
            let type_args = match self.types.get(&result_type).clone() {
              Type::Instance { args, .. } => args,
              _ => vec![],
            };

            hir.alloc(HIRNode {
              kind: HIRKind::EnumVariant {
                enum_def,
                type_args,
                variant_tag: variant_index,
                payload: vec![],
              },
              span: path.span.clone(),
              type_id: result_type,
            })
          },

          None => hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: path.span.clone(),
            type_id: self.types.error(),
          }),
        }
      },
      ASTExpression::PostfixIncrement { expr, span } => {
        let expr_id = self.lower_node_to_hir(expr, hir, scope_kind);
        let expr_type = self.lookup_type(expr).cloned().unwrap_or_else(|| self.types.error());

        let one_lit = hir.alloc(HIRNode {
          kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::Int32(1)),
          span: span.clone(),
          type_id: expr_type,
        });

        let hir_node = HIRNode {
          kind: HIRKind::Assign {
            target: expr_id,
            value: one_lit,
            operation: Some(BinaryOperation::Add),
          },
          span: span.clone(),
          type_id: self.types.void(),
        };

        hir.alloc(hir_node)
      },
      ASTExpression::PostfixDecrement { expr, span } => {
        let expr_id = self.lower_node_to_hir(expr, hir, scope_kind);
        let expr_type = self.lookup_type(expr).cloned().unwrap_or_else(|| self.types.error());

        let one_lit = hir.alloc(HIRNode {
          kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::Int32(1)),
          span: span.clone(),
          type_id: expr_type,
        });

        let hir_node = HIRNode {
          kind: HIRKind::Assign {
            target: expr_id,
            value: one_lit,
            operation: Some(BinaryOperation::Sub),
          },
          span: span.clone(),
          type_id: self.types.void(),
        };

        hir.alloc(hir_node)
      },
      ASTExpression::MemberAccess(ma) => self.lower_member_access(node_id, ma, hir, scope_kind),
      ASTExpression::RecordInit(ri) => self.lower_record_init(node_id, ri, hir, scope_kind),
      ASTExpression::BuiltinCall(bc) => self.lower_builtin_call(bc, hir, scope_kind),
      ASTExpression::LetCondition(let_condition) => {
        let scrutinee = self.lower_node_to_hir(&let_condition.value, hir, scope_kind);
        let pattern = self.lower_pattern(&let_condition.pattern, None, None);

        let true_node = hir.alloc(HIRNode {
          kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::Boolean(true)),
          span: let_condition.span.clone(),
          type_id: self.types.boolean(),
        });

        let false_node = hir.alloc(HIRNode {
          kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::Boolean(false)),
          span: let_condition.span.clone(),
          type_id: self.types.boolean(),
        });

        hir.alloc(HIRNode {
          kind: HIRKind::Match {
            scrutinee,
            arms: vec![
              HIRMatchArm {
                pattern,
                guard: None,
                body: true_node,
              },
              HIRMatchArm {
                pattern: HIRPattern::Wildcard,
                guard: None,
                body: false_node,
              },
            ],
          },
          span: let_condition.span.clone(),
          type_id: self.types.boolean(),
        })
      },
      ASTExpression::Match(match_expr) => {
        let scrutinee = self.lower_node_to_hir(&match_expr.scrutinee, hir, scope_kind);

        let arms: Vec<HIRMatchArm> = match_expr
          .arms
          .iter()
          .map(|arm| {
            let pattern = self.lower_pattern(&arm.pattern, arm.guard.as_ref(), Some(&arm.body));
            let guard = arm.guard.as_ref().map(|g| self.lower_node_to_hir(g, hir, scope_kind));
            let body = self.lower_node_to_hir(&arm.body, hir, scope_kind);
            HIRMatchArm { pattern, guard, body }
          })
          .collect();

        let result_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

        hir.alloc(HIRNode {
          kind: HIRKind::Match { scrutinee, arms },
          span: match_expr.span.clone(),
          type_id: result_type,
        })
      },
      ASTExpression::Lambda(lambda) => self.lower_lambda_to_hir(node_id, lambda, hir, scope_kind),

      ASTExpression::CaptureOverride(co) => {
        let mode = match co.kind {
          ignis_ast::expressions::CaptureOverrideKind::Move => ignis_hir::CaptureMode::ByValue,
          ignis_ast::expressions::CaptureOverrideKind::Ref => ignis_hir::CaptureMode::ByRef,
          ignis_ast::expressions::CaptureOverrideKind::RefMut => ignis_hir::CaptureMode::ByMutRef,
        };

        if let Some(def_id) = self.lookup_def(&co.inner).cloned()
          && let Some(top) = self.capture_override_stack.last_mut()
        {
          top.insert(def_id, mode);
        }

        self.lower_node_to_hir(&co.inner, hir, scope_kind)
      },

      ASTExpression::Pipe { lhs, rhs, .. } => self.lower_pipe_to_hir(node_id, lhs, rhs, hir, scope_kind),

      ASTExpression::Try { expr, span } => self.lower_try_to_hir(node_id, expr, span, hir, scope_kind),

      // In ambient-path pipe lowering, PipePlaceholder returns the LHS HIR node from the stack.
      // In ReplaceAt mode, the placeholder is substituted by assemble_pipe_args instead.
      // Outside any pipe context, falls back to Error (should not happen if typeck is correct).
      ASTExpression::PipePlaceholder { span, .. } => {
        if let Some(&lhs_hir) = self.pipe_lhs_hir_stack.last() {
          lhs_hir
        } else {
          hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: span.clone(),
            type_id: self.types.error(),
          })
        }
      },
    }
  }

  fn lower_try_to_hir(
    &mut self,
    node_id: &NodeId,
    expr: &NodeId,
    span: &Span,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let scrutinee = self.lower_node_to_hir(expr, hir, scope_kind);

    let scrutinee_type = hir.get(scrutinee).type_id;

    let (enum_def, type_args, try_capable) = match self.types.get(&scrutinee_type).clone() {
      Type::Instance { generic, args } => {
        if let DefinitionKind::Enum(ed) = &self.defs.get(&generic).kind {
          if let Some(tc) = &ed.try_capable {
            (generic, args, *tc)
          } else {
            return hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span: span.clone(),
              type_id: self.types.error(),
            });
          }
        } else {
          return hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: span.clone(),
            type_id: self.types.error(),
          });
        }
      },
      Type::Enum(def_id) => {
        if let DefinitionKind::Enum(ed) = &self.defs.get(&def_id).kind {
          if let Some(tc) = &ed.try_capable {
            (def_id, vec![], *tc)
          } else {
            return hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span: span.clone(),
              type_id: self.types.error(),
            });
          }
        } else {
          return hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: span.clone(),
            type_id: self.types.error(),
          });
        }
      },
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    let enum_def_data = match &self.defs.get(&enum_def).kind {
      DefinitionKind::Enum(ed) => ed.clone(),
      _ => unreachable!(),
    };

    let ok_variant = &enum_def_data.variants[try_capable.ok_variant as usize];
    let err_variant = &enum_def_data.variants[try_capable.err_variant as usize];

    let ok_inner_type = if ok_variant.payload.len() == 1 {
      let raw = ok_variant.payload[0];
      let subst = Substitution::for_generic(enum_def, &type_args);
      self.types.substitute(raw, &subst)
    } else {
      self.types.void()
    };

    let result_type = self.lookup_type(node_id).cloned().unwrap_or(ok_inner_type);

    let ok_binding_name = self.symbols.borrow_mut().intern("_try_ok_value");
    let ok_binding_def = Definition {
      kind: DefinitionKind::Variable(VariableDefinition {
        type_id: ok_inner_type,
        mutable: false,
      }),
      name: ok_binding_name,
      span: span.clone(),
      name_span: span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: None,
    };
    let ok_binding_id = self.defs.alloc(ok_binding_def);

    let ok_body = hir.alloc(HIRNode {
      kind: HIRKind::Variable(ok_binding_id),
      span: span.clone(),
      type_id: ok_inner_type,
    });

    let err_inner_type = if err_variant.payload.len() == 1 {
      let raw = err_variant.payload[0];
      let subst = Substitution::for_generic(enum_def, &type_args);
      self.types.substitute(raw, &subst)
    } else {
      self.types.void()
    };

    let err_binding_name = self.symbols.borrow_mut().intern("_try_err_value");
    let err_binding_def = Definition {
      kind: DefinitionKind::Variable(VariableDefinition {
        type_id: err_inner_type,
        mutable: false,
      }),
      name: err_binding_name,
      span: span.clone(),
      name_span: span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: None,
    };
    let err_binding_id = self.defs.alloc(err_binding_def);

    let err_payload = if err_variant.payload.is_empty() {
      vec![]
    } else {
      vec![hir.alloc(HIRNode {
        kind: HIRKind::Variable(err_binding_id),
        span: span.clone(),
        type_id: err_inner_type,
      })]
    };

    let err_return_value = hir.alloc(HIRNode {
      kind: HIRKind::EnumVariant {
        enum_def,
        type_args: type_args.clone(),
        variant_tag: try_capable.err_variant,
        payload: err_payload,
      },
      span: span.clone(),
      type_id: self
        .current_lowering_return_type()
        .filter(|ret_type| match self.types.get(ret_type) {
          Type::Instance { generic, .. } => *generic == enum_def,
          Type::Enum(def_id) => *def_id == enum_def,
          _ => false,
        })
        .unwrap_or(scrutinee_type),
    });

    let err_body = hir.alloc(HIRNode {
      kind: HIRKind::Return(Some(err_return_value)),
      span: span.clone(),
      type_id: self.types.void(),
    });

    let ok_pattern = if ok_variant.payload.is_empty() {
      HIRPattern::Variant {
        enum_def,
        variant_tag: try_capable.ok_variant,
        args: vec![],
      }
    } else {
      HIRPattern::Variant {
        enum_def,
        variant_tag: try_capable.ok_variant,
        args: vec![HIRPattern::Binding { def_id: ok_binding_id }],
      }
    };

    let err_pattern = if err_variant.payload.is_empty() {
      HIRPattern::Variant {
        enum_def,
        variant_tag: try_capable.err_variant,
        args: vec![],
      }
    } else {
      HIRPattern::Variant {
        enum_def,
        variant_tag: try_capable.err_variant,
        args: vec![HIRPattern::Binding { def_id: err_binding_id }],
      }
    };

    let arms = vec![
      HIRMatchArm {
        pattern: ok_pattern,
        guard: None,
        body: ok_body,
      },
      HIRMatchArm {
        pattern: err_pattern,
        guard: None,
        body: err_body,
      },
    ];

    hir.alloc(HIRNode {
      kind: HIRKind::Match { scrutinee, arms },
      span: span.clone(),
      type_id: result_type,
    })
  }

  fn lower_shorthand_let_else_pattern(
    &mut self,
    pattern: &ASTPattern,
    value: HIRId,
    hir: &HIR,
  ) -> Option<HIRPattern> {
    let ASTPattern::Path { segments, args, .. } = pattern else {
      return None;
    };

    if segments.len() != 1 || args.is_some() {
      return None;
    }

    let (binding_name, binding_span) = &segments[0];

    let value_type = hir.get(value).type_id;
    let (enum_def, ok_variant_tag) = self.resolve_try_ok_variant(value_type)?;

    let payload_len = match &self.defs.get(&enum_def).kind {
      DefinitionKind::Enum(ed) => ed
        .variants
        .get(ok_variant_tag as usize)
        .map(|variant| variant.payload.len())
        .unwrap_or(0),
      _ => 0,
    };

    let is_wildcard = {
      let symbols = self.symbols.borrow();
      symbols.get(binding_name) == "_"
    };

    let mut pattern_args = Vec::new();
    if payload_len > 0 {
      if is_wildcard {
        pattern_args.push(HIRPattern::Wildcard);
      } else {
        let binding_pattern = self
          .lookup_pattern_binding_def(*binding_name, binding_span)
          .or_else(|| self.scopes.lookup_def(binding_name).copied())
          .map(|def_id| HIRPattern::Binding { def_id })
          .unwrap_or(HIRPattern::Wildcard);

        pattern_args.push(binding_pattern);
      }

      for _ in 1..payload_len {
        pattern_args.push(HIRPattern::Wildcard);
      }
    }

    Some(HIRPattern::Variant {
      enum_def,
      variant_tag: ok_variant_tag,
      args: pattern_args,
    })
  }

  fn resolve_try_ok_variant(
    &self,
    value_type: TypeId,
  ) -> Option<(DefinitionId, u32)> {
    match self.types.get(&value_type).clone() {
      Type::Instance { generic, .. } => {
        let DefinitionKind::Enum(ed) = &self.defs.get(&generic).kind else {
          return None;
        };

        let try_capable = ed.try_capable?;
        Some((generic, try_capable.ok_variant))
      },
      Type::Enum(def_id) => {
        let DefinitionKind::Enum(ed) = &self.defs.get(&def_id).kind else {
          return None;
        };

        let try_capable = ed.try_capable?;
        Some((def_id, try_capable.ok_variant))
      },
      _ => None,
    }
  }

  fn lower_pattern(
    &mut self,
    pattern: &ASTPattern,
    arm_guard: Option<&NodeId>,
    arm_body: Option<&NodeId>,
  ) -> HIRPattern {
    match pattern {
      ASTPattern::Wildcard { .. } => HIRPattern::Wildcard,
      ASTPattern::Literal { value, .. } => HIRPattern::Literal { value: value.clone() },
      ASTPattern::Path { segments, args, .. } => {
        if let Some(ResolvedPath::EnumVariant {
          enum_def,
          variant_index,
        }) =
          self.resolve_qualified_path_from_symbols(segments.iter().map(|(s, _)| *s).collect::<Vec<_>>().as_slice())
        {
          let variant_args = args
            .as_ref()
            .map(|a| a.iter().map(|p| self.lower_pattern(p, arm_guard, arm_body)).collect())
            .unwrap_or_default();
          return HIRPattern::Variant {
            enum_def,
            variant_tag: variant_index,
            args: variant_args,
          };
        }

        if segments.len() > 1 {
          let syms: Vec<_> = segments.iter().map(|(s, _)| *s).collect();
          if let Some(ResolvedPath::Entry(SymbolEntry::Single(def_id))) =
            self.resolve_qualified_path_from_symbols(&syms)
            && matches!(self.defs.get(&def_id).kind, DefinitionKind::Constant(_))
          {
            return HIRPattern::Constant { def_id };
          }
        }

        if segments.len() == 1 {
          let (name, segment_span) = &segments[0];

          if let Some(def_id) = self.lookup_pattern_binding_def(*name, segment_span) {
            return HIRPattern::Binding { def_id };
          }

          if let Some(arm_body) = arm_body
            && let Some(def_id) = self.find_pattern_binding_def_in_arm(*name, arm_guard, arm_body)
          {
            return HIRPattern::Binding { def_id };
          }

          if let Some(def_id) = self.scopes.lookup_def(name).cloned() {
            return HIRPattern::Binding { def_id };
          }
        }

        HIRPattern::Wildcard
      },
      ASTPattern::Tuple { elements, .. } => {
        let hir_elements: Vec<HIRPattern> = elements
          .iter()
          .map(|e| self.lower_pattern(e, arm_guard, arm_body))
          .collect();
        HIRPattern::Tuple { elements: hir_elements }
      },
      ASTPattern::Or { patterns, .. } => {
        let hir_patterns: Vec<HIRPattern> = patterns
          .iter()
          .map(|p| self.lower_pattern(p, arm_guard, arm_body))
          .collect();
        HIRPattern::Or { patterns: hir_patterns }
      },
    }
  }

  fn resolve_qualified_path_from_symbols(
    &self,
    segments: &[SymbolId],
  ) -> Option<ResolvedPath> {
    if segments.is_empty() {
      return None;
    }

    if segments.len() == 1 {
      return self.scopes.lookup(&segments[0]).cloned().map(ResolvedPath::Entry);
    }

    let ns_path = &segments[..segments.len() - 1];
    let def_name = &segments[segments.len() - 1];

    if let Some(def_id) = self.scopes.lookup_def(&ns_path[0]) {
      let def = self.defs.get(def_id);

      match &def.kind {
        DefinitionKind::Namespace(ns_def) => {
          let mut current_ns = ns_def.namespace_id;

          for segment in &ns_path[1..] {
            current_ns = self.namespaces.lookup_child(current_ns, segment)?;
          }

          return self
            .namespaces
            .lookup_def(current_ns, def_name)
            .cloned()
            .map(ResolvedPath::Entry);
        },

        DefinitionKind::Enum(ed) if ns_path.len() == 1 => {
          if let Some(&tag) = ed.variants_by_name.get(def_name) {
            return Some(ResolvedPath::EnumVariant {
              enum_def: *def_id,
              variant_index: tag,
            });
          }
        },

        _ => {},
      }
    }

    // Fallback: namespace defined in the current module (e.g. `__fs_errno`)
    // but not imported into scope — look up through the global namespace tree.
    if let Some(ns_id) = self.namespaces.lookup(ns_path) {
      return self
        .namespaces
        .lookup_def(ns_id, def_name)
        .cloned()
        .map(ResolvedPath::Entry);
    }

    None
  }

  fn lookup_pattern_binding_def(
    &self,
    name: SymbolId,
    name_span: &Span,
  ) -> Option<DefinitionId> {
    self.defs.iter().find_map(|(def_id, def)| {
      if matches!(def.kind, DefinitionKind::Variable(_)) && def.name == name && def.name_span == *name_span {
        Some(def_id)
      } else {
        None
      }
    })
  }

  fn find_pattern_binding_def_in_arm(
    &self,
    name: SymbolId,
    guard: Option<&NodeId>,
    body: &NodeId,
  ) -> Option<DefinitionId> {
    if let Some(g) = guard
      && let Some(def_id) = self.find_binding_def_in_node(*g, name)
    {
      return Some(def_id);
    }

    self.find_binding_def_in_node(*body, name)
  }

  fn find_binding_def_in_node(
    &self,
    node_id: NodeId,
    name: SymbolId,
  ) -> Option<DefinitionId> {
    let node = self.ast.get(&node_id);

    match node {
      ASTNode::Expression(expr) => match expr {
        ASTExpression::Variable(var) if var.name == name => self.lookup_def(&node_id).cloned(),
        ASTExpression::Path(path) if path.segments.len() == 1 && path.segments[0].name == name => {
          self.lookup_def(&node_id).cloned()
        },
        ASTExpression::Binary(binary) => self
          .find_binding_def_in_node(binary.left, name)
          .or_else(|| self.find_binding_def_in_node(binary.right, name)),
        ASTExpression::Unary(unary) => self.find_binding_def_in_node(unary.operand, name),
        ASTExpression::Grouped(grouped) => self.find_binding_def_in_node(grouped.expression, name),
        ASTExpression::Assignment(assign) => self
          .find_binding_def_in_node(assign.target, name)
          .or_else(|| self.find_binding_def_in_node(assign.value, name)),
        ASTExpression::Call(call) => {
          if let Some(def_id) = self.find_binding_def_in_node(call.callee, name) {
            return Some(def_id);
          }
          for arg in &call.arguments {
            if let Some(def_id) = self.find_binding_def_in_node(*arg, name) {
              return Some(def_id);
            }
          }
          None
        },
        ASTExpression::Ternary(ternary) => self
          .find_binding_def_in_node(ternary.condition, name)
          .or_else(|| self.find_binding_def_in_node(ternary.then_expr, name))
          .or_else(|| self.find_binding_def_in_node(ternary.else_expr, name)),
        ASTExpression::MemberAccess(access) => self.find_binding_def_in_node(access.object, name),
        ASTExpression::RecordInit(record_init) => {
          for field in &record_init.fields {
            if let Some(def_id) = self.find_binding_def_in_node(field.value, name) {
              return Some(def_id);
            }
          }
          None
        },
        ASTExpression::Match(match_expr) => {
          if let Some(def_id) = self.find_binding_def_in_node(match_expr.scrutinee, name) {
            return Some(def_id);
          }

          for arm in &match_expr.arms {
            if let Some(g) = arm.guard
              && let Some(def_id) = self.find_binding_def_in_node(g, name)
            {
              return Some(def_id);
            }

            if let Some(def_id) = self.find_binding_def_in_node(arm.body, name) {
              return Some(def_id);
            }
          }

          None
        },
        _ => None,
      },
      ASTNode::Statement(stmt) => match stmt {
        ASTStatement::Expression(_) => None,
        ASTStatement::Return(ret) => ret
          .expression
          .and_then(|expr| self.find_binding_def_in_node(expr, name)),
        ASTStatement::Block(block) => {
          for stmt_id in &block.statements {
            if let Some(def_id) = self.find_binding_def_in_node(*stmt_id, name) {
              return Some(def_id);
            }
          }
          None
        },
        ASTStatement::Defer(_) => None,
        _ => None,
      },
    }
  }

  fn lower_member_access(
    &mut self,
    node_id: &NodeId,
    ma: &ignis_ast::expressions::member_access::ASTMemberAccess,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let stored_type = self.lookup_type(node_id).cloned();

    match ma.op {
      ASTAccessOp::Dot => {
        let base = self.lower_node_to_hir(&ma.object, hir, scope_kind);
        let base_type = hir.get(base).type_id;
        let (derefed_base, derefed_type) = self.auto_deref_for_lowering(base, base_type, hir, &ma.span);

        // Extract record definition from Type::Record or Type::Instance
        let def_id = match self.types.get(&derefed_type).clone() {
          Type::Record(def_id) => Some(def_id),
          Type::Instance { generic, .. } => Some(generic),
          _ => None,
        };

        if let Some(def_id) = def_id
          && let DefinitionKind::Record(rd) = &self.defs.get(&def_id).kind
          && let Some(field) = rd.fields.iter().find(|f| f.name == ma.member)
        {
          let field_type = stored_type.unwrap_or(field.type_id);
          return hir.alloc(HIRNode {
            kind: HIRKind::FieldAccess {
              base: derefed_base,
              field_index: field.index,
            },
            span: ma.span.clone(),
            type_id: field_type,
          });
        }

        // Error fallback
        hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: ma.span.clone(),
          type_id: self.types.error(),
        })
      },
      ASTAccessOp::DoubleColon => {
        // Static access: Type::member
        // Resolve the type expression to get the definition
        let def_id = self.resolve_type_expression_for_lowering(&ma.object);

        let Some(def_id) = def_id else {
          return hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: ma.span.clone(),
            type_id: self.types.error(),
          });
        };

        match &self.defs.get(&def_id).kind.clone() {
          DefinitionKind::Record(rd) => {
            // Static field or method
            if let Some(&field_id) = rd.static_fields.get(&ma.member) {
              let field_type = stored_type.unwrap_or_else(|| self.get_definition_type(&field_id));
              return hir.alloc(HIRNode {
                kind: HIRKind::StaticAccess { def: field_id },
                span: ma.span.clone(),
                type_id: field_type,
              });
            }
            // Static method - will be handled in call lowering
            if let Some(method_id) = rd.static_methods.get(&ma.member).and_then(|e| e.as_single()) {
              let method_type = stored_type.unwrap_or_else(|| self.get_definition_type(method_id));
              return hir.alloc(HIRNode {
                kind: HIRKind::StaticAccess { def: *method_id },
                span: ma.span.clone(),
                type_id: method_type,
              });
            }
          },
          DefinitionKind::Enum(ed) => {
            // Enum variant without payload
            if let Some(&tag) = ed.variants_by_name.get(&ma.member) {
              let variant = &ed.variants[tag as usize];
              if variant.payload.is_empty() {
                // Extract type_args from stored type if it's a generic instance
                let result_type = stored_type.unwrap_or(ed.type_id);
                let type_args = match self.types.get(&result_type).clone() {
                  Type::Instance { args, .. } => args,
                  _ => vec![],
                };

                // Unit variant
                return hir.alloc(HIRNode {
                  kind: HIRKind::EnumVariant {
                    enum_def: def_id,
                    type_args,
                    variant_tag: tag,
                    payload: vec![],
                  },
                  span: ma.span.clone(),
                  type_id: result_type,
                });
              }
              // Variant with payload - handled in call lowering
            }
            // Static field
            if let Some(&field_id) = ed.static_fields.get(&ma.member) {
              let field_type = stored_type.unwrap_or_else(|| self.get_definition_type(&field_id));
              return hir.alloc(HIRNode {
                kind: HIRKind::StaticAccess { def: field_id },
                span: ma.span.clone(),
                type_id: field_type,
              });
            }
            // Static method
            if let Some(method_id) = ed.static_methods.get(&ma.member).and_then(|e| e.as_single()) {
              let method_type = stored_type.unwrap_or_else(|| self.get_definition_type(method_id));
              return hir.alloc(HIRNode {
                kind: HIRKind::StaticAccess { def: *method_id },
                span: ma.span.clone(),
                type_id: method_type,
              });
            }
          },
          DefinitionKind::Namespace(ns_def) => {
            // Namespace member
            if let Some(member_def_id) = self
              .namespaces
              .lookup_def(ns_def.namespace_id, &ma.member)
              .and_then(|e| e.as_single())
              .cloned()
            {
              let member_type = stored_type.unwrap_or_else(|| self.get_definition_type(&member_def_id));
              return hir.alloc(HIRNode {
                kind: HIRKind::StaticAccess { def: member_def_id },
                span: ma.span.clone(),
                type_id: member_type,
              });
            }
          },
          _ => {},
        }

        hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: ma.span.clone(),
          type_id: self.types.error(),
        })
      },
    }
  }

  fn lower_record_init(
    &mut self,
    node_id: &NodeId,
    ri: &ignis_ast::expressions::record_init::ASTRecordInit,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let result_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

    // Resolve the record type
    let def_id = self.resolve_record_path_for_lowering(&ri.path);

    let Some(def_id) = def_id else {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: ri.span.clone(),
        type_id: self.types.error(),
      });
    };

    // Get the record definition to map field names to indices
    let rd = match &self.defs.get(&def_id).kind {
      DefinitionKind::Record(rd) => rd.clone(),
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: ri.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    // Lower each field value and map to field index
    let mut fields: Vec<(u32, HIRId)> = Vec::new();
    for init_field in &ri.fields {
      if let Some(field_def) = rd.fields.iter().find(|f| f.name == init_field.name) {
        let value = self.lower_node_to_hir(&init_field.value, hir, scope_kind);
        fields.push((field_def.index, value));
      }
    }

    // Sort by field index for consistent ordering
    fields.sort_by_key(|(idx, _)| *idx);

    // Extract type_args from result type if it's a generic instance
    let type_args = match self.types.get(&result_type).clone() {
      Type::Instance { args, .. } => args,
      _ => vec![],
    };

    hir.alloc(HIRNode {
      kind: HIRKind::RecordInit {
        record_def: def_id,
        type_args,
        fields,
      },
      span: ri.span.clone(),
      type_id: result_type,
    })
  }

  fn auto_deref_for_lowering(
    &self,
    base: HIRId,
    base_type: ignis_type::types::TypeId,
    hir: &mut HIR,
    span: &Span,
  ) -> (HIRId, ignis_type::types::TypeId) {
    match self.types.get(&base_type) {
      Type::Reference { inner, .. } => {
        let deref_node = hir.alloc(HIRNode {
          kind: HIRKind::Dereference(base),
          span: span.clone(),
          type_id: *inner,
        });
        (deref_node, *inner)
      },
      _ => (base, base_type),
    }
  }

  fn resolve_type_expression_for_lowering(
    &self,
    node_id: &NodeId,
  ) -> Option<ignis_type::definition::DefinitionId> {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Expression(ASTExpression::Variable(var)) => self.scopes.lookup_def(&var.name).cloned(),
      ASTNode::Expression(ASTExpression::Path(path)) => {
        if path.segments.is_empty() {
          return None;
        }
        let first_segment = &path.segments[0];
        let mut current_def = self.scopes.lookup_def(&first_segment.name).cloned()?;

        for segment in path.segments.iter().skip(1) {
          match &self.defs.get(&current_def).kind {
            DefinitionKind::Namespace(ns_def) => {
              current_def = self
                .namespaces
                .lookup_def(ns_def.namespace_id, &segment.name)
                .and_then(|e| e.as_single())
                .cloned()?;
            },
            _ => return None,
          }
        }
        Some(current_def)
      },
      ASTNode::Expression(ASTExpression::MemberAccess(ma)) => {
        let base_def = self.resolve_type_expression_for_lowering(&ma.object)?;
        match &self.defs.get(&base_def).kind {
          DefinitionKind::Namespace(ns_def) => self
            .namespaces
            .lookup_def(ns_def.namespace_id, &ma.member)
            .and_then(|e| e.as_single())
            .cloned(),
          _ => None,
        }
      },
      _ => None,
    }
  }

  fn resolve_record_path_for_lowering(
    &self,
    path: &[(ignis_type::symbol::SymbolId, Span)],
  ) -> Option<ignis_type::definition::DefinitionId> {
    if path.is_empty() {
      return None;
    }

    let (first_sym, _) = &path[0];
    let mut current_def = self.scopes.lookup_def(first_sym).cloned()?;

    for (segment_sym, _) in path.iter().skip(1) {
      match &self.defs.get(&current_def).kind {
        DefinitionKind::Namespace(ns_def) => {
          current_def = self
            .namespaces
            .lookup_def(ns_def.namespace_id, segment_sym)
            .and_then(|e| e.as_single())
            .cloned()?;
        },
        _ => return None,
      }
    }

    Some(current_def)
  }

  /// Collect all definitions inside a namespace, navigating through nested namespace segments.
  /// Returns pairs of (SymbolId, DefinitionId) for each definition in the namespace.
  fn collect_namespace_defs(
    &self,
    ns_path: &[SymbolId],
  ) -> Vec<(SymbolId, DefinitionId)> {
    if ns_path.is_empty() {
      return Vec::new();
    }

    // Navigate through the namespace path segments
    let Some(mut current_def) = self.scopes.lookup_def(&ns_path[0]).cloned() else {
      return Vec::new();
    };

    for segment in &ns_path[1..] {
      match &self.defs.get(&current_def).kind {
        DefinitionKind::Namespace(ns_def) => {
          let Some(next) = self
            .namespaces
            .lookup_def(ns_def.namespace_id, segment)
            .and_then(|e| e.as_single())
            .cloned()
          else {
            return Vec::new();
          };
          current_def = next;
        },
        _ => return Vec::new(),
      }
    }

    // current_def should now be the namespace definition
    let DefinitionKind::Namespace(ns_def) = &self.defs.get(&current_def).kind else {
      return Vec::new();
    };

    self.namespaces.all_defs(ns_def.namespace_id)
  }

  fn lower_typeof_builtin(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    if call.arguments.len() != 1 {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: call.span.clone(),
        type_id: self.types.error(),
      });
    }

    let arg = self.lower_node_to_hir(&call.arguments[0], hir, scope_kind);
    hir.alloc(HIRNode {
      kind: HIRKind::TypeOf(arg),
      span: call.span.clone(),
      type_id: self.types.u32(),
    })
  }

  fn lower_sizeof_builtin(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
  ) -> HIRId {
    let type_args = match &call.type_args {
      Some(args) if args.len() == 1 => args,
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: call.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    let value_type = self.resolve_type_syntax(&type_args[0]);

    hir.alloc(HIRNode {
      kind: HIRKind::SizeOf(value_type),
      span: call.span.clone(),
      type_id: self.types.u64(),
    })
  }

  fn lower_alignof_builtin(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
  ) -> HIRId {
    let type_args = match &call.type_args {
      Some(args) if args.len() == 1 => args,
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: call.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    let value_type = self.resolve_type_syntax(&type_args[0]);

    hir.alloc(HIRNode {
      kind: HIRKind::AlignOf(value_type),
      span: call.span.clone(),
      type_id: self.types.u64(),
    })
  }

  fn lower_maxof_builtin(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
  ) -> HIRId {
    let type_args = match &call.type_args {
      Some(args) if args.len() == 1 => args,
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: call.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    let value_type = self.resolve_type_syntax(&type_args[0]);

    hir.alloc(HIRNode {
      kind: HIRKind::MaxOf(value_type),
      span: call.span.clone(),
      type_id: value_type,
    })
  }

  fn lower_minof_builtin(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
  ) -> HIRId {
    let type_args = match &call.type_args {
      Some(args) if args.len() == 1 => args,
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: call.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    let value_type = self.resolve_type_syntax(&type_args[0]);

    hir.alloc(HIRNode {
      kind: HIRKind::MinOf(value_type),
      span: call.span.clone(),
      type_id: value_type,
    })
  }

  // ========================================================================
  // @builtin(...) Lowering
  // ========================================================================

  fn lower_builtin_call(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let name = self.get_symbol_name(&bc.name);

    match name.as_str() {
      "compileError" => self.lower_builtin_compile_error(bc, hir),
      "sizeOf" => self.lower_builtin_sizeof_new(bc, hir),
      "alignOf" => self.lower_builtin_alignof_new(bc, hir),
      "typeName" => self.lower_builtin_typename(bc, hir),
      "bitCast" => self.lower_builtin_bitcast(bc, hir, scope_kind),
      "pointerCast" => self.lower_builtin_pointer_cast(bc, hir, scope_kind),
      "integerFromPointer" => self.lower_builtin_integer_from_pointer(bc, hir, scope_kind),
      "pointerFromInteger" => self.lower_builtin_pointer_from_integer(bc, hir, scope_kind),
      "read" => self.lower_builtin_read(bc, hir, scope_kind),
      "write" => self.lower_builtin_write(bc, hir, scope_kind),
      "dropInPlace" => self.lower_builtin_drop_in_place(bc, hir, scope_kind),
      "dropGlue" => self.lower_builtin_drop_glue(bc, hir),
      "panic" => self.lower_builtin_panic(bc, hir, scope_kind),
      "trap" => self.lower_builtin_trap(bc, hir),
      "unreachable" => self.lower_builtin_unreachable(bc, hir),
      _ => hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      }),
    }
  }

  fn lower_builtin_read(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let type_args = match &bc.type_args {
      Some(args) if args.len() == 1 => args,
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: bc.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    if bc.args.len() != 1 {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    }

    let value_type = self.resolve_type_syntax(&type_args[0]);
    let ptr = self.lower_node_to_hir(&bc.args[0], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::BuiltinLoad { ty: value_type, ptr },
      span: bc.span.clone(),
      type_id: value_type,
    })
  }

  fn lower_builtin_write(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let type_args = match &bc.type_args {
      Some(args) if args.len() == 1 => args,
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: bc.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    if bc.args.len() != 2 {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    }

    let value_type = self.resolve_type_syntax(&type_args[0]);
    let ptr = self.lower_node_to_hir(&bc.args[0], hir, scope_kind);
    let value = self.lower_node_to_hir(&bc.args[1], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::BuiltinStore {
        ty: value_type,
        ptr,
        value,
      },
      span: bc.span.clone(),
      type_id: self.types.void(),
    })
  }

  fn lower_builtin_drop_in_place(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let type_args = match &bc.type_args {
      Some(args) if args.len() == 1 => args,
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: bc.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    if bc.args.len() != 1 {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    }

    let value_type = self.resolve_type_syntax(&type_args[0]);
    let ptr = self.lower_node_to_hir(&bc.args[0], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::BuiltinDropInPlace { ty: value_type, ptr },
      span: bc.span.clone(),
      type_id: self.types.void(),
    })
  }

  fn lower_builtin_drop_glue(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
  ) -> HIRId {
    let type_args = match &bc.type_args {
      Some(args) if args.len() == 1 => args,
      _ => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: bc.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    let value_type = self.resolve_type_syntax(&type_args[0]);

    let u8_type = self.types.u8();
    let u8_ptr = self.types.pointer(u8_type, true);
    let void_type = self.types.void();
    let fn_ptr_type = self.types.function(vec![u8_ptr], void_type, false);

    hir.alloc(HIRNode {
      kind: HIRKind::BuiltinDropGlue { ty: value_type },
      span: bc.span.clone(),
      type_id: fn_ptr_type,
    })
  }

  fn lower_builtin_compile_error(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
  ) -> HIRId {
    // Diagnostic already emitted during typechecking
    hir.alloc(HIRNode {
      kind: HIRKind::Error,
      span: bc.span.clone(),
      type_id: self.types.never(),
    })
  }

  fn lower_builtin_sizeof_new(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
  ) -> HIRId {
    let Some(ref type_args) = bc.type_args else {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    };

    let value_type = self.resolve_type_syntax(&type_args[0]);

    hir.alloc(HIRNode {
      kind: HIRKind::SizeOf(value_type),
      span: bc.span.clone(),
      type_id: self.types.u64(),
    })
  }

  fn lower_builtin_alignof_new(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
  ) -> HIRId {
    let Some(ref type_args) = bc.type_args else {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    };

    let value_type = self.resolve_type_syntax(&type_args[0]);

    hir.alloc(HIRNode {
      kind: HIRKind::AlignOf(value_type),
      span: bc.span.clone(),
      type_id: self.types.u64(),
    })
  }

  fn lower_builtin_typename(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
  ) -> HIRId {
    let Some(ref type_args) = bc.type_args else {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    };

    let value_type = self.resolve_type_syntax(&type_args[0]);

    let name = {
      let symbols = self.symbols.borrow();
      ignis_type::types::format_type_name(&value_type, &self.types, &self.defs, &symbols)
    };

    hir.alloc(HIRNode {
      kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::String(name)),
      span: bc.span.clone(),
      type_id: self.types.str(),
    })
  }

  fn lower_builtin_bitcast(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let Some(ref type_args) = bc.type_args else {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    };

    if bc.args.is_empty() {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    }

    let target = self.resolve_type_syntax(&type_args[0]);
    let expr_id = self.lower_node_to_hir(&bc.args[0], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::BitCast {
        expression: expr_id,
        target,
      },
      span: bc.span.clone(),
      type_id: target,
    })
  }

  fn lower_builtin_pointer_cast(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let Some(ref type_args) = bc.type_args else {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    };

    if bc.args.is_empty() {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    }

    let target = self.resolve_type_syntax(&type_args[0]);
    let expr_id = self.lower_node_to_hir(&bc.args[0], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::Cast {
        expression: expr_id,
        target,
      },
      span: bc.span.clone(),
      type_id: target,
    })
  }

  fn lower_builtin_integer_from_pointer(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    if bc.args.is_empty() {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    }

    let u64_ty = self.types.u64();
    let expr_id = self.lower_node_to_hir(&bc.args[0], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::Cast {
        expression: expr_id,
        target: u64_ty,
      },
      span: bc.span.clone(),
      type_id: u64_ty,
    })
  }

  fn lower_builtin_pointer_from_integer(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let Some(ref type_args) = bc.type_args else {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    };

    if bc.args.is_empty() {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    }

    let target = self.resolve_type_syntax(&type_args[0]);
    let expr_id = self.lower_node_to_hir(&bc.args[0], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::Cast {
        expression: expr_id,
        target,
      },
      span: bc.span.clone(),
      type_id: target,
    })
  }

  fn lower_builtin_panic(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    if bc.args.is_empty() {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: bc.span.clone(),
        type_id: self.types.error(),
      });
    }

    let msg = self.lower_node_to_hir(&bc.args[0], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::Panic(msg),
      span: bc.span.clone(),
      type_id: self.types.never(),
    })
  }

  fn lower_builtin_trap(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
  ) -> HIRId {
    hir.alloc(HIRNode {
      kind: HIRKind::Trap,
      span: bc.span.clone(),
      type_id: self.types.never(),
    })
  }

  fn lower_builtin_unreachable(
    &mut self,
    bc: &ASTBuiltinCall,
    hir: &mut HIR,
  ) -> HIRId {
    hir.alloc(HIRNode {
      kind: HIRKind::BuiltinUnreachable,
      span: bc.span.clone(),
      type_id: self.types.never(),
    })
  }

  /// Lower a method call: obj.method(args) or Type::method(args)
  fn lower_method_call(
    &mut self,
    node_id: &NodeId,
    ma: &ignis_ast::expressions::member_access::ASTMemberAccess,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    use ignis_ast::expressions::member_access::ASTAccessOp;

    let result_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

    match ma.op {
      ASTAccessOp::Dot => {
        // Instance method call: obj.method(args)
        self.lower_instance_method_call(node_id, ma, call, hir, scope_kind, result_type)
      },
      ASTAccessOp::DoubleColon => {
        // Static method call or enum variant: Type::method(args) or Enum::Variant(args)
        self.lower_static_call(node_id, ma, call, hir, scope_kind, result_type)
      },
    }
  }

  /// Lower an instance method call: obj.method(args)
  fn lower_instance_method_call(
    &mut self,
    node_id: &NodeId,
    ma: &ignis_ast::expressions::member_access::ASTMemberAccess,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
    result_type: TypeId,
  ) -> HIRId {
    let base = self.lower_node_to_hir(&ma.object, hir, scope_kind);
    let base_type = hir.get(base).type_id;

    // Determine the underlying record type (strip reference if present)
    let (is_already_ref, receiver_record_type) =
      if let Type::Reference { inner, .. } = self.types.get(&base_type).clone() {
        (true, inner)
      } else {
        (false, base_type)
      };

    // Look up the method to determine if it requires &mut self
    let method_info: Option<(DefinitionId, bool)> = match self.types.get(&receiver_record_type).clone() {
      Type::Record(def_id) | Type::Instance { generic: def_id, .. } | Type::Enum(def_id) => {
        let instance_methods = match &self.defs.get(&def_id).kind {
          DefinitionKind::Record(rd) => rd.instance_methods.clone(),
          DefinitionKind::Enum(ed) => ed.instance_methods.clone(),
          _ => {
            return hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span: call.span.clone(),
              type_id: self.types.error(),
            });
          },
        };

        // First check lookup_resolved_call (for overloaded methods)
        if let Some(method_id) = self.lookup_resolved_call(node_id).cloned() {
          if let DefinitionKind::Method(md) = &self.defs.get(&method_id).kind {
            Some((method_id, md.self_mutable))
          } else {
            None
          }
        } else if let Some(method_id) = instance_methods.get(&ma.member).and_then(|e| e.as_single()) {
          if let DefinitionKind::Method(md) = &self.defs.get(method_id).kind {
            Some((*method_id, md.self_mutable))
          } else {
            None
          }
        } else {
          None
        }
      },
      _ => None,
    };

    let Some((method_id, self_mutable)) = method_info else {
      if let Some(ext_def_id) = self.lookup_resolved_call(node_id).cloned()
        && matches!(self.defs.get(&ext_def_id).kind, DefinitionKind::Function(_))
      {
        return self.lower_extension_call(node_id, ma, call, hir, scope_kind, result_type, base, &ext_def_id);
      }

      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: call.span.clone(),
        type_id: self.types.error(),
      });
    };

    // Create the receiver with appropriate mutability.
    // If base is already a reference, use it directly.
    // If base is a value, take a reference to it (mutable if method requires &mut self).
    let receiver_hir = if is_already_ref {
      base
    } else {
      let ref_type = self.types.reference(base_type, self_mutable);
      hir.alloc(HIRNode {
        kind: HIRKind::Reference {
          expression: base,
          mutable: self_mutable,
        },
        span: ma.span.clone(),
        type_id: ref_type,
      })
    };

    let args_hir: Vec<HIRId> = call
      .arguments
      .iter()
      .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
      .collect();

    let type_args = self.resolve_or_infer_method_type_args(&method_id, call, &args_hir, hir);

    hir.alloc(HIRNode {
      kind: HIRKind::MethodCall {
        receiver: Some(receiver_hir),
        method: method_id,
        type_args,
        args: args_hir,
      },
      span: call.span.clone(),
      type_id: result_type,
    })
  }

  /// Lower `obj.extMethod(args)` → `extMethod(obj, args)` (regular call).
  #[allow(clippy::too_many_arguments)]
  fn lower_extension_call(
    &mut self,
    _node_id: &NodeId,
    ma: &ignis_ast::expressions::member_access::ASTMemberAccess,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
    result_type: TypeId,
    base: HIRId,
    ext_def_id: &DefinitionId,
  ) -> HIRId {
    let args_hir: Vec<HIRId> = call
      .arguments
      .iter()
      .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
      .collect();

    let mut all_args = vec![base];
    all_args.extend(args_hir);

    let type_args = self.resolve_or_infer_call_type_args(ext_def_id, call, &all_args, hir);

    hir.alloc(HIRNode {
      kind: HIRKind::Call {
        callee: *ext_def_id,
        type_args,
        args: all_args,
      },
      span: ma.span.clone(),
      type_id: result_type,
    })
  }

  /// Lower a static method call or enum variant: Type::method(args) or Enum::Variant(args)
  fn lower_static_call(
    &mut self,
    node_id: &NodeId,
    ma: &ignis_ast::expressions::member_access::ASTMemberAccess,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
    result_type: TypeId,
  ) -> HIRId {
    let def_id = self.resolve_type_expression_for_lowering(&ma.object);

    let Some(def_id) = def_id else {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: call.span.clone(),
        type_id: self.types.error(),
      });
    };

    if let Some(method_id) = self.lookup_resolved_call(node_id).cloned()
      && matches!(self.defs.get(&method_id).kind, DefinitionKind::Method(_))
    {
      let args_hir: Vec<HIRId> = call
        .arguments
        .iter()
        .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
        .collect();

      let type_args = self.resolve_or_infer_method_type_args(&method_id, call, &args_hir, hir);

      return hir.alloc(HIRNode {
        kind: HIRKind::MethodCall {
          receiver: None,
          method: method_id,
          type_args,
          args: args_hir,
        },
        span: call.span.clone(),
        type_id: result_type,
      });
    }

    match &self.defs.get(&def_id).kind.clone() {
      DefinitionKind::Record(rd) => {
        // Static method call
        if let Some(method_id) = rd.static_methods.get(&ma.member).and_then(|e| e.as_single()) {
          let args_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          // Resolve type arguments: explicit first, then infer if needed
          let type_args = self.resolve_or_infer_method_type_args(method_id, call, &args_hir, hir);

          return hir.alloc(HIRNode {
            kind: HIRKind::MethodCall {
              receiver: None,
              method: *method_id,
              type_args,
              args: args_hir,
            },
            span: call.span.clone(),
            type_id: result_type,
          });
        }

        hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: call.span.clone(),
          type_id: self.types.error(),
        })
      },
      DefinitionKind::Enum(ed) => {
        // Enum variant with payload
        if let Some(&tag) = ed.variants_by_name.get(&ma.member) {
          let payload_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          // Extract type_args from result type if it's a generic instance
          let type_args = match self.types.get(&result_type).clone() {
            Type::Instance { args, .. } => args,
            _ => vec![],
          };

          return hir.alloc(HIRNode {
            kind: HIRKind::EnumVariant {
              enum_def: def_id,
              type_args,
              variant_tag: tag,
              payload: payload_hir,
            },
            span: call.span.clone(),
            type_id: result_type,
          });
        }

        // Static method call on enum
        if let Some(method_id) = ed.static_methods.get(&ma.member).and_then(|e| e.as_single()) {
          let args_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          // Resolve type arguments: explicit first, then infer if needed
          let type_args = self.resolve_or_infer_method_type_args(method_id, call, &args_hir, hir);

          return hir.alloc(HIRNode {
            kind: HIRKind::MethodCall {
              receiver: None,
              method: *method_id,
              type_args,
              args: args_hir,
            },
            span: call.span.clone(),
            type_id: result_type,
          });
        }

        hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: call.span.clone(),
          type_id: self.types.error(),
        })
      },
      _ => hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: call.span.clone(),
        type_id: self.types.error(),
      }),
    }
  }

  /// Try to lower a path-based call as an enum variant constructor or static method call.
  /// Handles paths of any depth: `Type::method()`, `Ns::Type::method()`, `Ns1::Ns2::Type::method()`, etc.
  /// Returns None if not an enum variant or static method, so caller can fall through to normal call.
  fn try_lower_path_call(
    &mut self,
    node_id: &NodeId,
    path: &ignis_ast::expressions::path::ASTPath,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> Option<HIRId> {
    if path.segments.len() < 2 {
      return None;
    }

    let result_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

    // All segments except the last form the path to the type (possibly through namespaces).
    // The last segment is the method or variant name.
    let type_path: Vec<(SymbolId, Span)> = path.segments[..path.segments.len() - 1]
      .iter()
      .map(|s| (s.name, s.span.clone()))
      .collect();
    let member_segment = &path.segments[path.segments.len() - 1];

    let type_def_id = self.resolve_record_path_for_lowering(&type_path)?;

    match &self.defs.get(&type_def_id).kind.clone() {
      DefinitionKind::Enum(ed) => {
        // Enum variant with payload
        if let Some(&tag) = ed.variants_by_name.get(&member_segment.name) {
          let payload_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          // Extract type_args from result type if it's a generic instance
          let type_args = match self.types.get(&result_type).clone() {
            Type::Instance { args, .. } => args,
            _ => vec![],
          };

          return Some(hir.alloc(HIRNode {
            kind: HIRKind::EnumVariant {
              enum_def: type_def_id,
              type_args,
              variant_tag: tag,
              payload: payload_hir,
            },
            span: call.span.clone(),
            type_id: result_type,
          }));
        }

        // Static method call on enum
        let method_id = self.lookup_resolved_call(node_id).cloned().or_else(|| {
          ed.static_methods
            .get(&member_segment.name)
            .and_then(|e| e.as_single())
            .cloned()
        });

        if let Some(method_id) = method_id {
          let args_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          let type_args = self.resolve_or_infer_method_type_args(&method_id, call, &args_hir, hir);

          return Some(hir.alloc(HIRNode {
            kind: HIRKind::MethodCall {
              receiver: None,
              method: method_id,
              type_args,
              args: args_hir,
            },
            span: call.span.clone(),
            type_id: result_type,
          }));
        }

        None
      },
      DefinitionKind::Record(rd) => {
        // Static method call on record
        // First try to get resolved method from type checker (handles overloads)
        let method_id = self.lookup_resolved_call(node_id).cloned().or_else(|| {
          rd.static_methods
            .get(&member_segment.name)
            .and_then(|e| e.as_single())
            .cloned()
        });

        if let Some(method_id) = method_id {
          let args_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          let type_args = self.resolve_or_infer_method_type_args(&method_id, call, &args_hir, hir);

          return Some(hir.alloc(HIRNode {
            kind: HIRKind::MethodCall {
              receiver: None,
              method: method_id,
              type_args,
              args: args_hir,
            },
            span: call.span.clone(),
            type_id: result_type,
          }));
        }

        None
      },
      _ => None,
    }
  }

  /// Insert implicit cast from *T to *u8 for deallocate's pointer argument.
  fn lower_deallocate_args(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> Vec<HIRId> {
    call
      .arguments
      .iter()
      .enumerate()
      .map(|(i, arg)| {
        let arg_hir = self.lower_node_to_hir(arg, hir, scope_kind);

        if i == 0 {
          let arg_type = hir.get(arg_hir).type_id;
          let ptr_u8 = self.types.pointer(self.types.u8(), false);

          if !self.types.types_equal(&arg_type, &ptr_u8)
            && let ignis_type::types::Type::Pointer { .. } = self.types.get(&arg_type)
          {
            let cast_node = HIRNode {
              kind: HIRKind::Cast {
                expression: arg_hir,
                target: ptr_u8,
              },
              span: hir.get(arg_hir).span.clone(),
              type_id: ptr_u8,
            };
            return hir.alloc(cast_node);
          }
        }

        arg_hir
      })
      .collect()
  }

  fn next_synthetic_id(&mut self) -> u32 {
    let id = self.lowering_counter;
    self.lowering_counter += 1;
    id
  }

  fn lower_for_of(
    &mut self,
    node_id: &NodeId,
    for_of: &ASTForOf,
    hir: &mut HIR,
  ) -> HIRId {
    self.scopes.push(ScopeKind::Loop);
    let synth_id = self.next_synthetic_id();
    let span = &for_of.span;

    let iter_type = self
      .lookup_type(&for_of.iter)
      .cloned()
      .unwrap_or_else(|| self.types.error());

    enum IterKind {
      FixedVector(usize),
      Record(DefinitionId),
    }

    let (element_type, iter_kind) = match self.types.get(&iter_type).clone() {
      Type::Vector { element, size } => (element, IterKind::FixedVector(size)),
      Type::Record(def_id) => match self.extract_record_iterable_info(&def_id) {
        Some((elem_ty, _data_index, _len_index)) => (elem_ty, IterKind::Record(def_id)),
        None => {
          self.scopes.pop();
          return hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: span.clone(),
            type_id: self.types.error(),
          });
        },
      },
      _ => {
        self.scopes.pop();
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    let binding_def_id = match self.for_of_binding_defs.get(node_id).cloned() {
      Some(id) => id,
      None => {
        self.scopes.pop();
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    let binding_type = *self.defs.type_of(&binding_def_id);
    let (is_by_ref, is_mut_ref) = match self.types.get(&binding_type).clone() {
      Type::Reference { mutable, .. } => (true, mutable),
      _ => (false, false),
    };

    let _ = self.scopes.define(&for_of.binding.name, &binding_def_id, false);

    let (iter_hir, prefix_stmts) =
      self.lower_iter_with_temp_if_needed(&for_of.iter, hir, ScopeKind::Loop, synth_id, span);

    let ctx = ForOfContext {
      for_of,
      synth_id,
      element_type,
      iter_hir,
      is_by_ref,
      is_mut_ref,
      binding_def_id,
      span,
    };

    let loop_hir = match iter_kind {
      IterKind::FixedVector(n) => self.lower_for_of_fixed(&ctx, hir, n),
      IterKind::Record(def_id) => self.lower_for_of_record(&ctx, hir, &def_id),
    };

    self.scopes.pop();

    if prefix_stmts.is_empty() {
      loop_hir
    } else {
      let mut stmts = prefix_stmts;
      stmts.push(loop_hir);
      hir.alloc(HIRNode {
        kind: HIRKind::Block {
          statements: stmts,
          expression: None,
        },
        span: span.clone(),
        type_id: self.types.void(),
      })
    }
  }

  fn lower_iter_with_temp_if_needed(
    &mut self,
    iter_node: &NodeId,
    hir: &mut HIR,
    scope_kind: ScopeKind,
    synth_id: u32,
    span: &Span,
  ) -> (HIRId, Vec<HIRId>) {
    use ignis_ast::expressions::ASTExpression;

    let node = self.ast.get(iter_node);

    if let ASTNode::Expression(ASTExpression::Variable(_)) = node {
      let iter_hir = self.lower_node_to_hir(iter_node, hir, scope_kind);
      return (iter_hir, vec![]);
    }

    let tmp_name = self.symbols.borrow_mut().intern(&format!("__for_of_tmp_{}", synth_id));
    let iter_type = self
      .lookup_type(iter_node)
      .cloned()
      .unwrap_or_else(|| self.types.error());

    let tmp_def = ignis_type::definition::Definition {
      kind: ignis_type::definition::DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
        type_id: iter_type,
        mutable: false,
      }),
      name: tmp_name,
      span: span.clone(),
      name_span: span.clone(),
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
      doc: None,
    };
    let tmp_def_id = self.defs.alloc(tmp_def);

    let iter_value = self.lower_node_to_hir(iter_node, hir, scope_kind);
    let let_tmp = hir.alloc(HIRNode {
      kind: HIRKind::Let {
        name: tmp_def_id,
        value: Some(iter_value),
      },
      span: span.clone(),
      type_id: self.types.void(),
    });

    let tmp_ref = hir.alloc(HIRNode {
      kind: HIRKind::Variable(tmp_def_id),
      span: span.clone(),
      type_id: iter_type,
    });

    (tmp_ref, vec![let_tmp])
  }

  fn lower_for_of_fixed(
    &mut self,
    ctx: &ForOfContext<'_>,
    hir: &mut HIR,
    size: usize,
  ) -> HIRId {
    let u64_type = self.types.u64();

    let idx_name = self
      .symbols
      .borrow_mut()
      .intern(&format!("__for_of_i_{}", ctx.synth_id));

    let idx_def = ignis_type::definition::Definition {
      kind: ignis_type::definition::DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
        type_id: u64_type,
        mutable: true,
      }),
      name: idx_name,
      span: ctx.span.clone(),
      name_span: ctx.span.clone(),
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
      doc: None,
    };
    let idx_def_id = self.defs.alloc(idx_def);
    self.scopes.define(&idx_name, &idx_def_id, false).ok();

    let zero_lit = hir.alloc(HIRNode {
      kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::UnsignedInt64(0)),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let init = hir.alloc(HIRNode {
      kind: HIRKind::Let {
        name: idx_def_id,
        value: Some(zero_lit),
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    let idx_var = hir.alloc(HIRNode {
      kind: HIRKind::Variable(idx_def_id),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let len_lit = hir.alloc(HIRNode {
      kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::UnsignedInt64(size as u64)),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let condition = hir.alloc(HIRNode {
      kind: HIRKind::Binary {
        operation: BinaryOperation::LessThan,
        left: idx_var,
        right: len_lit,
      },
      span: ctx.span.clone(),
      type_id: self.types.boolean(),
    });

    let idx_var_update = hir.alloc(HIRNode {
      kind: HIRKind::Variable(idx_def_id),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let one_lit = hir.alloc(HIRNode {
      kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::UnsignedInt64(1)),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let update = hir.alloc(HIRNode {
      kind: HIRKind::Assign {
        target: idx_var_update,
        value: one_lit,
        operation: Some(BinaryOperation::Add),
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    let idx_var_access = hir.alloc(HIRNode {
      kind: HIRKind::Variable(idx_def_id),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let index_expr = hir.alloc(HIRNode {
      kind: HIRKind::Index {
        base: ctx.iter_hir,
        index: idx_var_access,
      },
      span: ctx.span.clone(),
      type_id: ctx.element_type,
    });

    let elem_value = if ctx.is_by_ref {
      hir.alloc(HIRNode {
        kind: HIRKind::Reference {
          expression: index_expr,
          mutable: ctx.is_mut_ref,
        },
        span: ctx.span.clone(),
        type_id: self.types.reference(ctx.element_type, ctx.is_mut_ref),
      })
    } else {
      index_expr
    };

    let binding_let = hir.alloc(HIRNode {
      kind: HIRKind::Let {
        name: ctx.binding_def_id,
        value: Some(elem_value),
      },
      span: ctx.for_of.binding.span.clone(),
      type_id: self.types.void(),
    });

    let original_body = self.lower_node_to_hir(&ctx.for_of.body, hir, ScopeKind::Loop);

    let body_block = hir.alloc(HIRNode {
      kind: HIRKind::Block {
        statements: vec![binding_let, original_body],
        expression: None,
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    hir.alloc(HIRNode {
      kind: HIRKind::Loop {
        condition: LoopKind::For {
          init: Some(init),
          condition: Some(condition),
          update: Some(update),
        },
        body: body_block,
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    })
  }

  /// Extracts iteration info from a record type for `for..of`.
  ///
  /// Returns `(element_type, data_field_index, length_field_index)` if the
  /// record has a pointer-typed `data` field and a `u64`-typed `length` field.
  fn extract_record_iterable_info(
    &self,
    def_id: &DefinitionId,
  ) -> Option<(TypeId, u32, u32)> {
    let rd = match &self.defs.get(def_id).kind {
      DefinitionKind::Record(rd) => rd.clone(),
      _ => return None,
    };

    let sym = self.symbols.borrow();
    let data_sym = *sym.map.get("data")?;
    let length_sym = *sym.map.get("length")?;

    let mut data_info: Option<(TypeId, u32)> = None;
    let mut length_index: Option<u32> = None;

    for field in &rd.fields {
      if field.name == data_sym {
        if let Type::Pointer { inner, .. } = self.types.get(&field.type_id) {
          data_info = Some((*inner, field.index));
        }
      } else if field.name == length_sym && self.types.types_equal(&field.type_id, &self.types.u64()) {
        length_index = Some(field.index);
      }
    }

    match (data_info, length_index) {
      (Some((elem_ty, data_idx)), Some(len_idx)) => Some((elem_ty, data_idx, len_idx)),
      _ => None,
    }
  }

  /// Lowers `for..of` over a record with `data: *T` and `length: u64` fields.
  ///
  /// Desugars to:
  /// ```text
  /// {
  ///   let __data: *T = iter.data;       // FieldAccess
  ///   let __len: u64 = iter.length;     // FieldAccess
  ///   let mut __i: u64 = 0;
  ///   for (; __i < __len; __i += 1) {
  ///     let elem: T = __data[__i];      // Index on pointer
  ///     <body>
  ///   }
  /// }
  /// ```
  fn lower_for_of_record(
    &mut self,
    ctx: &ForOfContext<'_>,
    hir: &mut HIR,
    def_id: &DefinitionId,
  ) -> HIRId {
    let u64_type = self.types.u64();

    let (_, data_field_index, length_field_index) = match self.extract_record_iterable_info(def_id) {
      Some(info) => info,
      None => {
        return hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: ctx.span.clone(),
          type_id: self.types.error(),
        });
      },
    };

    // Get the data pointer type from the field
    let data_ptr_type = {
      let rd = match &self.defs.get(def_id).kind {
        DefinitionKind::Record(rd) => rd.clone(),
        _ => {
          return hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: ctx.span.clone(),
            type_id: self.types.error(),
          });
        },
      };
      rd.fields[data_field_index as usize].type_id
    };

    // -- __data = iter.data (FieldAccess) --

    let data_access = hir.alloc(HIRNode {
      kind: HIRKind::FieldAccess {
        base: ctx.iter_hir,
        field_index: data_field_index,
      },
      span: ctx.span.clone(),
      type_id: data_ptr_type,
    });

    let data_name = self
      .symbols
      .borrow_mut()
      .intern(&format!("__for_of_data_{}", ctx.synth_id));

    let data_def = ignis_type::definition::Definition {
      kind: ignis_type::definition::DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
        type_id: data_ptr_type,
        mutable: false,
      }),
      name: data_name,
      span: ctx.span.clone(),
      name_span: ctx.span.clone(),
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
      doc: None,
    };
    let data_def_id = self.defs.alloc(data_def);
    self.scopes.define(&data_name, &data_def_id, false).ok();

    let data_let = hir.alloc(HIRNode {
      kind: HIRKind::Let {
        name: data_def_id,
        value: Some(data_access),
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    // -- __len = iter.length (FieldAccess) --

    let len_access = hir.alloc(HIRNode {
      kind: HIRKind::FieldAccess {
        base: ctx.iter_hir,
        field_index: length_field_index,
      },
      span: ctx.span.clone(),
      type_id: u64_type,
    });

    let len_name = self
      .symbols
      .borrow_mut()
      .intern(&format!("__for_of_len_{}", ctx.synth_id));

    let len_def = ignis_type::definition::Definition {
      kind: ignis_type::definition::DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
        type_id: u64_type,
        mutable: false,
      }),
      name: len_name,
      span: ctx.span.clone(),
      name_span: ctx.span.clone(),
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
      doc: None,
    };
    let len_def_id = self.defs.alloc(len_def);
    self.scopes.define(&len_name, &len_def_id, false).ok();

    let len_let = hir.alloc(HIRNode {
      kind: HIRKind::Let {
        name: len_def_id,
        value: Some(len_access),
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    // -- index variable: let mut __i: u64 = 0 --

    let idx_name = self
      .symbols
      .borrow_mut()
      .intern(&format!("__for_of_i_{}", ctx.synth_id));

    let idx_def = ignis_type::definition::Definition {
      kind: ignis_type::definition::DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
        type_id: u64_type,
        mutable: true,
      }),
      name: idx_name,
      span: ctx.span.clone(),
      name_span: ctx.span.clone(),
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
      doc: None,
    };
    let idx_def_id = self.defs.alloc(idx_def);
    self.scopes.define(&idx_name, &idx_def_id, false).ok();

    let zero_lit = hir.alloc(HIRNode {
      kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::UnsignedInt64(0)),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let init = hir.alloc(HIRNode {
      kind: HIRKind::Let {
        name: idx_def_id,
        value: Some(zero_lit),
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    // -- condition: __i < __len --

    let idx_var = hir.alloc(HIRNode {
      kind: HIRKind::Variable(idx_def_id),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let len_var = hir.alloc(HIRNode {
      kind: HIRKind::Variable(len_def_id),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let condition = hir.alloc(HIRNode {
      kind: HIRKind::Binary {
        operation: BinaryOperation::LessThan,
        left: idx_var,
        right: len_var,
      },
      span: ctx.span.clone(),
      type_id: self.types.boolean(),
    });

    // -- update: __i += 1 --

    let idx_var_update = hir.alloc(HIRNode {
      kind: HIRKind::Variable(idx_def_id),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let one_lit = hir.alloc(HIRNode {
      kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::UnsignedInt64(1)),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let update = hir.alloc(HIRNode {
      kind: HIRKind::Assign {
        target: idx_var_update,
        value: one_lit,
        operation: Some(BinaryOperation::Add),
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    // -- body: let elem = __data[__i] --

    let data_var = hir.alloc(HIRNode {
      kind: HIRKind::Variable(data_def_id),
      span: ctx.span.clone(),
      type_id: data_ptr_type,
    });
    let idx_var_access = hir.alloc(HIRNode {
      kind: HIRKind::Variable(idx_def_id),
      span: ctx.span.clone(),
      type_id: u64_type,
    });
    let index_expr = hir.alloc(HIRNode {
      kind: HIRKind::Index {
        base: data_var,
        index: idx_var_access,
      },
      span: ctx.span.clone(),
      type_id: ctx.element_type,
    });

    let elem_value = if ctx.is_by_ref {
      hir.alloc(HIRNode {
        kind: HIRKind::Reference {
          expression: index_expr,
          mutable: ctx.is_mut_ref,
        },
        span: ctx.span.clone(),
        type_id: self.types.reference(ctx.element_type, ctx.is_mut_ref),
      })
    } else {
      index_expr
    };

    let binding_let = hir.alloc(HIRNode {
      kind: HIRKind::Let {
        name: ctx.binding_def_id,
        value: Some(elem_value),
      },
      span: ctx.for_of.binding.span.clone(),
      type_id: self.types.void(),
    });

    let original_body = self.lower_node_to_hir(&ctx.for_of.body, hir, ScopeKind::Loop);

    let body_block = hir.alloc(HIRNode {
      kind: HIRKind::Block {
        statements: vec![binding_let, original_body],
        expression: None,
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    let loop_hir = hir.alloc(HIRNode {
      kind: HIRKind::Loop {
        condition: LoopKind::For {
          init: Some(init),
          condition: Some(condition),
          update: Some(update),
        },
        body: body_block,
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

    hir.alloc(HIRNode {
      kind: HIRKind::Block {
        statements: vec![data_let, len_let, loop_hir],
        expression: None,
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    })
  }

  /// Resolve or infer type arguments for a function call.
  ///
  /// If explicit type arguments are provided, uses those directly.
  /// Otherwise, attempts to infer type arguments by unifying parameter types
  /// with argument types.
  fn resolve_or_infer_call_type_args(
    &mut self,
    callee_def_id: &DefinitionId,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    args_hir: &[HIRId],
    hir: &HIR,
  ) -> Vec<TypeId> {
    // Get type parameters from definition
    let type_params = match &self.defs.get(callee_def_id).kind {
      DefinitionKind::Function(fd) => fd.type_params.clone(),
      DefinitionKind::Method(md) => md.type_params.clone(),
      _ => return vec![],
    };

    // Not generic - return empty
    if type_params.is_empty() {
      return vec![];
    }

    // If explicit type args provided, use them
    if let Some(explicit_args) = &call.type_args {
      let resolved: Vec<_> = explicit_args.iter().map(|t| self.resolve_type_syntax(t)).collect();
      return resolved;
    }

    // Infer from argument types
    let arg_types: Vec<TypeId> = args_hir.iter().map(|id| hir.get(*id).type_id).collect();

    // Get parameter definitions
    let param_defs = match &self.defs.get(callee_def_id).kind {
      DefinitionKind::Function(fd) => fd.params.clone(),
      DefinitionKind::Method(md) => {
        if md.is_static {
          md.params.clone()
        } else {
          // Skip `self` parameter for instance methods
          md.params.iter().skip(1).cloned().collect()
        }
      },
      _ => return vec![],
    };

    let mut subst = Substitution::new();

    for (i, &arg_ty) in arg_types.iter().enumerate() {
      if let Some(&param_def_id) = param_defs.get(i) {
        let param_ty = *self.defs.type_of(&param_def_id);
        self.types.unify_for_inference(param_ty, arg_ty, &mut subst);
      }
    }

    // Extract inferred types in order
    type_params
      .iter()
      .enumerate()
      .map(|(i, _)| {
        subst
          .get(*callee_def_id, i as u32)
          .unwrap_or_else(|| self.types.error())
      })
      .collect()
  }

  /// Resolve or infer type arguments for a method call.
  ///
  /// If explicit type arguments are provided, uses those directly.
  /// Otherwise, attempts to infer type arguments by unifying parameter types
  /// with argument types.
  ///
  /// For static methods on generic types (e.g., `Vector<T>::init()`), the returned
  /// type args include both owner type args and method type args:
  /// [owner_arg_0, ..., owner_arg_n, method_arg_0, ..., method_arg_m]
  fn resolve_or_infer_method_type_args(
    &mut self,
    method_def_id: &DefinitionId,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    args_hir: &[HIRId],
    hir: &HIR,
  ) -> Vec<TypeId> {
    // Get method definition and extract owner/method type param counts
    let (owner_type_params, method_type_params, is_static) = match &self.defs.get(method_def_id).kind {
      DefinitionKind::Method(md) => {
        let owner_def = self.defs.get(&md.owner_type);
        let owner_params = match &owner_def.kind {
          DefinitionKind::Record(rd) => rd.type_params.clone(),
          DefinitionKind::Enum(ed) => ed.type_params.clone(),
          _ => vec![],
        };
        (owner_params, md.type_params.clone(), md.is_static)
      },
      _ => return vec![],
    };

    // For static methods, we need both owner and method type params
    // For instance methods, only method type params (owner args come from receiver)
    let total_type_params = if is_static {
      owner_type_params.len() + method_type_params.len()
    } else {
      method_type_params.len()
    };

    // If no type params needed at all, return empty
    if total_type_params == 0 {
      return vec![];
    }

    // If explicit type args provided, use them
    if let Some(explicit_args) = &call.type_args {
      return explicit_args.iter().map(|t| self.resolve_type_syntax(t)).collect();
    }

    // Infer from argument types
    let arg_types: Vec<TypeId> = args_hir.iter().map(|id| hir.get(*id).type_id).collect();

    // Get parameter definitions (skip `self` for instance methods)
    let param_defs = match &self.defs.get(method_def_id).kind {
      DefinitionKind::Method(md) => {
        if md.is_static {
          md.params.clone()
        } else {
          md.params.iter().skip(1).cloned().collect()
        }
      },
      _ => return vec![],
    };

    let mut subst = Substitution::new();

    for (i, &arg_ty) in arg_types.iter().enumerate() {
      if let Some(&param_def_id) = param_defs.get(i) {
        let param_ty = *self.defs.type_of(&param_def_id);
        self.types.unify_for_inference(param_ty, arg_ty, &mut subst);
      }
    }

    // Extract inferred types in order
    // For static methods: first owner params, then method params
    // For instance methods: only method params
    if is_static {
      let owner_def = match &self.defs.get(method_def_id).kind {
        DefinitionKind::Method(md) => md.owner_type,
        _ => unreachable!(),
      };

      let mut result = Vec::with_capacity(total_type_params);

      // Owner type params
      for (i, _) in owner_type_params.iter().enumerate() {
        result.push(subst.get(owner_def, i as u32).unwrap_or_else(|| self.types.error()));
      }

      // Method type params
      for (i, _) in method_type_params.iter().enumerate() {
        result.push(
          subst
            .get(*method_def_id, i as u32)
            .unwrap_or_else(|| self.types.error()),
        );
      }

      result
    } else {
      method_type_params
        .iter()
        .enumerate()
        .map(|(i, _)| {
          subst
            .get(*method_def_id, i as u32)
            .unwrap_or_else(|| self.types.error())
        })
        .collect()
    }
  }

  /// Pushes a generic scope and registers type params for an owner definition.
  /// Used when lowering method bodies to make type params visible for type resolution.
  pub(crate) fn enter_type_params_scope_for_method(
    &mut self,
    method_def_id: &DefinitionId,
  ) {
    // Get owner's type params
    let owner_type_params = match &self.defs.get(method_def_id).kind {
      DefinitionKind::Method(md) => {
        let owner_def = self.defs.get(&md.owner_type);
        match &owner_def.kind {
          DefinitionKind::Record(rd) => rd.type_params.clone(),
          DefinitionKind::Enum(ed) => ed.type_params.clone(),
          _ => Vec::new(),
        }
      },
      _ => Vec::new(),
    };

    // Get method's own type params
    let method_type_params = match &self.defs.get(method_def_id).kind {
      DefinitionKind::Method(md) => md.type_params.clone(),
      _ => Vec::new(),
    };

    // If no type params at all, nothing to do
    if owner_type_params.is_empty() && method_type_params.is_empty() {
      return;
    }

    self.scopes.push(ScopeKind::Generic);

    // Register owner type params first
    for param_id in &owner_type_params {
      let name = self.defs.get(param_id).name;
      let _ = self.scopes.define(&name, param_id, false);
    }

    // Then method type params
    for param_id in &method_type_params {
      let name = self.defs.get(param_id).name;
      let _ = self.scopes.define(&name, param_id, false);
    }
  }

  /// Pops the generic scope if the method or its owner has type params.
  pub(crate) fn exit_type_params_scope_for_method(
    &mut self,
    method_def_id: &DefinitionId,
  ) {
    let has_owner_type_params = match &self.defs.get(method_def_id).kind {
      DefinitionKind::Method(md) => {
        let owner_def = self.defs.get(&md.owner_type);
        match &owner_def.kind {
          DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
          DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
          _ => false,
        }
      },
      _ => false,
    };

    let has_method_type_params = match &self.defs.get(method_def_id).kind {
      DefinitionKind::Method(md) => !md.type_params.is_empty(),
      _ => false,
    };

    if has_owner_type_params || has_method_type_params {
      self.scopes.pop();
    }
  }

  /// Pushes a generic scope and registers type params for a function definition.
  /// Used when lowering function bodies to make type params visible for type resolution.
  fn enter_type_params_scope_for_function(
    &mut self,
    func_def_id: &DefinitionId,
  ) {
    let type_params = match &self.defs.get(func_def_id).kind {
      DefinitionKind::Function(fd) => fd.type_params.clone(),
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

  /// Pops the generic scope if the function has type params.
  fn exit_type_params_scope_for_function(
    &mut self,
    func_def_id: &DefinitionId,
  ) {
    let has_type_params = match &self.defs.get(func_def_id).kind {
      DefinitionKind::Function(fd) => !fd.type_params.is_empty(),
      _ => false,
    };

    if has_type_params {
      self.scopes.pop();
    }
  }
}

fn convert_binary_op(op: &ASTBinaryOperator) -> BinaryOperation {
  match op {
    ASTBinaryOperator::Add => BinaryOperation::Add,
    ASTBinaryOperator::Subtract => BinaryOperation::Sub,
    ASTBinaryOperator::Multiply => BinaryOperation::Mul,
    ASTBinaryOperator::Divide => BinaryOperation::Div,
    ASTBinaryOperator::Modulo => BinaryOperation::Mod,
    ASTBinaryOperator::Equal => BinaryOperation::Equal,
    ASTBinaryOperator::NotEqual => BinaryOperation::NotEqual,
    ASTBinaryOperator::LessThan => BinaryOperation::LessThan,
    ASTBinaryOperator::LessThanOrEqual => BinaryOperation::LessEqual,
    ASTBinaryOperator::GreaterThan => BinaryOperation::GreaterThan,
    ASTBinaryOperator::GreaterThanOrEqual => BinaryOperation::GreaterEqual,
    ASTBinaryOperator::And => BinaryOperation::And,
    ASTBinaryOperator::Or => BinaryOperation::Or,
    ASTBinaryOperator::BitAnd => BinaryOperation::BitAnd,
    ASTBinaryOperator::BitOr => BinaryOperation::BitOr,
    ASTBinaryOperator::BitXor => BinaryOperation::BitXor,
    ASTBinaryOperator::ShiftLeft => BinaryOperation::BitShiftLeft,
    ASTBinaryOperator::ShiftRight => BinaryOperation::BitShiftRight,
  }
}

fn convert_unary_op(op: &UnaryOperator) -> UnaryOperation {
  match op {
    UnaryOperator::Not => UnaryOperation::Not,
    UnaryOperator::Negate => UnaryOperation::Neg,
    UnaryOperator::BitNot => UnaryOperation::BitNot,
    _ => UnaryOperation::Neg,
  }
}

fn convert_assignment_op(op: &ASTAssignmentOperator) -> BinaryOperation {
  match op {
    ASTAssignmentOperator::Assign => BinaryOperation::Add,
    ASTAssignmentOperator::AddAssign => BinaryOperation::Add,
    ASTAssignmentOperator::SubAssign => BinaryOperation::Sub,
    ASTAssignmentOperator::MulAssign => BinaryOperation::Mul,
    ASTAssignmentOperator::DivAssign => BinaryOperation::Div,
    ASTAssignmentOperator::ModAssign => BinaryOperation::Mod,
    ASTAssignmentOperator::ShiftLeftAssign => BinaryOperation::BitShiftLeft,
    ASTAssignmentOperator::ShiftRightAssign => BinaryOperation::BitShiftRight,
    _ => BinaryOperation::Add,
  }
}

impl Analyzer<'_> {
  fn lower_pipe_to_hir(
    &mut self,
    node_id: &NodeId,
    lhs: &NodeId,
    rhs: &NodeId,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let lhs_hir = self.lower_node_to_hir(lhs, hir, scope_kind);
    let pipe_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());
    let span = self.node_span(node_id).clone();

    let resolution = self.pipe_resolutions.get(node_id).cloned();

    match resolution {
      Some(PipeResolution::DirectCall {
        def_id,
        extra_args,
        type_args,
        insertion,
      }) => {
        let all_args = self.assemble_pipe_args(lhs_hir, &extra_args, insertion, hir, scope_kind);

        hir.alloc(HIRNode {
          kind: HIRKind::Call {
            callee: def_id,
            type_args,
            args: all_args,
          },
          span,
          type_id: pipe_type,
        })
      },
      Some(PipeResolution::ClosureCall {
        callee_node,
        extra_args,
        insertion,
      }) => {
        let callee_hir = self.lower_node_to_hir(&callee_node, hir, scope_kind);
        let all_args = self.assemble_pipe_args(lhs_hir, &extra_args, insertion, hir, scope_kind);

        hir.alloc(HIRNode {
          kind: HIRKind::CallClosure {
            callee: callee_hir,
            args: all_args,
          },
          span,
          type_id: pipe_type,
        })
      },

      Some(PipeResolution::MethodCall {
        receiver_node,
        method_id,
        extra_args,
        type_args,
        self_mutable,
        insertion,
      }) => {
        let base = self.lower_node_to_hir(&receiver_node, hir, scope_kind);
        let base_type = hir.get(base).type_id;

        let is_already_ref = matches!(self.types.get(&base_type).clone(), Type::Reference { .. });

        let receiver_hir = if is_already_ref {
          base
        } else {
          let ref_type = self.types.reference(base_type, self_mutable);
          hir.alloc(HIRNode {
            kind: HIRKind::Reference {
              expression: base,
              mutable: self_mutable,
            },
            span: span.clone(),
            type_id: ref_type,
          })
        };

        let all_args = self.assemble_pipe_args(lhs_hir, &extra_args, insertion, hir, scope_kind);

        hir.alloc(HIRNode {
          kind: HIRKind::MethodCall {
            receiver: Some(receiver_hir),
            method: method_id,
            type_args,
            args: all_args,
          },
          span,
          type_id: pipe_type,
        })
      },

      // Ambient path: deep placeholder or non-call RHS (RecordInit, Vector).
      // Lower the entire RHS normally; PipePlaceholder nodes read lhs_hir from the stack.
      None => {
        self.pipe_lhs_hir_stack.push(lhs_hir);
        let rhs_hir = self.lower_node_to_hir(rhs, hir, scope_kind);
        self.pipe_lhs_hir_stack.pop();
        rhs_hir
      },
    }
  }

  /// Assembles the final argument list for a pipe call, respecting Prepend vs ReplaceAt.
  fn assemble_pipe_args(
    &mut self,
    lhs_hir: ignis_hir::HIRId,
    extra_args: &[NodeId],
    insertion: PipeArgInsertion,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> Vec<ignis_hir::HIRId> {
    match insertion {
      PipeArgInsertion::Prepend => {
        let mut all_args = vec![lhs_hir];
        for arg in extra_args {
          all_args.push(self.lower_node_to_hir(arg, hir, scope_kind));
        }
        all_args
      },

      PipeArgInsertion::ReplaceAt(index) => extra_args
        .iter()
        .enumerate()
        .map(|(i, arg)| {
          if i == index {
            debug_assert!(
              matches!(self.ast.get(arg), ASTNode::Expression(ASTExpression::PipePlaceholder { .. })),
              "ReplaceAt({}) but extra_args[{}] is not PipePlaceholder — typeck/lowering desync",
              index,
              index
            );
            lhs_hir
          } else {
            self.lower_node_to_hir(arg, hir, scope_kind)
          }
        })
        .collect(),
    }
  }

  fn lower_lambda_to_hir(
    &mut self,
    node_id: &ignis_ast::NodeId,
    lambda: &ignis_ast::expressions::lambda::ASTLambda,
    hir: &mut ignis_hir::HIR,
    _scope_kind: ScopeKind,
  ) -> ignis_hir::HIRId {
    use ignis_hir::{HIRKind, HIRNode};

    let fn_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

    let param_defs = self.lambda_param_defs.get(node_id).cloned().unwrap_or_default();

    let return_type = match self.types.get(&fn_type) {
      ignis_type::types::Type::Function { ret, .. } => *ret,
      _ => self.types.error(),
    };

    self.scopes.push(ScopeKind::Function);
    for &param_id in &param_defs {
      let name = &self.defs.get(&param_id).name;
      let _ = self.scopes.define(name, &param_id, false);
    }

    self.lowering_return_type_stack.push(return_type);

    self.capture_override_stack.push(std::collections::HashMap::new());

    // Expression bodies get an implicit Return so the thunk returns the value.
    let body = match &lambda.body {
      ignis_ast::expressions::lambda::LambdaBody::Expression(id) => {
        let expr_hir = self.lower_node_to_hir(id, hir, ScopeKind::Function);
        let span = hir.get(expr_hir).span.clone();
        hir.alloc(HIRNode {
          kind: HIRKind::Return(Some(expr_hir)),
          span,
          type_id: return_type,
        })
      },
      ignis_ast::expressions::lambda::LambdaBody::Block(id) => self.lower_node_to_hir(id, hir, ScopeKind::Function),
    };

    let capture_overrides = self.capture_override_stack.pop().unwrap_or_default();

    self.lowering_return_type_stack.pop();

    self.scopes.pop();

    // Captures and escape analysis will be filled in by a later pass.
    hir.alloc(HIRNode {
      kind: HIRKind::Closure {
        params: param_defs,
        return_type,
        body,
        captures: Vec::new(),
        escapes: false,
        thunk_def: None,
        drop_def: None,
        capture_overrides,
      },
      span: lambda.span.clone(),
      type_id: fn_type,
    })
  }
}
