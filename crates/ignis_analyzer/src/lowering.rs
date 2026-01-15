use crate::{Analyzer, ResolvedPath, ScopeKind};
use ignis_ast::{ASTNode, NodeId, statements::ASTStatement, expressions::ASTExpression};
use ignis_ast::expressions::binary::ASTBinaryOperator;
use ignis_ast::expressions::assignment::ASTAssignmentOperator;
use ignis_ast::expressions::member_access::ASTAccessOp;
use ignis_ast::expressions::unary::UnaryOperator;
use ignis_ast::statements::record::{ASTRecord, ASTRecordItem};
use ignis_ast::statements::enum_::{ASTEnum, ASTEnumItem};
use ignis_ast::statements::for_of::ASTForOf;
use ignis_hir::{
  HIR, HIRNode, HIRKind, HIRId,
  operation::{BinaryOperation, UnaryOperation},
  statement::LoopKind,
};
use ignis_type::definition::{DefinitionId, DefinitionKind, SymbolEntry};
use ignis_type::span::Span;
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

    for root in roots {
      self.lower_node_to_hir(root, &mut hir, ScopeKind::Global);
    }

    hir
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
          self.scopes.push(ScopeKind::Function);

          if let Some(def_id_val) = self.lookup_def(node_id).cloned() {
            self.define_function_params_in_scope(&def_id_val);
          }

          let body_hir_id = self.lower_node_to_hir(&body_id, hir, ScopeKind::Function);
          self.scopes.pop();
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
        // Lower all items in namespace block
        let mut hir_ids = Vec::new();
        for item in &ns.items {
          hir_ids.push(self.lower_node_to_hir(item, hir, scope_kind));
        }
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
        ignis_ast::statements::ASTExport::Name { .. } => hir.alloc(HIRNode {
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
      ASTStatement::ForOf(for_of) => self.lower_for_of(node_id, for_of, hir),
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

          // Look up method in instance_methods or static_methods
          if method.is_static() {
            rd.static_methods.get(&method.name).and_then(|e| e.as_single()).cloned()
          } else {
            rd.instance_methods
              .get(&method.name)
              .and_then(|e| e.as_single())
              .cloned()
          }
        };

        let Some(method_def_id) = method_def_id else {
          continue;
        };

        // Lower the method body
        self.scopes.push(ScopeKind::Function);
        self.define_function_params_in_scope(&method_def_id);

        let body_hir_id = self.lower_node_to_hir(&method.body, hir, ScopeKind::Function);
        self.scopes.pop();

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

          // Enum methods are always static
          ed.static_methods.get(&method.name).and_then(|e| e.as_single()).cloned()
        };

        let Some(method_def_id) = method_def_id else {
          continue;
        };

        // Lower the method body
        self.scopes.push(ScopeKind::Function);
        self.define_function_params_in_scope(&method_def_id);

        let body_hir_id = self.lower_node_to_hir(&method.body, hir, ScopeKind::Function);
        self.scopes.pop();

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
          ignis_type::value::IgnisLiteralValue::String(_) => self.types.string(),
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
        let def_id = match self.scopes.lookup_def(&var.name) {
          Some(id) => id.clone(),
          None => {
            return hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span: var.span.clone(),
              type_id: self.types.error(),
            });
          },
        };
        let var_type = self.type_of(&def_id).clone();

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

          if name == "typeOf" {
            return self.lower_typeof_builtin(call, hir, scope_kind);
          }
          if name == "sizeOf" {
            return self.lower_sizeof_builtin(call, hir, scope_kind);
          }
          if name == "__builtin_read" {
            return self.lower_builtin_read(call, hir, scope_kind);
          }
          if name == "__builtin_write" {
            return self.lower_builtin_write(call, hir, scope_kind);
          }
        }

        if let ASTNode::Expression(ASTExpression::MemberAccess(ma)) = callee_node {
          return self.lower_method_call(node_id, ma, call, hir, scope_kind);
        }

        if let ASTNode::Expression(ASTExpression::Path(path)) = callee_node {
          if let Some(result) = self.try_lower_path_call(node_id, path, call, hir, scope_kind) {
            return result;
          }
        }

        // Get the callee def_id by looking up the variable name
        let callee_def_id = if let Some(def_id) = self.lookup_resolved_call(node_id).cloned() {
          def_id
        } else {
          match callee_node {
            ASTNode::Expression(ASTExpression::Variable(var)) => match self.scopes.lookup_def(&var.name) {
              Some(def_id) => def_id.clone(),
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
          ignis_type::types::Type::Pointer(inner) => inner,
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

        let elem_type = if let ignis_type::types::Type::Vector { element, .. } = self.types.get(&base_type).clone() {
          element
        } else {
          self.types.error()
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
          Some(vector.items.len()),
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

        if let Some(def_id) = def_id {
          if let DefinitionKind::Record(rd) = &self.defs.get(&def_id).kind {
            if let Some(field) = rd.fields.iter().find(|f| f.name == ma.member) {
              let field_type = stored_type.unwrap_or_else(|| field.type_id);
              return hir.alloc(HIRNode {
                kind: HIRKind::FieldAccess {
                  base: derefed_base,
                  field_index: field.index,
                },
                span: ma.span.clone(),
                type_id: field_type,
              });
            }
          }
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
        let mut current_def = self.scopes.lookup_def(first_segment).cloned()?;

        for segment in path.segments.iter().skip(1) {
          match &self.defs.get(&current_def).kind {
            DefinitionKind::Namespace(ns_def) => {
              current_def = self
                .namespaces
                .lookup_def(ns_def.namespace_id, segment)
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
    let arg_type = hir.get(arg).type_id;
    let base_type = self.unwrap_reference_type(&arg_type);

    if matches!(self.types.get(&base_type), ignis_type::types::Type::Infer) {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: call.span.clone(),
        type_id: self.types.error(),
      });
    }

    hir.alloc(HIRNode {
      kind: HIRKind::SizeOf(base_type),
      span: call.span.clone(),
      type_id: self.types.u64(),
    })
  }

  fn lower_builtin_read(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
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

    if call.arguments.len() != 1 {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: call.span.clone(),
        type_id: self.types.error(),
      });
    }

    let value_type = self.resolve_type_syntax(&type_args[0]);
    let ptr = self.lower_node_to_hir(&call.arguments[0], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::BuiltinLoad { ty: value_type, ptr },
      span: call.span.clone(),
      type_id: value_type,
    })
  }

  fn lower_builtin_write(
    &mut self,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
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

    if call.arguments.len() != 2 {
      return hir.alloc(HIRNode {
        kind: HIRKind::Error,
        span: call.span.clone(),
        type_id: self.types.error(),
      });
    }

    let value_type = self.resolve_type_syntax(&type_args[0]);
    let ptr = self.lower_node_to_hir(&call.arguments[0], hir, scope_kind);
    let value = self.lower_node_to_hir(&call.arguments[1], hir, scope_kind);

    hir.alloc(HIRNode {
      kind: HIRKind::BuiltinStore {
        ty: value_type,
        ptr,
        value,
      },
      span: call.span.clone(),
      type_id: self.types.void(),
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

    // For instance methods, self is passed by reference.
    // If base is already a reference, use it directly.
    // If base is a value, take a reference to it.
    let (receiver_hir, receiver_record_type) = if let Type::Reference { inner, .. } = self.types.get(&base_type).clone()
    {
      // Already a reference, use as-is
      (base, inner)
    } else {
      // Value type - we need to take a reference to it
      // Create a reference node
      let ref_type = self.types.reference(base_type, false);
      let ref_node = hir.alloc(HIRNode {
        kind: HIRKind::Reference {
          expression: base,
          mutable: false,
        },
        span: ma.span.clone(),
        type_id: ref_type,
      });
      (ref_node, base_type)
    };

    match self.types.get(&receiver_record_type).clone() {
      Type::Record(def_id) | Type::Instance { generic: def_id, .. } => {
        let rd = if let DefinitionKind::Record(rd) = &self.defs.get(&def_id).kind {
          rd.clone()
        } else {
          return hir.alloc(HIRNode {
            kind: HIRKind::Error,
            span: call.span.clone(),
            type_id: self.types.error(),
          });
        };

        if let Some(method_id) = self.lookup_resolved_call(node_id).cloned() {
          if matches!(self.defs.get(&method_id).kind, DefinitionKind::Method(_)) {
            let args_hir: Vec<HIRId> = call
              .arguments
              .iter()
              .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
              .collect();

            let type_args = self.resolve_or_infer_method_type_args(&method_id, call, &args_hir, hir);

            return hir.alloc(HIRNode {
              kind: HIRKind::MethodCall {
                receiver: Some(receiver_hir),
                method: method_id,
                type_args,
                args: args_hir,
              },
              span: call.span.clone(),
              type_id: result_type,
            });
          }
        }

        // Look up instance method
        if let Some(method_id) = rd.instance_methods.get(&ma.member).and_then(|e| e.as_single()) {
          let args_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          // Resolve type arguments: explicit first, then infer if needed
          let type_args = self.resolve_or_infer_method_type_args(method_id, call, &args_hir, hir);

          return hir.alloc(HIRNode {
            kind: HIRKind::MethodCall {
              receiver: Some(receiver_hir),
              method: *method_id,
              type_args,
              args: args_hir,
            },
            span: call.span.clone(),
            type_id: result_type,
          });
        }

        // Method not found - error
        hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: call.span.clone(),
          type_id: self.types.error(),
        })
      },
      _ => {
        // Not a record - error
        hir.alloc(HIRNode {
          kind: HIRKind::Error,
          span: call.span.clone(),
          type_id: self.types.error(),
        })
      },
    }
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

    if let Some(method_id) = self.lookup_resolved_call(node_id).cloned() {
      if matches!(self.defs.get(&method_id).kind, DefinitionKind::Method(_)) {
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

  /// Try to lower a path-based call as an enum variant constructor.
  /// Returns None if not an enum variant, so caller can fall through to normal call.
  fn try_lower_path_call(
    &mut self,
    node_id: &NodeId,
    path: &ignis_ast::expressions::path::ASTPath,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> Option<HIRId> {
    if path.segments.len() != 2 {
      return None;
    }

    let first_segment = &path.segments[0];
    let second_segment = &path.segments[1];

    let type_def_id = self.scopes.lookup_def(first_segment)?.clone();
    let result_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

    match &self.defs.get(&type_def_id).kind.clone() {
      DefinitionKind::Enum(ed) => {
        // Enum variant with payload
        if let Some(&tag) = ed.variants_by_name.get(second_segment) {
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
        if let Some(method_id) = ed.static_methods.get(second_segment).and_then(|e| e.as_single()) {
          let args_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          // Resolve explicit type arguments for the method if present
          let type_args = call
            .type_args
            .as_ref()
            .map(|args| args.iter().map(|t| self.resolve_type_syntax(t)).collect())
            .unwrap_or_else(Vec::new);

          return Some(hir.alloc(HIRNode {
            kind: HIRKind::MethodCall {
              receiver: None,
              method: *method_id,
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
        if let Some(method_id) = rd.static_methods.get(second_segment).and_then(|e| e.as_single()) {
          let args_hir: Vec<HIRId> = call
            .arguments
            .iter()
            .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
            .collect();

          // Resolve explicit type arguments for the method if present
          let type_args = call
            .type_args
            .as_ref()
            .map(|args| args.iter().map(|t| self.resolve_type_syntax(t)).collect())
            .unwrap_or_else(Vec::new);

          return Some(hir.alloc(HIRNode {
            kind: HIRKind::MethodCall {
              receiver: None,
              method: *method_id,
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
          let ptr_u8 = self.types.pointer(self.types.u8());

          if !self.types.types_equal(&arg_type, &ptr_u8) {
            if let ignis_type::types::Type::Pointer(_) = self.types.get(&arg_type) {
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

    let (element_type, size) = match self.types.get(&iter_type).clone() {
      Type::Vector { element, size } => (element, size),
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

    let binding_type = self.defs.type_of(&binding_def_id).clone();
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

    let loop_hir = match size {
      Some(n) => self.lower_for_of_fixed(&ctx, hir, n),
      None => self.lower_for_of_dynamic(&ctx, hir),
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
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
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
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
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

  fn lower_for_of_dynamic(
    &mut self,
    ctx: &ForOfContext<'_>,
    hir: &mut HIR,
  ) -> HIRId {
    let runtime = self.runtime.as_ref().expect("runtime builtins not initialized");
    let u64_type = self.types.u64();
    let void_ptr_type = self.types.pointer(self.types.void());

    let len_call = hir.alloc(HIRNode {
      kind: HIRKind::Call {
        callee: runtime.buf_len,
        type_args: vec![],
        args: vec![ctx.iter_hir],
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
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
    };
    let len_def_id = self.defs.alloc(len_def);

    let len_let = hir.alloc(HIRNode {
      kind: HIRKind::Let {
        name: len_def_id,
        value: Some(len_call),
      },
      span: ctx.span.clone(),
      type_id: self.types.void(),
    });

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
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
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

    let buf_at_builtin = if ctx.is_mut_ref {
      runtime.buf_at
    } else {
      runtime.buf_at_const
    };

    let idx_var_access = hir.alloc(HIRNode {
      kind: HIRKind::Variable(idx_def_id),
      span: ctx.span.clone(),
      type_id: u64_type,
    });

    let buf_at_call = hir.alloc(HIRNode {
      kind: HIRKind::Call {
        callee: buf_at_builtin,
        type_args: vec![],
        args: vec![ctx.iter_hir, idx_var_access],
      },
      span: ctx.span.clone(),
      type_id: void_ptr_type,
    });

    let ref_type = self.types.reference(ctx.element_type, ctx.is_mut_ref);
    let casted = hir.alloc(HIRNode {
      kind: HIRKind::Cast {
        expression: buf_at_call,
        target: ref_type,
      },
      span: ctx.span.clone(),
      type_id: ref_type,
    });

    let elem_value = if ctx.is_by_ref {
      casted
    } else {
      hir.alloc(HIRNode {
        kind: HIRKind::Dereference(casted),
        span: ctx.span.clone(),
        type_id: ctx.element_type,
      })
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
        statements: vec![len_let, loop_hir],
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
      return explicit_args.iter().map(|t| self.resolve_type_syntax(t)).collect();
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
  fn resolve_or_infer_method_type_args(
    &mut self,
    method_def_id: &DefinitionId,
    call: &ignis_ast::expressions::call::ASTCallExpression,
    args_hir: &[HIRId],
    hir: &HIR,
  ) -> Vec<TypeId> {
    // Get type parameters from method definition
    let type_params = match &self.defs.get(method_def_id).kind {
      DefinitionKind::Method(md) => md.type_params.clone(),
      _ => return vec![],
    };

    // Not generic - return empty
    if type_params.is_empty() {
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
    type_params
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
