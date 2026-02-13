//! Inlay hints implementation.
//!
//! Provides parameter name hints at call sites:
//! ```ignis
//! createUser("john", 25) → createUser(name: "john", age: 25)
//! ```

use std::collections::HashMap;

use ignis_ast::expressions::ASTExpression;
use ignis_ast::statements::{ASTEnumItem, ASTRecordItem, ASTStatement};
use ignis_ast::{ASTNode, NodeId};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore};
use ignis_type::file::FileId;
use ignis_type::span::Span;
use ignis_type::symbol::SymbolId;
use ignis_type::Store;
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position};

use crate::convert::LineIndex;

/// A call expression found in the AST with its resolved definition.
struct CallInfo {
  /// Node IDs of the arguments (for checking arg expressions).
  argument_ids: Vec<NodeId>,

  /// Spans of the arguments for position calculation.
  argument_spans: Vec<Span>,

  /// The resolved definition of the callee (function or method).
  def_id: DefinitionId,

  /// Whether this is an instance method call (has implicit self).
  is_instance_call: bool,
}

/// Collect all call expressions from the AST that have resolved definitions.
fn collect_calls(
  nodes: &Store<ASTNode>,
  roots: &[NodeId],
  resolved_calls: &HashMap<NodeId, DefinitionId>,
  node_defs: &HashMap<NodeId, DefinitionId>,
  file_id: &FileId,
) -> Vec<CallInfo> {
  let mut calls = Vec::new();
  let mut stack: Vec<NodeId> = roots.to_vec();

  while let Some(node_id) = stack.pop() {
    let node = nodes.get(&node_id);

    match node {
      ASTNode::Expression(ASTExpression::Call(call)) => {
        // Check if call is in current file BEFORE processing, but ALWAYS traverse children
        let in_file = &call.span.file == file_id;

        if in_file {
          let def_id = resolved_calls
            .get(&node_id)
            .or_else(|| node_defs.get(&node_id))
            .cloned();

          if let Some(def_id) = def_id {
            let argument_ids: Vec<_> = call.arguments.to_vec();

            let argument_spans: Vec<_> = call
              .arguments
              .iter()
              .map(|arg_id| nodes.get(arg_id).span().clone())
              .collect();

            // Check if this is an instance method call (callee is member access)
            let is_instance_call =
              matches!(nodes.get(&call.callee), ASTNode::Expression(ASTExpression::MemberAccess(_)));

            calls.push(CallInfo {
              argument_ids,
              argument_spans,
              def_id,
              is_instance_call,
            });
          }
        }

        // ALWAYS traverse children regardless of file
        stack.push(call.callee);
        stack.extend(call.arguments.iter().cloned());
      },

      ASTNode::Expression(expr) => {
        collect_expression_children(expr, &mut stack);
      },
      ASTNode::Statement(stmt) => {
        collect_statement_children(stmt, nodes, &mut stack);
      },
    }
  }

  calls
}

/// Push child node IDs from an expression onto the stack.
fn collect_expression_children(
  expr: &ASTExpression,
  stack: &mut Vec<NodeId>,
) {
  match expr {
    ASTExpression::Assignment(a) => {
      stack.push(a.target);
      stack.push(a.value);
    },
    ASTExpression::Binary(b) => {
      stack.push(b.left);
      stack.push(b.right);
    },
    ASTExpression::Ternary(t) => {
      stack.push(t.condition);
      stack.push(t.then_expr);
      stack.push(t.else_expr);
    },
    ASTExpression::Cast(c) => {
      stack.push(c.expression);
    },
    ASTExpression::Call(c) => {
      stack.push(c.callee);
      stack.extend(c.arguments.iter().cloned());
    },
    ASTExpression::Dereference(d) => {
      stack.push(d.inner);
    },
    ASTExpression::Grouped(g) => {
      stack.push(g.expression);
    },
    ASTExpression::Reference(r) => {
      stack.push(r.inner);
    },
    ASTExpression::Unary(u) => {
      stack.push(u.operand);
    },
    ASTExpression::Literal(_) => {},
    ASTExpression::Variable(_) => {},
    ASTExpression::Vector(v) => {
      stack.extend(v.items.iter().cloned());
    },
    ASTExpression::VectorAccess(va) => {
      stack.push(va.name);
      stack.push(va.index);
    },
    ASTExpression::Path(_) => {},
    ASTExpression::PostfixIncrement { expr, .. } => {
      stack.push(*expr);
    },
    ASTExpression::PostfixDecrement { expr, .. } => {
      stack.push(*expr);
    },
    ASTExpression::MemberAccess(m) => {
      stack.push(m.object);
    },
    ASTExpression::RecordInit(r) => {
      for field in &r.fields {
        stack.push(field.value);
      }
    },
    ASTExpression::BuiltinCall(bc) => {
      for arg_id in &bc.args {
        stack.push(*arg_id);
      }
    },
    ASTExpression::Match(match_expr) => {
      stack.push(match_expr.scrutinee);

      for arm in &match_expr.arms {
        if let Some(guard) = arm.guard {
          stack.push(guard);
        }

        stack.push(arm.body);
      }
    },
  }
}

/// Push child node IDs from a statement onto the stack.
fn collect_statement_children(
  stmt: &ASTStatement,
  _nodes: &Store<ASTNode>,
  stack: &mut Vec<NodeId>,
) {
  match stmt {
    ASTStatement::Variable(v) => {
      if let Some(value) = &v.value {
        stack.push(*value);
      }
    },
    ASTStatement::Expression(e) => {
      collect_expression_children(e, stack);
    },
    ASTStatement::Block(b) => {
      stack.extend(b.statements.iter().cloned());
    },
    ASTStatement::If(i) => {
      stack.push(i.condition);
      stack.push(i.then_block);
      if let Some(else_block) = &i.else_block {
        stack.push(*else_block);
      }
    },
    ASTStatement::While(w) => {
      stack.push(w.condition);
      stack.push(w.body);
    },
    ASTStatement::For(f) => {
      stack.push(f.initializer);
      stack.push(f.condition);
      stack.push(f.increment);
      stack.push(f.body);
    },
    ASTStatement::ForOf(fo) => {
      stack.push(fo.iter);
      stack.push(fo.body);
    },
    ASTStatement::Return(r) => {
      if let Some(expr) = &r.expression {
        stack.push(*expr);
      }
    },
    ASTStatement::Function(f) => {
      if let Some(body) = &f.body {
        stack.push(*body);
      }
    },
    ASTStatement::Namespace(n) => {
      stack.extend(n.items.iter().cloned());
    },
    ASTStatement::Record(r) => {
      for item in &r.items {
        match item {
          ASTRecordItem::Method(method) => {
            stack.push(method.body);
          },
          ASTRecordItem::Field(field) => {
            if let Some(value) = &field.value {
              stack.push(*value);
            }
          },
        }
      }
    },
    ASTStatement::Enum(e) => {
      for item in &e.items {
        match item {
          ASTEnumItem::Method(method) => {
            stack.push(method.body);
          },
          ASTEnumItem::Field(field) => {
            if let Some(value) = &field.value {
              stack.push(*value);
            }
          },
          ASTEnumItem::Variant(_) => {},
        }
      }
    },
    ASTStatement::Constant(c) => {
      if let Some(value) = &c.value {
        stack.push(*value);
      }
    },

    ASTStatement::Import(_)
    | ASTStatement::Export(_)
    | ASTStatement::Extern(_)
    | ASTStatement::TypeAlias(_)
    | ASTStatement::Break(_)
    | ASTStatement::Continue(_)
    | ASTStatement::Comment(_)
    | ASTStatement::Trait(_) => {},
  }
}

/// Generate inlay hints for parameter names at call sites.
pub fn generate_parameter_hints(
  nodes: &Store<ASTNode>,
  roots: &[NodeId],
  resolved_calls: &HashMap<NodeId, DefinitionId>,
  node_defs: &HashMap<NodeId, DefinitionId>,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  file_id: &FileId,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let calls = collect_calls(nodes, roots, resolved_calls, node_defs, file_id);
  let mut hints = Vec::new();

  for call in calls {
    let def = defs.get(&call.def_id);

    // Get parameter definitions and offset based on definition kind
    let (param_ids, param_offset, is_variadic): (&[DefinitionId], usize, bool) = match &def.kind {
      DefinitionKind::Function(func) => (&func.params, 0, func.is_variadic),
      DefinitionKind::Method(method) => {
        // Instance methods have `self` as first param, skip it for hints
        let offset = if !method.is_static && call.is_instance_call {
          1
        } else {
          0
        };
        (&method.params, offset, false)
      },
      _ => continue,
    };

    for (arg_idx, (arg_id, arg_span)) in call.argument_ids.iter().zip(call.argument_spans.iter()).enumerate() {
      // Calculate the corresponding parameter index (accounting for self offset)
      let param_idx = arg_idx + param_offset;

      if param_idx >= param_ids.len() {
        if is_variadic {
          continue;
        }
        break;
      }

      let param_def = defs.get(&param_ids[param_idx]);
      let param_name = match symbol_names.get(&param_def.name) {
        Some(name) => name.as_str(),
        None => continue,
      };

      // Skip if parameter name is not meaningful
      if param_name.len() <= 1 || param_name.starts_with('_') {
        continue;
      }

      // Skip if argument is a variable with the same name as the parameter
      if let ASTNode::Expression(ASTExpression::Variable(var)) = nodes.get(arg_id)
        && let Some(arg_name) = symbol_names.get(&var.name)
        && arg_name == param_name
      {
        continue;
      }

      let (line, col) = line_index.line_col_utf16(arg_span.start);

      hints.push(InlayHint {
        position: Position { line, character: col },
        label: InlayHintLabel::String(format!("{}: ", param_name)),
        kind: Some(InlayHintKind::PARAMETER),
        text_edits: None,
        tooltip: None,
        padding_left: None,
        padding_right: None,
        data: None,
      });
    }
  }

  hints
}
