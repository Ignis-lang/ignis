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
use ignis_type::symbol::SymbolId;
use ignis_type::Store;
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position};

use crate::convert::LineIndex;

/// A call expression found in the AST with its resolved definition.
struct CallInfo {
  /// Spans of the arguments for position calculation.
  argument_spans: Vec<ignis_type::span::Span>,

  /// The resolved definition of the callee (function or method).
  def_id: DefinitionId,
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
  let mut stack: Vec<NodeId> = roots.iter().cloned().collect();

  while let Some(node_id) = stack.pop() {
    let node = nodes.get(&node_id);

    match node {
      ASTNode::Expression(ASTExpression::Call(call)) => {
        // Only process calls in the current file
        if &call.span.file != file_id {
          continue;
        }

        // Get the resolved definition (handles overloads)
        let def_id = resolved_calls
          .get(&node_id)
          .or_else(|| node_defs.get(&node_id))
          .cloned();

        if let Some(def_id) = def_id {
          // Collect argument spans
          let argument_spans: Vec<_> = call
            .arguments
            .iter()
            .map(|arg_id| nodes.get(arg_id).span().clone())
            .collect();

          calls.push(CallInfo { argument_spans, def_id });
        }

        // Continue traversing into callee and arguments
        stack.push(call.callee.clone());
        stack.extend(call.arguments.iter().cloned());
      },

      // Traverse into child nodes
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
      stack.push(a.target.clone());
      stack.push(a.value.clone());
    },
    ASTExpression::Binary(b) => {
      stack.push(b.left.clone());
      stack.push(b.right.clone());
    },
    ASTExpression::Ternary(t) => {
      stack.push(t.condition.clone());
      stack.push(t.then_expr.clone());
      stack.push(t.else_expr.clone());
    },
    ASTExpression::Cast(c) => {
      stack.push(c.expression.clone());
    },
    ASTExpression::Call(c) => {
      stack.push(c.callee.clone());
      stack.extend(c.arguments.iter().cloned());
    },
    ASTExpression::Dereference(d) => {
      stack.push(d.inner.clone());
    },
    ASTExpression::Grouped(g) => {
      stack.push(g.expression.clone());
    },
    ASTExpression::Reference(r) => {
      stack.push(r.inner.clone());
    },
    ASTExpression::Unary(u) => {
      stack.push(u.operand.clone());
    },
    ASTExpression::Literal(_) => {},
    ASTExpression::Variable(_) => {},
    ASTExpression::Vector(v) => {
      stack.extend(v.items.iter().cloned());
    },
    ASTExpression::VectorAccess(va) => {
      stack.push(va.name.clone());
      stack.push(va.index.clone());
    },
    ASTExpression::Path(_) => {},
    ASTExpression::PostfixIncrement { expr, .. } => {
      stack.push(expr.clone());
    },
    ASTExpression::PostfixDecrement { expr, .. } => {
      stack.push(expr.clone());
    },
    ASTExpression::MemberAccess(m) => {
      stack.push(m.object.clone());
    },
    ASTExpression::RecordInit(r) => {
      for field in &r.fields {
        stack.push(field.value.clone());
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
        stack.push(value.clone());
      }
    },
    ASTStatement::Expression(e) => {
      collect_expression_children(e, stack);
    },
    ASTStatement::Block(b) => {
      stack.extend(b.statements.iter().cloned());
    },
    ASTStatement::If(i) => {
      stack.push(i.condition.clone());
      stack.push(i.then_block.clone());
      if let Some(else_block) = &i.else_block {
        stack.push(else_block.clone());
      }
    },
    ASTStatement::While(w) => {
      stack.push(w.condition.clone());
      stack.push(w.body.clone());
    },
    ASTStatement::For(f) => {
      stack.push(f.initializer.clone());
      stack.push(f.condition.clone());
      stack.push(f.increment.clone());
      stack.push(f.body.clone());
    },
    ASTStatement::ForOf(fo) => {
      stack.push(fo.iter.clone());
      stack.push(fo.body.clone());
    },
    ASTStatement::Return(r) => {
      if let Some(expr) = &r.expression {
        stack.push(expr.clone());
      }
    },
    ASTStatement::Function(f) => {
      if let Some(body) = &f.body {
        stack.push(body.clone());
      }
    },
    ASTStatement::Namespace(n) => {
      stack.extend(n.items.iter().cloned());
    },
    ASTStatement::Record(r) => {
      for item in &r.items {
        match item {
          ASTRecordItem::Method(method) => {
            stack.push(method.body.clone());
          },
          ASTRecordItem::Field(field) => {
            if let Some(value) = &field.value {
              stack.push(value.clone());
            }
          },
        }
      }
    },
    ASTStatement::Enum(e) => {
      for item in &e.items {
        match item {
          ASTEnumItem::Method(method) => {
            stack.push(method.body.clone());
          },
          ASTEnumItem::Field(field) => {
            if let Some(value) = &field.value {
              stack.push(value.clone());
            }
          },
          ASTEnumItem::Variant(_) => {},
        }
      }
    },
    ASTStatement::Constant(c) => {
      if let Some(value) = &c.value {
        stack.push(value.clone());
      }
    },

    // Statements without child expressions to traverse
    ASTStatement::Import(_)
    | ASTStatement::Export(_)
    | ASTStatement::Extern(_)
    | ASTStatement::TypeAlias(_)
    | ASTStatement::Break(_)
    | ASTStatement::Continue(_)
    | ASTStatement::Comment(_) => {},
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

    // Get parameter definitions based on definition kind
    let param_ids: &[DefinitionId] = match &def.kind {
      DefinitionKind::Function(func) => &func.params,
      DefinitionKind::Method(method) => &method.params,
      _ => continue,
    };

    // Check for variadic (extern functions)
    let is_variadic = match &def.kind {
      DefinitionKind::Function(func) => func.is_variadic,
      _ => false,
    };

    // Generate hints for each argument that has a corresponding parameter
    for (i, arg_span) in call.argument_spans.iter().enumerate() {
      // Skip if this is a variadic argument (beyond named params)
      if i >= param_ids.len() {
        if is_variadic {
          // Variadic args don't get hints
          continue;
        }
        // More args than params and not variadic - likely an error, skip
        break;
      }

      let param_def = defs.get(&param_ids[i]);
      let param_name = symbol_names
        .get(&param_def.name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());

      // Skip if parameter name is not meaningful (single char or underscore)
      if param_name.len() <= 1 || param_name.starts_with('_') {
        continue;
      }

      // Position hint at the start of the argument
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
