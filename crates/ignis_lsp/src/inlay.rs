//! Inlay hints implementation.
//!
//! Provides:
//! - Parameter name hints at call sites
//! - Type hints for match scrutinees
//! - Type hints for variables with inferred types

use std::collections::HashMap;

use ignis_ast::expressions::{ASTAccessOp, ASTExpression};
use ignis_ast::statements::{ASTEnumItem, ASTRecordItem, ASTStatement};
use ignis_ast::type_::IgnisTypeSyntax;
use ignis_ast::{ASTNode, NodeId};
use ignis_driver::PerFileAnalysis;
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore};
use ignis_type::symbol::SymbolId;
use ignis_type::types::{Type, TypeStore};
use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position, Range};

use crate::convert::LineIndex;

struct CallInfo {
  argument_ids: Vec<NodeId>,
  argument_spans: Vec<ignis_type::span::Span>,
  def_id: DefinitionId,
  is_instance_call: bool,
}

struct MatchScrutineeInfo {
  node_id: NodeId,
  span: ignis_type::span::Span,
}

struct InferredVariableInfo {
  def_id: DefinitionId,
}

fn collect_calls(file: &PerFileAnalysis) -> Vec<CallInfo> {
  let mut calls = Vec::new();
  let mut stack: Vec<NodeId> = file.roots.clone();

  while let Some(node_id) = stack.pop() {
    let node = file.nodes.get(&node_id);

    match &node {
      ASTNode::Expression(ASTExpression::Call(call)) => {
        let def_id = file
          .resolved_calls
          .get(&node_id)
          .or_else(|| file.node_defs.get(&node_id))
          .cloned();

        if let Some(def_id) = def_id {
          let argument_ids = call.arguments.to_vec();

          let argument_spans = call
            .arguments
            .iter()
            .map(|arg_id| file.nodes.get(arg_id).span().clone())
            .collect();

          let is_instance_call = matches!(
            file.nodes.get(&call.callee),
            ASTNode::Expression(ASTExpression::MemberAccess(member_access)) if member_access.op == ASTAccessOp::Dot
          );

          calls.push(CallInfo {
            argument_ids,
            argument_spans,
            def_id,
            is_instance_call,
          });
        }

        stack.push(call.callee);
        stack.extend(call.arguments.iter().cloned());
      },
      ASTNode::Expression(expr) => {
        collect_expression_children(expr, &mut stack);
      },
      ASTNode::Statement(stmt) => {
        collect_statement_children(stmt, &mut stack);
      },
    }
  }

  calls
}

fn collect_match_scrutinees(file: &PerFileAnalysis) -> Vec<MatchScrutineeInfo> {
  let mut scrutinees = Vec::new();
  let mut stack: Vec<NodeId> = file.roots.clone();

  while let Some(node_id) = stack.pop() {
    let node = file.nodes.get(&node_id);

    match &node {
      ASTNode::Expression(ASTExpression::Match(match_expr)) => {
        let scrutinee_span = file.nodes.get(&match_expr.scrutinee).span().clone();
        scrutinees.push(MatchScrutineeInfo {
          node_id: match_expr.scrutinee,
          span: scrutinee_span,
        });

        stack.push(match_expr.scrutinee);
        for arm in &match_expr.arms {
          if let Some(guard) = arm.guard {
            stack.push(guard);
          }
          stack.push(arm.body);
        }
      },
      ASTNode::Expression(expr) => {
        collect_expression_children(expr, &mut stack);
      },
      ASTNode::Statement(stmt) => {
        collect_statement_children(stmt, &mut stack);
      },
    }
  }

  scrutinees
}

fn collect_inferred_variables(file: &PerFileAnalysis) -> Vec<InferredVariableInfo> {
  let mut results = Vec::new();
  let mut stack: Vec<NodeId> = file.roots.clone();

  while let Some(node_id) = stack.pop() {
    let node = file.nodes.get(&node_id);

    match &node {
      ASTNode::Statement(ASTStatement::Variable(var)) => {
        if matches!(var.type_, IgnisTypeSyntax::Implicit)
          && let Some(def_id) = file.node_defs.get(&node_id)
        {
          results.push(InferredVariableInfo { def_id: *def_id });
        }

        if let Some(value) = &var.value {
          stack.push(*value);
        }
      },
      ASTNode::Expression(expr) => {
        collect_expression_children(expr, &mut stack);
      },
      ASTNode::Statement(stmt) => {
        collect_statement_children(stmt, &mut stack);
      },
    }
  }

  results
}

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
    ASTExpression::LetCondition(let_condition) => {
      stack.push(let_condition.value);
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
    ASTExpression::Lambda(lambda) => match &lambda.body {
      ignis_ast::expressions::lambda::LambdaBody::Expression(id) => stack.push(*id),
      ignis_ast::expressions::lambda::LambdaBody::Block(id) => stack.push(*id),
    },
    ASTExpression::CaptureOverride(co) => {
      stack.push(co.inner);
    },
    ASTExpression::Pipe { lhs, rhs, .. } => {
      stack.push(*lhs);
      stack.push(*rhs);
    },
  }
}

fn collect_statement_children(
  stmt: &ASTStatement,
  stack: &mut Vec<NodeId>,
) {
  match stmt {
    ASTStatement::Variable(v) => {
      if let Some(value) = &v.value {
        stack.push(*value);
      }
    },
    ASTStatement::LetElse(let_else) => {
      stack.push(let_else.value);
      stack.push(let_else.else_block);
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

pub fn generate_hints(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbol_names: &HashMap<SymbolId, String>,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let mut hints = generate_parameter_hints(file, defs, symbol_names, line_index);
  hints.extend(generate_match_scrutinee_type_hints(file, defs, types, symbol_names, line_index));
  hints.extend(generate_inferred_variable_type_hints(
    file,
    defs,
    types,
    symbol_names,
    line_index,
  ));
  hints
}

fn generate_parameter_hints(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let calls = collect_calls(file);
  let mut hints = Vec::new();

  for call in calls {
    let def = defs.get(&call.def_id);

    let (param_ids, param_offset, is_variadic): (&[DefinitionId], usize, bool) = match &def.kind {
      DefinitionKind::Function(func) => (&func.params, 0, func.is_variadic),
      DefinitionKind::Method(method) => {
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

      if param_name.len() <= 1 || param_name.starts_with('_') {
        continue;
      }

      if let ASTNode::Expression(ASTExpression::Variable(var)) = file.nodes.get(arg_id)
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

fn generate_match_scrutinee_type_hints(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbol_names: &HashMap<SymbolId, String>,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let scrutinees = collect_match_scrutinees(file);
  let mut hints = Vec::new();

  for scrutinee in scrutinees {
    let Some(type_id) = file.node_types.get(&scrutinee.node_id) else {
      continue;
    };

    let ty = types.get(type_id);
    if matches!(ty, Type::Error | Type::Infer) {
      continue;
    }

    let type_label = crate::type_format::format_type(types, defs, symbol_names, type_id);
    let (line, col) = line_index.line_col_utf16(scrutinee.span.end);

    hints.push(InlayHint {
      position: Position { line, character: col },
      label: InlayHintLabel::String(format!(": {}", type_label)),
      kind: Some(InlayHintKind::TYPE),
      text_edits: None,
      tooltip: None,
      padding_left: Some(true),
      padding_right: None,
      data: None,
    });
  }

  hints
}

fn generate_inferred_variable_type_hints(
  file: &PerFileAnalysis,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbol_names: &HashMap<SymbolId, String>,
  line_index: &LineIndex,
) -> Vec<InlayHint> {
  let variables = collect_inferred_variables(file);
  let mut hints = Vec::new();

  for var_info in variables {
    let def = defs.get(&var_info.def_id);

    let type_id = match &def.kind {
      DefinitionKind::Variable(v) => &v.type_id,
      _ => continue,
    };

    let ty = types.get(type_id);
    if matches!(ty, Type::Error | Type::Infer | Type::InferVar(_) | Type::Unknown) {
      continue;
    }

    let type_label = crate::type_format::format_type(types, defs, symbol_names, type_id);
    let (line, col) = line_index.line_col_utf16(def.name_span.end);

    hints.push(InlayHint {
      position: Position { line, character: col },
      label: InlayHintLabel::String(format!(": {}", type_label)),
      kind: Some(InlayHintKind::TYPE),
      text_edits: None,
      tooltip: None,
      padding_left: None,
      padding_right: None,
      data: None,
    });
  }

  hints
}

pub fn filter_hints_by_range(
  hints: &[InlayHint],
  range: &Range,
) -> Vec<InlayHint> {
  hints
    .iter()
    .filter(|hint| {
      let pos = hint.position;
      let starts_after_or_at =
        pos.line > range.start.line || (pos.line == range.start.line && pos.character >= range.start.character);
      let ends_before_or_at =
        pos.line < range.end.line || (pos.line == range.end.line && pos.character < range.end.character);
      starts_after_or_at && ends_before_or_at
    })
    .cloned()
    .collect()
}
