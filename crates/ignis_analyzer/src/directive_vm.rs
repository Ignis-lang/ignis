use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use ignis_ast::{ASTNode, NodeId, expressions::ASTExpression, statements::ASTStatement};
use ignis_diagnostics::diagnostic_report::{Diagnostic, Severity};
use ignis_type::Store as ASTStore;
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore};
use ignis_type::namespace::NamespaceStore;
use ignis_type::span::Span;
use ignis_type::symbol::{SymbolId, SymbolTable};
use ignis_type::types::{Type, TypeStore};
use ignis_type::value::IgnisLiteralValue;

use crate::directive_scheduler::{DirectiveExecutionError, DirectiveScheduleEntry};

const DEFAULT_VM_STEP_LIMIT: usize = 256;
const DEFAULT_VM_CALL_DEPTH_LIMIT: usize = 8;
const VM_DIAGNOSTIC_ERROR_CODE: &str = "A0198";

#[derive(Debug, Clone)]
pub enum DirectiveValue {
  Void,
  Bool(bool),
  String(String),
  Context,
  ItemReference { target_span: Span },
  Unsupported,
}

#[derive(Clone)]
pub struct DirectiveVm {
  ast: ASTStore<ASTNode>,
  types: TypeStore,
  defs: DefinitionStore,
  namespaces: NamespaceStore,
  symbols: Rc<RefCell<SymbolTable>>,
  resolved_calls: HashMap<NodeId, DefinitionId>,
  function_entries: HashMap<DefinitionId, DirectiveFunctionEntry>,
  step_limit: usize,
  call_depth_limit: usize,
}

#[derive(Debug, Clone)]
struct DirectiveFunctionEntry {
  body: NodeId,
  parameter_names: Vec<SymbolId>,
}

#[derive(Debug, Clone)]
struct ExecutionState {
  remaining_steps: usize,
  remaining_call_depth: usize,
  locals: HashMap<SymbolId, DirectiveValue>,
  diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ControlFlow {
  Continue,
  Return,
}

impl DirectiveVm {
  pub fn new(
    ast: &ASTStore<ASTNode>,
    types: &TypeStore,
    defs: &DefinitionStore,
    namespaces: &NamespaceStore,
    symbols: Rc<RefCell<SymbolTable>>,
    node_defs: &HashMap<NodeId, DefinitionId>,
    resolved_calls: &HashMap<NodeId, DefinitionId>,
  ) -> Self {
    let function_entries = ast
      .iter()
      .filter_map(|(node_id, node)| match node {
        ASTNode::Statement(ASTStatement::Function(function)) => node_defs.get(&node_id).and_then(|definition_id| {
          function.body.map(|body| {
            (
              *definition_id,
              DirectiveFunctionEntry {
                body,
                parameter_names: function
                  .signature
                  .parameters
                  .iter()
                  .map(|parameter| parameter.name)
                  .collect(),
              },
            )
          })
        }),
        _ => None,
      })
      .collect();

    Self {
      ast: ast.clone(),
      types: types.clone(),
      defs: defs.clone(),
      namespaces: namespaces.clone(),
      symbols,
      resolved_calls: resolved_calls.clone(),
      function_entries,
      step_limit: DEFAULT_VM_STEP_LIMIT,
      call_depth_limit: DEFAULT_VM_CALL_DEPTH_LIMIT,
    }
  }

  pub fn execute_entry(
    &self,
    entry: &DirectiveScheduleEntry,
  ) -> Result<Vec<Diagnostic>, DirectiveExecutionError> {
    let Some(function_entry) = self.function_entries.get(&entry.function_def_id) else {
      return Ok(Vec::new());
    };

    let mut state = ExecutionState {
      remaining_steps: self.step_limit,
      remaining_call_depth: self.call_depth_limit,
      locals: HashMap::new(),
      diagnostics: Vec::new(),
    };

    if let Some(context_name) = function_entry.parameter_names.first() {
      state.locals.insert(*context_name, DirectiveValue::Context);
    }

    if let Some(target_name) = function_entry.parameter_names.get(1) {
      state.locals.insert(
        *target_name,
        DirectiveValue::ItemReference {
          target_span: self.ast.get(&entry.target_node).span().clone(),
        },
      );
    }

    let _ = self.execute_statement(function_entry.body, entry, &mut state)?;

    Ok(state.diagnostics)
  }

  pub fn target_span(
    &self,
    target_node: NodeId,
  ) -> Span {
    self.ast.get(&target_node).span().clone()
  }

  fn execute_statement(
    &self,
    statement_id: NodeId,
    entry: &DirectiveScheduleEntry,
    state: &mut ExecutionState,
  ) -> Result<ControlFlow, DirectiveExecutionError> {
    self.consume_step(statement_id, entry, state)?;

    let node = self.ast.get(&statement_id);
    let ASTNode::Statement(statement) = node else {
      return Ok(ControlFlow::Continue);
    };

    match statement {
      ASTStatement::Block(block) => {
        for nested in &block.statements {
          if self.execute_statement(*nested, entry, state)? == ControlFlow::Return {
            return Ok(ControlFlow::Return);
          }
        }

        Ok(ControlFlow::Continue)
      },
      ASTStatement::Expression(expression) => {
        let _ = self.evaluate_expression(statement_id, expression, entry, state)?;
        Ok(ControlFlow::Continue)
      },
      ASTStatement::If(if_statement) => {
        let condition = self.evaluate_node(if_statement.condition, entry, state)?;

        if matches!(condition, DirectiveValue::Bool(true)) {
          self.execute_statement(if_statement.then_block, entry, state)
        } else if let Some(else_block) = if_statement.else_block {
          self.execute_statement(else_block, entry, state)
        } else {
          Ok(ControlFlow::Continue)
        }
      },
      ASTStatement::Return(return_statement) => {
        if let Some(expression) = return_statement.expression {
          let _ = self.evaluate_node(expression, entry, state)?;
        }

        Ok(ControlFlow::Return)
      },
      ASTStatement::Variable(variable) => {
        let value = variable
          .value
          .map(|expression| self.evaluate_node(expression, entry, state))
          .transpose()?
          .unwrap_or(DirectiveValue::Void);

        state.locals.insert(variable.name, value);

        Ok(ControlFlow::Continue)
      },
      _ => Ok(ControlFlow::Continue),
    }
  }

  fn evaluate_node(
    &self,
    node_id: NodeId,
    entry: &DirectiveScheduleEntry,
    state: &mut ExecutionState,
  ) -> Result<DirectiveValue, DirectiveExecutionError> {
    self.consume_step(node_id, entry, state)?;

    match self.ast.get(&node_id) {
      ASTNode::Expression(expression) => self.evaluate_expression(node_id, expression, entry, state),
      ASTNode::Statement(_) => {
        let _ = self.execute_statement(node_id, entry, state)?;
        Ok(DirectiveValue::Void)
      },
    }
  }

  fn evaluate_expression(
    &self,
    node_id: NodeId,
    expression: &ASTExpression,
    entry: &DirectiveScheduleEntry,
    state: &mut ExecutionState,
  ) -> Result<DirectiveValue, DirectiveExecutionError> {
    match expression {
      ASTExpression::Literal(literal) => Ok(match &literal.value {
        IgnisLiteralValue::Boolean(value) => DirectiveValue::Bool(*value),
        IgnisLiteralValue::String(value) => DirectiveValue::String(value.clone()),
        _ => DirectiveValue::Unsupported,
      }),
      ASTExpression::Variable(variable) => Ok(
        state
          .locals
          .get(&variable.name)
          .cloned()
          .unwrap_or(DirectiveValue::Unsupported),
      ),
      ASTExpression::Path(path) => Ok(
        path
          .segments
          .last()
          .and_then(|segment| state.locals.get(&segment.name).cloned())
          .unwrap_or(DirectiveValue::Unsupported),
      ),
      ASTExpression::Grouped(grouped) => self.evaluate_node(grouped.expression, entry, state),
      ASTExpression::Call(call) => self.execute_call(node_id, call.arguments.as_slice(), entry, state),
      _ => Ok(DirectiveValue::Unsupported),
    }
  }

  fn execute_call(
    &self,
    node_id: NodeId,
    arguments: &[NodeId],
    entry: &DirectiveScheduleEntry,
    state: &mut ExecutionState,
  ) -> Result<DirectiveValue, DirectiveExecutionError> {
    if state.remaining_call_depth == 0 {
      return Err(DirectiveExecutionError::CallDepthExceeded {
        span: self.ast.get(&node_id).span().clone(),
        provenance: entry.provenance.clone(),
        directive_use_span: entry.span.clone(),
        target_span: self.target_span(entry.target_node),
      });
    }

    let Some(definition_id) = self.resolved_calls.get(&node_id).copied() else {
      return Ok(DirectiveValue::Unsupported);
    };

    let definition = self.defs.get(&definition_id);
    let operation_name = self.symbol_name(definition.name);

    if self.is_root_compile_namespace_function(definition_id) {
      if !self.is_compile_namespace_function(definition_id, entry.function_def_id) {
        return Err(DirectiveExecutionError::UnsupportedGeneration {
          operation: operation_name,
          span: self.ast.get(&node_id).span().clone(),
          provenance: entry.provenance.clone(),
          directive_use_span: entry.span.clone(),
          target_span: self.target_span(entry.target_node),
        });
      }

      let previous_call_depth = state.remaining_call_depth;
      state.remaining_call_depth -= 1;

      let result = match operation_name.as_str() {
        "error" => {
          self.emit_host_diagnostic(node_id, Severity::Error, arguments, entry, state)?;
          Ok(DirectiveValue::Void)
        },
        "warning" => {
          self.emit_host_diagnostic(node_id, Severity::Warning, arguments, entry, state)?;
          Ok(DirectiveValue::Void)
        },
        "note" => {
          self.emit_host_diagnostic(node_id, Severity::Info, arguments, entry, state)?;
          Ok(DirectiveValue::Void)
        },
        _ => Err(DirectiveExecutionError::UnsupportedGeneration {
          operation: operation_name,
          span: self.ast.get(&node_id).span().clone(),
          provenance: entry.provenance.clone(),
          directive_use_span: entry.span.clone(),
          target_span: self.target_span(entry.target_node),
        }),
      };

      state.remaining_call_depth = previous_call_depth;

      return result;
    }

    Ok(DirectiveValue::Unsupported)
  }

  fn emit_host_diagnostic(
    &self,
    node_id: NodeId,
    severity: Severity,
    arguments: &[NodeId],
    entry: &DirectiveScheduleEntry,
    state: &mut ExecutionState,
  ) -> Result<(), DirectiveExecutionError> {
    if arguments.len() != 3 {
      return Ok(());
    }

    let context = self.evaluate_node(arguments[0], entry, state)?;
    let target = self.evaluate_node(arguments[1], entry, state)?;
    let message = self.evaluate_node(arguments[2], entry, state)?;

    if !matches!(context, DirectiveValue::Context) {
      return Ok(());
    }

    let DirectiveValue::ItemReference { target_span, .. } = target else {
      return Ok(());
    };

    let DirectiveValue::String(message) = message else {
      return Ok(());
    };

    let diagnostic = Diagnostic::new(
      severity,
      message,
      VM_DIAGNOSTIC_ERROR_CODE.to_string(),
      self.ast.get(&node_id).span().clone(),
    )
    .with_label(entry.provenance.origin_attr_span.clone(), "directive declaration".to_string())
    .with_label(entry.span.clone(), "directive use".to_string())
    .with_label(target_span, "target item".to_string());

    state.diagnostics.push(diagnostic);

    Ok(())
  }

  fn consume_step(
    &self,
    node_id: NodeId,
    entry: &DirectiveScheduleEntry,
    state: &mut ExecutionState,
  ) -> Result<(), DirectiveExecutionError> {
    if state.remaining_steps == 0 {
      return Err(DirectiveExecutionError::StepLimitExceeded {
        span: self.ast.get(&node_id).span().clone(),
        provenance: entry.provenance.clone(),
        directive_use_span: entry.span.clone(),
        target_span: self.target_span(entry.target_node),
      });
    }

    state.remaining_steps -= 1;

    Ok(())
  }

  fn is_compile_namespace_function(
    &self,
    definition_id: DefinitionId,
    caller_function_id: DefinitionId,
  ) -> bool {
    if !self.is_root_compile_namespace_function(definition_id) {
      return false;
    }

    if !self.directive_uses_external_compile_boundary(caller_function_id) {
      return true;
    }

    self.defs.get(&definition_id).owner_module != self.defs.get(&caller_function_id).owner_module
  }

  fn is_root_compile_namespace_function(
    &self,
    definition_id: DefinitionId,
  ) -> bool {
    let definition = self.defs.get(&definition_id);

    let Some(namespace_id) = definition.owner_namespace else {
      return false;
    };

    let namespace_path = self.namespaces.full_path(namespace_id);

    namespace_path.len() == 1 && self.symbol_name(namespace_path[0]) == "Compile"
  }

  fn directive_uses_external_compile_boundary(
    &self,
    function_def_id: DefinitionId,
  ) -> bool {
    let definition = self.defs.get(&function_def_id);
    let DefinitionKind::Function(function) = &definition.kind else {
      return false;
    };

    function.params.iter().take(2).any(|param_id| {
      let type_id = *self.defs.type_of(param_id);

      match self.types.get(&type_id) {
        Type::Record(type_def_id) => self.defs.get(type_def_id).owner_module != definition.owner_module,
        _ => false,
      }
    })
  }

  fn symbol_name(
    &self,
    symbol_id: SymbolId,
  ) -> String {
    self.symbols.borrow().get(&symbol_id).to_string()
  }
}

#[cfg(test)]
mod tests {
  use std::cell::RefCell;
  use std::rc::Rc;

  use ignis_parser::{IgnisLexer, IgnisParser};
  use ignis_type::file::SourceMap;
  use ignis_type::symbol::SymbolTable;

  use crate::Analyzer;

  use super::*;

  #[test]
  fn directive_vm_emits_supported_compile_diagnostics() {
    let src = r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function error(context: Context, target: ItemReference, message: str): void {
          return;
        }
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function validateRecord(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::error(context, target, "record failed validation");
      }

      @validateRecord
      record User {
        value: i32;
      }
    "#;

    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file("test.ign", src.to_string());
    let mut lexer = IgnisLexer::new(file_id, source_map.get(&file_id).text.as_str());
    lexer.scan_tokens();

    let symbols = Rc::new(RefCell::new(SymbolTable::new()));
    let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
    let (nodes, roots) = parser.parse().expect("parse should succeed");
    let mut analyzer = Analyzer::new(&nodes, symbols.clone(), ignis_type::module::ModuleId::new(0));

    crate::phases::run_semantic_passes(&mut analyzer, &roots);

    let directive_use = analyzer.directive_registry.uses[0].clone();
    let directive_def = analyzer.directive_registry.defs[0].clone();
    let vm = DirectiveVm::new(
      &nodes,
      &analyzer.types,
      &analyzer.defs,
      &analyzer.namespaces,
      symbols,
      &analyzer.node_defs,
      &analyzer.resolved_calls,
    );

    let diagnostics = vm
      .execute_entry(&DirectiveScheduleEntry {
        source_order: 0,
        directive: directive_use.directive,
        function_def_id: directive_def.function_def_id,
        target_node: directive_use.target_node,
        effect: directive_def.effect,
        capabilities: directive_def.capabilities,
        span: directive_use.span,
        provenance: directive_use.provenance,
      })
      .expect("vm execution should succeed");

    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].error_code, VM_DIAGNOSTIC_ERROR_CODE);
    assert!(diagnostics[0].message.contains("record failed validation"));
  }
}
