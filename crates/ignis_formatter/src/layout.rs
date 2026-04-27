use std::{cell::RefCell, rc::Rc};

use ignis_ast::{
  ASTNode, NodeId,
  attribute::ASTAttribute,
  metadata::ASTMetadata,
  expressions::{ASTAccessOp, ASTExpression},
  expressions::lambda::LambdaBody,
  expressions::assignment::ASTAssignmentOperator,
  expressions::binary::ASTBinaryOperator,
  expressions::unary::UnaryOperator,
  generics::ASTGenericParams,
  pattern::ASTPattern,
  statements::{ASTFunction, ASTImport, ASTStatement, block::ASTBlock, function::ASTParameter},
  type_::IgnisTypeSyntax,
};
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::{Store, file::SourceMap, span::Span, symbol::SymbolTable};

use crate::doc::{format_inline_or_multiline, render_doc};
use crate::model::FormatItem;
use crate::{CodeRegion, CommentBlock, CommentPlacement, DirectiveBlock, FormatError, FormatFile, FormatterConfig};

pub(crate) fn layout_file(
  file: &FormatFile,
  config: &FormatterConfig,
) -> Result<FormatFile, FormatError> {
  Ok(FormatFile {
    items: layout_items(&file.items, config)?,
  })
}

fn layout_items(
  items: &[FormatItem],
  config: &FormatterConfig,
) -> Result<Vec<FormatItem>, FormatError> {
  let mut formatted = Vec::with_capacity(items.len());

  for item in items {
    match item {
      FormatItem::Code(region) => formatted.push(FormatItem::Code(CodeRegion {
        raw: layout_code_chunk(&region.raw, config)?,
        leading: region.leading.clone(),
        trailing: region.trailing.clone(),
        leading_after_attr_prefix: region.leading_after_attr_prefix,
      })),
      FormatItem::DetachedComment(comment) => formatted.push(FormatItem::DetachedComment(comment.clone())),
      FormatItem::Directive(block) => {
        formatted.push(FormatItem::Directive(DirectiveBlock {
          leading: block.leading.clone(),
          header: block.header.clone(),
          then_items: layout_items(&block.then_items, config)?,
          else_header: block.else_header.clone(),
          else_items: layout_items(&block.else_items, config)?,
          trailing: block.trailing.clone(),
          braced: block.braced,
        }));
      },
    }
  }

  Ok(formatted)
}

fn layout_code_chunk(
  raw: &str,
  config: &FormatterConfig,
) -> Result<String, FormatError> {
  let trimmed = raw.trim();
  if trimmed.is_empty() {
    return Ok(String::new());
  }

  layout_program_chunk(trimmed, config).map_err(LayoutFailure::into_error)
}

fn layout_program_chunk(
  source: &str,
  config: &FormatterConfig,
) -> Result<String, LayoutFailure> {
  let mut source_map = SourceMap::new();
  let file_id = source_map.add_file("formatter_chunk.ign", source.to_string());

  let mut lexer = IgnisLexer::new(file_id, source);
  lexer.scan_tokens();

  if !lexer.diagnostics.is_empty() {
    return Err(LayoutFailure::Fatal(FormatError::Lex {
      diagnostics: lexer.diagnostics.iter().map(ToString::to_string).collect(),
    }));
  }

  let symbols = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbols);
  let (nodes, roots) = parser.parse_formatter_program().map_err(|diagnostics| {
    LayoutFailure::Fatal(FormatError::Parse {
      diagnostics: diagnostics.iter().map(ToString::to_string).collect(),
    })
  })?;

  AstChunkFormatter::new(source, nodes, config).format_program(&roots)
}

fn collapse_spaces(raw: &str) -> String {
  raw.split_whitespace().collect::<Vec<_>>().join(" ")
}

enum LayoutFailure {
  Fatal(FormatError),
}

impl LayoutFailure {
  fn unsupported(message: impl Into<String>) -> Self {
    Self::Fatal(FormatError::Unsupported {
      message: message.into(),
    })
  }

  fn into_error(self) -> FormatError {
    match self {
      Self::Fatal(error) => error,
    }
  }
}

struct AstChunkFormatter<'a> {
  source: &'a str,
  nodes: Store<ASTNode>,
  config: &'a FormatterConfig,
}

struct OwnedSpannedItem<T> {
  item: T,
  leading: Vec<CommentBlock>,
  trailing: Vec<CommentBlock>,
}

enum PipeStep {
  Head(NodeId),
  Call(NodeId),
}

impl PipeStep {
  fn id(&self) -> NodeId {
    match self {
      PipeStep::Head(id) | PipeStep::Call(id) => *id,
    }
  }
}

struct ImportGroup {
  items: Vec<NodeId>,
  has_preceding_import_group: bool,
}

#[derive(Eq, PartialEq)]
struct ImportSortKey {
  is_wildcard: bool,
  is_std: bool,
  path: String,
}

impl Ord for ImportSortKey {
  fn cmp(
    &self,
    other: &Self,
  ) -> std::cmp::Ordering {
    self.is_wildcard
      .cmp(&other.is_wildcard)
      .then_with(|| other.is_std.cmp(&self.is_std))
      .then_with(|| self.path.cmp(&other.path))
  }
}

impl PartialOrd for ImportSortKey {
  fn partial_cmp(
    &self,
    other: &Self,
  ) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a> AstChunkFormatter<'a> {
  fn new(
    source: &'a str,
    nodes: Store<ASTNode>,
    config: &'a FormatterConfig,
  ) -> Self {
    Self { source, nodes, config }
  }

  fn format_program(
    &self,
    roots: &[NodeId],
  ) -> Result<String, LayoutFailure> {
    if self.config.sort_imports {
      return self.format_program_with_sorted_imports(roots);
    }

    self.format_program_preserving_order(roots)
  }

  fn format_program_preserving_order(
    &self,
    roots: &[NodeId],
  ) -> Result<String, LayoutFailure> {
    let mut formatted = String::new();
    let mut previous_was_import = false;
    let mut previous_end = None;

    for (index, root) in roots.iter().enumerate() {
      let statement = self.format_statement_node(*root, 0)?;
      let current_is_import = self.is_import_node(root);
      let span = self.nodes.get(root).span().clone();

      if index > 0 {
        if let Some(prev_end) = previous_end {
          self.preserve_single_blank_line_for_whitespace_gap(&mut formatted, prev_end, span.start.0 as usize);
        }

        if !formatted.ends_with('\n') {
          formatted.push('\n');
        }

        if previous_was_import && !current_is_import {
          if !formatted.ends_with("\n\n") {
            formatted.push('\n');
          }
        }
      }

      formatted.push_str(&statement);
      previous_was_import = current_is_import;
      previous_end = Some(self.node_true_end(root));
    }

    Ok(formatted)
  }

  fn format_program_with_sorted_imports(
    &self,
    roots: &[NodeId],
  ) -> Result<String, LayoutFailure> {
    let mut formatted = String::new();
    let mut previous_end = None;

    let groups = self.collect_import_groups(roots);

    for group in &groups {
      for (index, root) in group.items.iter().enumerate() {
        let statement = self.format_statement_node(*root, 0)?;
        let span = self.nodes.get(root).span().clone();

        if let Some(prev_end) = previous_end {
          self.preserve_single_blank_line_for_whitespace_gap(&mut formatted, prev_end, span.start.0 as usize);
        }

        if !formatted.is_empty() && !formatted.ends_with('\n') {
          formatted.push('\n');
        }

        let current_is_import = self.is_import_node(root);
        if index == 0 && group.has_preceding_import_group {
          if !formatted.ends_with("\n\n") {
            formatted.push('\n');
          }
        } else if index == 0 && !current_is_import && previous_end.is_some() {
          if !formatted.ends_with("\n\n") {
            formatted.push('\n');
          }
        }

        formatted.push_str(&statement);
        previous_end = Some(self.node_true_end(root));
      }
    }

    Ok(formatted)
  }

  fn is_import_node(
    &self,
    node_id: &NodeId,
  ) -> bool {
    match self.nodes.get(node_id) {
      ASTNode::Statement(ASTStatement::Import(_)) => true,
      ASTNode::Statement(ASTStatement::Export(export)) => {
        matches!(export, ignis_ast::statements::ASTExport::ReExportFrom { .. })
      },
      _ => false,
    }
  }

  fn collect_import_groups(
    &self,
    roots: &[NodeId],
  ) -> Vec<ImportGroup> {
    let mut groups: Vec<ImportGroup> = Vec::new();
    let mut current_group: Vec<NodeId> = Vec::new();
    let mut in_import_run = false;
    let mut has_seen_import_group = false;

    for (index, root) in roots.iter().enumerate() {
      let span = self.nodes.get(root).span().clone();

      let is_import = self.is_import_node(root);

      if is_import {
        if !in_import_run {
          if !current_group.is_empty() {
            groups.push(ImportGroup {
              items: std::mem::take(&mut current_group),
              has_preceding_import_group: has_seen_import_group,
            });
          }
          in_import_run = true;
        }

        let blank_before = if index > 0 {
          let prev_root = roots[index - 1];
          let prev_end = self.node_true_end(&prev_root);
          let gap_start = prev_end;
          let gap_end = span.start.0 as usize;
          self.whitespace_gap_has_intentional_blank_line(gap_start, gap_end)
        } else {
          false
        };

        if blank_before && !current_group.is_empty() {
          groups.push(ImportGroup {
            items: std::mem::take(&mut current_group),
            has_preceding_import_group: has_seen_import_group,
          });
          has_seen_import_group = true;
        }

        current_group.push(*root);
      } else {
        if in_import_run && !current_group.is_empty() {
          has_seen_import_group = true;
        }
        in_import_run = false;

        if let Some(prev_root) = roots.get(index.wrapping_sub(1)) {
          let prev_end = self.node_true_end(prev_root);
          let gap_start = prev_end;
          let gap_end = span.start.0 as usize;
          let blank_before = index > 0 && self.whitespace_gap_has_intentional_blank_line(gap_start, gap_end);

          if blank_before && !current_group.is_empty() {
            groups.push(ImportGroup {
              items: std::mem::take(&mut current_group),
              has_preceding_import_group: false,
            });
          }
        }

        current_group.push(*root);
      }
    }

    if !current_group.is_empty() {
      groups.push(ImportGroup {
        items: current_group,
        has_preceding_import_group: has_seen_import_group,
      });
    }

    for group in &mut groups {
      if group.items.iter().all(|root| self.is_import_node(root)) {
        group.items.sort_by(|a, b| self.compare_imports(a, b));
      }
    }

    groups
  }

  fn compare_imports(
    &self,
    a: &NodeId,
    b: &NodeId,
  ) -> std::cmp::Ordering {
    let a_info = self.import_sort_key(a);
    let b_info = self.import_sort_key(b);

    a_info.cmp(&b_info)
  }

  fn import_sort_key(
    &self,
    node_id: &NodeId,
  ) -> ImportSortKey {
    match self.nodes.get(node_id) {
      ASTNode::Statement(ASTStatement::Import(import)) => {
        let is_wildcard = self.slice_span(&import.items[0].span).trim() == "_";
        let is_std = import.from.starts_with("std::");
        ImportSortKey {
          is_wildcard,
          is_std,
          path: import.from.clone(),
        }
      },
      ASTNode::Statement(ASTStatement::Export(export)) => {
        if let ignis_ast::statements::ASTExport::ReExportFrom { from, .. } = export {
          let is_std = from.starts_with("std::");
          ImportSortKey {
            is_wildcard: false,
            is_std,
            path: from.clone(),
          }
        } else {
          ImportSortKey {
            is_wildcard: false,
            is_std: false,
            path: String::new(),
          }
        }
      },
      _ => ImportSortKey {
        is_wildcard: false,
        is_std: false,
        path: String::new(),
      },
    }
  }

  fn indent(
    &self,
    level: usize,
  ) -> String {
    self.config.indent_string(level)
  }

  fn format_statement_node(
    &self,
    node_id: NodeId,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    match self.nodes.get(&node_id) {
      ASTNode::Statement(statement) => self.format_statement(statement, indent_level),
      ASTNode::Expression(expression) => {
        let mut formatted = self.indent(indent_level);
        formatted.push_str(&self.format_expression(expression, 0, indent_level)?);
        formatted.push(';');
        Ok(formatted)
      },
    }
  }

  fn format_statement(
    &self,
    statement: &ASTStatement,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    match statement {
      ASTStatement::Import(import) => Ok(format!("{}{}", self.indent(indent_level), self.format_import(import))),
      ASTStatement::Export(export) => self.format_export(export, indent_level),
      ASTStatement::Function(function) => self.format_function(function, indent_level),
      ASTStatement::TypeAlias(type_alias) => self.format_type_alias(type_alias, indent_level),
      ASTStatement::Record(record) => self.format_record(record, indent_level),
      ASTStatement::Enum(enum_) => self.format_enum(enum_, indent_level),
      ASTStatement::Trait(trait_) => self.format_trait(trait_, indent_level),
      ASTStatement::Block(block) => self.format_block_statement(block, indent_level),
      ASTStatement::If(if_statement) => self.format_if_statement(if_statement, indent_level),
      ASTStatement::While(while_statement) => self.format_while_statement(while_statement, indent_level),
      ASTStatement::LetElse(let_else) => self.format_let_else_statement(let_else, indent_level),
      ASTStatement::For(for_stmt) => self.format_for_statement(for_stmt, indent_level),
      ASTStatement::ForOf(for_of) => self.format_for_of_statement(for_of, indent_level),
      ASTStatement::Namespace(namespace) => self.format_namespace(namespace, indent_level),
      ASTStatement::Extern(extern_) => self.format_extern(extern_, indent_level),
      ASTStatement::Comment(comment) => {
        Ok(format!("{}{}", self.indent(indent_level), comment.content.trim()))
      },
      ASTStatement::Continue(_) => Ok(format!("{}continue;", self.indent(indent_level))),
      ASTStatement::Break(_) => Ok(format!("{}break;", self.indent(indent_level))),
      ASTStatement::Defer(defer_statement) => {
        let mut formatted = self.indent(indent_level);
        formatted.push_str("defer ");
        formatted.push_str(&self.format_expression_node(defer_statement.expression, 0, indent_level)?);
        formatted.push(';');
        Ok(formatted)
      },
      ASTStatement::Return(return_statement) => {
        let mut formatted = self.indent(indent_level);
        formatted.push_str("return");

        if let Some(expression) = return_statement.expression {
          formatted.push(' ');

          match self.nodes.get(&expression) {
            ASTNode::Expression(ASTExpression::RecordInit(record_init)) => {
              formatted.push_str(&self.format_record_init_multiline(record_init, indent_level)?);
            },
            ASTNode::Expression(_) => formatted.push_str(&self.format_expression_node(expression, 0, indent_level)?),
            ASTNode::Statement(_) => {
              return Err(LayoutFailure::unsupported("return statements require expression children"));
            },
          }
        }

        formatted.push(';');
        Ok(formatted)
      },
      ASTStatement::Expression(expression) => {
        let mut formatted = self.indent(indent_level);
        formatted.push_str(&self.format_expression(expression, 0, indent_level)?);
        formatted.push(';');
        Ok(formatted)
      },
      ASTStatement::Variable(variable) => {
        let mut formatted = self.indent(indent_level);
        formatted.push_str(&collapse_spaces(self.slice_span(&Span::new(
          variable.span.file,
          variable.span.start,
          variable.name_span.start,
        ))));
        formatted.push(' ');
        formatted.push_str(self.slice_span(&variable.name_span).trim());

        if variable.type_ != IgnisTypeSyntax::Implicit {
          formatted.push_str(": ");
          formatted.push_str(&self.format_type(&variable.type_));
        }

        if let Some(value) = variable.value {
          formatted.push_str(" = ");

          match self.nodes.get(&value) {
            ASTNode::Expression(ASTExpression::RecordInit(record_init)) => {
              formatted.push_str(&self.format_record_init_multiline(record_init, indent_level)?);
            },
            ASTNode::Expression(_) => formatted.push_str(&self.format_expression_node(value, 0, indent_level)?),
            ASTNode::Statement(_) => {
              return Err(LayoutFailure::unsupported("variable initializers require expression children"));
            },
          }
        }

        formatted.push(';');
        Ok(formatted)
      },
      ASTStatement::Constant(constant) => {
        let mut formatted = self.indent(indent_level);
        let raw = self.slice_span(&constant.span).trim();
        let name = self.slice_constant_name(constant);
        let header = raw.split(':').next().unwrap_or(raw);
        let name_start = header.rfind(name).unwrap_or(0);
        let prefix = collapse_spaces(header[..name_start].trim());

        formatted.push_str(&prefix);
        if !prefix.is_empty() {
          formatted.push(' ');
        }
        formatted.push_str(name);
        formatted.push_str(": ");
        formatted.push_str(&self.format_type(&constant.ty));

        if let Some(value) = constant.value {
          formatted.push_str(" = ");

          match self.nodes.get(&value) {
            ASTNode::Expression(ASTExpression::RecordInit(record_init)) => {
              formatted.push_str(&self.format_record_init_multiline(record_init, indent_level)?);
            },
            ASTNode::Expression(_) => formatted.push_str(&self.format_expression_node(value, 0, indent_level)?),
            ASTNode::Statement(_) => {
              return Err(LayoutFailure::unsupported("constant initializers require expression children"));
            },
          }
        }

        formatted.push(';');
        Ok(formatted)
      },
    }
  }

  fn format_import(
    &self,
    import: &ASTImport,
  ) -> String {
    let items = import
      .items
      .iter()
      .map(|item| self.slice_span(&item.span).trim().to_string())
      .collect::<Vec<_>>()
      .join(", ");

    format!("import {} from \"{}\";", items, import.from)
  }

  fn format_function(
    &self,
    function: &ASTFunction,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let signature = &function.signature;

    let mut header_prefix = self.format_attributes(&signature.attrs, indent_level);
    header_prefix.push_str(&format!(
      "{}function {}",
      self.indent(indent_level),
      self.slice_span(&signature.name_span).trim()
    ));
    if let Some(type_params) = &signature.type_params {
      header_prefix.push_str(&self.format_type_params(type_params));
    }

    let parameters = signature
      .parameters
      .iter()
      .map(|parameter| self.format_parameter(parameter))
      .collect::<Vec<_>>();

    let formatted = self.format_signature_body(
      header_prefix,
      &parameters,
      self.callable_has_trailing_comma(&signature.span),
      &signature.return_type,
      function.body,
      indent_level,
    )?;

    Ok(formatted)
  }

  fn format_type_params(
    &self,
    type_params: &ASTGenericParams,
  ) -> String {
    let params = type_params
      .params
      .iter()
      .map(|param| collapse_spaces(self.slice_span(&param.span)))
      .collect::<Vec<_>>()
      .join(", ");

    format!("<{params}>")
  }

  fn format_block_node(
    &self,
    node_id: NodeId,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    match self.nodes.get(&node_id) {
      ASTNode::Statement(ASTStatement::Block(block)) => self.format_block_statement(block, indent_level),
      ASTNode::Statement(statement) => self.format_statement(statement, indent_level),
      ASTNode::Expression(expression) => self.format_expression(expression, 0, 0),
    }
  }

  fn format_block_statement(
    &self,
    block: &ASTBlock,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = String::new();
    formatted.push_str("{\n");

    if let Ok((body_start, body_end)) = self.braced_body_bounds(&block.span) {
      let mut cursor = body_start;
      let mut wrote_statement = false;

      for statement in &block.statements {
        let span = self.nodes.get(statement).span().clone();
        if wrote_statement {
          self.preserve_single_blank_line_for_whitespace_gap(&mut formatted, cursor, span.start.0 as usize);
        }
        self.render_comment_gap(&mut formatted, cursor, span.start.0 as usize, indent_level + 1, wrote_statement)?;

        let statement_text = self.format_statement_node(*statement, indent_level + 1)?;

        formatted.push_str(&statement_text);

        let stmt_end = span.end.0 as usize;
        let line_end = self.source[stmt_end..]
          .find('\n')
          .map(|offset| stmt_end + offset)
          .unwrap_or(self.source.len());

        if self.gap_starts_with_same_line_comment(stmt_end, line_end) {
          let trailing_blocks = self.comment_blocks_in_slice(stmt_end, line_end)?;
          self.push_comment_blocks(&mut formatted, &trailing_blocks, indent_level + 1, true);
        }

        // Advance cursor to end-of-line so the gap to the next statement starts
        // at the newline boundary. This lets preserve_single_blank_line_for_whitespace_gap
        // correctly detect intentional blank lines even when statement spans end before ';'.
        cursor = line_end;

        formatted.push('\n');
        wrote_statement = true;
      }

      if wrote_statement {
        self.preserve_single_blank_line_for_whitespace_gap(&mut formatted, cursor, body_end);
      }
      self.render_comment_gap(&mut formatted, cursor, body_end, indent_level + 1, wrote_statement)?;
    } else {
      for statement in &block.statements {
        let statement_text = self.format_statement_node(*statement, indent_level + 1)?;

        formatted.push_str(&statement_text);
        formatted.push('\n');
      }
    }

    formatted.push_str(&self.indent(indent_level));
    formatted.push('}');
    Ok(formatted)
  }

  fn format_if_statement(
    &self,
    if_statement: &ignis_ast::statements::ASTIf,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = self.indent(indent_level);
    formatted.push_str("if (");
    formatted.push_str(&self.format_expression_node(if_statement.condition, 0, 0)?);
    formatted.push_str(") ");
    formatted.push_str(&self.format_block_node(if_statement.then_block, indent_level)?);

    if let Some(else_block) = if_statement.else_block {
      formatted.push_str(" else ");

      match self.nodes.get(&else_block) {
        ASTNode::Statement(ASTStatement::Block(_)) => {
          formatted.push_str(&self.format_block_node(else_block, indent_level)?);
        },
        ASTNode::Statement(ASTStatement::If(else_if)) => {
          formatted.push_str(self.format_if_statement(else_if, indent_level)?.trim_start());
        },
        _ => {
          // Source-preserving fallback: format whatever node the parser produced.
          let inner = self.format_block_node(else_block, indent_level)?;
          formatted.push_str(&inner);
        },
      }
    }

    Ok(formatted)
  }

  fn format_while_statement(
    &self,
    while_statement: &ignis_ast::statements::ASTWhile,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = self.indent(indent_level);
    formatted.push_str("while (");
    formatted.push_str(&self.format_expression_node(while_statement.condition, 0, 0)?);
    formatted.push_str(") ");
    formatted.push_str(&self.format_block_node(while_statement.body, indent_level)?);
    Ok(formatted)
  }

  fn format_let_else_statement(
    &self,
    let_else: &ignis_ast::statements::ASTLetElse,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = self.indent(indent_level);
    formatted.push_str("let ");
    formatted.push_str(&self.format_pattern(&let_else.pattern));

    if let Some(binding_type) = &let_else.binding_type {
      formatted.push_str(": ");
      formatted.push_str(&self.format_type(binding_type));
    }

    formatted.push_str(" = ");
    formatted.push_str(&self.format_expression_node(let_else.value, 0, 0)?);
    formatted.push_str(" else ");
    formatted.push_str(&self.format_block_node(let_else.else_block, indent_level)?);
    formatted.push(';');
    Ok(formatted)
  }

  fn format_for_statement(
    &self,
    for_stmt: &ignis_ast::statements::ASTFor,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let init_raw = self.format_statement_node(for_stmt.initializer, 0)?;
    let init = init_raw.trim().trim_end_matches(';').trim();
    let condition = self.format_expression_node(for_stmt.condition, 0, 0)?;
    let increment = self.format_expression_node(for_stmt.increment, 0, 0)?;

    let mut formatted = self.indent(indent_level);
    formatted.push_str("for (");
    formatted.push_str(init);
    formatted.push_str("; ");
    formatted.push_str(&condition);
    formatted.push_str("; ");
    formatted.push_str(&increment);
    formatted.push_str(") ");
    formatted.push_str(&self.format_block_node(for_stmt.body, indent_level)?);
    Ok(formatted)
  }

  fn format_for_of_statement(
    &self,
    for_of: &ignis_ast::statements::ASTForOf,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let binding_text = collapse_spaces(self.slice_span(&for_of.binding.span).trim());
    let iter = self.format_expression_node(for_of.iter, 0, 0)?;

    let mut formatted = self.indent(indent_level);
    formatted.push_str("for (let ");
    formatted.push_str(&binding_text);
    formatted.push_str(" of ");
    formatted.push_str(&iter);
    formatted.push_str(") ");
    formatted.push_str(&self.format_block_node(for_of.body, indent_level)?);
    Ok(formatted)
  }

  fn format_namespace(
    &self,
    namespace: &ignis_ast::statements::ASTNamespace,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let (body_start, body_end) = self.braced_body_bounds(&namespace.span)?;
    if self.region_contains_raw_directive(body_start, body_end) {
      return Ok(self.slice_span(&namespace.span).trim().to_string());
    }

    if self.braced_body_is_empty(body_start, body_end) {
      let raw = self.slice_span(&namespace.span).trim();
      let after_kw = raw.strip_prefix("namespace").unwrap_or("").trim_start();
      let brace_pos = after_kw.find('{').unwrap_or(after_kw.len());
      let path_str = collapse_spaces(after_kw[..brace_pos].trim());
      return Ok(format!("{}namespace {} {{}}", self.indent(indent_level), path_str));
    }

    let raw = self.slice_span(&namespace.span).trim();
    let after_kw = raw.strip_prefix("namespace").unwrap_or("").trim_start();
    let brace_pos = after_kw.find('{').unwrap_or(after_kw.len());
    let path_str = collapse_spaces(after_kw[..brace_pos].trim());

    let mut formatted = format!("{}namespace {} {{\n", self.indent(indent_level), path_str);

    let owned_items = self.owned_spanned_items(&namespace.items, body_start, |item| self.node_owned_span(item))?;

    let mut cursor = body_start;
    let mut wrote_any = false;

    for owned_item in &owned_items {
      let item_span = self.node_owned_span(owned_item.item);
      let directive_segment_start = self.directive_prefixed_item_start(owned_item.item, cursor);

      if wrote_any {
        self.preserve_single_blank_line_for_whitespace_gap(&mut formatted, cursor, directive_segment_start);
      }

      if directive_segment_start < item_span.start.0 as usize {
        formatted.push_str(self.slice_range(directive_segment_start, self.node_true_end(owned_item.item)).trim_end());
      } else {
        let item_text = self.format_statement_node(*owned_item.item, indent_level + 1)?;
        formatted.push_str(&self.render_owned_segment(
          &owned_item.leading,
          item_text,
          &owned_item.trailing,
          indent_level + 1,
        ));
      }
      formatted.push('\n');
      cursor = self.node_true_end(owned_item.item);
      wrote_any = true;
    }

    self.render_region_tail(&mut formatted, cursor, body_end, indent_level + 1)?;

    formatted.push_str(&self.indent(indent_level));
    formatted.push('}');
    Ok(formatted)
  }

  fn format_extern(
    &self,
    extern_: &ignis_ast::statements::ASTExtern,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let (body_start, body_end) = self.braced_body_bounds(&extern_.span)?;
    if self.region_contains_raw_directive(body_start, body_end) {
      let mut raw = self.format_attributes(&extern_.attrs, indent_level);
      raw.push_str(self.slice_span(&extern_.span).trim());
      return Ok(raw);
    }

    if self.braced_body_is_empty(body_start, body_end) {
      let mut formatted = self.format_attributes(&extern_.attrs, indent_level);
      let raw = self.slice_span(&extern_.span).trim();
      let extern_line = raw
        .lines()
        .find(|line| line.trim_start().starts_with("extern"))
        .unwrap_or("");
      let after_kw = extern_line.trim().strip_prefix("extern").unwrap_or("").trim_start();
      let brace_pos = after_kw.find('{').unwrap_or(after_kw.len());
      let path_str = collapse_spaces(after_kw[..brace_pos].trim());
      formatted.push_str(&format!("{}extern {} {{}}", self.indent(indent_level), path_str));
      return Ok(formatted);
    }

    let mut formatted = self.format_attributes(&extern_.attrs, indent_level);

    let raw = self.slice_span(&extern_.span).trim();
    let extern_line = raw
      .lines()
      .find(|line| line.trim_start().starts_with("extern"))
      .unwrap_or("");
    let after_kw = extern_line.trim().strip_prefix("extern").unwrap_or("").trim_start();
    let brace_pos = after_kw.find('{').unwrap_or(after_kw.len());
    let path_str = collapse_spaces(after_kw[..brace_pos].trim());

    formatted.push_str(&format!("{}extern {} {{\n", self.indent(indent_level), path_str));

    let owned_items = self.owned_spanned_items(&extern_.items, body_start, |item| self.node_owned_span(item))?;

    let mut cursor = body_start;
    let mut wrote_any = false;

    for owned_item in &owned_items {
      let item_span = self.node_owned_span(owned_item.item);
      let directive_segment_start = self.directive_prefixed_item_start(owned_item.item, cursor);

      if wrote_any {
        self.preserve_single_blank_line_for_whitespace_gap(&mut formatted, cursor, directive_segment_start);
      }

      if directive_segment_start < item_span.start.0 as usize {
        formatted.push_str(self.slice_range(directive_segment_start, self.node_true_end(owned_item.item)).trim_end());
      } else {
        let item_text = self.format_statement_node(*owned_item.item, indent_level + 1)?;
        formatted.push_str(&self.render_owned_segment(
          &owned_item.leading,
          item_text,
          &owned_item.trailing,
          indent_level + 1,
        ));
      }
      formatted.push('\n');
      cursor = self.node_true_end(owned_item.item);
      wrote_any = true;
    }

    self.render_region_tail(&mut formatted, cursor, body_end, indent_level + 1)?;

    formatted.push_str(&self.indent(indent_level));
    formatted.push('}');
    Ok(formatted)
  }

  fn format_export(
    &self,
    export: &ignis_ast::statements::ASTExport,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    match export {
      ignis_ast::statements::ASTExport::Declaration { decl, .. } => match self.nodes.get(decl) {
        ASTNode::Statement(ASTStatement::Record(record)) => self
          .format_record(record, indent_level)
          .map(|formatted| self.insert_export_keyword(&formatted, indent_level)),
        ASTNode::Statement(ASTStatement::Enum(enum_)) => self
          .format_enum(enum_, indent_level)
          .map(|formatted| self.insert_export_keyword(&formatted, indent_level)),
        ASTNode::Statement(ASTStatement::TypeAlias(type_alias)) => self
          .format_type_alias(type_alias, indent_level)
          .map(|formatted| self.insert_export_keyword(&formatted, indent_level)),
        ASTNode::Statement(ASTStatement::Function(function)) => self
          .format_function(function, indent_level)
          .map(|formatted| self.insert_export_keyword(&formatted, indent_level)),
        ASTNode::Statement(ASTStatement::Trait(trait_)) => self
          .format_trait(trait_, indent_level)
          .map(|formatted| self.insert_export_keyword(&formatted, indent_level)),
        ASTNode::Statement(ASTStatement::Namespace(namespace)) => self
          .format_namespace(namespace, indent_level)
          .map(|formatted| self.insert_export_keyword(&formatted, indent_level)),
        // Source-preserving fallback for export kinds without specialized printers.
        _ => {
          let inner = self.format_statement_node(*decl, indent_level)?;
          Ok(self.insert_export_keyword(&inner, indent_level))
        },
      },
      ignis_ast::statements::ASTExport::ReExportFrom { items, from, .. } => {
        let items_text = items
          .iter()
          .map(|item| self.slice_span(&item.span).trim().to_string())
          .collect::<Vec<_>>()
          .join(", ");
        Ok(format!("{}export {} from \"{}\";", self.indent(indent_level), items_text, from))
      },
      ignis_ast::statements::ASTExport::Name { span, .. } => {
        let raw = collapse_spaces(self.slice_span(span).trim());
        let without_semi = raw.trim_end_matches(';').trim().to_string();
        Ok(format!("{}{};", self.indent(indent_level), without_semi))
      },
    }
  }

  fn format_type_alias(
    &self,
    type_alias: &ignis_ast::statements::ASTTypeAlias,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let raw_name = self
      .slice_span(&type_alias.span)
      .split_whitespace()
      .nth(1)
      .unwrap_or_default();

    // Strip any trailing generic params from the name since they are formatted
    // separately via `format_type_params`. The source text includes `<T, U>`
    // in the name token when generics are present.
    let name = raw_name.split('<').next().unwrap_or(raw_name);

    let mut formatted = format!(
      "{}type {}",
      self.indent(indent_level),
      name
    );

    if let Some(type_params) = &type_alias.type_params {
      formatted.push_str(&self.format_type_params(type_params));
    }

    formatted.push_str(" = ");
    formatted.push_str(&self.format_type(&type_alias.target));
    formatted.push(';');
    Ok(formatted)
  }

  fn format_record(
    &self,
    record: &ignis_ast::statements::ASTRecord,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = self.format_attributes(&record.attrs, indent_level);
    formatted.push_str(&format!(
      "{}record {}",
      self.indent(indent_level),
      self.symbol_slice(&record.span)
    ));
    if let Some(type_params) = &record.type_params {
      formatted.push_str(&self.format_type_params(type_params));
    }

    let body_start = self.braced_body_bounds(&record.span)?.0;
    if self.braced_body_is_empty(body_start, self.braced_body_bounds(&record.span)?.1) {
      formatted.push_str(" {}");
      return Ok(formatted);
    }

    formatted.push_str(" {\n");

    let mut wrote_any = false;
    let (_, body_end) = self.braced_body_bounds(&record.span)?;
    let owned_items = self.owned_spanned_items(&record.items, body_start, |item| self.record_item_span(item).clone())?;

    let mut cursor = body_start;

    for owned_item in &owned_items {
      let item_span = self.record_item_span(owned_item.item).clone();
      let is_method = matches!(owned_item.item, ignis_ast::statements::ASTRecordItem::Method(_));

      if wrote_any {
        self.preserve_single_blank_line_for_whitespace_gap(&mut formatted, cursor, item_span.start.0 as usize);

        // In JS/TS style, only methods are separated by blank lines — not fields.
        if is_method && !formatted.ends_with("\n\n") {
          formatted.push('\n');
        }
      }

      let item_text = match owned_item.item {
        ignis_ast::statements::ASTRecordItem::Field(field) => self.format_record_field(field, indent_level + 1)?,
        ignis_ast::statements::ASTRecordItem::Method(method) => self.format_method(method, indent_level + 1)?,
      };

      formatted.push_str(&self.render_owned_segment(
        &owned_item.leading,
        item_text,
        &owned_item.trailing,
        indent_level + 1,
      ));

      formatted.push('\n');
      wrote_any = true;
      cursor = item_span.end.0 as usize;
    }

    self.render_region_tail(&mut formatted, cursor, body_end, indent_level + 1)?;

    formatted.push_str(&self.indent(indent_level));
    formatted.push('}');
    Ok(formatted)
  }

  fn format_enum(
    &self,
    enum_: &ignis_ast::statements::ASTEnum,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = self.format_attributes(&enum_.attrs, indent_level);
    formatted.push_str(&format!("{}enum {}", self.indent(indent_level), self.symbol_slice(&enum_.span)));
    if let Some(type_params) = &enum_.type_params {
      formatted.push_str(&self.format_type_params(type_params));
    }

    let body_start = self.braced_body_bounds(&enum_.span)?.0;
    if self.braced_body_is_empty(body_start, self.braced_body_bounds(&enum_.span)?.1) {
      formatted.push_str(" {}");
      return Ok(formatted);
    }

    formatted.push_str(" {\n");

    let (_, body_end) = self.braced_body_bounds(&enum_.span)?;
    let owned_items = self.owned_spanned_items(&enum_.items, body_start, |item| self.enum_item_span(item).clone())?;

    let mut cursor = body_start;

    for (index, owned_item) in owned_items.iter().enumerate() {
      let item_span = self.enum_item_span(owned_item.item).clone();

      if index > 0 {
        self.preserve_single_blank_line_for_whitespace_gap(&mut formatted, cursor, item_span.start.0 as usize);
      }

      let item_text = match owned_item.item {
        ignis_ast::statements::ASTEnumItem::Variant(variant) => {
          let mut rendered = self.indent(indent_level + 1);
          rendered.push_str(self.slice_span(&variant.name_span).trim());
          if !variant.payload.is_empty() {
            let payload = variant
              .payload
              .iter()
              .map(|type_| self.format_type(type_))
              .collect::<Vec<_>>();
            rendered.push_str(&render_doc(
              &format_inline_or_multiline("(", &payload, ")", indent_level + 1, self.config, false),
              self.config,
              indent_level + 1,
            ));
          }
          rendered.push(',');
          rendered
        },
        ignis_ast::statements::ASTEnumItem::Field(field) => self.format_enum_field(field, indent_level + 1)?,
        ignis_ast::statements::ASTEnumItem::Method(method) => self.format_method(method, indent_level + 1)?,
      };

      formatted.push_str(&self.render_owned_segment(
        &owned_item.leading,
        item_text,
        &owned_item.trailing,
        indent_level + 1,
      ));
      formatted.push('\n');
      cursor = item_span.end.0 as usize;
    }

    self.render_region_tail(&mut formatted, cursor, body_end, indent_level + 1)?;

    formatted.push_str(&self.indent(indent_level));
    formatted.push('}');
    Ok(formatted)
  }

  fn format_trait(
    &self,
    trait_: &ignis_ast::statements::ASTTrait,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = self.format_attributes(&trait_.attrs, indent_level);
    formatted.push_str(&format!(
      "{}trait {}",
      self.indent(indent_level),
      self.symbol_slice(&trait_.span)
    ));
    if let Some(type_params) = &trait_.type_params {
      formatted.push_str(&self.format_type_params(type_params));
    }

    let body_start = self.braced_body_bounds(&trait_.span)?.0;
    if self.braced_body_is_empty(body_start, self.braced_body_bounds(&trait_.span)?.1) {
      formatted.push_str(" {}");
      return Ok(formatted);
    }

    formatted.push_str(" {\n");

    let (_, body_end) = self.braced_body_bounds(&trait_.span)?;
    let owned_items = self.owned_spanned_items(&trait_.methods, body_start, |method| method.span.clone())?;
    let mut cursor = body_start;

    for (index, owned_item) in owned_items.iter().enumerate() {
      if index > 0 {
        self.preserve_single_blank_line_for_whitespace_gap(
          &mut formatted,
          cursor,
          owned_item.item.span.start.0 as usize,
        );

        if !formatted.ends_with("\n\n") {
          formatted.push('\n');
        }
      }

      let item_text = self.format_trait_method(owned_item.item, indent_level + 1)?;
      formatted.push_str(&self.render_owned_segment(
        &owned_item.leading,
        item_text,
        &owned_item.trailing,
        indent_level + 1,
      ));
      formatted.push('\n');
      cursor = owned_item.item.span.end.0 as usize;
    }

    self.render_region_tail(&mut formatted, cursor, body_end, indent_level + 1)?;

    formatted.push_str(&self.indent(indent_level));
    formatted.push('}');
    Ok(formatted)
  }

  fn format_record_field(
    &self,
    field: &ignis_ast::statements::ASTRecordField,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = self.format_attributes(&field.attrs, indent_level);
    formatted.push_str(&self.indent(indent_level));
    self.push_visibility_modifier(&mut formatted, field.metadata);
    if field.is_static() {
      formatted.push_str("static ");
    }
    formatted.push_str(self.slice_span(&field.name_span).trim());
    formatted.push_str(": ");
    formatted.push_str(&self.format_type(&field.type_));
    if let Some(value) = field.value {
      formatted.push_str(" = ");
      formatted.push_str(&self.format_expression_node(value, 0, 0)?);
    }
    formatted.push(';');
    Ok(formatted)
  }

  fn format_enum_field(
    &self,
    field: &ignis_ast::statements::ASTEnumField,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = self.format_attributes(&[], indent_level);
    formatted.push_str(&self.indent(indent_level));
    self.push_visibility_modifier(&mut formatted, field.metadata);
    formatted.push_str(self.slice_span(&field.name_span).trim());
    formatted.push_str(": ");
    formatted.push_str(&self.format_type(&field.type_));
    if let Some(value) = field.value {
      formatted.push_str(" = ");
      formatted.push_str(&self.format_expression_node(value, 0, 0)?);
    }
    formatted.push(';');
    Ok(formatted)
  }

  fn format_method(
    &self,
    method: &ignis_ast::statements::ASTMethod,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut header = self.format_attributes(&method.attrs, indent_level);
    header.push_str(&self.indent(indent_level));
    self.push_visibility_modifier(&mut header, method.metadata);
    if method.is_static() {
      header.push_str("static ");
    }
    header.push_str(self.slice_span(&method.name_span).trim());
    if let Some(type_params) = &method.type_params {
      header.push_str(&self.format_type_params(type_params));
    }

    let mut parameters = Vec::new();
    if let Some(mutable) = method.self_param {
      parameters.push(if mutable {
        "&mut self".to_string()
      } else {
        "&self".to_string()
      });
    }
    parameters.extend(
      method
        .parameters
        .iter()
        .map(|parameter| self.format_parameter(parameter)),
    );

    let formatted = self.format_signature_body(
      header,
      &parameters,
      self.callable_has_trailing_comma(&method.span),
      &method.return_type,
      Some(method.body),
      indent_level,
    )?;

    Ok(formatted)
  }

  fn format_trait_method(
    &self,
    method: &ignis_ast::statements::ASTTraitMethod,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut header = self.format_attributes(&method.attrs, indent_level);
    header.push_str(&self.indent(indent_level));
    header.push_str(self.slice_span(&method.name_span).trim());
    if let Some(type_params) = &method.type_params {
      header.push_str(&self.format_type_params(type_params));
    }

    let mut parameters = Vec::new();
    if let Some(mutable) = method.self_param {
      parameters.push(if mutable {
        "&mut self".to_string()
      } else {
        "&self".to_string()
      });
    }
    parameters.extend(
      method
        .parameters
        .iter()
        .map(|parameter| self.format_parameter(parameter)),
    );

    let formatted = self.format_signature_body(
      header,
      &parameters,
      self.callable_has_trailing_comma(&method.span),
      &method.return_type,
      method.body,
      indent_level,
    )?;

    Ok(formatted)
  }

  /// Shared helper: formats a callable signature (parameters + return type)
  /// using the inline-or-multiline doc, then appends body block or semicolon.
  fn format_signature_body(
    &self,
    header: String,
    parameters: &[String],
    has_trailing_comma: bool,
    return_type: &IgnisTypeSyntax,
    body: Option<NodeId>,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let signature_doc = format_inline_or_multiline(
      &format!("{header}("),
      parameters,
      &format!("): {}", self.format_type(return_type)),
      indent_level,
      self.config,
      has_trailing_comma,
    );

    let mut formatted = render_doc(&signature_doc, self.config, indent_level);

    if let Some(body) = body {
      formatted.push(' ');
      formatted.push_str(&self.format_block_node(body, indent_level)?);
    } else {
      formatted.push(';');
    }

    Ok(formatted)
  }

  fn push_visibility_modifier(
    &self,
    target: &mut String,
    metadata: ASTMetadata,
  ) {
    if metadata.contains(ASTMetadata::PUBLIC) {
      target.push_str("public ");
    } else if metadata.contains(ASTMetadata::PRIVATE) {
      target.push_str("private ");
    }
  }

  fn format_parameter(
    &self,
    parameter: &ASTParameter,
  ) -> String {
    let mut formatted = String::new();

    if !parameter.attrs.is_empty() {
      formatted.push_str(
        &parameter
          .attrs
          .iter()
          .map(|attr| self.slice_span(&attr.span).trim().to_string())
          .collect::<Vec<_>>()
          .join(" "),
      );
      formatted.push(' ');
    }

    formatted.push_str(
      self
        .slice_span(&parameter.span)
        .trim()
        .split(':')
        .next()
        .unwrap_or_default()
        .trim(),
    );
    formatted.push_str(": ");
    formatted.push_str(&self.format_type(&parameter.type_));
    formatted
  }

  fn callable_has_trailing_comma(
    &self,
    span: &Span,
  ) -> bool {
    let raw = self.slice_span(span);
    let Some(open_index) = raw.find('(') else {
      return false;
    };

    let mut depth = 0usize;
    let mut close_index = None;

    for (offset, character) in raw[open_index..].char_indices() {
      match character {
        '(' => depth += 1,
        ')' => {
          depth = depth.saturating_sub(1);
          if depth == 0 {
            close_index = Some(open_index + offset);
            break;
          }
        },
        _ => {},
      }
    }

    let Some(close_index) = close_index else {
      return false;
    };

    raw[open_index + 1..close_index].trim_end().ends_with(',')
  }

  fn symbol_slice(
    &self,
    span: &Span,
  ) -> &str {
    let declaration = self.slice_span(span).trim();
    let candidate = declaration.split_whitespace().nth(1).unwrap_or_default();
    let end = candidate.find(['<', '{', '(', ':', '=']).unwrap_or(candidate.len());
    &candidate[..end]
  }

  fn format_expression_node(
    &self,
    node_id: NodeId,
    parent_precedence: u8,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    match self.nodes.get(&node_id) {
      ASTNode::Expression(expression) => self.format_expression(expression, parent_precedence, indent_level),
      ASTNode::Statement(_) => Err(LayoutFailure::unsupported("expression nodes cannot contain statements")),
    }
  }

  fn format_expression(
    &self,
    expression: &ASTExpression,
    parent_precedence: u8,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    match expression {
      ASTExpression::Variable(variable) => Ok(self.slice_span(&variable.span).trim().to_string()),
      ASTExpression::Path(path) => Ok(self.slice_span(&path.span).trim().to_string()),
      ASTExpression::Literal(literal) => Ok(self.slice_span(&literal.span).trim().to_string()),
      ASTExpression::Grouped(grouped) => Ok(format!("({})", self.format_expression_node(grouped.expression, 0, 0)?)),
      ASTExpression::Unary(unary) => {
        let operator = match unary.operator {
          UnaryOperator::Not => "!",
          UnaryOperator::Negate => "-",
          UnaryOperator::Increment => "++",
          UnaryOperator::Decrement => "--",
          UnaryOperator::BitNot => "~",
        };

        Ok(format!("{}{}", operator, self.format_expression_node(unary.operand, 9, 0)?))
      },
      ASTExpression::Binary(binary) => {
        let precedence = binary_precedence(binary.operator.clone());
        let left = self.format_binary_child(binary.left, precedence, false, indent_level)?;
        let right = self.format_binary_child(binary.right, precedence, true, indent_level)?;
        let formatted = format!("{} {} {}", left, binary_operator_text(binary.operator.clone()), right);

        if precedence < parent_precedence {
          Ok(format!("({formatted})"))
        } else {
          Ok(formatted)
        }
      },
      ASTExpression::Assignment(assignment) => {
        let formatted = format!(
          "{} {} {}",
          self.format_expression_node(assignment.target, 1, 0)?,
          assignment_operator_text(assignment.operator.clone()),
          self.format_expression_node(assignment.value, 1, 0)?
        );

        if parent_precedence > 1 {
          Ok(format!("({formatted})"))
        } else {
          Ok(formatted)
        }
      },
      ASTExpression::Ternary(ternary) => {
        let formatted = format!(
          "{} ? {} : {}",
          self.format_expression_node(ternary.condition, 0, 0)?,
          self.format_expression_node(ternary.then_expr, 0, 0)?,
          self.format_expression_node(ternary.else_expr, 0, 0)?
        );

        if parent_precedence > 1 {
          Ok(format!("({formatted})"))
        } else {
          Ok(formatted)
        }
      },
      ASTExpression::Call(call) => {
        let source_text = self.slice_span(&call.span).trim();

        if self.is_multiline_source(source_text) {
          return Ok(source_text.to_string());
        }

        let mut formatted = self.format_expression_node(call.callee, 10, 0)?;

        if let Some(type_args) = &call.type_args {
          let type_args = type_args
            .iter()
            .map(|type_| self.format_type(type_))
            .collect::<Vec<_>>()
            .join(", ");
          formatted.push('<');
          formatted.push_str(&type_args);
          formatted.push('>');
        }

        let arguments = call
          .arguments
          .iter()
          .map(|argument| self.format_expression_node(*argument, 0, 0))
          .collect::<Result<Vec<_>, _>>()?
          .join(", ");

        let trailing = if !call.arguments.is_empty() {
          let last_arg_end = self.nodes.get(call.arguments.last().unwrap()).span().end.0 as usize;
          let call_end = call.span.end.0 as usize;
          if self.slice_range(last_arg_end, call_end).contains(',') { "," } else { "" }
        } else {
          ""
        };

        formatted.push('(');
        formatted.push_str(&arguments);
        formatted.push_str(trailing);
        formatted.push(')');

        Ok(formatted)
      },
      ASTExpression::Match(match_expression) => self.format_match_expression(match_expression, parent_precedence, indent_level),
      ASTExpression::Cast(cast) => {
        let inner = self.format_expression_node(cast.expression, 11, 0)?;
        let formatted = format!("{} as {}", inner, self.format_type(&cast.target_type));

        if parent_precedence > 10 {
          Ok(format!("({formatted})"))
        } else {
          Ok(formatted)
        }
      },
      ASTExpression::Reference(reference) => {
        let prefix = if reference.mutable { "&mut " } else { "&" };
        Ok(format!("{}{}", prefix, self.format_expression_node(reference.inner, 11, 0)?))
      },
      ASTExpression::Dereference(dereference) => {
        Ok(format!("*{}", self.format_expression_node(dereference.inner, 11, 0)?))
      },
      ASTExpression::MemberAccess(access) => {
        let op = match access.op {
          ASTAccessOp::Dot => ".",
          ASTAccessOp::DoubleColon => "::",
        };

        Ok(format!(
          "{}{}{}",
          self.format_expression_node(access.object, 10, 0)?,
          op,
          self.slice_span(&access.member_span).trim()
        ))
      },
      ASTExpression::Vector(vector) => Ok(format!(
        "[{}]",
        vector
          .items
          .iter()
          .map(|item| self.format_expression_node(*item, 0, 0))
          .collect::<Result<Vec<_>, _>>()?
          .join(", ")
      )),
      ASTExpression::VectorAccess(access) => Ok(format!(
        "{}[{}]",
        self.format_expression_node(access.name, 12, 0)?,
        self.format_expression_node(access.index, 0, 0)?
      )),
      ASTExpression::RecordInit(record_init) => {
        let source_text = self.slice_span(&record_init.span).trim();

        if self.is_multiline_source(source_text) {
          return Ok(source_text.to_string());
        }

        Ok(self.format_record_init_inline(record_init)?)
      },
      ASTExpression::BuiltinCall(builtin) => self.format_builtin_call(builtin),
      ASTExpression::Lambda(lambda) => self.format_lambda(lambda, parent_precedence),
      ASTExpression::PostfixIncrement { expr, .. } => Ok(format!("{}++", self.format_expression_node(*expr, 12, 0)?)),
      ASTExpression::PostfixDecrement { expr, .. } => Ok(format!("{}--", self.format_expression_node(*expr, 12, 0)?)),
      ASTExpression::Try { expr, .. } => Ok(format!("{}!", self.format_expression_node(*expr, 12, 0)?)),
      ASTExpression::LetCondition(let_cond) => {
        let value = self.format_expression_node(let_cond.value, 0, 0)?;
        Ok(format!("let {} = {}", self.format_pattern(&let_cond.pattern), value))
      },
      ASTExpression::Pipe { lhs, rhs, .. } => {
        let steps = self.flatten_pipe_chain(*lhs, *rhs);
        self.format_pipe_chain(&steps, indent_level)
      },
      ASTExpression::PipePlaceholder { .. } => Ok("_".to_string()),
      ASTExpression::CaptureOverride(cap) => {
        let prefix = match cap.kind {
          ignis_ast::expressions::CaptureOverrideKind::Move => "@move",
          ignis_ast::expressions::CaptureOverrideKind::Ref => "@ref",
          ignis_ast::expressions::CaptureOverrideKind::RefMut => "@refMut",
        };
        Ok(format!("{} {}", prefix, self.format_expression_node(cap.inner, 11, 0)?))
      },
    }
  }

  /// Returns true when source text spans multiple lines, meaning it cannot be
  /// re-indented without knowing the absolute indent level (which format_expression
  /// does not carry). Such expressions are returned verbatim from the source.
  fn is_multiline_source(
    &self,
    source_text: &str,
  ) -> bool {
    source_text.contains('\n')
  }

  /// Flattens a nested `ASTExpression::Pipe` tree into a linear list of steps.
  /// Each step is either the initial left-hand side or a `|> rhs` segment.
  fn flatten_pipe_chain(
    &self,
    lhs: NodeId,
    rhs: NodeId,
  ) -> Vec<PipeStep> {
    let mut steps = Vec::new();
    self.collect_pipe_steps(lhs, &mut steps);

    let right_expr = match self.nodes.get(&rhs) {
      ASTNode::Expression(expression) => expression.clone(),
      ASTNode::Statement(_) => {
        steps.push(PipeStep::Call(rhs));
        return steps;
      },
    };

    match &right_expr {
      ASTExpression::Pipe {
        lhs: inner_lhs,
        rhs: inner_rhs,
        ..
      } => {
        self.collect_pipe_steps(*inner_lhs, &mut steps);
        let right_id = *inner_rhs;
        steps.push(PipeStep::Call(right_id));
      },
      _ => {
        steps.push(PipeStep::Call(rhs));
      },
    }

    steps
  }

  fn collect_pipe_steps(
    &self,
    node_id: NodeId,
    steps: &mut Vec<PipeStep>,
  ) {
    match self.nodes.get(&node_id) {
      ASTNode::Expression(ASTExpression::Pipe {
        lhs,
        rhs,
        ..
      }) => {
        self.collect_pipe_steps(*lhs, steps);
        steps.push(PipeStep::Call(*rhs));
      },
      ASTNode::Expression(_) => {
        steps.push(PipeStep::Head(node_id));
      },
      ASTNode::Statement(_) => {
        steps.push(PipeStep::Head(node_id));
      },
    }
  }

  fn format_pipe_chain(
    &self,
    steps: &[PipeStep],
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let pipe_count = steps.len().saturating_sub(1);

    if pipe_count < 2 {
      let head = match steps.first() {
        Some(PipeStep::Head(id)) | Some(PipeStep::Call(id)) => {
          self.format_expression_node(*id, 0, 0)?
        },
        None => return Ok(String::new()),
      };

      if let Some(PipeStep::Call(rhs_id)) = steps.get(1) {
        let right = self.format_expression_node(*rhs_id, 0, 0)?;
        let inline = format!("{head} |> {right}");
        if inline.len() <= self.config.line_width {
          return Ok(inline);
        }
      }

      return Ok(head);
    }

    let mut formatted = String::new();

    if let Some(step) = steps.first() {
      formatted.push_str(&self.format_expression_node(step.id(), 0, 0)?);
    }

    for step in steps.iter().skip(1) {
      if let PipeStep::Call(rhs_id) = step {
        formatted.push('\n');
        formatted.push_str(&self.indent(indent_level + 1));
        formatted.push_str("|> ");
        formatted.push_str(&self.format_expression_node(*rhs_id, 0, 0)?);
      }
    }

    Ok(formatted)
  }

  fn format_match_expression(
    &self,
    match_expression: &ignis_ast::expressions::ASTMatch,
    parent_precedence: u8,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let source_text = self.slice_span(&match_expression.span).trim();

    if self.is_multiline_source(source_text) {
      return Ok(if parent_precedence > 0 {
        format!("({source_text})")
      } else {
        source_text.to_string()
      });
    }

    let arms = match_expression
      .arms
      .iter()
      .map(|arm| {
        let mut formatted = self.format_pattern(&arm.pattern);

        if let Some(guard) = arm.guard {
          formatted.push_str(" if ");
          formatted.push_str(&self.format_expression_node(guard, 0, 0)?);
        }

        formatted.push_str(" -> ");
        let body = match self.nodes.get(&arm.body) {
          ASTNode::Expression(expression) => self.format_expression(expression, 0, indent_level)?,
          ASTNode::Statement(ASTStatement::Block(block)) => self.format_block_statement(block, 0)?,
          ASTNode::Statement(_) => {
            return Err(LayoutFailure::unsupported("match arm body must be an expression or block"));
          },
        };
        formatted.push_str(&body);
        Ok::<String, LayoutFailure>(formatted)
      })
      .collect::<Result<Vec<_>, _>>()?;

    let formatted = format!(
      "match ({}) {{ {}, }}",
      self.format_expression_node(match_expression.scrutinee, 0, 0)?,
      arms.join(", ")
    );

    if parent_precedence > 0 {
      Ok(format!("({formatted})"))
    } else {
      Ok(formatted)
    }
  }

  fn format_builtin_call(
    &self,
    builtin: &ignis_ast::expressions::ASTBuiltinCall,
  ) -> Result<String, LayoutFailure> {
    let raw = self.slice_span(&builtin.span).trim();
    let name_end = raw.find(['<', '(']).unwrap_or(raw.len());
    let mut formatted = raw[..name_end].to_string();

    if let Some(type_args) = &builtin.type_args {
      formatted.push('<');
      formatted.push_str(
        &type_args
          .iter()
          .map(|type_| self.format_type(type_))
          .collect::<Vec<_>>()
          .join(", "),
      );
      formatted.push('>');
    }

    formatted.push('(');
    formatted.push_str(
      &builtin
        .args
        .iter()
        .map(|argument| self.format_expression_node(*argument, 0, 0))
        .collect::<Result<Vec<_>, _>>()?
        .join(", "),
    );
    formatted.push(')');
    Ok(formatted)
  }

  fn format_lambda(
    &self,
    lambda: &ignis_ast::expressions::ASTLambda,
    parent_precedence: u8,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = String::from("(");
    formatted.push_str(
      &lambda
        .params
        .iter()
        .map(|parameter| self.format_parameter(parameter))
        .collect::<Vec<_>>()
        .join(", "),
    );
    formatted.push(')');
    formatted.push_str(": ");
    formatted.push_str(&self.format_type(&lambda.return_type));
    formatted.push_str(" -> ");

    match &lambda.body {
      LambdaBody::Expression(expression) => formatted.push_str(&self.format_expression_node(*expression, 0, 0)?),
      LambdaBody::Block(block) => formatted.push_str(&self.format_block_node(*block, 0)?),
    }

    if parent_precedence > 0 {
      Ok(format!("({formatted})"))
    } else {
      Ok(formatted)
    }
  }

  fn format_pattern(
    &self,
    pattern: &ASTPattern,
  ) -> String {
    match pattern {
      ASTPattern::Wildcard { .. } => "_".to_string(),
      ASTPattern::Literal { span, .. } => self.slice_span(span).trim().to_string(),
      ASTPattern::Path { segments, args, .. } => {
        let mut formatted = segments
          .iter()
          .map(|(_, span)| self.slice_span(span).trim().to_string())
          .collect::<Vec<_>>()
          .join("::");

        if let Some(args) = args {
          formatted.push('(');
          formatted.push_str(
            &args
              .iter()
              .map(|arg| self.format_pattern(arg))
              .collect::<Vec<_>>()
              .join(", "),
          );
          formatted.push(')');
        }

        formatted
      },
      ASTPattern::Tuple { elements, .. } => format!(
        "({})",
        elements
          .iter()
          .map(|element| self.format_pattern(element))
          .collect::<Vec<_>>()
          .join(", ")
      ),
      ASTPattern::Or { patterns, .. } => patterns
        .iter()
        .map(|pattern| self.format_pattern(pattern))
        .collect::<Vec<_>>()
        .join(" | "),
    }
  }

  fn format_binary_child(
    &self,
    node_id: NodeId,
    parent_precedence: u8,
    is_right_child: bool,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let expression = match self.nodes.get(&node_id) {
      ASTNode::Expression(expression) => expression,
      ASTNode::Statement(_) => {
        return Err(LayoutFailure::unsupported("binary expressions require expression operands"));
      },
    };

    let child_precedence = expression_precedence(expression);
    let formatted = self.format_expression(expression, parent_precedence, indent_level)?;

    if child_precedence < parent_precedence
      || (is_right_child && child_precedence == parent_precedence && matches!(expression, ASTExpression::Binary(_)))
    {
      Ok(format!("({formatted})"))
    } else {
      Ok(formatted)
    }
  }

  fn format_type(
    &self,
    type_: &IgnisTypeSyntax,
  ) -> String {
    match type_ {
      IgnisTypeSyntax::I8 => "i8".to_string(),
      IgnisTypeSyntax::I16 => "i16".to_string(),
      IgnisTypeSyntax::I32 => "i32".to_string(),
      IgnisTypeSyntax::I64 => "i64".to_string(),
      IgnisTypeSyntax::U8 => "u8".to_string(),
      IgnisTypeSyntax::U16 => "u16".to_string(),
      IgnisTypeSyntax::U32 => "u32".to_string(),
      IgnisTypeSyntax::U64 => "u64".to_string(),
      IgnisTypeSyntax::F32 => "f32".to_string(),
      IgnisTypeSyntax::F64 => "f64".to_string(),
      IgnisTypeSyntax::Implicit => "_".to_string(),
      IgnisTypeSyntax::Str => "str".to_string(),
      IgnisTypeSyntax::Boolean => "boolean".to_string(),
      IgnisTypeSyntax::Atom => "atom".to_string(),
      IgnisTypeSyntax::Void => "void".to_string(),
      IgnisTypeSyntax::Null => "null".to_string(),
      IgnisTypeSyntax::Char => "char".to_string(),
      IgnisTypeSyntax::Vector(inner, size) => match size {
        Some(size) => format!("{}[{}]", self.format_type(inner), size),
        None => format!("{}[]", self.format_type(inner)),
      },
      IgnisTypeSyntax::Tuple(items) => format!(
        "({})",
        items
          .iter()
          .map(|item| self.format_type(item))
          .collect::<Vec<_>>()
          .join(", ")
      ),
      IgnisTypeSyntax::Callable(parameters, return_type) => format!(
        "({}) -> {}",
        parameters
          .iter()
          .map(|parameter| self.format_type(parameter))
          .collect::<Vec<_>>()
          .join(", "),
        self.format_type(return_type)
      ),
      IgnisTypeSyntax::Pointer { inner, mutable } => {
        if *mutable {
          format!("*mut {}", self.format_type(inner))
        } else {
          format!("*{}", self.format_type(inner))
        }
      },
      IgnisTypeSyntax::Reference { inner, mutable } => {
        if *mutable {
          format!("&mut {}", self.format_type(inner))
        } else {
          format!("&{}", self.format_type(inner))
        }
      },
      IgnisTypeSyntax::Named { span, .. } => self.slice_span(span).trim().to_string(),
      IgnisTypeSyntax::Applied { base, args } => format!(
        "{}<{}>",
        self.format_type(base),
        args
          .iter()
          .map(|arg| self.format_type(arg))
          .collect::<Vec<_>>()
          .join(", ")
      ),
      IgnisTypeSyntax::Path { span, .. } => self.slice_span(span).trim().to_string(),
      IgnisTypeSyntax::Union(items) => items
        .iter()
        .map(|item| self.format_type(item))
        .collect::<Vec<_>>()
        .join(" | "),
      IgnisTypeSyntax::Intersection(items) => items
        .iter()
        .map(|item| self.format_type(item))
        .collect::<Vec<_>>()
        .join(" & "),
    }
  }

  /// Returns the true end byte of a node, including any separately-stored child
  /// such as a function body. `ASTStatement::Function.span()` only covers the
  /// signature; the body is a distinct node, so we must check it explicitly.
  fn node_true_end(
    &self,
    node_id: &NodeId,
  ) -> usize {
    match self.nodes.get(node_id) {
      ASTNode::Statement(ASTStatement::Function(f)) => {
        if let Some(body_id) = f.body {
          self.nodes.get(&body_id).span().end.0 as usize
        } else {
          self.nodes.get(node_id).span().end.0 as usize
        }
      },
      node => node.span().end.0 as usize,
    }
  }

  /// Returns the effective start byte of a node, which is the start of its
  /// first leading attribute annotation if any, falling back to the node's own
  /// span start. This is needed because attribute annotations like
  /// `@implements(Drop)` are stored separately from the declaration span, so
  /// the gap before the node must include them rather than treating them as
  /// non-comment noise.
  fn node_effective_start(
    &self,
    node_id: &NodeId,
  ) -> usize {
    let attrs: &[ignis_ast::attribute::ASTAttribute] = match self.nodes.get(node_id) {
      ASTNode::Statement(ASTStatement::Record(r)) => &r.attrs,
      ASTNode::Statement(ASTStatement::Function(f)) => &f.signature.attrs,
      ASTNode::Statement(ASTStatement::Enum(en)) => &en.attrs,
      ASTNode::Statement(ASTStatement::Trait(tr)) => &tr.attrs,
      ASTNode::Statement(ASTStatement::Extern(ex)) => &ex.attrs,
      _ => &[],
    };

    let node_start = self.nodes.get(node_id).span().start.0 as usize;

    attrs
      .iter()
      .map(|attr| attr.span.start.0 as usize)
      .fold(node_start, usize::min)
  }

  fn node_owned_span(
    &self,
    node_id: &NodeId,
  ) -> Span {
    let node_span = self.nodes.get(node_id).span().clone();

    Span::new(
      node_span.file,
      ignis_type::BytePosition(self.node_effective_start(node_id) as u32),
      ignis_type::BytePosition(self.node_true_end(node_id) as u32),
    )
  }

  fn slice_span(
    &self,
    span: &Span,
  ) -> &str {
    self.slice_range(span.start.0 as usize, span.end.0 as usize)
  }

  fn slice_range(
    &self,
    start: usize,
    end: usize,
  ) -> &str {
    &self.source[start..end]
  }

  fn slice_constant_name(
    &self,
    constant: &ignis_ast::statements::ASTConstant,
  ) -> &str {
    let span = &constant.span;
    let raw = self.slice_span(span);
    raw
      .split(':')
      .next()
      .unwrap_or(raw)
      .split_whitespace()
      .last()
      .unwrap_or(raw)
      .trim()
  }

  fn format_record_init_inline(
    &self,
    record_init: &ignis_ast::expressions::ASTRecordInit,
  ) -> Result<String, LayoutFailure> {
    let path = self.record_init_path(record_init);
    let fields = record_init
      .fields
      .iter()
      .map(|field| self.format_record_init_field_inline(field))
      .collect::<Result<Vec<_>, _>>()?;

    Ok(format!("{} {{ {} }}", path, fields.join(", ")))
  }

  fn format_record_init_multiline(
    &self,
    record_init: &ignis_ast::expressions::ASTRecordInit,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let path = self.record_init_path(record_init);
    let fields = record_init
      .fields
      .iter()
      .map(|field| self.format_record_init_field_multiline(field, indent_level + 1))
      .collect::<Result<Vec<_>, _>>()?;

    let inline = format!("{} {{ {} }}", path, fields.join(", "));
    let source_is_inline = !self.slice_span(&record_init.span).contains('\n');
    if source_is_inline && self.indent(indent_level).len() + "return ".len() + inline.len() <= self.config.line_width {
      return Ok(inline);
    }

    let mut formatted = String::new();
    formatted.push_str(&path);
    formatted.push_str(" {\n");

    for field in &fields {
      formatted.push_str(&self.indent(indent_level + 1));
      formatted.push_str(field);

      if !record_init.fields.is_empty() {
        formatted.push(',');
      }

      formatted.push('\n');
    }

    formatted.push_str(&self.indent(indent_level));
    formatted.push('}');
    Ok(formatted)
  }

  fn record_init_path(
    &self,
    record_init: &ignis_ast::expressions::ASTRecordInit,
  ) -> String {
    let mut path = record_init
      .path
      .iter()
      .map(|(_, span)| self.slice_span(span).trim().to_string())
      .collect::<Vec<_>>()
      .join("::");

    if let Some(type_args) = &record_init.type_args {
      let rendered = type_args
        .iter()
        .map(|type_| self.format_type(type_))
        .collect::<Vec<_>>()
        .join(", ");
      path.push('<');
      path.push_str(&rendered);
      path.push('>');
    }

    path
  }

  fn format_record_init_field_inline(
    &self,
    field: &ignis_ast::expressions::ASTRecordInitField,
  ) -> Result<String, LayoutFailure> {
    Ok(format!(
      "{}: {}",
      self.slice_span(&field.name_span).trim(),
      self.format_expression_node(field.value, 0, 0)?
    ))
  }

  fn format_record_init_field_multiline(
    &self,
    field: &ignis_ast::expressions::ASTRecordInitField,
    indent_level: usize,
  ) -> Result<String, LayoutFailure> {
    let mut formatted = String::new();
    formatted.push_str(self.slice_span(&field.name_span).trim());
    formatted.push_str(": ");

    match self.nodes.get(&field.value) {
      ASTNode::Expression(ASTExpression::RecordInit(record_init)) => {
        formatted.push_str(&self.format_record_init_multiline(record_init, indent_level)?);
      },
      ASTNode::Expression(_) => formatted.push_str(&self.format_expression_node(field.value, 0, 0)?),
      ASTNode::Statement(_) => {
        return Err(LayoutFailure::unsupported(
          "record initializer fields require expression values",
        ));
      },
    }

    Ok(formatted)
  }

  fn braced_body_bounds(
    &self,
    span: &Span,
  ) -> Result<(usize, usize), LayoutFailure> {
    let raw = self.slice_span(span);
    let open = raw
      .find('{')
      .ok_or_else(|| LayoutFailure::unsupported("expected braced body in formatter span"))?;
    let close = raw
      .rfind('}')
      .ok_or_else(|| LayoutFailure::unsupported("expected braced body in formatter span"))?;

    Ok((span.start.0 as usize + open + 1, span.start.0 as usize + close))
  }

  fn record_item_span<'b>(
    &self,
    item: &'b ignis_ast::statements::ASTRecordItem,
  ) -> &'b Span {
    match item {
      ignis_ast::statements::ASTRecordItem::Field(field) => &field.span,
      ignis_ast::statements::ASTRecordItem::Method(method) => &method.span,
    }
  }

  fn enum_item_span<'b>(
    &self,
    item: &'b ignis_ast::statements::ASTEnumItem,
  ) -> &'b Span {
    match item {
      ignis_ast::statements::ASTEnumItem::Variant(variant) => &variant.span,
      ignis_ast::statements::ASTEnumItem::Field(field) => &field.span,
      ignis_ast::statements::ASTEnumItem::Method(method) => &method.span,
    }
  }

  fn format_attributes(
    &self,
    attrs: &[ASTAttribute],
    indent_level: usize,
  ) -> String {
    let mut formatted = String::new();

    for attr in attrs {
      formatted.push_str(&self.indent(indent_level));
      formatted.push_str(self.slice_span(&attr.span).trim());
      formatted.push('\n');
    }

    formatted
  }

  fn insert_export_keyword(
    &self,
    formatted: &str,
    indent_level: usize,
  ) -> String {
    let indent = self.indent(indent_level);
    let attr_prefix = format!("{indent}@");
    let mut insert_at = 0;

    while formatted[insert_at..].starts_with(&attr_prefix) {
      let line_end = formatted[insert_at..]
        .find('\n')
        .map(|offset| insert_at + offset + 1)
        .unwrap_or(formatted.len());
      insert_at = line_end;
    }

    let declaration = formatted[insert_at..]
      .strip_prefix(&indent)
      .unwrap_or(&formatted[insert_at..]);

    let mut exported = String::new();
    exported.push_str(&formatted[..insert_at]);
    exported.push_str(&indent);
    exported.push_str("export ");
    exported.push_str(declaration);
    exported
  }

  fn render_owned_segment(
    &self,
    leading: &[CommentBlock],
    body: String,
    trailing: &[CommentBlock],
    indent_level: usize,
  ) -> String {
    let mut rendered = String::new();

    self.push_comment_blocks(&mut rendered, leading, indent_level, false);

    if leading
      .last()
      .is_some_and(|block| matches!(block.placement, CommentPlacement::Detached))
      && !rendered.ends_with("\n\n")
    {
      rendered.push('\n');
    }

    rendered.push_str(&body);

    if !trailing.is_empty() {
      self.push_comment_blocks(&mut rendered, trailing, indent_level, true);
    }

    while rendered.starts_with('\n') {
      rendered.remove(0);
    }

    rendered
  }

  fn render_region_tail(
    &self,
    output: &mut String,
    start: usize,
    end: usize,
    indent_level: usize,
  ) -> Result<(), LayoutFailure> {
    let blocks = self.comment_blocks_in_slice(start, end)?;

    if !blocks.is_empty() {
      self.push_comment_blocks(output, &blocks, indent_level, false);
    }

    Ok(())
  }

  fn render_comment_gap(
    &self,
    output: &mut String,
    start: usize,
    end: usize,
    indent_level: usize,
    separate_from_previous: bool,
  ) -> Result<(), LayoutFailure> {
    let mut blocks = self.comment_blocks_in_slice(start, end)?;

    if blocks.is_empty() {
      return Ok(());
    }

    if let Some(last_block) = blocks.last_mut() {
      if matches!(last_block.placement, CommentPlacement::Detached)
        && !self.comment_gap_preserves_detached_spacing(start, end)
      {
        last_block.placement = CommentPlacement::Leading;
      }
    }

    let has_detached_block = blocks
      .iter()
      .any(|block| matches!(block.placement, CommentPlacement::Detached));

    if separate_from_previous && !output.ends_with("\n\n") {
      output.push('\n');
    }

    self.push_comment_blocks(output, &blocks, indent_level, false);

    if has_detached_block && !output.ends_with("\n\n") {
      output.push('\n');
    }

    Ok(())
  }

  fn owned_spanned_items<'b, T, F>(
    &self,
    items: &'b [T],
    first_item_region_start: usize,
    span_of: F,
  ) -> Result<Vec<OwnedSpannedItem<&'b T>>, LayoutFailure>
  where
    F: Fn(&T) -> Span,
  {
    let mut owned_items = Vec::with_capacity(items.len());

    if items.is_empty() {
      return Ok(owned_items);
    }

    let first_span = span_of(&items[0]);
    let mut cursor = first_span.start.0 as usize;

    for (index, item) in items.iter().enumerate() {
      let span = span_of(item);
      let span_start = self.line_start(span.start.0 as usize);
      if index == 0 {
        cursor = span_start;
      }

      let mut owned_item = OwnedSpannedItem {
        item,
        leading: Vec::new(),
        trailing: Vec::new(),
      };

      if index == 0 {
        let region_start = first_item_region_start.min(span_start);
        if self.gap_contains_directive_syntax(region_start, span_start) {
          owned_items.push(owned_item);
          cursor = span.end.0 as usize;
          continue;
        }

        let mut blocks = self.comment_blocks_in_slice(region_start, span_start)?;

        if let Some(last_block) = blocks.last_mut() {
          if matches!(last_block.placement, CommentPlacement::Detached)
            && !self.comment_gap_preserves_detached_spacing(region_start, span_start)
          {
            last_block.placement = CommentPlacement::Leading;
          }
        }

        for block in blocks {
          match block.placement {
            CommentPlacement::Leading | CommentPlacement::Detached => owned_item.leading.push(block),
            CommentPlacement::Trailing => {
              return Err(LayoutFailure::unsupported(
                "first formatter item cannot start with trailing trivia",
              ));
            },
          }
        }
      } else {
        let force_first_trailing = self.gap_starts_with_same_line_comment(cursor, span_start);

        if self.gap_contains_directive_syntax(cursor, span_start) {
          owned_items.push(owned_item);
          cursor = span.end.0 as usize;
          continue;
        }

        let mut blocks = self.comment_blocks_in_slice(cursor, span_start)?;

        if let Some(last_block) = blocks.last_mut() {
          if matches!(last_block.placement, CommentPlacement::Detached)
            && !self.comment_gap_preserves_detached_spacing(cursor, span_start)
          {
            last_block.placement = CommentPlacement::Leading;
          }
        }

        for (comment_index, mut block) in blocks.into_iter().enumerate() {
          if comment_index == 0 && force_first_trailing {
            block.placement = CommentPlacement::Trailing;
          }

          match block.placement {
            CommentPlacement::Trailing => owned_items
              .last_mut()
              .expect("previous owned formatter item")
              .trailing
              .push(block),
            CommentPlacement::Leading | CommentPlacement::Detached => owned_item.leading.push(block),
          }
        }
      }

      cursor = span.end.0 as usize;
      owned_items.push(owned_item);
    }

    Ok(owned_items)
  }

  fn line_start(
    &self,
    byte_index: usize,
  ) -> usize {
    let clamped = byte_index.min(self.source.len());
    self.source[..clamped].rfind('\n').map(|index| index + 1).unwrap_or(0)
  }

  fn gap_starts_with_same_line_comment(
    &self,
    start: usize,
    end: usize,
  ) -> bool {
    if start >= end {
      return false;
    }

    let gap = self.slice_range(start, end);
    let comment_start = gap.find("//").or_else(|| gap.find("/*"));

    match comment_start {
      Some(index) => !gap[..index].contains('\n'),
      None => false,
    }
  }

  fn push_comment_blocks(
    &self,
    output: &mut String,
    blocks: &[CommentBlock],
    indent_level: usize,
    trailing: bool,
  ) {
    for (index, block) in blocks.iter().enumerate() {
      if trailing && index == 0 && block.lines.len() == 1 {
        output.push(' ');
        output.push_str(block.lines[0].trim());
        continue;
      }

      if !output.ends_with('\n') {
        output.push('\n');
      }

      if index > 0
        && matches!(block.placement, CommentPlacement::Leading | CommentPlacement::Detached)
        && !output.ends_with("\n\n")
      {
        output.push('\n');
      }

      for line in &block.lines {
        output.push_str(&self.indent(indent_level));
        output.push_str(line.trim());
        output.push('\n');
      }
    }
  }

  fn comment_blocks_in_slice(
    &self,
    start: usize,
    end: usize,
  ) -> Result<Vec<crate::CommentBlock>, LayoutFailure> {
    if start >= end {
      return Ok(Vec::new());
    }

    let slice = self.slice_range(start, end);
    if slice.trim().is_empty() {
      return Ok(Vec::new());
    }

    let file = FormatFile::from_source(slice).map_err(LayoutFailure::Fatal)?;
    let mut blocks = Vec::new();

    for item in file.items {
      match item {
        FormatItem::DetachedComment(block) => blocks.push(block),
        FormatItem::Code(region) => {
          blocks.extend(region.leading);
          blocks.extend(region.trailing);

          if !region.raw.trim().is_empty() && !gap_code_is_ignorable(&region.raw) {
            return Err(LayoutFailure::unsupported("comment ownership gap contains non-comment syntax"));
          }
        },
        FormatItem::Directive(_) => {
          return Err(LayoutFailure::unsupported("comment ownership gap contains non-comment syntax"));
        },
      }
    }

    Ok(blocks)
  }

  fn preserve_single_blank_line_for_whitespace_gap(
    &self,
    output: &mut String,
    start: usize,
    end: usize,
  ) {
    if !self.whitespace_gap_has_intentional_blank_line(start, end) {
      return;
    }

    if output.ends_with('\n') {
      output.push('\n');
    } else {
      output.push_str("\n\n");
    }
  }

  fn whitespace_gap_has_intentional_blank_line(
    &self,
    start: usize,
    end: usize,
  ) -> bool {
    if start >= end {
      return false;
    }

    let gap = self.slice_range(start, end);

    // Skip ignorable trailing punctuation (commas, semicolons) at the start of the gap.
    // Variant and statement spans may not include their trailing punctuation token,
    // so the gap starts with ',' or ';' rather than whitespace.
    let gap = gap.trim_start_matches([',', ';']);

    // Count newlines in the leading whitespace of the gap only.
    // Stop at the first non-whitespace character (e.g. a leading doc comment) so
    // that blank lines before doc-commented items are detected correctly.
    let whitespace_prefix_len = gap.len() - gap.trim_start().len();
    let whitespace_prefix = &gap[..whitespace_prefix_len];

    whitespace_prefix.chars().filter(|c| *c == '\n').count() >= 2
  }

  fn comment_gap_preserves_detached_spacing(
    &self,
    start: usize,
    end: usize,
  ) -> bool {
    if start >= end {
      return false;
    }

    let gap = self.slice_range(start, end);
    let last_non_whitespace = gap.rfind(|character: char| !character.is_whitespace());

    let Some(last_non_whitespace) = last_non_whitespace else {
      return false;
    };

    gap[last_non_whitespace + 1..].chars().filter(|character| *character == '\n').count() >= 2
  }

  fn gap_contains_directive_syntax(
    &self,
    start: usize,
    end: usize,
  ) -> bool {
    if start >= end {
      return false;
    }

    self
      .slice_range(start, end)
      .lines()
      .any(|line| line.trim_start().starts_with('@'))
  }

  fn region_contains_raw_directive(
    &self,
    start: usize,
    end: usize,
  ) -> bool {
    if start >= end {
      return false;
    }

    self.slice_range(start, end).lines().any(|line| {
      let trimmed = line.trim_start();
      trimmed.starts_with("@configFlag(")
        || trimmed.starts_with("@if(")
        || trimmed.starts_with("@ifelse(")
        || trimmed.starts_with("@else")
    })
  }

  fn braced_body_is_empty(
    &self,
    start: usize,
    end: usize,
  ) -> bool {
    self.slice_range(start, end).trim().is_empty()
  }

  fn directive_prefixed_item_start(
    &self,
    node_id: &NodeId,
    lower_bound: usize,
  ) -> usize {
    let node_start = self.node_effective_start(node_id);
    let mut current_line_start = self.line_start(node_start);
    let mut candidate = current_line_start;
    let mut saw_directive = false;

    while current_line_start > lower_bound {
      let previous_line_end = current_line_start.saturating_sub(1);
      let previous_line_start = self.source[..previous_line_end]
        .rfind('\n')
        .map(|index| index + 1)
        .unwrap_or(0);
      let previous_line = &self.source[previous_line_start..previous_line_end];
      let trimmed = previous_line.trim();

      if trimmed.is_empty() {
        break;
      }

      if trimmed.starts_with('@') {
        saw_directive = true;
        candidate = previous_line_start;
        current_line_start = previous_line_start;
        continue;
      }

      if saw_directive && (trimmed.starts_with("//") || trimmed.starts_with("/*")) {
        candidate = previous_line_start;
        current_line_start = previous_line_start;
        continue;
      }

      break;
    }

    if saw_directive {
      candidate
    } else {
      node_start
    }
  }
}

fn gap_code_is_ignorable(code: &str) -> bool {
  code.trim().chars().all(|character| matches!(character, ';' | ','))
}

fn binary_precedence(operator: ASTBinaryOperator) -> u8 {
  match operator {
    ASTBinaryOperator::Or => 2,
    ASTBinaryOperator::And => 3,
    ASTBinaryOperator::Equal | ASTBinaryOperator::NotEqual => 4,
    ASTBinaryOperator::LessThan
    | ASTBinaryOperator::LessThanOrEqual
    | ASTBinaryOperator::GreaterThan
    | ASTBinaryOperator::GreaterThanOrEqual => 5,
    ASTBinaryOperator::BitOr => 6,
    ASTBinaryOperator::BitXor => 7,
    ASTBinaryOperator::BitAnd => 8,
    ASTBinaryOperator::ShiftLeft | ASTBinaryOperator::ShiftRight => 9,
    ASTBinaryOperator::Add | ASTBinaryOperator::Subtract => 10,
    ASTBinaryOperator::Multiply | ASTBinaryOperator::Divide | ASTBinaryOperator::Modulo => 11,
  }
}

fn expression_precedence(expression: &ASTExpression) -> u8 {
  match expression {
    ASTExpression::Assignment(_) => 1,
    ASTExpression::Ternary(_) => 1,
    ASTExpression::Binary(binary) => binary_precedence(binary.operator.clone()),
    ASTExpression::Grouped(_) => u8::MAX,
    ASTExpression::Call(_) | ASTExpression::MemberAccess(_) | ASTExpression::VectorAccess(_) => 12,
    ASTExpression::Cast(_) => 10,
    ASTExpression::Unary(_) => 11,
    ASTExpression::Reference(_) | ASTExpression::Dereference(_) => 11,
    ASTExpression::PostfixIncrement { .. } | ASTExpression::PostfixDecrement { .. } | ASTExpression::Try { .. } => 12,
    ASTExpression::Lambda(_) | ASTExpression::Match(_) => 0,
    _ => u8::MAX,
  }
}

fn binary_operator_text(operator: ASTBinaryOperator) -> &'static str {
  match operator {
    ASTBinaryOperator::Add => "+",
    ASTBinaryOperator::And => "&&",
    ASTBinaryOperator::BitAnd => "&",
    ASTBinaryOperator::BitOr => "|",
    ASTBinaryOperator::BitXor => "^",
    ASTBinaryOperator::Divide => "/",
    ASTBinaryOperator::Equal => "==",
    ASTBinaryOperator::GreaterThan => ">",
    ASTBinaryOperator::GreaterThanOrEqual => ">=",
    ASTBinaryOperator::LessThan => "<",
    ASTBinaryOperator::LessThanOrEqual => "<=",
    ASTBinaryOperator::Modulo => "%",
    ASTBinaryOperator::Multiply => "*",
    ASTBinaryOperator::NotEqual => "!=",
    ASTBinaryOperator::Or => "||",
    ASTBinaryOperator::ShiftLeft => "<<",
    ASTBinaryOperator::ShiftRight => ">>",
    ASTBinaryOperator::Subtract => "-",
  }
}

fn assignment_operator_text(operator: ASTAssignmentOperator) -> &'static str {
  match operator {
    ASTAssignmentOperator::Assign => "=",
    ASTAssignmentOperator::AddAssign => "+=",
    ASTAssignmentOperator::SubAssign => "-=",
    ASTAssignmentOperator::MulAssign => "*=",
    ASTAssignmentOperator::DivAssign => "/=",
    ASTAssignmentOperator::ModAssign => "%=",
    ASTAssignmentOperator::ShiftLeftAssign => "<<=",
    ASTAssignmentOperator::ShiftRightAssign => ">>=",
    ASTAssignmentOperator::BitAndAssign => "&=",
    ASTAssignmentOperator::BitOrAssign => "|=",
    ASTAssignmentOperator::BitXorAssign => "^=",
    ASTAssignmentOperator::NotAssign => "!=",
    ASTAssignmentOperator::AndAssign => "&&=",
    ASTAssignmentOperator::OrAssign => "||=",
  }
}

#[cfg(test)]
mod tests {
  use crate::{FormatFile, FormatterConfig, layout::layout_file, printer::print_file};

  #[test]
  fn preserves_import_prefix_order() {
    let file = FormatFile::from_source("import zoo from \"std::zoo\";\nimport alpha from \"std::alpha\";\n")
      .expect("build file");
    let layout = layout_file(&file, &FormatterConfig::default()).expect("layout file");
    let printed = print_file(&layout, &FormatterConfig::default()).expect("print file");

    assert!(printed.starts_with("import zoo"));
  }
}
