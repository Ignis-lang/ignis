use ignis_ast::{ASTNode, NodeId, statements::ASTStatement};
use ignis_diagnostics::{diagnostic_report::Severity, message::DiagnosticMessage};
use ignis_type::{
  definition::{DefinitionKind, FunctionDefinition, MethodDefinition},
  file::FileId,
  lint::{LintId, LintLevel},
};

use crate::Analyzer;

impl<'a> Analyzer<'a> {
  pub fn lint_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    self.collect_lint_directives(roots);
    self.lint_unused_variables();
    self.lint_unused_imports();
    self.lint_deprecated_calls();
  }

  fn collect_lint_directives(
    &mut self,
    roots: &[NodeId],
  ) {
    for root in roots {
      let node = self.ast.get(root);

      if let ASTNode::Statement(stmt) = node {
        let ast_attrs = match stmt {
          ASTStatement::Function(f) => f.signature.attrs.clone(),
          ASTStatement::Export(ignis_ast::statements::ASTExport::Declaration { decl, .. }) => {
            let inner = self.ast.get(decl);
            if let ASTNode::Statement(ASTStatement::Function(f)) = inner {
              f.signature.attrs.clone()
            } else {
              continue;
            }
          },
          _ => continue,
        };

        let directives = self.bind_lint_directives(&ast_attrs);
        self.push_lint_overrides(&directives);
      }
    }
  }

  fn lint_unused_variables(&mut self) {
    let level = self.effective_lint_level(LintId::UnusedVariable);
    if level == LintLevel::Allow {
      return;
    }

    let severity = match level {
      LintLevel::Warn => Severity::Warning,
      LintLevel::Deny => Severity::Error,
      LintLevel::Allow => unreachable!(),
    };

    let symbols = self.symbols.borrow();
    let mut diagnostics = Vec::new();

    for (def_id, def) in self.defs.iter() {
      if !matches!(def.kind, DefinitionKind::Variable(_)) {
        continue;
      }

      if def.span.file == FileId::SYNTHETIC {
        continue;
      }

      let name = symbols.get(&def.name);
      if name.starts_with('_') {
        continue;
      }

      if self.referenced_defs.contains(&def_id) {
        continue;
      }

      diagnostics.push(
        DiagnosticMessage::UnusedVariable {
          name: name.to_string(),
          span: def.span.clone(),
        }
        .report_with_severity(severity.clone()),
      );
    }

    drop(symbols);
    self.diagnostics.extend(diagnostics);
  }

  fn lint_unused_imports(&mut self) {
    let level = self.effective_lint_level(LintId::UnusedImport);
    if level == LintLevel::Allow {
      return;
    }

    let severity = match level {
      LintLevel::Warn => Severity::Warning,
      LintLevel::Deny => Severity::Error,
      LintLevel::Allow => unreachable!(),
    };

    let symbols = self.symbols.borrow();
    let mut diagnostics = Vec::new();

    for (def_id, import_span) in &self.imported_defs {
      if self.referenced_defs.contains(def_id) {
        continue;
      }

      let def = self.defs.get(def_id);
      let name = symbols.get(&def.name);

      diagnostics.push(
        DiagnosticMessage::UnusedImport {
          name: name.to_string(),
          span: import_span.clone(),
        }
        .report_with_severity(severity.clone()),
      );
    }

    drop(symbols);
    self.diagnostics.extend(diagnostics);
  }

  fn lint_deprecated_calls(&mut self) {
    let level = self.effective_lint_level(LintId::Deprecated);
    if level == LintLevel::Allow {
      return;
    }

    let severity = match level {
      LintLevel::Warn => Severity::Warning,
      LintLevel::Deny => Severity::Error,
      LintLevel::Allow => unreachable!(),
    };

    let symbols = self.symbols.borrow();
    let mut diagnostics = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for (node_id, def_id) in &self.resolved_calls {
      let call_span = self.node_span(node_id).clone();

      // Deduplicate: resolved_calls stores entries for both the call expression
      // and the callee node. Use (def_id, span.file, span.start) to emit only once.
      if !seen.insert((*def_id, call_span.file, call_span.start)) {
        continue;
      }

      let def = self.defs.get(def_id);

      let deprecated_msg = match &def.kind {
        DefinitionKind::Function(FunctionDefinition { attrs, .. })
        | DefinitionKind::Method(MethodDefinition { attrs, .. }) => attrs.iter().find_map(|a| match a {
          ignis_type::attribute::FunctionAttr::Deprecated(msg) => Some(msg.clone()),
          _ => None,
        }),
        _ => None,
      };

      if let Some(msg) = deprecated_msg {
        let name = symbols.get(&def.name);

        diagnostics.push(
          DiagnosticMessage::DeprecatedCall {
            name: name.to_string(),
            message: msg.unwrap_or_default(),
            span: call_span,
          }
          .report_with_severity(severity.clone()),
        );
      }
    }

    drop(symbols);
    self.diagnostics.extend(diagnostics);
  }
}
