use std::collections::HashMap;

use ignis_ast::{ASTNode, NodeId, statements::ASTStatement};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, NamespaceDefinition},
  module::ModuleId,
  symbol::SymbolId,
};

use crate::Analyzer;

#[derive(Clone)]
pub struct ModuleExportData {
  /// Map from symbol name to the original DefinitionId.
  /// Since we use shared stores, the definition is already in the shared DefinitionStore.
  pub exports: HashMap<SymbolId, DefinitionId>,
}

/// All exports indexed by module
pub type ExportTable = HashMap<ModuleId, ModuleExportData>;

impl<'a> Analyzer<'a> {
  pub fn import_phase(
    &mut self,
    roots: &[NodeId],
    export_table: &ExportTable,
    module_for_path: &HashMap<String, ModuleId>,
  ) {
    for root in roots {
      self.process_import(root, export_table, module_for_path);
    }
  }

  fn process_import(
    &mut self,
    node_id: &NodeId,
    export_table: &ExportTable,
    module_for_path: &HashMap<String, ModuleId>,
  ) {
    let node = self.ast.get(node_id);

    if let ASTNode::Statement(ASTStatement::Import(import)) = node {
      let import_path = import.from.clone();
      let items = import.items.clone();
      let span = import.span.clone();

      let source_module = match module_for_path.get(&import_path) {
        Some(id) => *id,
        None => {
          self.add_diagnostic(
            DiagnosticMessage::ModuleNotFound {
              path: import_path,
              at: span,
            }
            .report(),
          );
          return;
        },
      };

      let export_data = match export_table.get(&source_module) {
        Some(e) => e,
        None => {
          self.add_diagnostic(
            DiagnosticMessage::ModuleNotFound {
              path: import_path,
              at: span,
            }
            .report(),
          );
          return;
        },
      };

      for item in items {
        let item_name = self.get_symbol_name(&item.name);

        if let Some(existing_def_id) = self.scopes.lookup_def(&item.name) {
          let existing_def = self.defs.get(existing_def_id);
          self.add_diagnostic(
            DiagnosticMessage::ImportShadowsLocal {
              name: item_name.clone(),
              at: item.span.clone(),
              previous_span: existing_def.span.clone(),
            }
            .report(),
          );
          continue;
        }

        match export_data.exports.get(&item.name) {
          Some(&def_id) => {
            let def_kind = self.defs.get(&def_id).kind.clone();
            self.set_import_item_def(&item.span, &def_id);

            if let DefinitionKind::Namespace(ns_def) = &def_kind {
              self.import_namespace(def_id, ns_def);
            } else {
              let _ = self.scopes.define(&item.name, &def_id, false);
            }
          },
          None => {
            self.add_diagnostic(
              DiagnosticMessage::SymbolNotExported {
                symbol: item_name,
                module: import_path.clone(),
                at: item.span.clone(),
              }
              .report(),
            );
          },
        }
      }
    }
  }

  fn import_namespace(
    &mut self,
    ns_def_id: DefinitionId,
    ns_def: &NamespaceDefinition,
  ) {
    let ns = self.namespaces.get(&ns_def.namespace_id);
    let _ = self.scopes.define(&ns.name, &ns_def_id, false);
  }
}
