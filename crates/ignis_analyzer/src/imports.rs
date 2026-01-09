use std::collections::HashMap;

use ignis_ast::{ASTNode, NodeId, statements::ASTStatement};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::{
  definition::{Definition, DefinitionKind, DefinitionStore, FunctionDefinition},
  module::ModuleId,
  symbol::SymbolId,
  types::{Type, TypeId, TypeStore},
};

use crate::Analyzer;

#[derive(Clone)]
pub struct ModuleExportData {
  pub exports: HashMap<SymbolId, Definition>,
  pub types: TypeStore,
  pub defs: DefinitionStore,
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
        let item_name = self.get_symbol_name(&item);

        if let Some(existing_def_id) = self.scopes.lookup(&item) {
          let existing_def = self.defs.get(existing_def_id);
          self.add_diagnostic(
            DiagnosticMessage::ImportShadowsLocal {
              name: item_name.clone(),
              at: span.clone(),
              previous_span: existing_def.span.clone(),
            }
            .report(),
          );
          continue;
        }

        match export_data.exports.get(&item) {
          Some(def) => {
            let local_def = self.translate_definition(def, &export_data.types, &export_data.defs);
            let local_def_id = self.defs.alloc(local_def);
            let _ = self.scopes.define(&item, &local_def_id);
          },
          None => {
            self.add_diagnostic(
              DiagnosticMessage::SymbolNotExported {
                symbol: item_name,
                module: import_path.clone(),
                at: span.clone(),
              }
              .report(),
            );
          },
        }
      }
    }
  }

  fn translate_definition(
    &mut self,
    def: &Definition,
    source_types: &TypeStore,
    source_defs: &DefinitionStore,
  ) -> Definition {
    let translated_kind = match &def.kind {
      DefinitionKind::Function(func_def) => {
        let translated_return = self.translate_type(&func_def.return_type, source_types);
        let mut translated_params = Vec::new();
        for param_id in &func_def.params {
          let param_def = source_defs.get(param_id);
          let translated_param = self.translate_definition(param_def, source_types, source_defs);
          let local_param_id = self.defs.alloc(translated_param);
          translated_params.push(local_param_id);
        }

        DefinitionKind::Function(FunctionDefinition {
          params: translated_params,
          return_type: translated_return,
          is_extern: func_def.is_extern,
          is_variadic: func_def.is_variadic,
        })
      },
      DefinitionKind::Variable(var_def) => {
        let translated_type = self.translate_type(&var_def.type_id, source_types);
        DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
          type_id: translated_type,
          mutable: var_def.mutable,
        })
      },
      DefinitionKind::Constant(const_def) => {
        let translated_type = self.translate_type(&const_def.type_id, source_types);
        DefinitionKind::Constant(ignis_type::definition::ConstantDefinition {
          type_id: translated_type,
          value: const_def.value.clone(),
        })
      },
      DefinitionKind::Parameter(param_def) => {
        let translated_type = self.translate_type(&param_def.type_id, source_types);
        DefinitionKind::Parameter(ignis_type::definition::ParameterDefinition {
          type_id: translated_type,
          mutable: param_def.mutable,
        })
      },
    };

    Definition {
      kind: translated_kind,
      name: def.name.clone(),
      span: def.span.clone(),
      visibility: def.visibility.clone(),
    }
  }

  fn translate_type(
    &mut self,
    source_type_id: &TypeId,
    source_types: &TypeStore,
  ) -> TypeId {
    let source_type = source_types.get(source_type_id);

    match source_type {
      Type::I8 => self.types.i8(),
      Type::I16 => self.types.i16(),
      Type::I32 => self.types.i32(),
      Type::I64 => self.types.i64(),
      Type::U8 => self.types.u8(),
      Type::U16 => self.types.u16(),
      Type::U32 => self.types.u32(),
      Type::U64 => self.types.u64(),
      Type::F32 => self.types.f32(),
      Type::F64 => self.types.f64(),
      Type::Boolean => self.types.boolean(),
      Type::Char => self.types.char(),
      Type::String => self.types.string(),
      Type::Void => self.types.void(),
      Type::Never => self.types.never(),
      Type::Unknown => self.types.unknown(),
      Type::Error => self.types.error(),

      Type::Pointer(inner) => {
        let local_inner = self.translate_type(inner, source_types);
        self.types.pointer(local_inner)
      },
      Type::Reference { inner, mutable } => {
        let local_inner = self.translate_type(inner, source_types);
        self.types.reference(local_inner, *mutable)
      },
      Type::Vector { element, size } => {
        let local_element = self.translate_type(element, source_types);
        self.types.vector(local_element, *size)
      },
      Type::Tuple(elements) => {
        let local_elements: Vec<TypeId> = elements
          .iter()
          .map(|e| self.translate_type(e, source_types))
          .collect();
        self.types.tuple(local_elements)
      },
      Type::Function { params, ret, is_variadic } => {
        let local_params: Vec<TypeId> = params
          .iter()
          .map(|p| self.translate_type(p, source_types))
          .collect();
        let local_return = self.translate_type(ret, source_types);
        self.types.function(local_params, local_return, *is_variadic)
      },
    }
  }
}
