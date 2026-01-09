use std::fmt::Write;

use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
  types::TypeStore,
  symbol::SymbolTable,
};
use ignis_hir::HIR;

/// Dump types in simple format
pub fn dump_types(types: &TypeStore) -> String {
  let mut output = String::new();

  writeln!(&mut output, "=== Type Store ===").unwrap();

  writeln!(&mut output, "  i8: {:?}", types.i8()).unwrap();
  writeln!(&mut output, "  i16: {:?}", types.i16()).unwrap();
  writeln!(&mut output, "  i32: {:?}", types.i32()).unwrap();
  writeln!(&mut output, "  i64: {:?}", types.i64()).unwrap();
  writeln!(&mut output, "  u8: {:?}", types.u8()).unwrap();
  writeln!(&mut output, "  u16: {:?}", types.u16()).unwrap();
  writeln!(&mut output, "  u32: {:?}", types.u32()).unwrap();
  writeln!(&mut output, "  u64: {:?}", types.u64()).unwrap();
  writeln!(&mut output, "  f32: {:?}", types.f32()).unwrap();
  writeln!(&mut output, "  f64: {:?}", types.f64()).unwrap();
  writeln!(&mut output, "  bool: {:?}", types.boolean()).unwrap();
  writeln!(&mut output, "  char: {:?}", types.char()).unwrap();
  writeln!(&mut output, "  string: {:?}", types.string()).unwrap();
  writeln!(&mut output, "  void: {:?}", types.void()).unwrap();
  writeln!(&mut output, "  never: {:?}", types.never()).unwrap();
  writeln!(&mut output, "  unknown: {:?}", types.unknown()).unwrap();
  writeln!(&mut output, "  error: {:?}", types.error()).unwrap();

  output
}

/// Dump definitions in simple format
pub fn dump_defs(
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
) -> String {
  let mut output = String::new();

  writeln!(&mut output, "=== Definitions ===").unwrap();

  // Get all definitions
  let all_defs = defs.get_all();

  for (idx, def) in all_defs.iter().enumerate() {
    let def_id = DefinitionId::new(idx as u32);
    let type_id = defs.type_of(&def_id);
    let type_kind = types.get(type_id);
    let name = symbols.get(&def.name).to_string();

    match &def.kind {
      DefinitionKind::Function(func_def) => {
        writeln!(&mut output, "  DefId({:?}): function {} -> {:?}", def_id, name, type_kind).unwrap();
        writeln!(&mut output, "    params: {:?}", func_def.params).unwrap();
        writeln!(&mut output, "    return_type: {:?}", func_def.return_type).unwrap();
        writeln!(&mut output, "    is_extern: {}", func_def.is_extern).unwrap();
      },
      DefinitionKind::Variable(var_def) => {
        writeln!(
          &mut output,
          "  DefId({:?}): variable {}: {:?} (mut: {})",
          def_id, name, type_kind, var_def.mutable
        )
        .unwrap();
      },
      DefinitionKind::Constant(const_def) => {
        writeln!(&mut output, "  DefId({:?}): constant {}: {:?}", def_id, name, type_kind).unwrap();
        if let Some(value) = &const_def.value {
          writeln!(&mut output, "    value: {:?}", value).unwrap();
        }
      },
      DefinitionKind::Parameter(param_def) => {
        writeln!(
          &mut output,
          "  DefId({:?}): parameter {}: {:?} (mut: {})",
          def_id, name, type_kind, param_def.mutable
        )
        .unwrap();
      },
    }
  }

  output
}

/// Dump HIR for a specific function
pub fn dump_hir_function(
  hir: &HIR,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  function_name: &str,
) -> Result<String, String> {
  // Find function by name
  let all_defs = defs.get_all();
  let func_def_id = all_defs
    .iter()
    .enumerate()
    .find(|(_, def)| {
      matches!(&def.kind, DefinitionKind::Function(_)) && symbols.get(&def.name).to_string() == function_name
    })
    .map(|(idx, _)| DefinitionId::new(idx as u32))
    .ok_or_else(|| format!("Function '{}' not found", function_name))?;

  let mut output = String::new();

  writeln!(&mut output, "=== HIR for function '{}' ===", function_name).unwrap();
  writeln!(&mut output, "DefId: {:?}", func_def_id).unwrap();

  if let Some(body) = hir.function_bodies.get(&func_def_id) {
    writeln!(&mut output, "\nBody:").unwrap();
    writeln!(&mut output, "{:#?}", body).unwrap();
  } else {
    writeln!(&mut output, "  (no body)").unwrap();
  }

  Ok(output)
}

/// Dump HIR summary
pub fn dump_hir_summary(
  hir: &HIR,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  let mut output = String::new();

  writeln!(&mut output, "=== HIR Summary ===").unwrap();
  writeln!(&mut output, "Items: {}", hir.items.len()).unwrap();
  writeln!(&mut output, "Function bodies: {}", hir.function_bodies.len()).unwrap();
  writeln!(&mut output, "Variable inits: {}", hir.variables_inits.len()).unwrap();

  if let Some(entry_def_id) = &hir.entry_point {
    let entry_def = defs.get(entry_def_id);
    let name = symbols.get(&entry_def.name).to_string();
    writeln!(&mut output, "Entry point: DefId({:?}) [{}]", entry_def_id, name).unwrap();
  } else {
    writeln!(&mut output, "Entry point: None").unwrap();
  }

  writeln!(&mut output, "\nFunctions with bodies:").unwrap();
  for (def_id, _) in &hir.function_bodies {
    let def = defs.get(def_id);
    let name = symbols.get(&def.name).to_string();
    writeln!(&mut output, "  - DefId({:?}): {}", def_id, name).unwrap();
  }

  output
}

/// Dump complete HIR with pretty printing
pub fn dump_hir_complete(
  hir: &HIR,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  ignis_hir::display::print_hir(hir, types, defs, symbols)
}
