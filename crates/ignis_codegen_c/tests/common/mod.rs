use std::cell::RefCell;
use std::rc::Rc;

use ignis_analyzer::Analyzer;
use ignis_config::CHeader;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, Visibility};
use ignis_type::file::SourceMap;
use ignis_type::symbol::SymbolTable;

/// Compile Ignis source code to C.
pub fn compile_to_c(source: &str) -> String {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", source.to_string());
  let src = &sm.get(&file_id).text;

  let mut lexer = IgnisLexer::new(file_id, src);
  lexer.scan_tokens();
  assert!(lexer.diagnostics.is_empty(), "Lexer errors: {:?}", lexer.diagnostics);

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbol_table.clone());
  let (nodes, roots) = parser.parse().expect("Parse failed");

  let result = Analyzer::analyze(&nodes, &roots, symbol_table);
  let has_errors = result
    .diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));
  assert!(!has_errors, "Analyzer errors: {:?}", result.diagnostics);

  let mut types = result.types.clone();

  // Monomorphization
  let mono_roots = {
    let sym_table = result.symbols.borrow();
    collect_mono_roots(&result.defs, &sym_table)
  };
  let mono_output =
    ignis_analyzer::mono::Monomorphizer::new(&result.hir, &result.defs, &mut types, result.symbols.clone())
      .run(&mono_roots);

  let sym_table = result.symbols.borrow();

  // Run ownership analysis to produce drop schedules
  let ownership_checker =
    ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table);
  let (drop_schedules, _) = ownership_checker.check();

  let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
    &mono_output.hir,
    &mut types,
    &mono_output.defs,
    &sym_table,
    &drop_schedules,
    None,
  );

  if let Err(errors) = &verify_result {
    panic!("LIR verification errors: {:?}", errors);
  }

  let headers = vec![CHeader {
    path: "types.h".to_string(),
    quoted: true,
  }];

  ignis_codegen_c::emit_c(
    &lir_program,
    &types,
    &mono_output.defs,
    &result.namespaces,
    &sym_table,
    &headers,
  )
}

fn collect_mono_roots(
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> Vec<DefinitionId> {
  let mut roots = Vec::new();

  for (def_id, def) in defs.iter() {
    match &def.kind {
      DefinitionKind::Function(fd) if !fd.is_extern => {
        let name = symbols.get(&def.name);
        if name == "main" {
          roots.push(def_id);
        }
      },

      DefinitionKind::Function(fd) if def.visibility == Visibility::Public && !fd.is_extern => {
        roots.push(def_id);
      },

      DefinitionKind::Record(rd) if rd.type_params.is_empty() => {
        for method_id in rd.instance_methods.values() {
          roots.push(*method_id);
        }
        for method_id in rd.static_methods.values() {
          roots.push(*method_id);
        }
      },

      _ => {},
    }
  }

  roots
}
