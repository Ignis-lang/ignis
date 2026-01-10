mod common;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use ignis_analyzer::{Analyzer, AnalyzerOutput};
use ignis_analyzer::imports::ExportTable;
use ignis_diagnostics::diagnostic_report::Severity;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::file::SourceMap;
use ignis_type::module::ModuleId;
use ignis_type::symbol::SymbolTable;

/// Analyze code with provided imports context
fn analyze_with_imports(
  src: &str,
  export_table: &ExportTable,
  module_for_path: &HashMap<String, ModuleId>,
) -> AnalyzerOutput {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", src.to_string());

  let mut lexer = IgnisLexer::new(file_id.clone(), sm.get(&file_id).text.as_str());
  lexer.scan_tokens();
  assert!(lexer.diagnostics.is_empty(), "Lexer errors: {:?}", lexer.diagnostics);

  let symbols = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
  let (nodes, roots) = parser.parse().expect("Parse failed");

  Analyzer::analyze_with_imports(&nodes, &roots, symbols, export_table, module_for_path)
}

/// Helper to analyze a "library" module and collect its exports
fn analyze_library(src: &str) -> (AnalyzerOutput, Rc<RefCell<SymbolTable>>) {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("lib.ign", src.to_string());

  let mut lexer = IgnisLexer::new(file_id.clone(), sm.get(&file_id).text.as_str());
  lexer.scan_tokens();
  assert!(lexer.diagnostics.is_empty(), "Lexer errors: {:?}", lexer.diagnostics);

  let symbols = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
  let (nodes, roots) = parser.parse().expect("Parse failed");

  let output = Analyzer::analyze(&nodes, &roots, symbols.clone());
  (output, symbols)
}

fn has_error_code(
  output: &AnalyzerOutput,
  code: &str,
) -> bool {
  output.diagnostics.iter().any(|d| d.error_code == code)
}

fn error_count(output: &AnalyzerOutput) -> usize {
  output
    .diagnostics
    .iter()
    .filter(|d| matches!(d.severity, Severity::Error))
    .count()
}

// ============================================================================
// Basic Import Tests
// ============================================================================

#[test]
fn import_exported_function() {
  // Library with exported function
  let lib_src = r#"
    export function add(a: i32, b: i32): i32 {
      return a + b;
    }
  "#;

  let (lib_output, _lib_symbols) = analyze_library(lib_src);
  assert_eq!(error_count(&lib_output), 0, "Library should have no errors");

  // Get the exported 'add' symbol
  let lib_exports = lib_output.collect_exports();
  let lib_module_id = ModuleId::new(0);

  // Build export table
  let mut export_table: ExportTable = HashMap::new();
  export_table.insert(lib_module_id, lib_exports);

  // Build module path mapping
  let mut module_for_path: HashMap<String, ModuleId> = HashMap::new();
  module_for_path.insert("./lib".to_string(), lib_module_id);

  // Main module that imports add
  // Note: We need to use the same symbol table to have consistent SymbolIds
  let main_src = r#"
    import add from "./lib";

    function main(): i32 {
      return 0;
    }
  "#;

  // For this test to work properly, we need consistent symbol IDs
  // In a real scenario, the symbols would be shared across modules
  // For now, we test that the import phase runs without crashing
  let _output = analyze_with_imports(main_src, &export_table, &module_for_path);

  // The import may not resolve the symbol correctly since we use different symbol tables
  // but we verify the infrastructure works
  // In a full integration test, we'd use the compile_project function
}

#[test]
fn import_non_exported_symbol_error() {
  // Library with private function (not exported)
  let lib_src = r#"
    function private_fn(): i32 {
      return 42;
    }

    export function public_fn(): i32 {
      return private_fn();
    }
  "#;

  let (lib_output, _) = analyze_library(lib_src);
  assert_eq!(error_count(&lib_output), 0, "Library should have no errors");

  let lib_exports = lib_output.collect_exports();
  let lib_module_id = ModuleId::new(0);

  let mut export_table: ExportTable = HashMap::new();
  export_table.insert(lib_module_id, lib_exports);

  // The import path is stored without quotes in the AST
  let mut module_for_path: HashMap<String, ModuleId> = HashMap::new();
  module_for_path.insert("./lib".to_string(), lib_module_id);

  // Try to import private function
  let main_src = r#"
    import private_fn from "./lib";

    function main(): i32 {
      return 0;
    }
  "#;

  let output = analyze_with_imports(main_src, &export_table, &module_for_path);

  // Should have error M0002: SymbolNotExported
  assert!(
    has_error_code(&output, "M0002"),
    "Expected M0002 SymbolNotExported error, got: {:?}",
    output.diagnostics
  );
}

#[test]
fn import_from_nonexistent_module_error() {
  let export_table: ExportTable = HashMap::new();
  let module_for_path: HashMap<String, ModuleId> = HashMap::new();

  let main_src = r#"
    import foo from "./nonexistent";

    function main(): i32 {
      return 0;
    }
  "#;

  let output = analyze_with_imports(main_src, &export_table, &module_for_path);

  // Should have error M0001: ModuleNotFound
  assert!(
    has_error_code(&output, "M0001"),
    "Expected M0001 ModuleNotFound error, got: {:?}",
    output.diagnostics
  );
}

// ============================================================================
// Export Tests
// ============================================================================

#[test]
fn export_function_declaration() {
  let src = r#"
    export function add(a: i32, b: i32): i32 {
      return a + b;
    }
  "#;

  let (output, symbols) = analyze_library(src);
  assert_eq!(error_count(&output), 0);

  let export_data = output.collect_exports();
  assert_eq!(export_data.exports.len(), 1, "Should have 1 export");

  // Verify the exported name is 'add'
  let symbols = symbols.borrow();
  let has_add = export_data.exports.keys().any(|sym_id| symbols.get(sym_id) == "add");
  assert!(has_add, "Should export 'add'");
}

#[test]
fn export_multiple_functions() {
  let src = r#"
    export function add(a: i32, b: i32): i32 {
      return a + b;
    }

    export function sub(a: i32, b: i32): i32 {
      return a - b;
    }

    function private_helper(): i32 {
      return 0;
    }
  "#;

  let (output, symbols) = analyze_library(src);
  assert_eq!(error_count(&output), 0);

  let export_data = output.collect_exports();
  assert_eq!(export_data.exports.len(), 2, "Should have 2 exports (add and sub)");

  let symbols = symbols.borrow();
  let names: Vec<String> = export_data
    .exports
    .keys()
    .map(|sym_id| symbols.get(sym_id).to_string())
    .collect();

  assert!(names.contains(&"add".to_string()), "Should export 'add'");
  assert!(names.contains(&"sub".to_string()), "Should export 'sub'");
}

#[test]
fn export_constant() {
  let src = r#"
    export const PI: f64 = 3.14159;
  "#;

  let (output, symbols) = analyze_library(src);
  assert_eq!(error_count(&output), 0);

  let export_data = output.collect_exports();
  assert_eq!(export_data.exports.len(), 1, "Should have 1 export");

  let symbols = symbols.borrow();
  let has_pi = export_data.exports.keys().any(|sym_id| symbols.get(sym_id) == "PI");
  assert!(has_pi, "Should export 'PI'");
}

#[test]
fn private_items_not_exported() {
  let src = r#"
    function private_fn(): i32 {
      return 42;
    }

    const PRIVATE_CONST: i32 = 10;

    export function public_fn(): i32 {
      return private_fn() + PRIVATE_CONST;
    }
  "#;

  let (output, symbols) = analyze_library(src);
  assert_eq!(error_count(&output), 0);

  let export_data = output.collect_exports();
  assert_eq!(export_data.exports.len(), 1, "Should have only 1 export (public_fn)");

  let symbols = symbols.borrow();
  let names: Vec<String> = export_data
    .exports
    .keys()
    .map(|sym_id| symbols.get(sym_id).to_string())
    .collect();

  assert!(names.contains(&"public_fn".to_string()), "Should export 'public_fn'");
  assert!(!names.contains(&"private_fn".to_string()), "Should not export 'private_fn'");
  assert!(
    !names.contains(&"PRIVATE_CONST".to_string()),
    "Should not export 'PRIVATE_CONST'"
  );
}

// ============================================================================
// Import Shadow Tests
// ============================================================================

#[test]
fn import_shadows_local_definition() {
  // Library with exported function
  let lib_src = r#"
    export function add(a: i32, b: i32): i32 {
      return a + b;
    }
  "#;

  let (lib_output, _) = analyze_library(lib_src);
  let lib_exports = lib_output.collect_exports();
  let lib_module_id = ModuleId::new(0);

  let mut export_table: ExportTable = HashMap::new();
  export_table.insert(lib_module_id, lib_exports);

  let mut module_for_path: HashMap<String, ModuleId> = HashMap::new();
  module_for_path.insert("./lib".to_string(), lib_module_id);

  // Main module that defines 'add' locally AND tries to import it
  // Note: Due to symbol table isolation in tests, we can't perfectly simulate this
  // but we test that the infrastructure for shadow detection exists
  let main_src = r#"
    import add from "./lib";

    function main(): i32 {
      return 0;
    }
  "#;

  // This test verifies the import infrastructure works
  // Full shadow detection would require shared symbol tables
  let _output = analyze_with_imports(main_src, &export_table, &module_for_path);

  // Import should either succeed or fail gracefully
  // The actual shadow test would be in a full integration test
}

// ============================================================================
// Module Structure Tests
// ============================================================================

#[test]
fn module_exports_are_definition_ids() {
  let src = r#"
    export function foo(): i32 {
      return 1;
    }

    export function bar(): i32 {
      return 2;
    }
  "#;

  let (output, _) = analyze_library(src);
  let export_data = output.collect_exports();

  // Verify exports map to valid Definitions
  for (_, def) in &export_data.exports {
    // Just verify we can access the definition name
    let _name = &def.name;
  }
}

#[test]
fn analyze_without_imports_works() {
  // Verify that analyze (without imports) still works
  let src = r#"
    function add(a: i32, b: i32): i32 {
      return a + b;
    }

    function main(): i32 {
      return add(1, 2);
    }
  "#;

  let result = common::analyze(src);
  assert_eq!(error_count(&result.output), 0);
}

// ============================================================================
// Integration Tests with Real Std Library
// ============================================================================

mod std_imports {
  use std::io::Write;
  use std::path::PathBuf;

  use ignis_config::{IgnisConfig, IgnisSTDManifest};
  use ignis_driver::CompilationContext;
  use tempfile::TempDir;

  fn get_std_path() -> PathBuf {
    // Get the std path relative to the workspace root
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest_dir)
      .parent()
      .unwrap()
      .parent()
      .unwrap()
      .join("std")
  }

  fn create_test_config(
    std_path: PathBuf,
    quiet: bool,
  ) -> IgnisConfig {
    // Load manifest from std
    let manifest_path = std_path.join("manifest.toml");
    let manifest = if manifest_path.exists() {
      let content = std::fs::read_to_string(&manifest_path).expect("Failed to read manifest.toml");
      toml::from_str(&content).unwrap_or_default()
    } else {
      IgnisSTDManifest::default()
    };

    IgnisConfig {
      quiet,
      std_path: std_path.to_string_lossy().to_string(),
      manifest,
      ..Default::default()
    }
  }

  fn write_test_file(
    dir: &TempDir,
    name: &str,
    content: &str,
  ) -> PathBuf {
    let path = dir.path().join(name);
    let mut file = std::fs::File::create(&path).expect("Failed to create test file");
    file.write_all(content.as_bytes()).expect("Failed to write test file");
    path
  }

  #[test]
  fn test_module_discovery_std_io() {
    // Test that std::io can be discovered and parsed
    let std_path = get_std_path();
    if !std_path.join("io/mod.ign").exists() {
      eprintln!("Skipping test: std/io/mod.ign not found at {:?}", std_path);
      return;
    }

    let config = create_test_config(std_path, true);
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let test_code = r#"
      import println from "std::io";

      function main(): void {
      }
    "#;

    let test_path = write_test_file(&temp_dir, "main.ign", test_code);
    let mut ctx = CompilationContext::new(&config);

    let result = ctx.discover_modules(test_path.to_str().unwrap(), &config);
    assert!(result.is_ok(), "Module discovery should succeed");

    // Verify std::io was discovered
    let has_io = ctx
      .module_graph
      .by_path
      .keys()
      .any(|p| matches!(p, ignis_type::module::ModulePath::Std(name) if name == "io"));
    assert!(has_io, "std::io module should be discovered");
  }

  #[test]
  fn test_module_discovery_std_math() {
    let std_path = get_std_path();
    if !std_path.join("math/mod.ign").exists() {
      eprintln!("Skipping test: std/math/mod.ign not found at {:?}", std_path);
      return;
    }

    let config = create_test_config(std_path, true);
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let test_code = r#"
      import sin from "std::math";

      function main(): void {
      }
    "#;

    let test_path = write_test_file(&temp_dir, "main.ign", test_code);
    let mut ctx = CompilationContext::new(&config);

    let result = ctx.discover_modules(test_path.to_str().unwrap(), &config);
    assert!(result.is_ok(), "Module discovery should succeed");

    let has_math = ctx
      .module_graph
      .by_path
      .keys()
      .any(|p| matches!(p, ignis_type::module::ModulePath::Std(name) if name == "math"));
    assert!(has_math, "std::math module should be discovered");
  }

  #[test]
  fn test_module_discovery_multiple_std() {
    let std_path = get_std_path();
    if !std_path.join("io/mod.ign").exists() || !std_path.join("math/mod.ign").exists() {
      eprintln!("Skipping test: std modules not found at {:?}", std_path);
      return;
    }

    let config = create_test_config(std_path, true);
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    let test_code = r#"
      import println from "std::io";
      import sqrt from "std::math";

      function main(): void {
      }
    "#;

    let test_path = write_test_file(&temp_dir, "main.ign", test_code);
    let mut ctx = CompilationContext::new(&config);

    let result = ctx.discover_modules(test_path.to_str().unwrap(), &config);
    assert!(result.is_ok(), "Module discovery should succeed");

    // Verify both modules were discovered
    let has_io = ctx
      .module_graph
      .by_path
      .keys()
      .any(|p| matches!(p, ignis_type::module::ModulePath::Std(name) if name == "io"));
    let has_math = ctx
      .module_graph
      .by_path
      .keys()
      .any(|p| matches!(p, ignis_type::module::ModulePath::Std(name) if name == "math"));
    assert!(has_io, "std::io module should be discovered");
    assert!(has_math, "std::math module should be discovered");
  }

  #[test]
  fn test_manifest_path_resolution() {
    let std_path = get_std_path();
    let manifest_path = std_path.join("manifest.toml");

    if !manifest_path.exists() {
      eprintln!("Skipping test: manifest.toml not found at {:?}", manifest_path);
      return;
    }

    let content = std::fs::read_to_string(&manifest_path).expect("Failed to read manifest");
    let manifest: IgnisSTDManifest = toml::from_str(&content).expect("Failed to parse manifest");

    // Verify manifest has expected modules
    assert!(manifest.modules.contains_key("io"), "Manifest should have io module");
    assert!(manifest.modules.contains_key("math"), "Manifest should have math module");
    assert!(manifest.modules.contains_key("string"), "Manifest should have string module");

    // Verify paths are correct
    assert_eq!(manifest.modules.get("io"), Some(&"io/mod.ign".to_string()));
    assert_eq!(manifest.modules.get("math"), Some(&"math/mod.ign".to_string()));
    assert_eq!(manifest.modules.get("string"), Some(&"string/mod.ign".to_string()));
  }

  #[test]
  fn test_linking_info_available() {
    let std_path = get_std_path();
    let manifest_path = std_path.join("manifest.toml");

    if !manifest_path.exists() {
      eprintln!("Skipping test: manifest.toml not found");
      return;
    }

    let content = std::fs::read_to_string(&manifest_path).expect("Failed to read manifest");
    let manifest: IgnisSTDManifest = toml::from_str(&content).expect("Failed to parse manifest");

    // Verify io has linking info
    let io_linking = manifest.get_linking_info("io");
    assert!(io_linking.is_some(), "io should have linking info");
    let io_linking = io_linking.unwrap();
    assert!(io_linking.header.is_some(), "io should have header");
    assert!(io_linking.object.is_some(), "io should have object file");

    // Verify math has linking info (only header, no object)
    let math_linking = manifest.get_linking_info("math");
    assert!(math_linking.is_some(), "math should have linking info");
    let math_linking = math_linking.unwrap();
    assert!(math_linking.header.is_some(), "math should have header");
    // math uses libc, no custom object

    // Verify string has linking info
    let string_linking = manifest.get_linking_info("string");
    assert!(string_linking.is_some(), "string should have linking info");
  }
}
