use std::fs;
use std::path::PathBuf;
use std::process::Command;

use serde_json::Value;

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

fn fixture(
  name: &str,
  source: &str,
) -> PathBuf {
  let dir = std::env::temp_dir().join(format!("ignis-doc-{}", std::process::id()));
  fs::create_dir_all(&dir).expect("create fixture directory");

  let path = dir.join(format!("{name}.ign"));
  fs::write(&path, source).expect("write fixture");

  path
}

fn document(
  name: &str,
  source: &str,
) -> Value {
  let path = fixture(name, source);

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("doc")
    .arg(&path)
    .arg("--std-path")
    .arg(workspace_std_path())
    .output()
    .expect("run ignis doc");

  assert!(
    output.status.success(),
    "ignis doc failed: {}",
    String::from_utf8_lossy(&output.stderr)
  );

  serde_json::from_slice(&output.stdout).expect("parse the emitted document")
}

fn find<'a>(
  package: &'a Value,
  path: &str,
) -> &'a Value {
  package["items"]
    .as_array()
    .expect("items array")
    .iter()
    .find(|item| item["path"] == path)
    .unwrap_or_else(|| panic!("no item at path {path}"))
}

#[test]
fn documents_a_function_with_its_doc_comment() {
  let package = document(
    "adds",
    r#"
/// Adds two numbers.
export function add(a: i32, b: i32): i32 {
  return a + b;
}
"#,
  );

  let item = find(&package, "adds::add");

  assert_eq!(item["kind"], "function");
  assert_eq!(item["visibility"], "public");
  assert_eq!(item["signature"], "function add(a: i32, b: i32): i32");
  assert_eq!(item["doc"], "Adds two numbers.");
}

#[test]
fn reports_private_declarations_without_dropping_them() {
  let package = document(
    "helper",
    r#"
/// Not exported.
function helper(): i32 {
  return 0;
}
"#,
  );

  assert_eq!(find(&package, "helper::helper")["visibility"], "private");
}

#[test]
fn renders_records_with_their_fields_and_methods() {
  let package = document(
    "counter",
    r#"
/// A counter.
export record Counter {
  /// How far it has counted.
  public value: i32;

  get(&self): i32 {
    return self.value;
  }

  public static new(start: i32): Counter {
    return Counter { value: start };
  }
}
"#,
  );

  let item = find(&package, "counter::Counter");
  assert_eq!(item["kind"], "record");
  assert_eq!(item["signature"], "record Counter");
  assert_eq!(item["doc"], "A counter.");

  let members = item["members"].as_array().expect("members array");

  let field = members.iter().find(|m| m["name"] == "value").expect("value field");
  assert_eq!(field["kind"], "field");
  assert_eq!(field["signature"], "value: i32");
  assert_eq!(field["doc"], "How far it has counted.");

  // The receiver reads as it is written in source, not as a typed first parameter.
  let getter = members.iter().find(|m| m["name"] == "get").expect("get method");
  assert_eq!(getter["signature"], "get(&self): i32");

  let constructor = members.iter().find(|m| m["name"] == "new").expect("new method");
  assert_eq!(constructor["signature"], "static new(start: i32): Counter");
}

#[test]
fn renders_enum_variants_with_their_payloads() {
  let package = document(
    "outcome",
    r#"
/// A result of sorts.
export enum Outcome {
  DONE(i32),
  FAILED,
}
"#,
  );

  let item = find(&package, "outcome::Outcome");
  assert_eq!(item["kind"], "enum");

  let members = item["members"].as_array().expect("members array");
  let done = members.iter().find(|m| m["name"] == "DONE").expect("DONE variant");
  let failed = members.iter().find(|m| m["name"] == "FAILED").expect("FAILED variant");

  assert_eq!(done["signature"], "DONE(i32)");
  assert_eq!(failed["signature"], "FAILED");
}

#[test]
fn qualifies_namespace_members_with_the_namespace_path() {
  let package = document(
    "math",
    r#"
namespace Math {
  /// Adds two numbers.
  function add(a: i32, b: i32): i32 {
    return a + b;
  }
}
"#,
  );

  let item = find(&package, "math::Math::add");
  assert_eq!(item["kind"], "function");
  assert_eq!(item["doc"], "Adds two numbers.");

  // A namespace member is not exported, yet it is the module's callable surface. The
  // package reports that rather than dropping it, so a renderer can decide.
  assert_eq!(item["visibility"], "private");
}

#[test]
fn carries_the_modules_own_documentation() {
  let package = document(
    "described",
    r#"
//! # The module
//!
//! What the file as a whole is for.

/// A function.
export function noop(): void {
  return;
}
"#,
  );

  let module = package["modules"]
    .as_array()
    .expect("modules array")
    .iter()
    .find(|entry| entry["name"] == "described")
    .expect("the module");

  assert_eq!(module["doc"], "# The module  \n  \nWhat the file as a whole is for.");

  // The block at the top of the file documents the module, not the first declaration.
  assert_eq!(find(&package, "described::noop")["doc"], "A function.");
}

#[test]
fn writes_to_a_file_when_asked() {
  let path = fixture("written", "export function noop(): void {\n  return;\n}\n");
  let out = path.with_extension("json");

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("doc")
    .arg(&path)
    .arg("--std-path")
    .arg(workspace_std_path())
    .arg("--output")
    .arg(&out)
    .output()
    .expect("run ignis doc");

  assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));
  assert!(output.stdout.is_empty(), "the document went to stdout as well as the file");

  let written = fs::read_to_string(&out).expect("read the written document");
  let package: Value = serde_json::from_str(&written).expect("parse the written document");

  assert!(
    package["items"]
      .as_array()
      .expect("items array")
      .iter()
      .any(|item| item["name"] == "noop")
  );
}
