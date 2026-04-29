mod common;

use std::fs;
use std::path::PathBuf;

use ignis_config::{IgnisSTDManifest, TargetBackend};

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

fn load_std_manifest() -> IgnisSTDManifest {
  let manifest_path = workspace_std_path().join("manifest.toml");
  let content = fs::read_to_string(&manifest_path).expect("read std manifest");
  toml::from_str(&content).expect("parse std manifest")
}

#[test]
fn std_compile_manifest_registers_compile_only_module_without_runtime_linking() {
  let manifest = load_std_manifest();

  assert_eq!(
    manifest.get_module_path("compile").map(|path| path.as_str()),
    Some("compile/mod.ign")
  );
  assert!(manifest.is_compile_only("compile"), "std::compile should be compile-time-only");
  assert!(
    !manifest.is_auto_load("compile"),
    "std::compile should remain opt-in and stay out of auto_load.modules"
  );
  assert!(
    manifest.get_linking_info("compile").is_none(),
    "std::compile should not expose runtime linking metadata"
  );
}

#[test]
fn std_compile_build_std_skips_runtime_codegen_outputs() {
  let attempt = common::build_std_with_target(TargetBackend::C).expect("temporary std build setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected build_std to succeed with std::compile present"
  );
  assert!(
    !attempt.output_dir.join("std/include/std_compile.h").exists(),
    "compile-time-only std::compile should not emit a runtime header"
  );
  assert!(
    !attempt.output_dir.join("std/src/std_compile.c").exists(),
    "compile-time-only std::compile should not emit runtime C"
  );
  assert!(
    !attempt.output_dir.join("std/obj/std_compile.o").exists(),
    "compile-time-only std::compile should not emit a runtime object file"
  );
}

#[test]
fn std_compile_exposes_vm_diagnostic_surface_without_runtime_artifacts() {
  let attempt = common::compile_project_single_file_with_workspace_std(
    r#"
import Compile from "std::compile";

@directive(target: "record", phase: check, effect: diagnose)
function derive(
  context: Compile::Context,
  target: Compile::ItemReference,
): void {
  Compile::error(context, target, "directive failure");
  return;
}

function main(): i32 {
  return 0;
}
"#,
    TargetBackend::C,
  )
  .expect("temporary project build setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected std::compile to expose the VM diagnostic surface"
  );
}

#[test]
fn std_compile_keeps_generation_placeholders_available_for_future_directive_signatures() {
  let attempt = common::compile_project_single_file_with_workspace_std(
    r#"
import Compile from "std::compile";

@directive(target: "record", phase: expand, effect: emit)
function derive(
  context: Compile::Context,
  diagnostic: Compile::Diagnostic,
  itemBuilder: Compile::ItemBuilder,
  exprBuilder: Compile::ExprBuilder,
  typeBuilder: Compile::TypeBuilder,
  collector: Compile::Collector,
  gensym: Compile::Gensym,
  span: Compile::Span,
  symbol: Compile::Symbol,
  target: Compile::ItemReference,
  itemType: Compile::TypeRef,
): void {
  return;
}

function main(): i32 {
  return 0;
}
"#,
    TargetBackend::C,
  )
  .expect("temporary project build setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected std::compile generation placeholders to remain available as opaque compile-time handles"
  );
}

#[test]
fn std_compile_supports_bounded_generation_delta_calls_without_runtime_artifacts() {
  let attempt = common::compile_project_single_file_with_workspace_std(
    r#"
import Compile from "std::compile";

trait Serializable {
}

@directive(target: "record", phase: expand, effect: emit)
function derive(
  context: Compile::Context,
  target: Compile::ItemReference,
): void {
  Compile::emitMethod(context, target, "generatedMethod", false);
  Compile::emitRecord(context, target, "GeneratedUser");
  Compile::emitImplements(context, target, "Serializable");
}

@derive
record User {
  value: i32;
}

function main(): i32 {
  return 0;
}
"#,
    TargetBackend::C,
  )
  .expect("temporary project build setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected std::compile to expose the bounded generated-delta surface during analysis"
  );
}

#[test]
fn std_compile_repeated_builds_keep_generated_items_compile_only() {
  let source = r#"
import Compile from "std::compile";

trait Serializable {
}

@directive(target: "record", phase: expand, effect: emit)
function derive(
  context: Compile::Context,
  target: Compile::ItemReference,
): void {
  Compile::emitRecord(context, target, "GeneratedUser");
  Compile::emitMethod(context, target, "generatedDescribe", false);
  Compile::emitImplements(context, target, "Serializable");
}

@derive
record User {
  value: i32;
}

function requireSerializable<T: Serializable>(value: T): i32 {
  return 40;
}

function main(): i32 {
  let user = User { value: 1 };
  user.generatedDescribe();
  let generated = GeneratedUser {};
  return requireSerializable(user) + 2;
}
"#;

  let first_attempt = common::compile_project_single_file_with_workspace_std(source, TargetBackend::C)
    .expect("first temporary project build setup should succeed");
  let second_attempt = common::compile_project_single_file_with_workspace_std(source, TargetBackend::C)
    .expect("second temporary project build setup should succeed");

  assert!(
    first_attempt.result.is_ok(),
    "expected the first generated-items build to succeed"
  );
  assert!(
    second_attempt.result.is_ok(),
    "expected the second generated-items build to succeed"
  );
}

#[test]
fn std_compile_vm_boundary_rejects_local_root_compile_namespace_spoofing() {
  let attempt = common::compile_project_single_file_with_workspace_std(
    r#"
import _ from "std::compile";

namespace Compile {
  record Context {}
  record ItemReference {}

  function error(context: Context, target: ItemReference, message: str): void {
    return;
  }
}

@directive(target: "record", phase: check, effect: diagnose)
function derive(context: Compile::Context, target: Compile::ItemReference): void {
  Compile::error(context, target, "directive failure");
  return;
}

@derive
record User {
  value: i32;
}

function main(): i32 {
  return 0;
}
"#,
    TargetBackend::C,
  )
  .expect("temporary project build setup should succeed");

  assert!(
    attempt.result.is_err(),
    "expected local root namespace Compile to be rejected as the std::compile VM boundary"
  );
}
