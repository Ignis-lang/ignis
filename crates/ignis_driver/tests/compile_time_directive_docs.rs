use std::path::PathBuf;

fn repo_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("crate dir should have workspace parent")
    .parent()
    .expect("workspace should have repo root")
    .to_path_buf()
}

fn read_repo_file(relative: &str) -> String {
  std::fs::read_to_string(repo_root().join(relative))
    .unwrap_or_else(|error| panic!("failed to read {}: {}", relative, error))
}

#[test]
fn compile_time_directive_docs_lock_the_current_surface_and_limits() {
  let grammar = read_repo_file("docs/GRAMMAR.md");
  let language = read_repo_file("docs/LANGUAGE_REFERENCE_CURRENT.md");
  let traits = read_repo_file("docs/TRAITS.md");
  let testing = read_repo_file("docs/TESTING.md");
  let builtins = read_repo_file("docs/BUILTINS.md");

  assert!(grammar.contains("named directive arguments"));
  assert!(grammar.contains("@directive(target: \"record\", phase: expand, effect: emit)"));

  assert!(language.contains("### `@directive(...)`"));
  assert!(language.contains("`check`, `expand`, `collect`, `finalize`, and `transform`"));
  assert!(language.contains("`std::compile` is compile-time-only"));

  assert!(traits.contains("internal attached methods plus implemented-trait metadata"));
  assert!(traits.contains("Directive execution does not yet synthesize `Eq` methods for you"));

  assert!(testing.contains("`@test` remains the native runner's source of truth"));
  assert!(testing.contains("Directive functions may coexist with tests"));

  assert!(builtins.contains("user-defined compile-time directives are declaration attributes"));
  assert!(builtins.contains("default-deny sandbox"));
}
