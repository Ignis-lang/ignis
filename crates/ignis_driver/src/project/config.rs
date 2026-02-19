//! Raw configuration types for ignis.toml parsing.
//!
//! These types represent the TOML structure directly and use `Option` for
//! optional fields. The `resolve` module converts these into a fully resolved
//! `Project` with validated absolute paths.

use std::collections::HashMap;

use serde::Deserialize;

/// Root structure of ignis.toml.
#[derive(Debug, Deserialize)]
pub struct ProjectToml {
  pub package: PackageConfig,

  #[serde(default)]
  pub ignis: IgnisTomlConfig,

  #[serde(default)]
  pub build: BuildTomlConfig,

  /// Import path aliases: first segment -> directory path (relative to project root).
  #[serde(default)]
  pub aliases: HashMap<String, String>,
}

/// The `[package]` section.
#[derive(Debug, Deserialize)]
pub struct PackageConfig {
  pub name: String,
  pub version: String,

  #[serde(default)]
  pub authors: Vec<String>,

  #[serde(default)]
  pub description: String,

  #[serde(default)]
  pub keywords: Vec<String>,

  #[serde(default)]
  pub license: String,

  #[serde(default)]
  pub repository: String,
}

/// The `[ignis]` section.
#[derive(Debug, Deserialize)]
pub struct IgnisTomlConfig {
  /// Whether to use the standard library. Default: true.
  #[serde(default = "default_true")]
  pub std: bool,

  /// Path to the standard library (relative to project root or absolute).
  pub std_path: Option<String>,

  /// Path to the runtime (relative to project root or absolute).
  /// Default: std_path/runtime
  pub runtime_path: Option<String>,
}

impl Default for IgnisTomlConfig {
  fn default() -> Self {
    Self {
      std: true,
      std_path: None,
      runtime_path: None,
    }
  }
}

/// The `[build]` section.
#[derive(Debug, Deserialize)]
pub struct BuildTomlConfig {
  /// Produce an executable. Default: true.
  #[serde(default = "default_true")]
  pub bin: bool,

  /// Source directory relative to project root. Default: "src".
  #[serde(default = "default_source_dir")]
  pub source_dir: String,

  /// Entry file relative to source_dir. Default: "main.ign".
  #[serde(default = "default_entry")]
  pub entry: String,

  /// Output directory relative to project root. Default: "build".
  #[serde(default = "default_out_dir")]
  pub out_dir: String,

  /// Optimization level (0-3). Default: None (use 0).
  pub opt_level: Option<u8>,

  /// Include debug information. Default: false.
  #[serde(default)]
  pub debug: bool,

  /// Backend selector (preferred): "c" | "qbe" | legacy values.
  /// If omitted, resolver falls back to `target` alias.
  #[serde(default)]
  pub backend: Option<String>,

  /// Deprecated backend alias. Kept for backward compatibility.
  /// Default: "c".
  #[serde(default = "default_target")]
  pub target: String,

  /// Target triple used by compile-time directives.
  /// Example: "x86_64-unknown-linux-gnu".
  pub target_triple: Option<String>,

  /// Declared feature catalog for strict unknown-feature validation.
  #[serde(default)]
  pub known_features: Vec<String>,

  /// Features enabled by default for this project.
  #[serde(default)]
  pub default_features: Vec<String>,

  /// C compiler to use. Default: "cc".
  #[serde(default = "default_cc")]
  pub cc: String,

  /// Additional C compiler flags.
  #[serde(default)]
  pub cflags: Vec<String>,

  /// Extra artifacts to emit: "c", "obj".
  #[serde(default)]
  pub emit: Vec<String>,
}

impl Default for BuildTomlConfig {
  fn default() -> Self {
    Self {
      bin: true,
      source_dir: default_source_dir(),
      entry: default_entry(),
      out_dir: default_out_dir(),
      opt_level: None,
      debug: false,
      backend: None,
      target: default_target(),
      target_triple: None,
      known_features: Vec::new(),
      default_features: Vec::new(),
      cc: default_cc(),
      cflags: Vec::new(),
      emit: Vec::new(),
    }
  }
}

fn default_true() -> bool {
  true
}

fn default_source_dir() -> String {
  "src".to_string()
}

fn default_entry() -> String {
  "main.ign".to_string()
}

fn default_out_dir() -> String {
  "build".to_string()
}

fn default_target() -> String {
  "c".to_string()
}

fn default_cc() -> String {
  "cc".to_string()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_minimal_toml() {
    let toml_str = r#"
[package]
name = "test"
version = "0.1.0"
"#;
    let parsed: ProjectToml = toml::from_str(toml_str).unwrap();

    assert_eq!(parsed.package.name, "test");
    assert_eq!(parsed.package.version, "0.1.0");
    assert!(parsed.ignis.std);
    assert!(parsed.build.bin);
    assert_eq!(parsed.build.source_dir, "src");
    assert_eq!(parsed.build.entry, "main.ign");
    assert_eq!(parsed.build.out_dir, "build");
    assert_eq!(parsed.build.backend, None);
    assert_eq!(parsed.build.target, "c");
    assert_eq!(parsed.build.cc, "cc");
  }

  #[test]
  fn test_full_toml() {
    let toml_str = r#"
[package]
name = "allocator"
version = "0.1.0"
authors = ["test"]
description = "A test project"
keywords = ["test"]
license = "MIT"
repository = "https://github.com/test/test"

[ignis]
std = true
std_path = "../std"
runtime_path = "../std/runtime"

[build]
bin = true
source_dir = "src"
entry = "main.ign"
out_dir = "build"
opt_level = 2
debug = true
backend = "qbe"
target = "c"
target_triple = "x86_64-unknown-linux-gnu"
known_features = ["qbe", "simd"]
default_features = ["qbe"]
cc = "gcc"
cflags = ["-Wall", "-Wextra"]
emit = ["c", "obj"]
"#;
    let parsed: ProjectToml = toml::from_str(toml_str).unwrap();

    assert_eq!(parsed.package.name, "allocator");
    assert_eq!(parsed.ignis.std_path, Some("../std".to_string()));
    assert_eq!(parsed.ignis.runtime_path, Some("../std/runtime".to_string()));
    assert_eq!(parsed.build.opt_level, Some(2));
    assert!(parsed.build.debug);
    assert_eq!(parsed.build.backend, Some("qbe".to_string()));
    assert_eq!(parsed.build.target, "c");
    assert_eq!(parsed.build.target_triple, Some("x86_64-unknown-linux-gnu".to_string()));
    assert_eq!(parsed.build.known_features, vec!["qbe", "simd"]);
    assert_eq!(parsed.build.default_features, vec!["qbe"]);
    assert_eq!(parsed.build.cc, "gcc");
    assert_eq!(parsed.build.cflags, vec!["-Wall", "-Wextra"]);
    assert_eq!(parsed.build.emit, vec!["c", "obj"]);
  }

  #[test]
  fn test_std_disabled() {
    let toml_str = r#"
[package]
name = "nostd"
version = "0.1.0"

[ignis]
std = false
"#;
    let parsed: ProjectToml = toml::from_str(toml_str).unwrap();

    assert!(!parsed.ignis.std);
    assert!(parsed.ignis.std_path.is_none());
  }

  #[test]
  fn test_aliases_section() {
    let toml_str = r#"
[package]
name = "test"
version = "0.1.0"

[aliases]
mylib = "libs/mylib"
ext = "../external"
"#;
    let parsed: ProjectToml = toml::from_str(toml_str).unwrap();

    assert_eq!(parsed.aliases.len(), 2);
    assert_eq!(parsed.aliases["mylib"], "libs/mylib");
    assert_eq!(parsed.aliases["ext"], "../external");
  }

  #[test]
  fn test_aliases_defaults_to_empty() {
    let toml_str = r#"
[package]
name = "test"
version = "0.1.0"
"#;
    let parsed: ProjectToml = toml::from_str(toml_str).unwrap();

    assert!(parsed.aliases.is_empty());
  }
}
