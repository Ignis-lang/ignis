#![allow(clippy::too_many_arguments)]

use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

/// Base header the emitted C is built on, relative to the std root.
///
/// The standard library no longer ships a C runtime, so this path is not
/// configurable: it is always derived from `std_path`, and `ignis_rt.h` only
/// guards the same type prelude the emitter already writes inline.
pub const STD_BASE_HEADER: &str = "runtime/ignis_rt.h";

/// Header to include in generated C code with style info
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CHeader {
  /// The path/name of the header
  pub path: String,
  /// true = #include "...", false = #include <...>
  pub quoted: bool,
}

/// Toolchain configuration for the standard library
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StdToolchainConfig {
  /// Include directories relative to std_root (default: ["."])
  #[serde(default)]
  pub include_dirs: Vec<String>,
}

/// Linking information for a std module (header and object file)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StdLinkingInfo {
  /// Path to a single C header file (optional, use `headers` for multiple)
  pub header: Option<String>,
  /// Multiple C header files (optional, takes precedence over `header`)
  #[serde(default)]
  pub headers: Vec<String>,
  /// Whether headers use quoted includes (default: false for system headers)
  pub header_quoted: Option<bool>,
  /// Path to the object file for linking (optional, prefer archive)
  pub object: Option<String>,
  /// Path to a static archive for linking (optional, preferred over object)
  pub archive: Option<String>,
  /// External library to link (e.g., "m" for -lm)
  pub lib: Option<String>,
}

/// Configuration for auto-loading modules
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StdAutoLoad {
  /// List of module names to auto-load (e.g., ["string", "number"])
  pub modules: Vec<String>,
}

/// Configuration for std modules that are available during analysis but must
/// not become part of the runtime ABI surface.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StdCompileOnly {
  /// List of compile-time-only module names (e.g., ["compile"])
  pub modules: Vec<String>,
}

/// Manifest for the Ignis standard library
///
/// Expected format in manifest.toml:
/// ```toml
/// [modules]
/// io = "io/mod.ign"
/// math = "math/mod.ign"
///
/// [linking.io]
/// header = "runtime/io.h"
/// object = "runtime/libio.o"
///
/// [auto_load]
/// modules = ["string", "number", "vector", "types"]
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct IgnisSTDManifest {
  /// Toolchain configuration
  #[serde(default)]
  pub toolchain: StdToolchainConfig,
  /// Module name -> relative path to .ign file
  #[serde(default)]
  pub modules: HashMap<String, String>,
  /// Module name -> linking info (header and object file)
  #[serde(default)]
  pub linking: HashMap<String, StdLinkingInfo>,
  /// Modules to auto-load
  #[serde(default)]
  pub auto_load: Option<StdAutoLoad>,
  /// Modules that are compile-time-only and must stay out of runtime ABI/linking
  #[serde(default)]
  pub compile_only: Option<StdCompileOnly>,
}

impl IgnisSTDManifest {
  /// Get include directories (defaults to ["."] if empty)
  pub fn get_include_dirs(&self) -> Vec<&str> {
    if self.toolchain.include_dirs.is_empty() {
      vec!["."]
    } else {
      self.toolchain.include_dirs.iter().map(|s| s.as_str()).collect()
    }
  }

  /// Get the relative path to a module's .ign file
  pub fn get_module_path(
    &self,
    name: &str,
  ) -> Option<&String> {
    self.modules.get(name)
  }

  /// Get linking info for a module
  pub fn get_linking_info(
    &self,
    name: &str,
  ) -> Option<&StdLinkingInfo> {
    self.linking.get(name)
  }

  /// Check if a module should be auto-loaded
  pub fn is_auto_load(
    &self,
    name: &str,
  ) -> bool {
    self
      .auto_load
      .as_ref()
      .map(|a| a.modules.contains(&name.to_string()))
      .unwrap_or(false)
  }

  /// Check if a module is compile-time-only.
  pub fn is_compile_only(
    &self,
    name: &str,
  ) -> bool {
    self
      .compile_only
      .as_ref()
      .map(|compile_only| compile_only.modules.contains(&name.to_string()))
      .unwrap_or(false)
  }

  /// Get all compile-time-only module names.
  pub fn get_compile_only_modules(&self) -> Vec<&str> {
    self
      .compile_only
      .as_ref()
      .map(|compile_only| compile_only.modules.iter().map(|s| s.as_str()).collect())
      .unwrap_or_default()
  }

  /// Get all auto-load module names
  pub fn get_auto_load_modules(&self) -> Vec<&str> {
    self
      .auto_load
      .as_ref()
      .map(|a| a.modules.iter().map(|s| s.as_str()).collect())
      .unwrap_or_default()
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DumpKind {
  Lexer,
  Ast,
  Defs,
  Types,
  Hir,
  HirSummary,
  Lir,
  Ir,
  C,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugTrace {
  Analyzer,
  Parser,
  Lexer,
  Mono,
  Ownership,
  Lir,
  Codegen,
  Link,
  Std,
}

/// Controls the verbosity level of CLI output.
///
/// - `Quiet`: No output except errors
/// - `Detailed`: Structured progress output (default)
/// - `Verbose`: Detailed output with internal phases
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum OutputLevel {
  Quiet,
  #[default]
  Detailed,
  Verbose,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
pub enum TargetBackend {
  #[default]
  C,
  Iir,
  None,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct IgnisProjectBuildConfig {
  pub source_dir: String,
  pub main_file: String,
  pub target: TargetBackend,
  pub optimize: bool,
  pub output_dir: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct IgnisProjectIgnisConfig {
  pub std_path: String,
  pub std: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IgnisProjectConfig {
  pub name: String,
  pub version: String,
  pub authors: Vec<String>,
  pub description: String,
  pub keywords: Vec<String>,
  pub license: String,
  pub repository: String,
  pub build: IgnisProjectBuildConfig,
  pub ignis: IgnisProjectIgnisConfig,
  // TODO: dependencies
}

impl IgnisProjectConfig {
  pub fn new(
    name: String,
    version: String,
    authors: Vec<String>,
    description: String,
    keywords: Vec<String>,
    license: String,
    repository: String,
    target: TargetBackend,
    main_file: String,
    output_dir: String,
    source_dir: String,
    optimize: bool,
    std_path: String,
    std: bool,
  ) -> Self {
    Self {
      name,
      version,
      authors,
      description,
      keywords,
      license,
      repository,
      build: IgnisProjectBuildConfig {
        source_dir,
        main_file,
        target,
        optimize,
        output_dir,
      },
      ignis: IgnisProjectIgnisConfig { std_path, std },
    }
  }
}

#[derive(Debug, Clone)]
pub struct IgnisBuildConfig {
  pub file: Option<String>,
  pub is_project: bool,
  pub target: TargetBackend,
  pub optimize: bool,
  pub output_dir: String,
  pub dump: Vec<DumpKind>,
  pub dump_dir: Option<String>,
  pub dump_hir: Option<String>,
  pub emit_c: Option<String>,
  pub emit_obj: Option<String>,
  pub emit_bin: Option<String>,
  pub bin: bool,
  pub lib: bool,
  pub check_mode: bool,
  pub analyze_only: bool,
  /// Force a full rebuild, ignoring cached C/objects/archives (`ignis build --force`).
  pub force_rebuild: bool,
  /// Print the drop schedule of every function after ownership analysis.
  pub dump_drop_schedule: bool,
}

impl IgnisBuildConfig {
  pub fn new(
    file: Option<String>,
    target: TargetBackend,
    is_project: bool,
    optimize: bool,
    output_dir: String,
    dump: Vec<DumpKind>,
    dump_dir: Option<String>,
    dump_hir: Option<String>,
    emit_c: Option<String>,
    emit_obj: Option<String>,
    emit_bin: Option<String>,
    bin: bool,
    lib: bool,
    check_mode: bool,
    analyze_only: bool,
    force_rebuild: bool,
  ) -> Self {
    Self {
      file,
      is_project,
      target,
      optimize,
      output_dir,
      dump,
      dump_dir,
      dump_hir,
      emit_c,
      emit_obj,
      emit_bin,
      bin,
      lib,
      check_mode,
      analyze_only,
      force_rebuild,
      dump_drop_schedule: false,
    }
  }

  pub fn with_dump_drop_schedule(
    mut self,
    dump_drop_schedule: bool,
  ) -> Self {
    self.dump_drop_schedule = dump_drop_schedule;
    self
  }
}

#[derive(Debug, Clone)]
pub struct IgnisInitConfig {
  pub name: String,
  pub version: String,
  pub authors: Vec<String>,
  pub description: String,
  pub keywords: Vec<String>,
  pub license: String,
  pub repository: String,
  pub git: bool,
  pub target: TargetBackend,
}

impl IgnisInitConfig {
  pub fn new(
    name: String,
    version: String,
    authors: Vec<String>,
    description: String,
    keywords: Vec<String>,
    license: String,
    repository: String,
    git: bool,
    target: TargetBackend,
  ) -> Self {
    Self {
      name,
      version,
      authors,
      description,
      keywords,
      license,
      repository,
      git,
      target,
    }
  }
}

#[derive(Debug, Clone, Default)]
pub struct IgnisConfig {
  pub project_config: Option<IgnisProjectConfig>,
  pub build_config: Option<IgnisBuildConfig>,
  pub init_config: Option<IgnisInitConfig>,
  pub debug: bool,
  /// Build profile debug flag used by `@debug()` compile-time directives.
  /// Resolved from `ignis.toml [build] debug` / `--debug` / `--no-debug`,
  /// independent of the internal `debug` flag above.
  pub build_debug: bool,
  /// Optimization level handed to the C toolchain as `-O<level>`.
  /// Resolved from `ignis.toml [build] opt_level` / `-O` / `--opt-level`.
  pub opt_level: u8,
  pub debug_trace: Vec<DebugTrace>,
  pub quiet: bool,
  pub verbose: u8,
  pub output_level: OutputLevel,
  pub build: bool,
  pub test: bool,
  pub init: bool,
  pub build_std: bool,
  pub build_std_output_dir: Option<String>,
  /// Compilation target triple used by compile-time directives.
  pub target_triple: String,
  /// Enabled feature flags for compile-time directives.
  pub enabled_features: HashSet<String>,
  /// Declared feature catalog for strict unknown-feature validation.
  pub known_features: Option<HashSet<String>>,
  pub std_path: String,
  pub std: bool,
  pub auto_load_std: bool,
  pub manifest: IgnisSTDManifest,
  pub check_std: bool,
  /// C compiler executable used for C compilation/linking (e.g. gcc, clang).
  pub c_compiler: String,
  /// Additional C compiler/linker flags passed to the C toolchain invocation.
  pub cflags: Vec<String>,
  /// Import path aliases from `[aliases]` in ignis.toml (resolved to absolute paths).
  pub aliases: HashMap<String, std::path::PathBuf>,
  /// Skips the per-compile std freshness check and archive (re)build.
  ///
  /// Set by a caller that already built std once under this exact output
  /// directory and guarantees no other writer touches it for the lifetime of
  /// this config, such as the native test runner's fixture pool: every
  /// fixture shares the same std archive, so re-checking (and potentially
  /// rebuilding) it per fixture is both redundant and, under concurrent
  /// fixture workers, a data race on the shared archive file.
  pub assume_std_built: bool,
  /// Overrides where the per-module ("user" side) build tree is written,
  /// independent of `build_config.output_dir`.
  ///
  /// `output_dir` still locates the shared, read-only std archive, but a
  /// caller that compiles many small programs against that one archive
  /// concurrently (the fixture pool) needs each compile's own headers,
  /// objects, and umbrella header written somewhere private: the umbrella
  /// header in particular has one fixed name per output directory, so two
  /// fixtures sharing `output_dir` would overwrite each other's while a
  /// concurrent compile was still reading it.
  pub user_build_dir_override: Option<String>,
}

impl IgnisConfig {
  pub fn new(
    project_config: Option<IgnisProjectConfig>,
    build_config: Option<IgnisBuildConfig>,
    init_config: Option<IgnisInitConfig>,
    debug: bool,
    debug_trace: Vec<DebugTrace>,
    quiet: bool,
    verbose: u8,
    output_level: OutputLevel,
    build: bool,
    test: bool,
    init: bool,
    build_std: bool,
    build_std_output_dir: Option<String>,
    std_path: String,
    std: bool,
    auto_load_std: bool,
    manifest: IgnisSTDManifest,
    check_std: bool,
    c_compiler: String,
    cflags: Vec<String>,
  ) -> Self {
    Self {
      project_config,
      build_config,
      init_config,
      debug,
      build_debug: false,
      opt_level: 0,
      debug_trace,
      quiet,
      verbose,
      output_level,
      build,
      test,
      init,
      build_std,
      build_std_output_dir,
      target_triple: String::new(),
      enabled_features: HashSet::new(),
      known_features: None,
      std_path,
      std,
      auto_load_std,
      manifest,
      check_std,
      c_compiler,
      cflags,
      aliases: HashMap::new(),
      assume_std_built: false,
      user_build_dir_override: None,
    }
  }

  pub fn new_basic(
    debug: bool,
    debug_trace: Vec<DebugTrace>,
    quiet: bool,
    verbose: u8,
  ) -> Self {
    let output_level = if quiet {
      OutputLevel::Quiet
    } else if verbose > 0 {
      OutputLevel::Verbose
    } else {
      OutputLevel::Detailed
    };

    let host_arch = std::env::consts::ARCH;
    let host_os = std::env::consts::OS;
    let host_target = format!("{}-unknown-{}", host_arch, host_os);

    Self {
      debug,
      debug_trace,
      quiet,
      verbose,
      output_level,
      target_triple: host_target,
      c_compiler: "gcc".to_string(),
      ..Self::default()
    }
  }
}
