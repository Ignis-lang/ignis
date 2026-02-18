use clap::{Parser, ValueEnum, Subcommand, ColorChoice};
use ignis_config::{DebugTrace, DumpKind, TargetBackend};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum DumpKindCli {
  /// Dump the lexer output
  Lexer,
  /// Dump the AST struct
  Ast,
  /// Dump definitions
  Defs,
  /// Dump type store
  Types,
  /// Dump the full HIR
  Hir,
  /// Dump a HIR summary
  HirSummary,
  /// Dump LIR (Low-level IR)
  Lir,
  /// Dump IR (not yet supported)
  Ir,
  /// Dump generated C (not yet supported)
  C,
}

impl From<DumpKindCli> for DumpKind {
  fn from(value: DumpKindCli) -> DumpKind {
    match value {
      DumpKindCli::Lexer => DumpKind::Lexer,
      DumpKindCli::Ast => DumpKind::Ast,
      DumpKindCli::Defs => DumpKind::Defs,
      DumpKindCli::Types => DumpKind::Types,
      DumpKindCli::Hir => DumpKind::Hir,
      DumpKindCli::HirSummary => DumpKind::HirSummary,
      DumpKindCli::Lir => DumpKind::Lir,
      DumpKindCli::Ir => DumpKind::Ir,
      DumpKindCli::C => DumpKind::C,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum DebugTraceCli {
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

impl From<DebugTraceCli> for DebugTrace {
  fn from(value: DebugTraceCli) -> DebugTrace {
    match value {
      DebugTraceCli::Analyzer => DebugTrace::Analyzer,
      DebugTraceCli::Parser => DebugTrace::Parser,
      DebugTraceCli::Lexer => DebugTrace::Lexer,
      DebugTraceCli::Mono => DebugTrace::Mono,
      DebugTraceCli::Ownership => DebugTrace::Ownership,
      DebugTraceCli::Lir => DebugTrace::Lir,
      DebugTraceCli::Codegen => DebugTrace::Codegen,
      DebugTraceCli::Link => DebugTrace::Link,
      DebugTraceCli::Std => DebugTrace::Std,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Target {
  /// Transpile to C an compile to native code
  C,
  /// Export Intermediate Representation
  Iir,
  /// No target
  None,
}

impl From<Target> for TargetBackend {
  fn from(val: Target) -> Self {
    match val {
      Target::C => TargetBackend::C,
      Target::Iir => TargetBackend::Iir,
      Target::None => TargetBackend::None,
    }
  }
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct BuildCommand {
  /// File to build (single-file mode) or nothing for project mode
  pub file_path: Option<String>,

  /// Explicit project directory (overrides auto-detection)
  #[arg(long)]
  pub project: Option<String>,

  /// Optimization level (0-3)
  #[arg(long, value_parser = clap::value_parser!(u8).range(0..=3))]
  pub opt_level: Option<u8>,

  /// Include debug information
  #[arg(long)]
  pub debug: bool,

  /// Disable debug information (overrides --debug and TOML)
  #[arg(long, conflicts_with = "debug")]
  pub no_debug: bool,

  /// Output directory
  #[arg(short = 'o', long)]
  pub output_dir: Option<String>,

  /// Path to standard library (overrides TOML and env var)
  #[arg(long)]
  pub std_path: Option<String>,

  /// C compiler to use
  #[arg(long)]
  pub cc: Option<String>,

  /// Extra artifacts to emit (c, obj)
  #[arg(long, value_delimiter = ',')]
  pub emit: Vec<String>,

  /// Rebuild std runtime before linking
  #[arg(long)]
  pub rebuild_std: bool,

  /// Produce a linked executable (default)
  #[arg(long = "bin", short = 'b', conflicts_with = "lib")]
  pub bin: bool,

  /// Produce a static library
  #[arg(long = "lib", short = 'l', conflicts_with = "bin")]
  pub lib: bool,

  /// The target to compile to
  #[arg(short, long, value_enum, default_value = "c")]
  pub target: Target,

  /// Override target triple used by compile-time directives
  #[arg(long = "target-triple")]
  pub target_triple: Option<String>,

  /// Enable one feature (can be repeated)
  #[arg(long = "feature", action = clap::ArgAction::Append)]
  pub feature: Vec<String>,

  /// Enable multiple features separated by commas
  #[arg(long = "features", value_delimiter = ',')]
  pub features: Vec<String>,
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct InitCommand {
  /// The name of the project
  pub name: String,

  /// Disable Git repository initialization
  #[arg(long)]
  pub no_git: bool,

  /// Initialize as a library project
  #[arg(long)]
  pub lib: bool,

  /// The author of the project
  #[arg(short, long)]
  pub authors: Vec<String>,

  /// The email of the author
  #[arg(short, long, default_value = "ignis@example.com")]
  pub email: String,

  /// The description of the project
  #[arg(short, long, default_value = "A Ignis project")]
  pub description: String,

  /// The license of the project
  #[arg(short, long, default_value = "MIT")]
  pub license: String,

  /// The version of the project
  #[arg(short, long, default_value = "0.1.0")]
  pub project_version: String,

  /// The target to compile to
  #[arg(short, long, value_enum, default_value = "c")]
  pub target: Target,

  /// Entry file name (defaults to main.ign or lib.ign with --lib)
  #[arg(short, long)]
  pub main_file: Option<String>,

  /// Output directory
  #[arg(short, long, default_value = "build")]
  pub output_dir: String,

  /// Source directory
  #[arg(short, long, default_value = "src")]
  pub source_dir: String,

  /// Git repository
  #[arg(short, long, default_value = "")]
  pub repository: String,

  /// Keywords of the project
  #[arg(short, long)]
  pub keywords: Vec<String>,
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct BuildStdCommand {
  /// Output directory for compiled std artifacts
  #[arg(short = 'o', long, default_value = "build")]
  pub output_dir: String,
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct CheckCommand {
  /// File to check (single-file mode) or nothing for project mode
  pub file_path: Option<String>,

  /// Explicit project directory (overrides auto-detection)
  #[arg(long)]
  pub project: Option<String>,

  /// Only run frontend/analyzer; skip lowering/codegen
  #[arg(long)]
  pub analyze_only: bool,

  /// Output directory (used when emitting C)
  #[arg(short = 'o', long)]
  pub output_dir: Option<String>,

  /// Path to standard library (overrides TOML and env var)
  #[arg(long)]
  pub std_path: Option<String>,

  /// Extra artifacts to emit (c)
  #[arg(long, value_delimiter = ',')]
  pub emit: Vec<String>,

  /// Produce a linked executable (default)
  #[arg(long = "bin", short = 'b', conflicts_with = "lib")]
  pub bin: bool,

  /// Produce a static library
  #[arg(long = "lib", short = 'l', conflicts_with = "bin")]
  pub lib: bool,

  /// The target to compile to
  #[arg(short, long, value_enum, default_value = "c")]
  pub target: Target,

  /// Override target triple used by compile-time directives
  #[arg(long = "target-triple")]
  pub target_triple: Option<String>,

  /// Enable one feature (can be repeated)
  #[arg(long = "feature", action = clap::ArgAction::Append)]
  pub feature: Vec<String>,

  /// Enable multiple features separated by commas
  #[arg(long = "features", value_delimiter = ',')]
  pub features: Vec<String>,
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct CheckStdCommand {
  /// Output directory for generated C (if persisted)
  #[arg(short = 'o', long, default_value = "build")]
  pub output_dir: String,
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct CheckRuntimeCommand {
  /// Optional override for runtime root (defaults to std_path/runtime)
  #[arg(long)]
  pub runtime_path: Option<String>,
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct LspCommand {
  /// Log file path for debug output
  #[arg(long)]
  pub log_file: Option<String>,
}

#[derive(Subcommand, Clone, PartialEq)]
pub enum SubCommand {
  /// Build file or project
  Build(BuildCommand),
  /// Initialize a new Ignis project
  Init(InitCommand),
  /// Build the standard library
  BuildStd(BuildStdCommand),
  /// Check file or project up to C codegen without linking
  Check(CheckCommand),
  /// Check the standard library up to C codegen without archiving
  CheckStd(CheckStdCommand),
  /// Check the C runtime with syntax-only compilation
  CheckRuntime(CheckRuntimeCommand),
  /// Start the Language Server Protocol server
  Lsp(LspCommand),
}

#[derive(Parser)]
#[command(author, version, about = "The Ignis compiler", long_about = None)]
#[command(propagate_version = true)]
#[command(color = ColorChoice::Always)]
pub struct Cli {
  #[command(subcommand)]
  pub subcommand: SubCommand,

  /// Dump internal compiler representations
  #[arg(long, value_enum, action = clap::ArgAction::Append, global = true)]
  pub dump: Vec<DumpKindCli>,

  /// Write dumps to this directory (otherwise stdout)
  #[arg(long, global = true)]
  pub dump_dir: Option<String>,

  /// Dump HIR for a specific function
  #[arg(long, global = true)]
  pub dump_hir: Option<String>,

  /// Enable internal debug mode
  #[arg(long, default_value = "false", global = true)]
  pub debug: bool,

  /// Enable debug tracing for subsystems
  #[arg(long, value_enum, action = clap::ArgAction::Append, global = true)]
  pub debug_trace: Vec<DebugTraceCli>,

  /// Don't print any output
  #[arg(long, short = 'q', default_value = "false", global = true)]
  pub quiet: bool,

  /// Use verbose output
  #[arg(long, short, action = clap::ArgAction::Count, global = true)]
  pub verbose: u8,

  /// Path to the standard library
  #[arg(short, long, default_value = "IGNIS_STD_PATH")]
  pub std_path: String,

  /// Use standard library
  #[arg(long, default_value = "false")]
  pub std: bool,

  /// Automatically load standard library
  #[arg(short, long, default_value = "false")]
  pub auto_load_std: bool,
}
