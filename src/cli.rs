use clap::{Parser, ValueEnum, Subcommand};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum DebugPrint {
  /// Default value. Don't print anything
  None,
  /// Print the lexer output
  Lexer,
  /// Print the AST struct
  Ast,
  /// Print the IR struct
  Ir,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Target {
  /// Transpile to Lua
  Lua,
  /// Transpile to C an compile to native code (TODO)
  C,
  /// Transpile to bytecode for the IVM (TODO)
  Bytecode,
  /// Transpile to LLVM IR and compile to native code (TODO)
  Llvm,
  /// Run interpreted (TODO)
  Evaluator,
}

impl Target {
  pub fn to_backend(&self) -> ignis_backend::BackendTarget {
    match self {
      Target::Lua => ignis_backend::BackendTarget::Lua,
      Target::C => ignis_backend::BackendTarget::C,
      Target::Bytecode => ignis_backend::BackendTarget::Bytecode,
      Target::Llvm => ignis_backend::BackendTarget::Llvm,
      Target::Evaluator => ignis_backend::BackendTarget::Evaluator,
    }
  }
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct BuildCommand {
  pub file_path: String,

  #[arg(short, long, value_enum, default_value = "lua")]
  pub target: Target,
}

#[derive(Subcommand, Clone, PartialEq)]
pub enum SubCommand {
  /// Build the given file
  Build(BuildCommand),
  /// Run the given file
  Run(BuildCommand),
}

#[derive(Parser)]
#[command(author, version, about = "The Ignis compiler", long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
  #[command(subcommand)]
  pub subcommand: SubCommand,

  #[arg(short, long, value_enum, default_value = "none")]
  pub debug: Vec<DebugPrint>,

  #[arg(short, long)]
  pub optimize: bool,
}
