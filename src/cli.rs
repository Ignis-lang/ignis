use clap::{Parser, ValueEnum, Subcommand};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum DebugPrint {
  /// Default value. Don't print anything
  None,
  /// Print the lexer output
  Lexer,
  /// Print the parser output
  Parser,
  /// Print the AST struct
  Ast,
  /// Print the analyzer output
  Analyzer,
  /// Print the IR struct
  Ir,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Backend {
  /// Transpile to Lua
  Lua,
  /// Backend in C
  C,
  /// Transpile to bytecode for the IVM
  Bytecode,
  /// Transpile to LLVM IR and compile to native code
  LLVM,
}

#[derive(Parser, Debug, Clone, PartialEq)]
pub struct BuildCommand {
  pub file_path: String,
}

#[derive(Subcommand, Clone, PartialEq)]
pub enum SubCommand {
  Build(BuildCommand),
}

#[derive(Parser)]
#[command(author, version, about = "The Ignis land compiler", long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
  #[command(subcommand)]
  pub subcommand: SubCommand,

  #[arg(short, long, value_enum, default_value = "none")]
  pub debug: Vec<DebugPrint>,

  #[arg(short, long, value_enum, default_value = "c")]
  pub backend: Backend,
}
