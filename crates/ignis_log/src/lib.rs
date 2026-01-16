//! Logging utilities for the Ignis compiler.
//!
//! Provides macros for:
//! - Phase logging (`phase_log!`, `phase_ok!`, `phase_warn!`)
//! - Debug traces by category (`trace_dbg!`)
//! - Verbose logging (`log_dbg!`, `log_trc!`)
//!
//! All output goes to stderr to avoid mixing with dumps/stdout.

use ignis_config::{DebugTrace, IgnisConfig};

pub fn effective_verbose(config: &IgnisConfig) -> u8 {
  if config.quiet {
    return 0;
  }

  if config.debug && config.verbose < 2 {
    return 2;
  }

  config.verbose
}

pub fn log_phase(config: &IgnisConfig) -> bool {
  !config.quiet
}

pub fn log_info(config: &IgnisConfig) -> bool {
  effective_verbose(config) >= 1
}

pub fn log_debug(config: &IgnisConfig) -> bool {
  effective_verbose(config) >= 2
}

pub fn log_trace(config: &IgnisConfig) -> bool {
  effective_verbose(config) >= 3
}

pub fn debug_trace_enabled(
  config: &IgnisConfig,
  trace: DebugTrace,
) -> bool {
  !config.quiet && (config.debug || config.debug_trace.contains(&trace))
}

/// Returns lowercase name of a DebugTrace variant for log output.
pub fn trace_name(trace: DebugTrace) -> &'static str {
  match trace {
    DebugTrace::Analyzer => "analyzer",
    DebugTrace::Parser => "parser",
    DebugTrace::Lexer => "lexer",
    DebugTrace::Mono => "mono",
    DebugTrace::Ownership => "ownership",
    DebugTrace::Lir => "lir",
    DebugTrace::Codegen => "codegen",
    DebugTrace::Link => "link",
    DebugTrace::Std => "std",
  }
}

/// Log a compiler phase message with an arrow prefix.
///
/// All output goes to stderr to avoid mixing with dumps/stdout.
///
/// # Examples
///
/// ```ignore
/// phase_log!(&config, "Scanning... {}", file_path);
/// phase_log!(&config, indent = 8, "Sub-step {}", name);
/// ```
#[macro_export]
macro_rules! phase_log {
  ($config:expr, indent = $indent:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::log_phase($config) {
      use colored::Colorize;
      eprintln!(
        "{:indent$}{} {}",
        "",
        "-->".bright_green().bold(),
        format!($fmt $(, $arg)*),
        indent = $indent
      );
    }
  }};

  ($config:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    $crate::phase_log!($config, indent = 4, $fmt $(, $arg)*);
  }};
}

/// Log a successful phase completion (green arrow, no indent).
///
/// # Examples
///
/// ```ignore
/// phase_ok!(&config, "Build complete: {}", path);
/// ```
#[macro_export]
macro_rules! phase_ok {
  ($config:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::log_phase($config) {
      use colored::Colorize;
      eprintln!("{} {}", "-->".bright_green().bold(), format!($fmt $(, $arg)*));
    }
  }};
}

/// Log a warning during a phase (yellow arrow, no indent).
///
/// # Examples
///
/// ```ignore
/// phase_warn!(&config, "std library not found, building...");
/// ```
#[macro_export]
macro_rules! phase_warn {
  ($config:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::log_phase($config) {
      use colored::Colorize;
      eprintln!("{} {}", "-->".bright_yellow().bold(), format!($fmt $(, $arg)*));
    }
  }};
}

/// Log a debug trace for a specific compiler component.
///
/// Output format: `debug[component]: message`
///
/// # Examples
///
/// ```ignore
/// trace_dbg!(&config, DebugTrace::Lexer, "produced {} tokens", count);
/// // Output: debug[lexer]: produced 50 tokens
/// ```
#[macro_export]
macro_rules! trace_dbg {
  ($config:expr, $trace:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::debug_trace_enabled($config, $trace) {
      eprintln!(
        "debug[{}]: {}",
        $crate::trace_name($trace),
        format!($fmt $(, $arg)*)
      );
    }
  }};
}

/// Log a verbose debug message (verbosity >= 2).
///
/// # Examples
///
/// ```ignore
/// log_dbg!(&config, "compiling project entry {}", entry_path);
/// // Output: debug: compiling project entry main.ign
/// ```
#[macro_export]
macro_rules! log_dbg {
  ($config:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::log_debug($config) {
      eprintln!("debug: {}", format!($fmt $(, $arg)*));
    }
  }};
}

/// Log a trace message (verbosity >= 3).
///
/// # Examples
///
/// ```ignore
/// log_trc!(&config, "codegen module order {:?}", modules);
/// // Output: trace: codegen module order [...]
/// ```
#[macro_export]
macro_rules! log_trc {
  ($config:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::log_trace($config) {
      eprintln!("trace: {}", format!($fmt $(, $arg)*));
    }
  }};
}
