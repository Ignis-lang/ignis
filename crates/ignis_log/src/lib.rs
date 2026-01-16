//! Logging utilities for the Ignis compiler.
//!
//! Provides macros for:
//! - Phase logging (`phase_log!`, `phase_ok!`, `phase_warn!`)
//! - Debug traces by category (`trace_dbg!`)
//! - Verbose logging (`log_dbg!`, `log_trc!`)
//!
//! All output goes to stderr to avoid mixing with dumps/stdout.

use std::time::Duration;

use ignis_config::{DebugTrace, IgnisConfig, OutputLevel};

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

/// Returns true if output should be shown (Detailed or Verbose mode).
pub fn show_output(config: &IgnisConfig) -> bool {
  !matches!(config.output_level, OutputLevel::Quiet)
}

/// Returns true if verbose output should be shown.
pub fn show_verbose(config: &IgnisConfig) -> bool {
  matches!(config.output_level, OutputLevel::Verbose)
}

/// Format a duration for display (e.g., "75ms", "1.2s").
pub fn format_duration(d: Duration) -> String {
  let millis = d.as_millis();
  if millis < 1000 {
    format!("{}ms", millis)
  } else {
    format!("{:.1}s", d.as_secs_f64())
  }
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
/// Only shown in Verbose mode. All output goes to stderr.
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
    if $crate::show_verbose($config) {
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
/// Only shown in Verbose mode.
///
/// # Examples
///
/// ```ignore
/// phase_ok!(&config, "Build complete: {}", path);
/// ```
#[macro_export]
macro_rules! phase_ok {
  ($config:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::show_verbose($config) {
      use colored::Colorize;
      eprintln!("{} {}", "-->".bright_green().bold(), format!($fmt $(, $arg)*));
    }
  }};
}

/// Log a warning during a phase (yellow arrow, no indent).
///
/// Only shown in Verbose mode.
///
/// # Examples
///
/// ```ignore
/// phase_warn!(&config, "std library not found, building...");
/// ```
#[macro_export]
macro_rules! phase_warn {
  ($config:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::show_verbose($config) {
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

/// Print the command header (e.g., "Building example/main.ign").
#[macro_export]
macro_rules! cmd_header {
  ($config:expr, $cmd:expr, $path:expr) => {{
    if $crate::show_output($config) {
      eprintln!("{} {}\n", $cmd, $path);
    }
  }};
}

/// Print a section header (e.g., "- Scanning & parsing").
#[macro_export]
macro_rules! section {
  ($config:expr, $name:expr) => {{
    if $crate::show_output($config) {
      use colored::Colorize;
      eprintln!("{} {}", "\u{2022}".bright_cyan().bold(), $name);
    }
  }};
}

/// Print a section item (indented detail line).
#[macro_export]
macro_rules! section_item {
  ($config:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {{
    if $crate::show_output($config) && !$crate::show_verbose($config) {
      eprintln!("  - {}", format!($fmt $(, $arg)*));
    }
  }};
}

/// Print success footer with elapsed time.
#[macro_export]
macro_rules! cmd_ok {
  ($config:expr, $msg:expr, $elapsed:expr) => {{
    if $crate::show_output($config) {
      use colored::Colorize;
      eprintln!(
        "\n{} {} ({})",
        "\u{2713}".bright_green().bold(),
        $msg,
        $crate::format_duration($elapsed)
      );
    }
  }};
}

/// Print failure footer with elapsed time.
#[macro_export]
macro_rules! cmd_fail {
  ($config:expr, $msg:expr, $elapsed:expr) => {{
    if $crate::show_output($config) {
      use colored::Colorize;
      eprintln!(
        "\n{} {} ({})",
        "\u{2717}".bright_red().bold(),
        $msg,
        $crate::format_duration($elapsed)
      );
    }
  }};
}

/// Print an artifact line (e.g., "  Binary: build/main").
#[macro_export]
macro_rules! cmd_artifact {
  ($config:expr, $label:expr, $path:expr) => {{
    if $crate::show_output($config) {
      eprintln!("  {}: {}", $label, $path);
    }
  }};
}

/// Print error/warning stats.
#[macro_export]
macro_rules! cmd_stats {
  ($config:expr, $errors:expr, $warnings:expr) => {{
    if $crate::show_output($config) {
      use colored::Colorize;
      if $errors > 0 && $warnings > 0 {
        eprintln!(
          "  {} error(s), {} warning(s)",
          format!("{}", $errors).red(),
          format!("{}", $warnings).yellow()
        );
      } else if $errors > 0 {
        eprintln!("  {} error(s)", format!("{}", $errors).red());
      } else if $warnings > 0 {
        eprintln!("  {} warning(s)", format!("{}", $warnings).yellow());
      }
    }
  }};
}
