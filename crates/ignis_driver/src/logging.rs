use ignis_config::{DebugTrace, IgnisConfig};

pub(crate) fn effective_verbose(config: &IgnisConfig) -> u8 {
  if config.quiet {
    return 0;
  }

  if config.debug && config.verbose < 2 {
    return 2;
  }

  config.verbose
}

pub(crate) fn log_phase(config: &IgnisConfig) -> bool {
  !config.quiet
}

pub(crate) fn log_info(config: &IgnisConfig) -> bool {
  effective_verbose(config) >= 1
}

pub(crate) fn log_debug(config: &IgnisConfig) -> bool {
  effective_verbose(config) >= 2
}

pub(crate) fn log_trace(config: &IgnisConfig) -> bool {
  effective_verbose(config) >= 3
}

pub(crate) fn debug_trace_enabled(
  config: &IgnisConfig,
  trace: DebugTrace,
) -> bool {
  !config.quiet && (config.debug || config.debug_trace.contains(&trace))
}
