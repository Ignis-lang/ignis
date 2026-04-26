use std::path::{Path, PathBuf};

const DEFAULT_INDENT_WIDTH: usize = 2;
const DEFAULT_LINE_WIDTH: usize = 100;
const MIN_INDENT_WIDTH: usize = 1;
const MAX_INDENT_WIDTH: usize = 8;
const MIN_LINE_WIDTH: usize = 40;
const MAX_LINE_WIDTH: usize = 160;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatterConfig {
  pub indent_width: usize,
  pub line_width: usize,
  pub use_tabs: bool,
}

impl Default for FormatterConfig {
  fn default() -> Self {
    Self {
      indent_width: DEFAULT_INDENT_WIDTH,
      line_width: DEFAULT_LINE_WIDTH,
      use_tabs: false,
    }
  }
}

impl FormatterConfig {
  pub fn indent_string(
    &self,
    level: usize,
  ) -> String {
    if self.use_tabs {
      "\t".repeat(level)
    } else {
      " ".repeat(self.indent_columns(level))
    }
  }

  pub fn indent_columns(
    &self,
    level: usize,
  ) -> usize {
    level * self.indent_width
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FormatterCliOverrides {
  pub indent_width: Option<usize>,
  pub line_width: Option<usize>,
  pub use_tabs: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatterConfigPaths {
  pub project_root: PathBuf,
  pub ignis_toml: Option<PathBuf>,
  pub dedicated_config: Option<PathBuf>,
  pub explicit_config: Option<PathBuf>,
}

#[derive(Debug)]
pub enum FormatterConfigError {
  Io {
    path: PathBuf,
    message: String,
  },
  Parse {
    path: PathBuf,
    message: String,
  },
  UnknownKey {
    path: PathBuf,
    key: String,
  },
  InvalidValue {
    path: PathBuf,
    key: String,
    message: String,
  },
}

impl std::fmt::Display for FormatterConfigError {
  fn fmt(
    &self,
    formatter: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      Self::Io { path, message } => {
        write!(formatter, "failed to read formatter config '{}': {message}", path.display())
      },
      Self::Parse { path, message } => {
        write!(formatter, "failed to parse formatter config '{}': {message}", path.display())
      },
      Self::UnknownKey { path, key } => {
        write!(
          formatter,
          "formatter config '{}' contains unsupported key '{key}'",
          path.display()
        )
      },
      Self::InvalidValue { path, key, message } => {
        write!(
          formatter,
          "formatter config '{}' has invalid '{key}': {message}",
          path.display()
        )
      },
    }
  }
}

impl std::error::Error for FormatterConfigError {}

pub fn load_formatter_config(
  paths: &FormatterConfigPaths,
  cli_overrides: &FormatterCliOverrides,
) -> Result<FormatterConfig, FormatterConfigError> {
  let mut config = FormatterConfig::default();

  if let Some(path) = &paths.ignis_toml {
    apply_ignis_toml_config(path, &mut config)?;
  }

  let dedicated_path = paths.explicit_config.as_ref().or(paths.dedicated_config.as_ref());
  if let Some(path) = dedicated_path {
    apply_dedicated_config(path, &mut config)?;
  }

  if let Some(indent_width) = cli_overrides.indent_width {
    config.indent_width = validate_bound(
      Path::new("<cli>"),
      "indent_width",
      indent_width,
      MIN_INDENT_WIDTH,
      MAX_INDENT_WIDTH,
    )?;
  }

  if let Some(line_width) = cli_overrides.line_width {
    config.line_width = validate_bound(Path::new("<cli>"), "line_width", line_width, MIN_LINE_WIDTH, MAX_LINE_WIDTH)?;
  }

  if let Some(use_tabs) = cli_overrides.use_tabs {
    config.use_tabs = use_tabs;
  }

  Ok(config)
}

fn apply_ignis_toml_config(
  path: &Path,
  config: &mut FormatterConfig,
) -> Result<(), FormatterConfigError> {
  let value = read_toml(path)?;
  let Some(table) = value.get("formatter") else {
    return Ok(());
  };

  let Some(table) = table.as_table() else {
    return Err(FormatterConfigError::InvalidValue {
      path: path.to_path_buf(),
      key: "formatter".to_string(),
      message: "expected a table".to_string(),
    });
  };

  apply_table(path, table, config)
}

fn apply_dedicated_config(
  path: &Path,
  config: &mut FormatterConfig,
) -> Result<(), FormatterConfigError> {
  let value = read_toml(path)?;
  let Some(table) = value.as_table() else {
    return Err(FormatterConfigError::InvalidValue {
      path: path.to_path_buf(),
      key: "root".to_string(),
      message: "expected a table".to_string(),
    });
  };

  apply_table(path, table, config)
}

fn read_toml(path: &Path) -> Result<toml::Value, FormatterConfigError> {
  let contents = std::fs::read_to_string(path).map_err(|error| FormatterConfigError::Io {
    path: path.to_path_buf(),
    message: error.to_string(),
  })?;

  toml::from_str(&contents).map_err(|error| FormatterConfigError::Parse {
    path: path.to_path_buf(),
    message: error.to_string(),
  })
}

fn apply_table(
  path: &Path,
  table: &toml::Table,
  config: &mut FormatterConfig,
) -> Result<(), FormatterConfigError> {
  for key in table.keys() {
    if !matches!(key.as_str(), "indent_width" | "line_width" | "use_tabs") {
      return Err(FormatterConfigError::UnknownKey {
        path: path.to_path_buf(),
        key: key.clone(),
      });
    }
  }

  if let Some(indent_width) = table.get("indent_width") {
    config.indent_width = parse_bound(path, "indent_width", indent_width, MIN_INDENT_WIDTH, MAX_INDENT_WIDTH)?;
  }

  if let Some(line_width) = table.get("line_width") {
    config.line_width = parse_bound(path, "line_width", line_width, MIN_LINE_WIDTH, MAX_LINE_WIDTH)?;
  }

  if let Some(use_tabs) = table.get("use_tabs") {
    config.use_tabs = parse_bool(path, "use_tabs", use_tabs)?;
  }

  Ok(())
}

fn parse_bool(
  path: &Path,
  key: &str,
  value: &toml::Value,
) -> Result<bool, FormatterConfigError> {
  value.as_bool().ok_or_else(|| FormatterConfigError::InvalidValue {
    path: path.to_path_buf(),
    key: key.to_string(),
    message: "expected a boolean".to_string(),
  })
}

fn parse_bound(
  path: &Path,
  key: &str,
  value: &toml::Value,
  min: usize,
  max: usize,
) -> Result<usize, FormatterConfigError> {
  let Some(value) = value.as_integer() else {
    return Err(FormatterConfigError::InvalidValue {
      path: path.to_path_buf(),
      key: key.to_string(),
      message: "expected an integer".to_string(),
    });
  };

  validate_bound(path, key, value as usize, min, max)
}

fn validate_bound(
  path: &Path,
  key: &str,
  value: usize,
  min: usize,
  max: usize,
) -> Result<usize, FormatterConfigError> {
  if !(min..=max).contains(&value) {
    return Err(FormatterConfigError::InvalidValue {
      path: path.to_path_buf(),
      key: key.to_string(),
      message: format!("expected {value} to be within {min}..={max}"),
    });
  }

  Ok(value)
}
