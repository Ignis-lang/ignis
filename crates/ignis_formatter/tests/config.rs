use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use ignis_formatter::{
  FormatterCliOverrides, FormatterConfig, FormatterConfigError, FormatterConfigPaths, FormatOptions, format_text,
  load_formatter_config,
};

fn temp_dir(label: &str) -> PathBuf {
  let unique = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .expect("system time before unix epoch")
    .as_nanos();

  let path = std::env::temp_dir().join(format!("ignis-formatter-config-{label}-{}-{unique}", std::process::id()));
  fs::create_dir_all(&path).expect("create temp dir");
  path
}

fn write_file(
  root: &Path,
  relative_path: &str,
  contents: &str,
) {
  let file_path = root.join(relative_path);
  if let Some(parent) = file_path.parent() {
    fs::create_dir_all(parent).expect("create parent dir");
  }

  fs::write(file_path, contents).expect("write config file");
}

#[test]
fn loads_bounded_defaults_without_project_files() {
  let root = temp_dir("defaults");

  let config = load_formatter_config(
    &FormatterConfigPaths {
      project_root: root.clone(),
      ignis_toml: None,
      dedicated_config: None,
      explicit_config: None,
    },
    &FormatterCliOverrides::default(),
  )
  .expect("load default formatter config");

  assert_eq!(
    config,
    FormatterConfig {
      indent_width: 2,
      line_width: 100,
      use_tabs: false,
    }
  );
}

#[test]
fn resolves_bridge_file_and_cli_precedence_for_formatter_settings() {
  let root = temp_dir("precedence");
  write_file(
    &root,
    "ignis.toml",
    "[package]\nname = \"fixture\"\nversion = \"0.1.0\"\n\n[formatter]\nindent_width = 4\nline_width = 90\nuse_tabs = true\n",
  );
  write_file(&root, "ignisfmt.toml", "indent_width = 6\nline_width = 120\nuse_tabs = true\n");

  let config = load_formatter_config(
    &FormatterConfigPaths {
      project_root: root.clone(),
      ignis_toml: Some(root.join("ignis.toml")),
      dedicated_config: Some(root.join("ignisfmt.toml")),
      explicit_config: None,
    },
    &FormatterCliOverrides {
      indent_width: Some(8),
      line_width: Some(100),
      use_tabs: Some(false),
    },
  )
  .expect("load formatter config with precedence");

  assert_eq!(
    config,
    FormatterConfig {
      indent_width: 8,
      line_width: 100,
      use_tabs: false,
    }
  );
}

#[test]
fn dedicated_config_can_enable_tabs_without_cli_override() {
  let root = temp_dir("tabs-dedicated");
  write_file(&root, "ignisfmt.toml", "indent_width = 4\nuse_tabs = true\n");

  let config = load_formatter_config(
    &FormatterConfigPaths {
      project_root: root.clone(),
      ignis_toml: None,
      dedicated_config: Some(root.join("ignisfmt.toml")),
      explicit_config: None,
    },
    &FormatterCliOverrides::default(),
  )
  .expect("load formatter config with tabs from dedicated config");

  assert_eq!(
    config,
    FormatterConfig {
      indent_width: 4,
      line_width: 100,
      use_tabs: true,
    }
  );
}

#[test]
fn rejects_unknown_formatter_keys_in_dedicated_config() {
  let root = temp_dir("unknown-keys");
  write_file(&root, "ignisfmt.toml", "indent_width = 2\ntrailing_commas = true\n");

  let error = load_formatter_config(
    &FormatterConfigPaths {
      project_root: root.clone(),
      ignis_toml: None,
      dedicated_config: Some(root.join("ignisfmt.toml")),
      explicit_config: None,
    },
    &FormatterCliOverrides::default(),
  )
  .expect_err("unknown formatter key should fail");

  assert!(matches!(error, FormatterConfigError::UnknownKey { .. }));
  assert!(error.to_string().contains("trailing_commas"));
}

#[test]
fn format_text_honors_indent_width_inside_printer_owned_directive_branches() {
  let source = r#"@if(featureFlag) {
/// docs
function   main ( ) : void {return;}
}
@else {
// fallback
function   alt ( ) : void {return;}
}
"#;

  let formatted = format_text(
    source,
    &FormatOptions {
      check: false,
      config: FormatterConfig {
        indent_width: 2,
        line_width: 100,
        use_tabs: false,
      },
    },
  )
  .expect("format directive branch with custom indent width");

  assert_eq!(
    formatted,
    r#"@if(featureFlag) {
  /// docs
  function main(): void {
    return;
  }
}
@else {
  // fallback
  function alt(): void {
    return;
  }
}
"#
  );
}

#[test]
fn format_text_uses_tabs_when_requested() {
  let formatted = format_text(
    "function   main ( ) : void {return;}\n",
    &FormatOptions {
      check: false,
      config: FormatterConfig {
        indent_width: 4,
        line_width: 100,
        use_tabs: true,
      },
    },
  )
  .expect("format with tabs");

  assert_eq!(formatted, "function main(): void {\n\treturn;\n}\n");
}
