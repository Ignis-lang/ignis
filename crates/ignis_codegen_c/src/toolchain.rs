use std::path::{Path, PathBuf};
use std::process::Command;

/// Compile C source into an object file using the provided toolchain settings.
pub fn compile_c_to_object(
  c_path: &Path,
  obj_path: &Path,
  compiler: &str,
  cflags: &[String],
  include_dirs: &[PathBuf],
  quiet: bool,
) -> Result<(), String> {
  let compiler = if compiler.trim().is_empty() {
    "gcc"
  } else {
    compiler
  };

  let mut cmd = Command::new(compiler);
  cmd.arg("-c").arg(c_path).arg("-o").arg(obj_path);

  for flag in cflags {
    cmd.arg(flag);
  }

  for include_dir in include_dirs {
    cmd.arg("-I").arg(include_dir);
  }

  if !quiet {
    eprintln!(
      "    {} Compiling {} -> {}",
      "-->".to_string(),
      c_path.display(),
      obj_path.display()
    );
  }

  let output = cmd.output().map_err(|error| format!("Failed to run {}: {}", compiler, error))?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(format!(
      "Error: {} compilation failed:\n{}",
      compiler,
      stderr.trim()
    ));
  }

  Ok(())
}
