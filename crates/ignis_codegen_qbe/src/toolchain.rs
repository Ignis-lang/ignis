use std::path::Path;
use std::process::Command;

pub fn compile_qbe_to_object(
  qbe_path: &Path,
  obj_path: &Path,
  compiler: &str,
  cflags: &[String],
  quiet: bool,
) -> Result<(), String> {
  let asm_path = obj_path.with_extension("s");

  let mut qbe_cmd = Command::new("qbe");
  qbe_cmd.arg(qbe_path).arg("-o").arg(&asm_path);

  if !quiet {
    eprintln!(
      "    --> QBE {} -> {}",
      qbe_path.display(),
      asm_path.display()
    );
  }

  let qbe_output = qbe_cmd.output().map_err(|error| format!("Failed to run qbe: {}", error))?;
  if !qbe_output.status.success() {
    let stderr = String::from_utf8_lossy(&qbe_output.stderr);
    return Err(format!("Error: qbe compilation failed:\n{}", stderr.trim()));
  }

  let compiler = if compiler.trim().is_empty() {
    "gcc"
  } else {
    compiler
  };

  let mut cc_cmd = Command::new(compiler);
  cc_cmd.arg("-c").arg(&asm_path).arg("-o").arg(obj_path);

  for flag in cflags {
    cc_cmd.arg(flag);
  }

  if !quiet {
    eprintln!(
      "    --> Assembling {} -> {}",
      asm_path.display(),
      obj_path.display()
    );
  }

  let cc_output = cc_cmd
    .output()
    .map_err(|error| format!("Failed to run {}: {}", compiler, error))?;

  if !cc_output.status.success() {
    let stderr = String::from_utf8_lossy(&cc_output.stderr);
    return Err(format!(
      "Error: {} assembly failed:\n{}",
      compiler,
      stderr.trim()
    ));
  }

  Ok(())
}
