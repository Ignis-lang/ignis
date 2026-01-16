use std::collections::BTreeMap;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};

/// Build output directory structure.
///
/// ```text
/// {base}/
///   std/
///     include/   .h files (ignis_std.h, std_io.h, ...)
///     src/       .c files
///     obj/       .o files
///     lib/       libignis_std.a
///     .stamp     cache invalidation
///   user/
///     include/   .h files (preserves source structure)
///     src/       .c files
///     obj/       .o files
///     .stamp
///   bin/         final executables
/// ```
#[derive(Debug, Clone)]
pub struct BuildLayout {
  base: PathBuf,
  project_name: String,
}

impl BuildLayout {
  pub fn new(
    project_name: &str,
    out_dir: &Path,
  ) -> Self {
    Self {
      base: out_dir.to_path_buf(),
      project_name: project_name.to_string(),
    }
  }

  pub fn base(&self) -> &Path {
    &self.base
  }

  pub fn project_name(&self) -> &str {
    &self.project_name
  }

  // -- Std paths --

  pub fn std_dir(&self) -> PathBuf {
    self.base.join("std")
  }

  pub fn std_include_dir(&self) -> PathBuf {
    self.std_dir().join("include")
  }

  pub fn std_src_dir(&self) -> PathBuf {
    self.std_dir().join("src")
  }

  pub fn std_obj_dir(&self) -> PathBuf {
    self.std_dir().join("obj")
  }

  pub fn std_lib_dir(&self) -> PathBuf {
    self.std_dir().join("lib")
  }

  pub fn std_lib_path(&self) -> PathBuf {
    self.std_lib_dir().join("libignis_std.a")
  }

  pub fn std_umbrella_header(&self) -> PathBuf {
    self.std_include_dir().join("ignis_std.h")
  }

  pub fn std_module_header(
    &self,
    module: &str,
  ) -> PathBuf {
    self.std_include_dir().join(format!("std_{}.h", module))
  }

  pub fn std_module_src(
    &self,
    module: &str,
  ) -> PathBuf {
    self.std_src_dir().join(format!("std_{}.c", module))
  }

  pub fn std_module_obj(
    &self,
    module: &str,
  ) -> PathBuf {
    self.std_obj_dir().join(format!("std_{}.o", module))
  }

  pub fn std_stamp_path(&self) -> PathBuf {
    self.std_dir().join(".stamp")
  }

  // -- User paths --

  pub fn user_dir(&self) -> PathBuf {
    self.base.join("user")
  }

  pub fn user_include_dir(&self) -> PathBuf {
    self.user_dir().join("include")
  }

  pub fn user_src_dir(&self) -> PathBuf {
    self.user_dir().join("src")
  }

  pub fn user_obj_dir(&self) -> PathBuf {
    self.user_dir().join("obj")
  }

  pub fn user_umbrella_header(&self) -> PathBuf {
    self.user_include_dir().join("ignis_user.h")
  }

  /// Preserves source path structure: `src/math/utils.ign` -> `include/src/math/utils.h`
  pub fn user_module_header(
    &self,
    source_path: &Path,
  ) -> PathBuf {
    self.user_include_dir().join(source_path.with_extension("h"))
  }

  /// Preserves source path structure: `src/math/utils.ign` -> `src/src/math/utils.c`
  pub fn user_module_src(
    &self,
    source_path: &Path,
  ) -> PathBuf {
    self.user_src_dir().join(source_path.with_extension("c"))
  }

  /// Preserves source path structure: `src/math/utils.ign` -> `obj/src/math/utils.o`
  pub fn user_module_obj(
    &self,
    source_path: &Path,
  ) -> PathBuf {
    self.user_obj_dir().join(source_path.with_extension("o"))
  }

  pub fn user_stamp_path(&self) -> PathBuf {
    self.user_dir().join(".stamp")
  }

  // -- Output paths --

  pub fn bin_dir(&self) -> PathBuf {
    self.base.join("bin")
  }

  pub fn bin_path(&self) -> PathBuf {
    self.bin_dir().join(&self.project_name)
  }

  // -- Directory creation --

  pub fn create_all_dirs(&self) -> io::Result<()> {
    std::fs::create_dir_all(self.std_include_dir())?;
    std::fs::create_dir_all(self.std_src_dir())?;
    std::fs::create_dir_all(self.std_obj_dir())?;
    std::fs::create_dir_all(self.std_lib_dir())?;
    std::fs::create_dir_all(self.user_src_dir())?;
    std::fs::create_dir_all(self.user_obj_dir())?;
    std::fs::create_dir_all(self.bin_dir())?;
    Ok(())
  }

  pub fn create_std_dirs(&self) -> io::Result<()> {
    std::fs::create_dir_all(self.std_include_dir())?;
    std::fs::create_dir_all(self.std_src_dir())?;
    std::fs::create_dir_all(self.std_obj_dir())?;
    std::fs::create_dir_all(self.std_lib_dir())?;
    Ok(())
  }

  pub fn create_user_dirs(&self) -> io::Result<()> {
    std::fs::create_dir_all(self.user_include_dir())?;
    std::fs::create_dir_all(self.user_src_dir())?;
    std::fs::create_dir_all(self.user_obj_dir())?;
    std::fs::create_dir_all(self.bin_dir())?;
    Ok(())
  }

  pub fn ensure_user_module_dirs(
    &self,
    source_path: &Path,
  ) -> io::Result<()> {
    for path in [
      self.user_module_header(source_path),
      self.user_module_src(source_path),
      self.user_module_obj(source_path),
    ] {
      if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
      }
    }
    Ok(())
  }
}

// -- Stamp files for cache invalidation --
//
// Format: key=value lines, sorted by key.
// Keys: "compiler_version", "file:{path}" (mtime in seconds)

const STAMP_KEY_COMPILER_VERSION: &str = "compiler_version";
const STAMP_KEY_FILE_PREFIX: &str = "file:";

pub fn compute_std_stamp(
  std_path: &Path,
  compiler_version: &str,
) -> io::Result<BTreeMap<String, String>> {
  let mut stamp = BTreeMap::new();

  stamp.insert(STAMP_KEY_COMPILER_VERSION.to_string(), compiler_version.to_string());

  collect_source_mtimes(std_path, &mut stamp)?;

  Ok(stamp)
}

fn collect_source_mtimes(
  dir: &Path,
  stamp: &mut BTreeMap<String, String>,
) -> io::Result<()> {
  if !dir.is_dir() {
    return Ok(());
  }

  for entry in std::fs::read_dir(dir)? {
    let entry = entry?;
    let path = entry.path();

    if path.is_dir() {
      collect_source_mtimes(&path, stamp)?;
    } else if path.extension().map(|e| e == "ign").unwrap_or(false) {
      let mtime = path
        .metadata()?
        .modified()?
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

      let key = format!("{}{}", STAMP_KEY_FILE_PREFIX, path.display());
      stamp.insert(key, mtime.to_string());
    }
  }

  Ok(())
}

pub fn write_stamp(
  stamp_path: &Path,
  stamp: &BTreeMap<String, String>,
) -> io::Result<()> {
  let mut file = std::fs::File::create(stamp_path)?;
  for (key, value) in stamp {
    writeln!(file, "{}={}", key, value)?;
  }
  Ok(())
}

pub fn read_stamp(stamp_path: &Path) -> io::Result<BTreeMap<String, String>> {
  let file = std::fs::File::open(stamp_path)?;
  let reader = io::BufReader::new(file);
  let mut stamp = BTreeMap::new();

  for line in reader.lines() {
    let line = line?;
    if let Some((key, value)) = line.split_once('=') {
      stamp.insert(key.to_string(), value.to_string());
    }
  }

  Ok(stamp)
}

pub fn is_stamp_valid(
  stamp_path: &Path,
  std_path: &Path,
  compiler_version: &str,
) -> bool {
  let saved_stamp = match read_stamp(stamp_path) {
    Ok(s) => s,
    Err(_) => return false,
  };

  let current_stamp = match compute_std_stamp(std_path, compiler_version) {
    Ok(s) => s,
    Err(_) => return false,
  };

  saved_stamp == current_stamp
}

pub fn compute_user_stamp(
  source_files: &[PathBuf],
  compiler_version: &str,
) -> io::Result<BTreeMap<String, String>> {
  let mut stamp = BTreeMap::new();

  stamp.insert(STAMP_KEY_COMPILER_VERSION.to_string(), compiler_version.to_string());

  for path in source_files {
    if path.exists() {
      let mtime = path
        .metadata()?
        .modified()?
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

      let key = format!("{}{}", STAMP_KEY_FILE_PREFIX, path.display());
      stamp.insert(key, mtime.to_string());
    }
  }

  Ok(stamp)
}

pub fn is_user_stamp_valid(
  stamp_path: &Path,
  source_files: &[PathBuf],
  compiler_version: &str,
) -> bool {
  let saved_stamp = match read_stamp(stamp_path) {
    Ok(s) => s,
    Err(_) => return false,
  };

  let current_stamp = match compute_user_stamp(source_files, compiler_version) {
    Ok(s) => s,
    Err(_) => return false,
  };

  saved_stamp == current_stamp
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_std_paths() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    assert_eq!(layout.std_dir(), PathBuf::from("/tmp/build/std"));
    assert_eq!(layout.std_include_dir(), PathBuf::from("/tmp/build/std/include"));
    assert_eq!(layout.std_src_dir(), PathBuf::from("/tmp/build/std/src"));
    assert_eq!(layout.std_obj_dir(), PathBuf::from("/tmp/build/std/obj"));
    assert_eq!(layout.std_lib_dir(), PathBuf::from("/tmp/build/std/lib"));
    assert_eq!(layout.std_lib_path(), PathBuf::from("/tmp/build/std/lib/libignis_std.a"));
    assert_eq!(layout.std_stamp_path(), PathBuf::from("/tmp/build/std/.stamp"));
  }

  #[test]
  fn test_std_module_paths() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    assert_eq!(layout.std_module_header("io"), PathBuf::from("/tmp/build/std/include/std_io.h"));
    assert_eq!(
      layout.std_module_header("string"),
      PathBuf::from("/tmp/build/std/include/std_string.h")
    );
    assert_eq!(layout.std_module_src("io"), PathBuf::from("/tmp/build/std/src/std_io.c"));
    assert_eq!(layout.std_module_obj("io"), PathBuf::from("/tmp/build/std/obj/std_io.o"));
  }

  #[test]
  fn test_user_paths() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    assert_eq!(layout.user_dir(), PathBuf::from("/tmp/build/user"));
    assert_eq!(layout.user_include_dir(), PathBuf::from("/tmp/build/user/include"));
    assert_eq!(layout.user_src_dir(), PathBuf::from("/tmp/build/user/src"));
    assert_eq!(layout.user_obj_dir(), PathBuf::from("/tmp/build/user/obj"));
    assert_eq!(
      layout.user_umbrella_header(),
      PathBuf::from("/tmp/build/user/include/ignis_user.h")
    );
    assert_eq!(layout.user_stamp_path(), PathBuf::from("/tmp/build/user/.stamp"));
  }

  #[test]
  fn test_user_module_paths_preserves_structure() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    // Simple file in root
    let main = Path::new("main.ign");
    assert_eq!(layout.user_module_header(main), PathBuf::from("/tmp/build/user/include/main.h"));
    assert_eq!(layout.user_module_src(main), PathBuf::from("/tmp/build/user/src/main.c"));
    assert_eq!(layout.user_module_obj(main), PathBuf::from("/tmp/build/user/obj/main.o"));

    // Nested file preserves directory structure
    let nested = Path::new("src/math/utils.ign");
    assert_eq!(
      layout.user_module_header(nested),
      PathBuf::from("/tmp/build/user/include/src/math/utils.h")
    );
    assert_eq!(
      layout.user_module_src(nested),
      PathBuf::from("/tmp/build/user/src/src/math/utils.c")
    );
    assert_eq!(
      layout.user_module_obj(nested),
      PathBuf::from("/tmp/build/user/obj/src/math/utils.o")
    );
  }

  #[test]
  fn test_bin_paths() {
    let layout = BuildLayout::new("myapp", Path::new("/tmp/build"));

    assert_eq!(layout.bin_dir(), PathBuf::from("/tmp/build/bin"));
    assert_eq!(layout.bin_path(), PathBuf::from("/tmp/build/bin/myapp"));
  }

  #[test]
  fn test_stamp_roundtrip() {
    use std::collections::BTreeMap;
    let temp_dir = std::env::temp_dir().join("ignis_stamp_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let stamp_path = temp_dir.join(".stamp");

    let mut stamp = BTreeMap::new();
    stamp.insert("compiler_version".to_string(), "1.0.0".to_string());
    stamp.insert("file:foo.ign".to_string(), "12345".to_string());
    stamp.insert("file:bar.ign".to_string(), "67890".to_string());

    write_stamp(&stamp_path, &stamp).unwrap();
    let loaded = read_stamp(&stamp_path).unwrap();

    assert_eq!(stamp, loaded);

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_stamp_validity() {
    let temp_dir = std::env::temp_dir().join("ignis_stamp_validity_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let std_dir = temp_dir.join("std");
    std::fs::create_dir_all(&std_dir).unwrap();
    std::fs::write(std_dir.join("test.ign"), "// test").unwrap();

    let stamp_path = temp_dir.join(".stamp");

    // Compute and write stamp
    let stamp = compute_std_stamp(&std_dir, "1.0.0").unwrap();
    write_stamp(&stamp_path, &stamp).unwrap();

    // Should be valid immediately after
    assert!(is_stamp_valid(&stamp_path, &std_dir, "1.0.0"));

    // Should be invalid with different compiler version
    assert!(!is_stamp_valid(&stamp_path, &std_dir, "2.0.0"));

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_stamp_invalid_after_file_change() {
    let temp_dir = std::env::temp_dir().join("ignis_stamp_filechange_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let std_dir = temp_dir.join("std");
    std::fs::create_dir_all(&std_dir).unwrap();
    let test_file = std_dir.join("test.ign");
    std::fs::write(&test_file, "// test").unwrap();

    let stamp_path = temp_dir.join(".stamp");

    // Write a stamp with an old mtime (simulating that the file changed after the stamp)
    let mut stamp = BTreeMap::new();
    stamp.insert("compiler_version".to_string(), "1.0.0".to_string());
    // Use an old timestamp that will never match the actual file
    stamp.insert(format!("file:{}", test_file.display()), "0".to_string());
    write_stamp(&stamp_path, &stamp).unwrap();

    // Should be invalid because the mtime in stamp (0) doesn't match the actual file
    assert!(!is_stamp_valid(&stamp_path, &std_dir, "1.0.0"));

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_stamp_invalid_with_new_file() {
    let temp_dir = std::env::temp_dir().join("ignis_stamp_newfile_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let std_dir = temp_dir.join("std");
    std::fs::create_dir_all(&std_dir).unwrap();
    std::fs::write(std_dir.join("original.ign"), "// original").unwrap();

    let stamp_path = temp_dir.join(".stamp");

    // Compute and write stamp for current state
    let stamp = compute_std_stamp(&std_dir, "1.0.0").unwrap();
    write_stamp(&stamp_path, &stamp).unwrap();

    // Should be valid
    assert!(is_stamp_valid(&stamp_path, &std_dir, "1.0.0"));

    // Add a new file
    std::fs::write(std_dir.join("new.ign"), "// new file").unwrap();

    // Should be invalid because new file not in stamp
    assert!(!is_stamp_valid(&stamp_path, &std_dir, "1.0.0"));

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }

  #[test]
  fn test_stamp_missing_file_invalid() {
    let temp_dir = std::env::temp_dir().join("ignis_stamp_missing_test");
    let _ = std::fs::remove_dir_all(&temp_dir);
    std::fs::create_dir_all(&temp_dir).unwrap();

    let std_dir = temp_dir.join("std");
    std::fs::create_dir_all(&std_dir).unwrap();

    let stamp_path = temp_dir.join(".stamp");

    // Non-existent stamp should be invalid
    assert!(!is_stamp_valid(&stamp_path, &std_dir, "1.0.0"));

    std::fs::remove_dir_all(&temp_dir).unwrap();
  }
}
