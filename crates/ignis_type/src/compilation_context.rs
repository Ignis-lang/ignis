use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct TargetInfo {
  pub os: String,
  pub arch: String,
}

impl Default for TargetInfo {
  fn default() -> Self {
    Self {
      os: std::env::consts::OS.to_string(),
      arch: std::env::consts::ARCH.to_string(),
    }
  }
}

#[derive(Debug, Clone)]
pub struct CompilationContext {
  pub target: TargetInfo,
  pub debug: bool,
  pub features: HashSet<String>,
}

impl Default for CompilationContext {
  fn default() -> Self {
    Self {
      target: TargetInfo::default(),
      debug: cfg!(debug_assertions),
      features: HashSet::new(),
    }
  }
}

impl CompilationContext {
  pub fn resolve_flag(
    &self,
    key: &str,
  ) -> Option<bool> {
    if let Some(rest) = key.strip_prefix("os.") {
      return Some(self.target.os == rest);
    }

    if let Some(rest) = key.strip_prefix("arch.") {
      return Some(self.target.arch == rest);
    }

    match key {
      "build.debug" => Some(self.debug),
      "build.release" => Some(!self.debug),
      _ => key
        .strip_prefix("feature.")
        .map(|feature| self.features.contains(feature)),
    }
  }
}
