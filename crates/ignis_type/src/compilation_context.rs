use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct TargetInfo {
  pub triple: String,
  pub os: String,
  pub arch: String,
  pub abi: String,
}

impl Default for TargetInfo {
  fn default() -> Self {
    let os = std::env::consts::OS.to_string();
    let arch = std::env::consts::ARCH.to_string();
    let abi = host_abi().to_string();

    let triple = if abi.is_empty() {
      format!("{}-unknown-{}", arch, os)
    } else {
      format!("{}-unknown-{}-{}", arch, os, abi)
    };

    Self {
      triple,
      os,
      arch,
      abi,
    }
  }
}

impl TargetInfo {
  pub fn from_triple(triple: &str) -> Self {
    let trimmed = triple.trim();
    if trimmed.is_empty() {
      return Self::default();
    }

    let mut target = Self {
      triple: trimmed.to_string(),
      ..Self::default()
    };

    let parts: Vec<&str> = trimmed.split('-').collect();

    if let Some(arch) = parts.first()
      && !arch.is_empty()
    {
      target.arch = (*arch).to_string();
    }

    if parts.len() >= 3 {
      target.os = normalize_platform(parts[2]);
    } else if parts.len() == 2 {
      target.os = normalize_platform(parts[1]);
    }

    if parts.len() >= 4 {
      target.abi = parts[3].to_ascii_lowercase();
    } else {
      target.abi = String::new();
    }

    target
  }
}

fn host_abi() -> &'static str {
  if cfg!(target_env = "gnu") {
    "gnu"
  } else if cfg!(target_env = "musl") {
    "musl"
  } else if cfg!(target_env = "msvc") {
    "msvc"
  } else {
    ""
  }
}

fn normalize_platform(raw: &str) -> String {
  let normalized = raw.to_ascii_lowercase();

  if normalized.contains("darwin") || normalized.contains("macos") {
    return "macos".to_string();
  }

  if normalized.contains("windows") || normalized.contains("mingw") || normalized.contains("msvc") {
    return "windows".to_string();
  }

  if normalized.contains("linux") {
    return "linux".to_string();
  }

  normalized
}

#[derive(Debug, Clone)]
pub struct CompilationContext {
  pub target: TargetInfo,
  pub debug: bool,
  pub features: HashSet<String>,
  pub known_features: Option<HashSet<String>>,
}

impl Default for CompilationContext {
  fn default() -> Self {
    Self {
      target: TargetInfo::default(),
      debug: cfg!(debug_assertions),
      features: HashSet::new(),
      known_features: None,
    }
  }
}

impl CompilationContext {
  pub fn from_target_triple(target_triple: &str) -> Self {
    let mut ctx = Self::default();
    ctx.target = TargetInfo::from_triple(target_triple);
    ctx
  }

  pub fn is_known_feature(
    &self,
    feature: &str,
  ) -> bool {
    self
      .known_features
      .as_ref()
      .map(|known| known.contains(feature))
      .unwrap_or(true)
  }

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

    if let Some(rest) = key.strip_prefix("abi.") {
      return Some(self.target.abi == rest);
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
