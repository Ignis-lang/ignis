#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LintId {
  UnusedVariable,
  UnusedImport,
  UnusedMut,
  Deprecated,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintLevel {
  Allow,
  Warn,
  Deny,
}
