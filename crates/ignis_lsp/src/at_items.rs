/// Registry of `@`-prefixed items (builtins and directives) for LSP features.
///
/// Source of truth for what builtins and directives exist in the language.
pub use ignis_type::at_items::{AtItemKind, AtItemMeta};

/// Look up an `@`-item by name.
pub fn lookup(name: &str) -> Option<&'static AtItemMeta> {
  ignis_type::at_items::lookup(name)
}

/// Return all `@`-items matching a prefix.
pub fn completions_matching(prefix: &str) -> Vec<&'static AtItemMeta> {
  ignis_type::at_items::completions_matching(prefix)
}

/// Format an `@`-item as a Markdown hover string.
pub fn format_hover(item: &AtItemMeta) -> String {
  let kind_label = match item.kind {
    AtItemKind::Builtin => "builtin",
    AtItemKind::Directive => "directive",
  };

  let mut result = format!("```ignis\n{}\n```\n\n({}) {}", item.syntax, kind_label, item.summary);

  if let Some(doc) = item.doc {
    result.push_str("\n\n---\n\n");
    result.push_str(doc);
  }

  result
}

/// Generate a snippet insert text for a builtin (for completion).
pub fn snippet_for(item: &AtItemMeta) -> Option<String> {
  if item.kind != AtItemKind::Builtin {
    return None;
  }

  match item.name {
    "sizeOf" | "alignOf" | "typeName" | "dropGlue" => Some(format!("{}<${{1:T}}>()", item.name)),
    "bitCast" | "pointerCast" | "read" | "dropInPlace" => Some(format!("{}<${{1:T}}>(${{2:expr}})", item.name)),
    "pointerFromInteger" => Some(format!("{}<${{1:*const T}}>(${{2:addr}})", item.name)),
    "write" => Some(format!("{}<${{1:T}}>(${{2:ptr}}, ${{3:value}})", item.name)),
    "integerFromPointer" => Some(format!("{}(${{1:ptr}})", item.name)),
    "compileError" => Some(format!("{}(\"${{1:message}}\")", item.name)),
    "panic" => Some(format!("{}(\"${{1:message}}\")", item.name)),
    "trap" | "unreachable" => Some(format!("{}()", item.name)),
    _ => None,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn lookup_known_builtin() {
    let item = lookup("sizeOf").unwrap();
    assert_eq!(item.kind, AtItemKind::Builtin);
    assert!(item.syntax.contains("@sizeOf"));
  }

  #[test]
  fn lookup_known_directive() {
    let item = lookup("packed").unwrap();
    assert_eq!(item.kind, AtItemKind::Directive);
  }

  #[test]
  fn lookup_unknown_returns_none() {
    assert!(lookup("nonexistent").is_none());
  }

  #[test]
  fn completions_matching_prefix() {
    let matches = completions_matching("si");
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].name, "sizeOf");
  }

  #[test]
  fn completions_matching_empty_returns_all() {
    let all = completions_matching("");
    assert_eq!(all.len(), ignis_type::at_items::all().len());
  }

  #[test]
  fn completions_matching_case_insensitive() {
    let matches = completions_matching("SIZE");
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].name, "sizeOf");
  }

  #[test]
  fn hover_format_includes_syntax_and_summary() {
    let item = lookup("panic").unwrap();
    let hover = format_hover(item);
    assert!(hover.contains("@panic"));
    assert!(hover.contains("(builtin)"));
    assert!(hover.contains("Terminates"));
  }

  #[test]
  fn hover_format_directive_includes_doc() {
    let item = lookup("packed").unwrap();
    let hover = format_hover(item);
    assert!(hover.contains("(directive)"));
    assert!(hover.contains("record"));
  }

  #[test]
  fn snippet_for_builtin_with_type_arg() {
    let item = lookup("sizeOf").unwrap();
    let snippet = snippet_for(item).unwrap();
    assert!(snippet.contains("sizeOf<"));
    assert!(snippet.contains("${1:T}"));
  }

  #[test]
  fn snippet_for_directive_returns_none() {
    let item = lookup("packed").unwrap();
    assert!(snippet_for(item).is_none());
  }
}
