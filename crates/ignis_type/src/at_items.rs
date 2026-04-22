#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AtItemKind {
  Builtin,
  Directive,
}

#[derive(Debug, Clone)]
pub struct AtItemMeta {
  pub name: &'static str,
  pub kind: AtItemKind,
  pub syntax: &'static str,
  pub summary: &'static str,
  pub doc: Option<&'static str>,
}

static AT_ITEMS: &[AtItemMeta] = &[
  AtItemMeta {
    name: "typeOf",
    kind: AtItemKind::Builtin,
    syntax: "@typeOf(expr)",
    summary: "Returns the semantic type of `expr` for compile-time builtin dispatch.",
    doc: Some("Internal builtin used by semantic analysis. Not all tooling surfaces it yet."),
  },
  AtItemMeta {
    name: "sizeOf",
    kind: AtItemKind::Builtin,
    syntax: "@sizeOf<T>()",
    summary: "Returns the size in bytes of type `T` as `u64`.",
    doc: Some("Emits C `sizeof(T)`. The type argument is required; no expression arguments."),
  },
  AtItemMeta {
    name: "alignOf",
    kind: AtItemKind::Builtin,
    syntax: "@alignOf<T>()",
    summary: "Returns the alignment in bytes of type `T` as `u64`.",
    doc: Some("Emits C `_Alignof(T)`. The type argument is required; no expression arguments."),
  },
  AtItemMeta {
    name: "typeName",
    kind: AtItemKind::Builtin,
    syntax: "@typeName<T>()",
    summary: "Returns the name of type `T` as a `str`.",
    doc: Some("Resolved at compile time to a string literal (`str`). No runtime cost."),
  },
  AtItemMeta {
    name: "bitCast",
    kind: AtItemKind::Builtin,
    syntax: "@bitCast<T>(value)",
    summary: "Reinterprets the bits of `value` as type `T`.",
    doc: Some("Requires `@sizeOf<Source>() == @sizeOf<T>()`. Emits a `memcpy` with a compile-time size assertion."),
  },
  AtItemMeta {
    name: "pointerCast",
    kind: AtItemKind::Builtin,
    syntax: "@pointerCast<T>(ptr)",
    summary: "Casts a pointer to a different pointer type `T`.",
    doc: Some("Both the type argument and the expression must be pointer types."),
  },
  AtItemMeta {
    name: "integerFromPointer",
    kind: AtItemKind::Builtin,
    syntax: "@integerFromPointer(ptr)",
    summary: "Converts a pointer to its integer address as `u64`.",
    doc: None,
  },
  AtItemMeta {
    name: "pointerFromInteger",
    kind: AtItemKind::Builtin,
    syntax: "@pointerFromInteger<T>(addr)",
    summary: "Creates a pointer of type `T` from an integer address.",
    doc: Some("The type argument must be a pointer type. The expression must be an integer."),
  },
  AtItemMeta {
    name: "read",
    kind: AtItemKind::Builtin,
    syntax: "@read<T>(ptr)",
    summary: "Loads a value of type `T` from a pointer.",
    doc: Some("Performs an untyped load through a raw pointer. Equivalent to C dereference."),
  },
  AtItemMeta {
    name: "write",
    kind: AtItemKind::Builtin,
    syntax: "@write<T>(ptr, value)",
    summary: "Stores a value of type `T` through a pointer.",
    doc: Some("Performs an untyped store through a raw pointer. Takes the pointer and the value to write."),
  },
  AtItemMeta {
    name: "dropInPlace",
    kind: AtItemKind::Builtin,
    syntax: "@dropInPlace<T>(ptr)",
    summary: "Calls the drop glue for type `T` on the value behind `ptr`.",
    doc: Some("Used for manual resource cleanup through raw pointers. Only valid for types that implement Drop."),
  },
  AtItemMeta {
    name: "dropGlue",
    kind: AtItemKind::Builtin,
    syntax: "@dropGlue<T>()",
    summary: "Returns the drop glue function pointer for type `T`.",
    doc: Some("Returns `(*mut u8) -> void`. Used for passing drop functions to allocators and containers."),
  },
  AtItemMeta {
    name: "maxOf",
    kind: AtItemKind::Builtin,
    syntax: "@maxOf<T>()",
    summary: "Returns the maximum representable value of type `T`.",
    doc: Some("Compile-time builtin for integer limits."),
  },
  AtItemMeta {
    name: "minOf",
    kind: AtItemKind::Builtin,
    syntax: "@minOf<T>()",
    summary: "Returns the minimum representable value of type `T`.",
    doc: Some("Compile-time builtin for integer limits."),
  },
  AtItemMeta {
    name: "configFlag",
    kind: AtItemKind::Directive,
    syntax: "@configFlag(@platform(\"linux\"))",
    summary: "Compile-time condition: as item attribute strips the next declaration; as expression evaluates to `boolean`.",
    doc: Some(
      "**Item attribute**: applied before a declaration, includes/excludes it at parse time.\n\
       **Expression**: evaluates to a literal `true` or `false` for use in `if` conditions.\n\n\
       Predicates: `@platform(\"linux\")`, `@arch(\"x86_64\")`, `@abi(\"gnu\")`, `@target(\"triple\")`, `@feature(\"name\")`, `@debug()`, `@release()`.\n\
       Combinators: `&&`, `||`, `!`, parentheses.",
    ),
  },
  AtItemMeta {
    name: "compileError",
    kind: AtItemKind::Builtin,
    syntax: "@compileError(\"message\")",
    summary: "Emits a compile-time error with the given message.",
    doc: Some("Return type is `Never`. Useful for static assertions and unsupported platform guards."),
  },
  AtItemMeta {
    name: "panic",
    kind: AtItemKind::Builtin,
    syntax: "@panic(\"message\")",
    summary: "Terminates the program with an error message at runtime.",
    doc: Some("Emits `fprintf(stderr, ...)` + `exit(1)`. Return type is `Never`."),
  },
  AtItemMeta {
    name: "trap",
    kind: AtItemKind::Builtin,
    syntax: "@trap()",
    summary: "Emits a hardware trap instruction.",
    doc: Some("Low-level crash with no message. Return type is `Never`. Use for impossible states."),
  },
  AtItemMeta {
    name: "unreachable",
    kind: AtItemKind::Builtin,
    syntax: "@unreachable()",
    summary: "Marks a code point as unreachable for the optimizer.",
    doc: Some(
      "Emits `__builtin_unreachable()`. Return type is `Never`. No safety check — undefined behavior if reached.",
    ),
  },
  AtItemMeta {
    name: "packed",
    kind: AtItemKind::Directive,
    syntax: "@packed",
    summary: "Removes field padding from a record (tightly packed layout).",
    doc: Some("Valid on: record."),
  },
  AtItemMeta {
    name: "aligned",
    kind: AtItemKind::Directive,
    syntax: "@aligned(N)",
    summary: "Sets minimum alignment to `N` bytes.",
    doc: Some("Valid on: record, field. `N` must be a power of two."),
  },
  AtItemMeta {
    name: "implements",
    kind: AtItemKind::Directive,
    syntax: "@implements(TraitName)",
    summary: "Declares that a record implements a trait.",
    doc: Some("Valid on: record. Accepts one or more trait names: `@implements(Drop)`, `@implements(Clone, Copy)`."),
  },
  AtItemMeta {
    name: "lang",
    kind: AtItemKind::Directive,
    syntax: "@lang(try)",
    summary: "Marks a declaration with compiler-recognized language semantics.",
    doc: Some("Currently used for language hooks such as `try`-capable records."),
  },
  AtItemMeta {
    name: "externName",
    kind: AtItemKind::Directive,
    syntax: "@externName(\"symbol\")",
    summary: "Sets the C symbol name for an extern function.",
    doc: Some("Valid on: function. Overrides the default mangled name in the generated C code."),
  },
  AtItemMeta {
    name: "cold",
    kind: AtItemKind::Directive,
    syntax: "@cold",
    summary: "Marks a function as unlikely to be called (cold path).",
    doc: Some("Valid on: function. Hint to the optimizer for branch prediction and code layout."),
  },
  AtItemMeta {
    name: "deprecated",
    kind: AtItemKind::Directive,
    syntax: "@deprecated or @deprecated(\"message\")",
    summary: "Marks a function as deprecated.",
    doc: Some("Valid on: function. Emits a warning at call sites. Optional message for migration guidance."),
  },
  AtItemMeta {
    name: "extension",
    kind: AtItemKind::Directive,
    syntax: "@extension(TypeName) or @extension(TypeName, mut)",
    summary: "Declares a standalone function as an extension method on a type.",
    doc: Some("Valid on: function. Adds instance methods to types you don't own, including primitives."),
  },
  AtItemMeta {
    name: "inline",
    kind: AtItemKind::Directive,
    syntax: "@inline or @inline(always) or @inline(never)",
    summary: "Controls inlining behavior of a function.",
    doc: Some("Valid on: function. `@inline` suggests inlining, `always` forces it, `never` prevents it."),
  },
  AtItemMeta {
    name: "takes",
    kind: AtItemKind::Directive,
    syntax: "@takes",
    summary: "Declares that a parameter takes ownership of the passed value.",
    doc: Some("Valid on: parameter. Used for FFI functions that consume their arguments."),
  },
  AtItemMeta {
    name: "noescape",
    kind: AtItemKind::Directive,
    syntax: "@noescape",
    summary: "Declares that a closure parameter must not escape the callee.",
    doc: Some("Valid on: parameter. Used by escape analysis."),
  },
  AtItemMeta {
    name: "allow",
    kind: AtItemKind::Directive,
    syntax: "@allow(lint_name)",
    summary: "Suppresses a lint warning for the annotated item.",
    doc: Some("Valid on: record, function, field. Known lints: `unused_variable`, `unused_import`, `deprecated`."),
  },
  AtItemMeta {
    name: "warn",
    kind: AtItemKind::Directive,
    syntax: "@warn(lint_name)",
    summary: "Promotes a lint to warning level for the annotated item.",
    doc: Some("Valid on: record, function, field. Known lints: `unused_variable`, `unused_import`, `deprecated`."),
  },
  AtItemMeta {
    name: "deny",
    kind: AtItemKind::Directive,
    syntax: "@deny(lint_name)",
    summary: "Promotes a lint to error level for the annotated item.",
    doc: Some("Valid on: record, function, field. Known lints: `unused_variable`, `unused_import`, `deprecated`."),
  },
  AtItemMeta {
    name: "langHook",
    kind: AtItemKind::Directive,
    syntax: "@langHook(\"hook_name\")",
    summary: "Marks a namespace as a compiler-recognized language hook provider.",
    doc: Some("Valid on: namespace. Used internally for runtime integration (e.g. `rc_runtime`, `string_runtime`)."),
  },
];

pub fn all() -> &'static [AtItemMeta] {
  AT_ITEMS
}

pub fn lookup(name: &str) -> Option<&'static AtItemMeta> {
  AT_ITEMS.iter().find(|item| item.name == name)
}

pub fn is_builtin(name: &str) -> bool {
  matches!(lookup(name), Some(item) if item.kind == AtItemKind::Builtin)
}

pub fn is_directive(name: &str) -> bool {
  matches!(lookup(name), Some(item) if item.kind == AtItemKind::Directive)
}

pub fn is_lint_level_directive(name: &str) -> bool {
  matches!(name, "allow" | "warn" | "deny") && is_directive(name)
}

pub fn completions_matching(prefix: &str) -> Vec<&'static AtItemMeta> {
  let prefix_lower = prefix.to_lowercase();
  AT_ITEMS
    .iter()
    .filter(|item| item.name.to_lowercase().starts_with(&prefix_lower))
    .collect()
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
  fn completions_matching_is_case_insensitive() {
    let matches = completions_matching("SIZE");
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0].name, "sizeOf");
  }

  #[test]
  fn recognizes_semantic_only_builtins() {
    assert!(is_builtin("typeOf"));
    assert!(is_builtin("maxOf"));
    assert!(is_builtin("minOf"));
  }

  #[test]
  fn recognizes_binder_directives() {
    assert!(is_directive("lang"));
    assert!(is_directive("noescape"));
    assert!(is_lint_level_directive("warn"));
  }
}
