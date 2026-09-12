//! The C identifiers a user name may not become.
//!
//! Most emitted names are already unreachable by a user name: locals are `l<n>`,
//! temporaries `t<n>`, record and enum-payload fields `field_<n>`, and every
//! non-extern function carries a parameter/return type suffix. What is left bare
//! is the interesting set -- function parameters above all, which
//! [`crate::emit`] spells with the user's own identifier. A parameter named
//! `stderr` shadows the C global that the same function's use-after-drop guard
//! writes to, and a parameter named `int` is not an identifier at all.
//!
//! So this module answers one question: would this final C identifier collide
//! with something the emitted translation unit already means? If so the emitter
//! appends [`RESERVED_SUFFIX`]. The rule runs on the *final* name, after all
//! namespace and owner prefixing and all overload suffixing, which is why it
//! almost never fires: a namespaced or overloaded name cannot spell a reserved
//! word. `@externName` and `extern` declarations never reach here -- those name
//! a symbol the C side owns, and renaming them would break the link.
//!
//! The selfhost emitter carries the same set under the same rule
//! (`ignis/codegen/reserved.ign`). Keep the two in step: a name that mangles in
//! one compiler and not the other is a bootstrap fixed-point failure.

/// Appended to a user name whose emitted C spelling would be reserved.
pub const RESERVED_SUFFIX: &str = "__ign";

/// C11 keywords, plus the alternative spellings and common extensions a C
/// compiler also refuses as an identifier.
const C_KEYWORDS: &[&str] = &[
  "_Alignas",
  "_Alignof",
  "_Atomic",
  "_Bool",
  "_Complex",
  "_Generic",
  "_Imaginary",
  "_Noreturn",
  "_Static_assert",
  "_Thread_local",
  "alignas",
  "alignof",
  "asm",
  "auto",
  "bool",
  "break",
  "case",
  "char",
  "complex",
  "const",
  "continue",
  "default",
  "do",
  "double",
  "else",
  "enum",
  "extern",
  "false",
  "float",
  "for",
  "goto",
  "if",
  "imaginary",
  "inline",
  "int",
  "long",
  "noreturn",
  "register",
  "restrict",
  "return",
  "short",
  "signed",
  "sizeof",
  "static",
  "static_assert",
  "struct",
  "switch",
  "thread_local",
  "true",
  "typedef",
  "typeof",
  "union",
  "unsigned",
  "void",
  "volatile",
  "while",
];

/// Every name the runtime type prelude defines. Both compilers emit this block
/// into every translation unit, so all of it is visible everywhere.
///
/// Mirrors `std/runtime/ignis_rt.h` and `crate::emit::RUNTIME_TYPE_PRELUDE`.
const RUNTIME_PRELUDE_NAMES: &[&str] = &[
  "FALSE",
  "IGNIS_RT_TYPES_H",
  "IGNIS_TYPE_BOOL_ID",
  "IGNIS_TYPE_CHAR_ID",
  "IGNIS_TYPE_F32_ID",
  "IGNIS_TYPE_F64_ID",
  "IGNIS_TYPE_I16_ID",
  "IGNIS_TYPE_I32_ID",
  "IGNIS_TYPE_I64_ID",
  "IGNIS_TYPE_I8_ID",
  "IGNIS_TYPE_PTR_ID",
  "IGNIS_TYPE_STRING_ID",
  "IGNIS_TYPE_U16_ID",
  "IGNIS_TYPE_U32_ID",
  "IGNIS_TYPE_U64_ID",
  "IGNIS_TYPE_U8_ID",
  "IgnisString",
  "IgnisTypeId",
  "Pointer",
  "TRUE",
  "boolean",
  "f32",
  "f64",
  "i16",
  "i32",
  "i64",
  "i8",
  "ignis_atom_t",
  "ignis_char_t",
  "u16",
  "u32",
  "u64",
  "u8",
];

/// Typedefs and object-like macros the headers the emitted code includes make
/// visible in every scope. A user name that spells one of these either
/// redeclares a type (a hard syntax error in a declaration) or is textually
/// replaced by the preprocessor.
const SYSTEM_HEADER_NAMES: &[&str] = &[
  "EOF",
  "EXIT_FAILURE",
  "EXIT_SUCCESS",
  "FILE",
  "NULL",
  "errno",
  "int16_t",
  "int32_t",
  "int64_t",
  "int8_t",
  "intmax_t",
  "intptr_t",
  "max_align_t",
  "offsetof",
  "ptrdiff_t",
  "size_t",
  "ssize_t",
  "stderr",
  "stdin",
  "stdout",
  "uint16_t",
  "uint32_t",
  "uint64_t",
  "uint8_t",
  "uintmax_t",
  "uintptr_t",
  "wchar_t",
];

/// The libc and compiler-builtin functions the emitted bodies call by name. A
/// user name that shadows one of these turns a generated call into a call
/// through the user's value.
const EMITTED_CALL_NAMES: &[&str] = &[
  "__builtin_memset",
  "__builtin_trap",
  "__builtin_unreachable",
  "abort",
  "calloc",
  "exit",
  "fprintf",
  "fputs",
  "free",
  "malloc",
  "memcmp",
  "memcpy",
  "memmove",
  "memset",
  "pow",
  "printf",
  "putchar",
  "puts",
  "realloc",
  "snprintf",
  "sprintf",
  "strcmp",
  "strcpy",
  "strlen",
  "strncmp",
];

/// Identifiers the emitters invent inside generated bodies, plus the program
/// entry symbol.
///
/// Exact names only, not the `__ignis` / `__closure` / `__env` prefixes those
/// names share. The emitters' own synthesized definitions are *in* that
/// namespace by construction -- the selfhost names a closure thunk
/// `__closure_thunk_<n>` and then mangles it like any other definition -- so a
/// prefix rule renames the emitter's own symbols instead of protecting them.
/// C reserves leading double underscores to the implementation anyway, so a
/// program that spells `__envp_t3` has already left the language's guarantees.
///
/// The C `main` wrapper's own `argc`/`argv` and a method's `self` receiver are
/// absent for a different reason: the emitter owns those names in scopes no user
/// name can enter (`main`'s body, and the receiver slot itself), so reserving
/// them would rename every method signature without closing a collision.
const EMITTER_OWNED_NAMES: &[&str] = &[
  "__dip",
  "__ignis_drop_state",
  "__ignis_main_result",
  "__ignis_user_main",
  "ignis_runtime_init",
  "main",
];

/// Is `name` an identifier the emitted translation unit already means?
pub fn is_reserved_c_name(name: &str) -> bool {
  if C_KEYWORDS.contains(&name)
    || RUNTIME_PRELUDE_NAMES.contains(&name)
    || SYSTEM_HEADER_NAMES.contains(&name)
    || EMITTED_CALL_NAMES.contains(&name)
    || EMITTER_OWNED_NAMES.contains(&name)
  {
    return true;
  }

  is_indexed_slot_name(name)
}

/// `l3` is the fourth local of whatever function the name lands in, and `t7` its
/// eighth temporary. A parameter spelled that way is a silent rebinding rather
/// than a build break, which is the worse of the two.
fn is_indexed_slot_name(name: &str) -> bool {
  let Some(digits) = name.strip_prefix('l').or_else(|| name.strip_prefix('t')) else {
    return false;
  };

  !digits.is_empty() && digits.bytes().all(|byte| byte.is_ascii_digit())
}

/// The C identifier to emit for `name`: `name` itself, or `name` with
/// [`RESERVED_SUFFIX`] appended when emitting it bare would collide.
pub fn mangle_reserved_c_name(name: String) -> String {
  if is_reserved_c_name(&name) {
    return format!("{}{}", name, RESERVED_SUFFIX);
  }

  name
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn keywords_and_libc_names_are_reserved() {
    for name in [
      "int", "default", "stderr", "errno", "exit", "free", "main", "NULL", "size_t", "boolean",
    ] {
      assert!(is_reserved_c_name(name), "{} should be reserved", name);
    }
  }

  #[test]
  fn indexed_slot_names_are_reserved() {
    for name in ["l0", "l12", "t0", "t7"] {
      assert!(is_reserved_c_name(name), "{} should be reserved", name);
    }

    for name in ["l", "t", "lx", "t1a", "local0"] {
      assert!(!is_reserved_c_name(name), "{} should not be reserved", name);
    }
  }

  #[test]
  fn emitter_owned_names_are_reserved_exactly() {
    for name in ["__dip", "__ignis_drop_state", "__ignis_user_main", "ignis_runtime_init"] {
      assert!(is_reserved_c_name(name), "{} should be reserved", name);
    }

    // The emitters name their own synthesized definitions in this namespace and
    // then mangle them like any other definition, so matching the prefix would
    // rename the emitter's own symbols.
    for name in ["__closure_thunk_0_34538", "__envp_t3", "ignis_drop_glue_t9"] {
      assert!(!is_reserved_c_name(name), "{} should not be reserved", name);
    }
  }

  #[test]
  fn ordinary_and_already_mangled_names_pass_through() {
    for name in [
      "value",
      "length",
      "std_io_println_str_void",
      "show_i32_i32",
      "Holder",
      "stderr__ign",
    ] {
      assert!(!is_reserved_c_name(name), "{} should not be reserved", name);
      assert_eq!(mangle_reserved_c_name(name.to_string()), name);
    }
  }

  #[test]
  fn mangling_appends_the_suffix_once() {
    assert_eq!(mangle_reserved_c_name("stderr".to_string()), "stderr__ign");
    assert_eq!(mangle_reserved_c_name("stderr__ign".to_string()), "stderr__ign");
  }
}
