use std::path::PathBuf;

fn repo_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .expect("crate dir should have workspace parent")
    .parent()
    .expect("workspace should have repo root")
    .to_path_buf()
}

fn read_repo_file(relative: &str) -> String {
  std::fs::read_to_string(repo_root().join(relative))
    .unwrap_or_else(|error| panic!("failed to read {}: {}", relative, error))
}

#[test]
fn utf8_runtime_header_documents_owned_string_contract() {
  let header = read_repo_file("std/runtime/ignis_rt.h");

  assert!(header.contains("UTF-8 byte buffer"));
  assert!(header.contains("len is measured in bytes"));
  assert!(header.contains("interior NUL bytes"));
  assert!(header.contains("data[len] == '\\0'"));
}

#[test]
fn utf8_std_docs_distinguish_owned_string_from_borrowed_str() {
  let string_mod = read_repo_file("std/string/mod.ign");
  let hash_mod = read_repo_file("std/hash/mod.ign");
  let io_mod = read_repo_file("std/io/mod.ign");
  let libc_string_mod = read_repo_file("std/libc/string.ign");

  assert!(string_mod.contains("owned UTF-8 byte buffer"));
  assert!(string_mod.contains("borrowed NUL-terminated UTF-8 byte view"));
  assert!(string_mod.contains("Interior NUL bytes are preserved in owned storage"));
  assert!(string_mod.contains("toStr() is a zero-copy interop view"));
  assert!(string_mod.contains("first interior NUL"));

  assert!(hash_mod.contains("NUL-terminated `str` data"));
  assert!(hash_mod.contains("Owned `String` values should hash all owned bytes without routing through raw `toStr()`"));

  assert!(io_mod.contains("`String` overloads write `message.length()` bytes"));
  assert!(io_mod.contains("`str` overloads remain NUL-terminated C-string writes"));

  assert!(libc_string_mod.contains("Do not use these APIs for Ignis `String` byte-length semantics"));
}

#[test]
fn utf8_fs_len_constructor_and_v04_char_contract_are_locked() {
  let string_mod = read_repo_file("std/string/mod.ign");
  let fs_mod = read_repo_file("std/fs/mod.ign");
  let diagnostics = read_repo_file("crates/ignis_diagnostics/src/message.rs");
  let lexer = read_repo_file("crates/ignis_parser/src/lexer/mod.rs");
  let emit = read_repo_file("crates/ignis_codegen_c/src/emit.rs");

  assert!(string_mod.contains("function ignis_string_init_from_len(out: &mut String, s: *mut u8, len: u64): void;"));
  assert!(string_mod.contains("public static create(bytes: *mut u8, len: u64): String"));
  assert!(string_mod.contains("__string::ignis_string_init_from_cstr(&mut result, s);"));
  assert!(string_mod.contains("return __string::ignis_string_cstr(self);"));
  assert!(fs_mod.contains("return Result::OK(String::create(raw, len));"));
  assert!(!fs_mod.contains("String::create(raw as str)"));

  assert!(diagnostics.contains("multi-byte char literal; use str"));
  assert!(lexer.contains("DiagnosticMessage::MultiByteCharacterLiteral"));
  assert!(emit.contains("Type::Char => \"u8\".to_string()"));
  assert!(emit.contains("Type::Str => \"const char*\".to_string()"));
}
