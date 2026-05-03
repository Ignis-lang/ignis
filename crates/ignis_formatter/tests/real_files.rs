use std::fs;
use std::path::PathBuf;

use ignis_formatter::{FormatterConfig, FormatOptions, format_text};

fn workspace_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../..")
    .canonicalize()
    .expect("workspace root")
}

fn read_real_file(path: &str) -> String {
  fs::read_to_string(workspace_root().join(path)).unwrap_or_else(|error| panic!("read {path}: {error}"))
}

fn wrap_record_body(body: &str) -> String {
  format!("export record HeapAllocator {{\n{body}\n}}\n")
}

fn format_with_config(
  source: &str,
  config: FormatterConfig,
) -> String {
  format_text(source, &FormatOptions { check: false, config }).expect("format real-file slice")
}

fn assert_no_trailing_whitespace(text: &str) {
  for line in text.lines() {
    assert_eq!(line, line.trim_end(), "line has trailing whitespace: {line:?}");
  }
}

fn normalize_expected_indent(
  text: &str,
  config: &FormatterConfig,
) -> String {
  let normalized = text
    .lines()
    .map(|line| {
      if line.is_empty() {
        return String::new();
      }

      let leading_spaces = line.chars().take_while(|character| *character == ' ').count();
      let indent_level = leading_spaces / 4;
      let remainder = leading_spaces % 4;

      format!(
        "{}{}{}",
        config.indent_string(indent_level),
        " ".repeat(remainder),
        &line[leading_spaces..]
      )
    })
    .collect::<Vec<_>>()
    .join("\n");

  if text.ends_with('\n') {
    normalized + "\n"
  } else {
    normalized
  }
}

fn extract_slice(
  formatted: &str,
  start_marker: &str,
  end_marker: &str,
) -> String {
  let start = formatted
    .find(start_marker)
    .unwrap_or_else(|| panic!("missing start marker: {start_marker}\n{formatted}"));
  let end = formatted[start..]
    .find(end_marker)
    .map(|offset| start + offset)
    .unwrap_or_else(|| panic!("missing end marker: {end_marker}\n{formatted}"));

  formatted[start..end].trim_end().to_string()
}

/// Asserts that formatting a whole real file succeeds, is idempotent, and
/// produces no trailing whitespace. This is the core regression gate for
/// real-file corpus coverage.
fn assert_whole_file_corpus_pass(path: &str) {
  let source = read_real_file(path);
  let config = FormatterConfig::default();
  let options = FormatOptions { check: false, config };

  let first = format_text(&source, &options).unwrap_or_else(|error| panic!("first pass for {path}: {error}"));
  let second = format_text(&first, &options).unwrap_or_else(|error| panic!("idempotence pass for {path}: {error}"));

  assert_eq!(first, second, "idempotence violation for {path}");
  assert_no_trailing_whitespace(&first);
}

// --- Phase 5.2: Real-file corpus regressions grouped by AST domain ---

// Domain: imports and top-level declarations

#[test]
fn corpus_std_collections_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/collections/mod.ign");
}

#[test]
fn corpus_std_ffi_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/ffi/mod.ign");
}

#[test]
fn corpus_std_hash_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/hash/mod.ign");
}

#[test]
fn corpus_std_math_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/math/mod.ign");
}

#[test]
fn corpus_std_number_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/number/mod.ign");
}

// Domain: records, enums, and type declarations

#[test]
fn corpus_std_option_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/option/mod.ign");
}

#[test]
fn corpus_std_result_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/result/mod.ign");
}

#[test]
fn corpus_std_types_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/types/mod.ign");
}

#[test]
fn corpus_std_memory_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/memory/mod.ign");
}

// Domain: functions with generic parameters and builtins

#[test]
fn corpus_std_ptr_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/ptr/mod.ign");
}

#[test]
fn corpus_std_io_error_formats_idempotently() {
  assert_whole_file_corpus_pass("std/io/error.ign");
}

#[test]
fn corpus_std_fs_dir_formats_idempotently() {
  assert_whole_file_corpus_pass("std/fs/dir.ign");
}

#[test]
fn corpus_std_fs_file_formats_idempotently() {
  assert_whole_file_corpus_pass("std/fs/file.ign");
}

#[test]
fn corpus_std_fs_sys_unix_formats_idempotently() {
  assert_whole_file_corpus_pass("std/fs/sys/unix.ign");
}

// Domain: extern blocks and FFI

#[test]
fn corpus_std_libc_memory_formats_idempotently() {
  assert_whole_file_corpus_pass("std/libc/memory.ign");
}

#[test]
fn corpus_std_libc_errno_formats_idempotently() {
  assert_whole_file_corpus_pass("std/libc/errno.ign");
}

#[test]
fn corpus_std_libc_primitives_formats_idempotently() {
  assert_whole_file_corpus_pass("std/libc/primitives.ign");
}

#[test]
fn corpus_std_libc_string_formats_idempotently() {
  assert_whole_file_corpus_pass("std/libc/string.ign");
}

#[test]
fn corpus_std_libc_stdio_formats_idempotently() {
  assert_whole_file_corpus_pass("std/libc/stdio.ign");
}

#[test]
fn corpus_std_libc_misc_formats_idempotently() {
  assert_whole_file_corpus_pass("std/libc/misc.ign");
}

// Domain: string operations and traits

#[test]
fn corpus_std_string_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/string/mod.ign");
}

#[test]
fn corpus_std_path_mod_formats_idempotently() {
  assert_whole_file_corpus_pass("std/path/mod.ign");
}

// Domain: example whole-file programs

#[test]
fn corpus_example_lambda_formats_idempotently() {
  assert_whole_file_corpus_pass("example/lambda.ign");
}

#[test]
fn corpus_example_main_formats_idempotently() {
  assert_whole_file_corpus_pass("example/main.ign");
}

#[test]
fn corpus_example_day_1_formats_idempotently() {
  assert_whole_file_corpus_pass("example/day_1.ign");
}

#[test]
fn corpus_example_allocator_arena_formats_idempotently() {
  assert_whole_file_corpus_pass("example/allocator/src/arena_allocator.ign");
}

#[test]
fn corpus_example_allocator_main_formats_idempotently() {
  assert_whole_file_corpus_pass("example/allocator/src/main.ign");
}

// Deferred: `export inline function` compound modifier causes token shape drift.
// This is a specialized printer issue (Phase 2), not a safety gate issue.
#[test]
fn deferred_example_add_token_shape_drift_from_export_inline() {
  let source = read_real_file("example/add.ign");
  let error = format_text(&source, &FormatOptions::default())
    .expect_err("export inline should be deferred until compound modifier printer is stable");

  assert!(
    error.to_string().contains("token shape"),
    "expected token-shape drift for export inline, got {error}"
  );
}

// --- Existing tests from Phase 1-4 ---

#[test]
fn formats_heap_allocator_record_member_comment_window_exactly() {
  let source = read_real_file("example/allocator/src/heap_allocator.ign");
  let source_window = extract_slice(
    &source,
    "/// Pointer to the first (oldest) block in the linked list.",
    "/// Creates a new, empty `HeapAllocator` with the default search mode (FirstFit).",
  );

  let formatted_record = format_with_config(&wrap_record_body(&source_window), FormatterConfig::default());
  assert_eq!(
    formatted_record,
    normalize_expected_indent(
      r#"export record HeapAllocator {
    /// Pointer to the first (oldest) block in the linked list.
    /// Used as the starting point when searching for free blocks.
    first: *mut Block;

    /// Pointer to the last (newest) block in the linked list.
    /// New allocations are appended here. Deallocation of this
    /// block allows heap shrinking via `sbrk(-size)`.
    last: *mut Block;

    /// Strategy used to search for free blocks when reusing memory.
    /// See [`SearchMode`] for available strategies.
    searchMode: SearchMode;

    /// Pointer to the block where the last successful search ended.
    /// Used exclusively by [`SearchMode::NextFit`] to remember the
    /// starting position for the next search.
    lastSearch: *mut Block;
}
"#,
      &FormatterConfig::default()
    )
  );
  assert_no_trailing_whitespace(&formatted_record);
}

#[test]
fn formats_heap_allocator_set_search_mode_window_exactly() {
  let source = read_real_file("example/allocator/src/heap_allocator.ign");
  let set_search_mode_source_window = extract_slice(
    &source,
    "public setSearchMode(&mut self, mode: SearchMode): void {",
    "/// Searches the block list for a free block of sufficient size.",
  );

  let formatted = format_text(
    &wrap_record_body(&set_search_mode_source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("setSearchMode window should preserve method comment ownership now");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    public setSearchMode(&mut self, mode: SearchMode): void {\n        self.searchMode = mode;\n\n        // Reset lastSearch when changing modes to avoid stale pointers\n        if (mode != SearchMode::NextFit) {\n            self.lastSearch = null;\n        }\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_heap_allocator_set_search_mode_window_with_two_space_indent() {
  let source = read_real_file("example/allocator/src/heap_allocator.ign");
  let source_window = extract_slice(
    &source,
    "public setSearchMode(&mut self, mode: SearchMode): void {",
    "/// Searches the block list for a free block of sufficient size.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig {
        indent_width: 2,
        line_width: 100,
        use_tabs: false,
        sort_imports: false,
      },
    },
  )
  .expect("setSearchMode window should stay ownership-safe under alternate indentation");

  assert_eq!(
    formatted,
    "export record HeapAllocator {\n  public setSearchMode(&mut self, mode: SearchMode): void {\n    self.searchMode = mode;\n\n    // Reset lastSearch when changing modes to avoid stale pointers\n    if (mode != SearchMode::NextFit) {\n      self.lastSearch = null;\n    }\n  }\n}\n"
  );
}

#[test]
fn formats_heap_allocator_imports_and_search_mode_window_exactly() {
  let source = read_real_file("example/allocator/src/heap_allocator.ign");
  let source_window = extract_slice(
    &source,
    "import Block from \"./block\";",
    "/// A general-purpose heap allocator that manages memory using `sbrk`.",
  );

  let formatted = format_text(
    &source_window,
    &FormatOptions {
      check: false,
      config: FormatterConfig {
        sort_imports: false,
        ..FormatterConfig::default()
      },
    },
  )
  .expect("heap allocator import-to-enum window should preserve doc ownership");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "import Block from \"./block\";\nimport Layout, Align from \"std::memory\";\nimport LibC, CType from \"std::libc\";\n\n/// Strategy for searching free blocks in the allocator.\n///\n/// When reusing freed memory blocks, different search strategies offer\n/// different trade-offs between allocation speed and memory utilization.\n///\n/// # Strategies\n///\n/// ```text\n///   FREE BLOCK SEARCH STRATEGIES\n///   ═══════════════════════════════════════════════════════════════════════\n///\n///   Given blocks: [A:64] → [B:128,free] → [C:32,free] → [D:256,free] → [E:100]\n///   Request: 50 bytes\n///\n///   FIRST FIT: Start from beginning, return first match\n///   ┌──────────────────────────────────────────────────────────────────────┐\n///   │  [A:64] → [B:128,free] → [C:32,free] → [D:256,free] → [E:100]        │\n///   │     ↓           ↓                                                    │\n///   │   skip     ✓ MATCH! (128 >= 50)                                      │\n///   │  (in use)                                                            │\n///   │                                                                      │\n///   │  Returns: B (first free block that fits)                             │\n///   │  Pros: Fast - O(n) worst case, often much faster                     │\n///   │  Cons: Can cause fragmentation at the start of the heap              │\n///   └──────────────────────────────────────────────────────────────────────┘\n///\n///   NEXT FIT: Start from last allocation position, wrap around if needed\n///   ┌──────────────────────────────────────────────────────────────────────┐\n///   │  Last allocation was at C, so search starts after C:                 │\n///   │                                                                      │\n///   │  [A:64] → [B:128,free] → [C:32,free] → [D:256,free] → [E:100]        │\n///   │                               │             ↓                        │\n///   │                          last_search   ✓ MATCH! (256 >= 50)          │\n///   │                                                                      │\n///   │  Returns: D (first free block after last_search that fits)           │\n///   │  Pros: Spreads allocations, avoids always fragmenting start          │\n///   │  Cons: May miss better-fitting blocks earlier in list                │\n///   └──────────────────────────────────────────────────────────────────────┘\n///\n///   BEST FIT: Search entire list, return smallest adequate block\n///   ┌──────────────────────────────────────────────────────────────────────┐\n///   │  [A:64] → [B:128,free] → [C:32,free] → [D:256,free] → [E:100]       │\n///   │              ↓               ↓             ↓                         │\n///   │          128 >= 50       32 < 50       256 >= 50                     │\n///   │          candidate      too small      candidate                     │\n///   │              ↓                             ↓                         │\n///   │          128 bytes                     256 bytes                     │\n///   │              ↓                                                       │\n///   │          ✓ BEST! (128 < 256, smallest that fits)                     │\n///   │                                                                      │\n///   │  Returns: B (smallest free block that fits)                          │\n///   │  Pros: Minimizes wasted space within blocks                          │\n///   │  Cons: Slower - always O(n), must check all blocks                   │\n///   └──────────────────────────────────────────────────────────────────────┘\n/// ```\nexport enum SearchMode {\n    /// First Fit: Returns the first free block large enough.\n    ///\n    /// Starts searching from the beginning of the block list and returns\n    /// the first block that is both free and has sufficient size.\n    ///\n    /// - **Time Complexity**: O(n) worst case, but often faster\n    /// - **Memory Efficiency**: Can cause fragmentation at heap start\n    /// - **Best For**: General-purpose use, when speed is priority\n    FirstFit,\n\n    /// Next Fit: Like First Fit, but remembers where the last search ended.\n    ///\n    /// Starts searching from where the previous successful search ended,\n    /// wrapping around to the beginning if necessary. This distributes\n    /// allocations more evenly across the heap.\n    ///\n    /// - **Time Complexity**: O(n) worst case\n    /// - **Memory Efficiency**: Better distribution, less clustering\n    /// - **Best For**: Long-running programs with many alloc/free cycles\n    NextFit,\n\n    /// Best Fit: Returns the smallest free block that fits.\n    ///\n    /// Searches the entire list to find the free block that most closely\n    /// matches the requested size, minimizing internal fragmentation.\n    ///\n    /// - **Time Complexity**: Always O(n) - must check all blocks\n    /// - **Memory Efficiency**: Minimizes wasted space per allocation\n    /// - **Best For**: Memory-constrained environments\n    BestFit,\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_real_hello_world_file_exactly() {
  let source = read_real_file("example/hello-world.ign");
  let formatted = format_with_config(&source, FormatterConfig::default());

  assert_eq!(
    formatted,
    normalize_expected_indent(
      r#"import Io from "std::io";

function main(): i32 {
    Io::println("Hello, World!");
    return 0;
}
"#,
      &FormatterConfig::default()
    )
  );
  assert_no_trailing_whitespace(&formatted);
}

#[test]
fn formats_real_record_example_file_exactly() {
  let source = read_real_file("example/record.ign");
  let formatted = format_with_config(
    &source,
    FormatterConfig {
      sort_imports: false,
      ..FormatterConfig::default()
    },
  );

  assert_eq!(
    formatted,
    normalize_expected_indent(
      r#"import String from "std::string";
import Io from "std::io";

record Household {
    public city: str;
    public size: str;
    public price: i32;
    public roomies: u32;
    public hasWifi: boolean;
    public score: f64;
}

function main(): i32 {
    let myApartement: Household = Household {
        city: "Barcelona",
        size: "Apartamento",
        price: 109,
        roomies: 0,
        hasWifi: true,
        score: 4.9,
    };

    let cityLabel: String = String::create("City: ");
    let cityVal: String = String::create(myApartement.city);
    Io::println(cityLabel.concat(&cityVal));

    let sizeLabel: String = String::create("Size: ");
    let sizeVal: String = String::create(myApartement.size);
    Io::println(sizeLabel.concat(&sizeVal));

    let priceLabel: String = String::create("Price: ");
    let priceVal: String = String::create(myApartement.price);
    Io::println(priceLabel.concat(&priceVal));

    let roomiesLabel: String = String::create("Roomies: ");
    let roomiesVal: String = String::create(myApartement.roomies);
    Io::println(roomiesLabel.concat(&roomiesVal));

    let wifiLabel: String = String::create("Wifi: ");
    let wifiVal: String = myApartement.hasWifi.toString();
    Io::println(wifiLabel.concat(&wifiVal));

    let scoreLabel: String = String::create("Score: ");
    let scoreVal: String = String::create(myApartement.score);
    Io::println(scoreLabel.concat(&scoreVal));

    return 0;
}
"#,
      &FormatterConfig::default()
    )
  );
  assert_no_trailing_whitespace(&formatted);
}

#[test]
fn formats_std_io_println_slice_exactly() {
  let source = read_real_file("std/io/mod.ign");
  let source_window = extract_slice(
    &source,
    "/// Prints `message` to stdout followed by a newline (`\\n`).",
    "/// Prints `message` to stderr followed by a newline (`\\n`).",
  );

  let formatted = format_text(
    &source_window,
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("std::io println slice should format once cast-aware call slices are supported");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "/// Prints `message` to stdout followed by a newline (`\\n`).\n///\n/// This is the most common way to produce output. The String is\n/// moved into the function and dropped after printing.\n///\n/// # Arguments\n///\n/// * `message` - The String to print (consumed/moved).\n///\n/// # Example\n///\n/// ```ignis\n/// import Io from \"std::io\";\n/// import String from \"std::string\";\n///\n/// Io::println(String::create(\"hello\"));\n/// Io::println(\"hello from str\");\n/// Io::println(String::create(42));\n/// ```\nfunction println(message: String): void {\n    let s: str = message.toStr();\n    let len: u64 = message.length();\n\n    LibC::File::write(LibC::File::STDOUT_FILENO, s as *void, len);\n    LibC::File::write(LibC::File::STDOUT_FILENO, \"\\n\" as *void, 1);\n}\n\n/// Prints a `str` message to stdout followed by a newline (`\\n`).\n///\n/// Writes the C string directly via `write(2)`, no heap allocation.\n///\n/// # Arguments\n///\n/// * `message` - The `str` literal or slice to print.\nfunction println(message: str): void {\n    let len: u64 = LibC::String::strlen(message as *u8);\n\n    LibC::File::write(LibC::File::STDOUT_FILENO, message as *void, len);\n    LibC::File::write(LibC::File::STDOUT_FILENO, \"\\n\" as *void, 1);\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn rejects_std_io_top_level_structure_as_parser_invalid_slice() {
  let source = read_real_file("std/io/mod.ign");
  let source_window = extract_slice(
    &source,
    "//! # I/O",
    "/// Prints `message` to stdout followed by a newline (`\\n`).",
  );

  let error = format_text(
    &source_window,
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect_err("partial std::io top-level slice must fail fast");

  assert_eq!(error.to_string(), "parser failed: Expected RightBrace");
}

#[test]
fn formats_heap_allocator_init_record_literal_window_exactly() {
  let source = read_real_file("example/allocator/src/heap_allocator.ign");
  let source_window = extract_slice(
    &source,
    "public static init(): HeapAllocator {",
    "/// Creates a new, empty `HeapAllocator` with the specified search mode.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("heap allocator init window should format once public static record-init members stay stable");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    public static init(): HeapAllocator {\n        return HeapAllocator {\n            first: null,\n            last: null,\n            searchMode: SearchMode::FirstFit,\n            lastSearch: null,\n        };\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_heap_allocator_init_with_search_mode_window_exactly() {
  let source = read_real_file("example/allocator/src/heap_allocator.ign");
  let source_window = extract_slice(
    &source,
    "public static init(searchMode: SearchMode): HeapAllocator {",
    "/// Returns the current search mode of the allocator.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("search-mode init window should format once public static record-init members stay stable");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    public static init(searchMode: SearchMode): HeapAllocator {\n        return HeapAllocator {\n            first: null,\n            last: null,\n            searchMode: searchMode,\n            lastSearch: null,\n        };\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_std_vector_private_drop_elements_slice_exactly() {
  let source = read_real_file("std/vector/mod.ign");
  let source_window = extract_slice(
    &source,
    "private dropElements(&mut self): void {",
    "/// Grows the backing storage using geometric doubling.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("vector private helper slice should preserve private member visibility and generic body syntax");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    private dropElements(&mut self): void {\n        let mut i: u64 = 0;\n        while (i < self.length) {\n            @dropInPlace<T>((&mut self.data[i]) as *mut T);\n            i += 1;\n        }\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_std_vector_generic_init_overload_slice_exactly() {
  let source = read_real_file("std/vector/mod.ign");
  let source_window = extract_slice(
    &source,
    "/// Creates an empty vector with zero capacity.",
    "/// Returns the current number of elements.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("vector init overload slice should stay exact once private/public member visibility is stable");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    /// Creates an empty vector with zero capacity.\n    ///\n    /// # Example\n    ///\n    /// ```ignis\n    /// import Vector from \"std::vector\";\n    ///\n    /// let v: Vector<i32> = Vector::init<i32>();\n    /// ```\n    public static init(): Vector<T> {\n        return Vector {\n            data: null,\n            length: 0,\n            capacity: 0,\n        };\n    }\n\n    /// Creates an empty vector with preallocated `capacity` elements.\n    ///\n    /// # Example\n    ///\n    /// ```ignis\n    /// import Vector from \"std::vector\";\n    ///\n    /// let v: Vector<i32> = Vector::init<i32>(16);\n    /// ```\n    public static init(capacity: u64): Vector<T> {\n        return Vector {\n            data: Memory::allocateVector<T>(capacity),\n            length: 0,\n            capacity: capacity,\n        };\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_std_option_lang_enum_slice_exactly() {
  let source = read_real_file("std/option/mod.ign");
  let source_window = extract_slice(&source, "@lang(try)", "/// Returns `true` if the option is `NONE`.") + "}\n";

  let formatted = format_text(
    &source_window,
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("option lang enum slice should keep declaration attributes and generic variant layout stable");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "@lang(try)\nexport enum Option<S> {\n    SOME(S),\n    NONE,\n\n    /// Returns `true` if the option contains a value (`SOME`).\n    ///\n    /// # Example\n    ///\n    /// ```ignis\n    /// import Option from \"std::option\";\n    ///\n    /// let x: Option<i32> = Option::SOME(42);\n    /// let y: Option<i32> = Option::NONE;\n    ///\n    /// x.isSome();  // true\n    /// y.isSome();  // false\n    /// ```\n    public isSome(&self): boolean {\n        return match (self) {\n            Option::SOME(_) -> true,\n            _ -> false,\n        };\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_std_result_lang_enum_slice_exactly() {
  let source = read_real_file("std/result/mod.ign");
  let source_window = extract_slice(&source, "@lang(try)", "/// Returns `true` if the result is `ERROR`.") + "}\n";

  let formatted = format_text(
    &source_window,
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("result lang enum slice should keep declaration attributes and generic variant layout stable");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "@lang(try)\nexport enum Result<T, E> {\n    OK(T),\n    ERROR(E),\n\n    /// Returns `true` if the result is `OK`.\n    ///\n    /// # Example\n    ///\n    /// ```ignis\n    /// import Result from \"std::result\";\n    ///\n    /// let x: Result<i32, str> = Result::OK(42);\n    /// let y: Result<i32, str> = Result::ERROR(\"oops\");\n    ///\n    /// x.isOk();     // true\n    /// y.isOk();     // false\n    /// ```\n    public isOk(&self): boolean {\n        return match (self) {\n            Result::OK(_) -> true,\n            _ -> false,\n        };\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_real_function_signature_with_tight_line_width_as_multiline() {
  let source = read_real_file("std/fs/mod.ign");
  let source_window = extract_slice(
    &source,
    "function write(path: str, data: *void, len: u64): Result<void, Io::IoError> {",
    "/// Alias for `writeBytes(&String, data, len)`.",
  );

  let formatted = format_text(
    &source_window,
    &FormatOptions {
      check: false,
      config: FormatterConfig {
        indent_width: 4,
        line_width: 40,
        use_tabs: false,
        sort_imports: false,
      },
    },
  )
  .expect("tight line width function slice should format as multiline signature");

  assert_eq!(
    formatted,
    "function write(\n    path: str,\n    data: *void,\n    len: u64\n): Result<void, Io::IoError> {\n    return Fs::writeBytesRaw(path, data, len);\n}\n"
  );
}

#[test]
fn formats_std_vector_map_higher_order_slice_exactly() {
  let source = read_real_file("std/vector/mod.ign");
  let source_window = extract_slice(
    &source,
    "public map<U>(&self, @noescape f: (&T) -> U): Vector<U> {",
    "/// Folds the vector left-to-right starting from an explicit initial value.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("vector map slice should preserve @noescape generic member signatures and value bodies");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    public map<U>(&self, @noescape f: (&T) -> U): Vector<U> {\n        let mut result: Vector<U> = Vector::init<U>(self.length);\n        let mut i: u64 = 0;\n        while (i < self.length) {\n            result.push(f(&self.data[i]));\n            i += 1;\n        }\n        return result;\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_std_vector_fold_higher_order_slice_exactly() {
  let source = read_real_file("std/vector/mod.ign");
  let source_window = extract_slice(
    &source,
    "public fold<U>(&self, initial: U, @noescape f: (U, &T) -> U): U {",
    "/// Releases the backing storage and resets length/capacity to zero.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("vector fold slice should preserve @noescape generic member signatures and accumulator value bodies");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    public fold<U>(&self, initial: U, @noescape f: (U, &T) -> U): U {\n        let mut acc: U = initial;\n        let mut i: u64 = 0;\n        while (i < self.length) {\n            acc = f(acc, &self.data[i]);\n            i += 1;\n        }\n        return acc;\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_heap_allocator_allocate_grouped_pointer_assignment_window_exactly() {
  let source = read_real_file("example/allocator/src/heap_allocator.ign");
  let source_window = extract_slice(
    &source,
    "public allocate(&mut self, layout: Layout): *mut u8 {",
    "/// Deallocates a previously allocated block of memory.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("allocator grouped pointer assignment window should format without comment-gap failures");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    public allocate(&mut self, layout: Layout): *mut u8 {\n        let alignment: u64 = layout.align();\n        let headerSize: u64 = sizeOf<Block>();\n\n        // Calculate total size needed:\n        // - headerSize: space for Block metadata\n        // - layout.size(): user-requested allocation size\n        // - (alignment - 1): worst-case padding for alignment\n        // The result is word-aligned via the align() function\n        let sizeForSbrk: u64 = Align::align(headerSize + layout.size() + (alignment - 1));\n\n        // Extend the heap by requesting more memory from the OS\n        // sbrk returns the OLD program break (start of new memory)\n        let rawAddress: CType::CVoidPtr = LibC::Memory::sbrk(sizeForSbrk as CType::IntPtrT);\n\n        // sbrk returns (void*)-1 on failure\n        if (rawAddress == (maxOf<u64>() as CType::CVoidPtr)) {\n            return null;\n        }\n\n        // Calculate the aligned address for user content\n        // This ensures the returned pointer meets the layout's alignment requirements\n        let contentAddr: u64 = Align::alignUp((rawAddress as u64) + headerSize, alignment);\n\n        // Place the block header immediately before the content\n        // This allows us to find the header given only the content pointer\n        let block: *mut Block = (contentAddr - headerSize) as *mut Block;\n        (*block).isFree = false;\n        (*block).size = layout.size() as i64;\n        (*block).next = null;\n\n        // Update the linked list of blocks\n        if (self.first == null) {\n            // First allocation ever\n            self.first = block;\n            self.last = block;\n        } else {\n            // Append to the end of the list\n            (*self.last).next = block;\n            self.last = block;\n        }\n\n        return contentAddr as *mut u8;\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn preserves_heap_allocator_intentional_blank_lines_inside_best_fit_loop() {
  let source = read_real_file("example/allocator/src/heap_allocator.ign");
  let source_window = extract_slice(
    &source,
    "findFreeBlockBestFit(&self, size: i64): *mut Block {",
    "/// Allocates a block of memory with the specified layout.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("best-fit window should preserve intentional blank lines inside the loop");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    findFreeBlockBestFit(&self, size: i64): *mut Block {\n        let mut best: *mut Block = null;\n        let mut bestSize: i64 = maxOf<i64>();\n        let mut current: *mut Block = self.first;\n\n        while (current != null) {\n            let blockSize: i64 = (*current).size;\n\n            // Check if this block is free, large enough, and better than current best\n            if ((*current).isFree && blockSize >= size && blockSize < bestSize) {\n                best = current;\n                bestSize = blockSize;\n\n                // Perfect fit - no need to continue searching\n                if (blockSize == size) {\n                    return best;\n                }\n            }\n\n            current = (*current).next;\n        }\n\n        return best;\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}

#[test]
fn formats_std_vector_reduce_higher_order_slice_exactly() {
  let source = read_real_file("std/vector/mod.ign");
  let source_window = extract_slice(
    &source,
    "/// Folds the vector left-to-right without an explicit initial value.",
    "/// Sorts the vector in-place using a comparator.",
  );

  let formatted = format_text(
    &wrap_record_body(&source_window),
    &FormatOptions {
      check: false,
      config: FormatterConfig::default(),
    },
  )
  .expect("vector reduce slice should now format safely");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record HeapAllocator {\n    /// Folds the vector left-to-right without an explicit initial value.\n    ///\n    /// The first element is **copied** as the initial accumulator. Then `f`\n    /// is called for each subsequent element with `(accumulator, &element)`,\n    /// and its return value becomes the new accumulator.\n    ///\n    /// Returns `Option::NONE` for an empty vector.\n    ///\n    /// # Arguments\n    ///\n    /// * `f` - Callback that takes the accumulator by value and the next\n    ///   element by reference, and returns the updated accumulator.\n    ///\n    /// # Returns\n    ///\n    /// `Option::SOME(result)` with the final accumulated value, or\n    /// `Option::NONE` if the vector is empty.\n    ///\n    /// # Example\n    ///\n    /// ```ignis\n    /// import Vector from \"std::vector\";\n    /// import Option from \"std::option\";\n    ///\n    /// let mut v: Vector<i32> = Vector::init<i32>();\n    /// v.push(1);\n    /// v.push(2);\n    /// v.push(3);\n    ///\n    /// // Sum all elements: 1 + 2 + 3 = 6\n    /// let sum: Option<i32> = v.reduce((acc: i32, x: &i32): i32 -> {\n    ///   return acc + *x;\n    /// });\n    /// // sum == Option::SOME(6)\n    ///\n    /// // Empty vector returns NONE\n    /// let empty: Vector<i32> = Vector::init<i32>();\n    /// let result: Option<i32> = empty.reduce((acc: i32, x: &i32): i32 -> {\n    ///   return acc + *x;\n    /// });\n    /// // result == Option::NONE\n    /// ```\n    ///\n    /// # See Also\n    ///\n    /// Use `fold<U>` if you need an explicit initial value or a different\n    /// return type.\n    public reduce(&self, @noescape f: (T, &T) -> T): Option<T> {\n        if (self.length == 0) {\n            return Option::NONE;\n        }\n\n        let mut acc: T = self.data[0];\n        let mut i: u64 = 1;\n        while (i < self.length) {\n            acc = f(acc, &self.data[i]);\n            i += 1;\n        }\n        return Option::SOME(acc);\n    }\n}\n",
      &FormatterConfig::default()
    )
  );
}
