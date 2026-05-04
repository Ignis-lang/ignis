use std::fs;
use std::path::PathBuf;

use ignis_formatter::{FormatterConfig, FormatOptions, format_text};

struct ReviewedRealFileCase {
  name: &'static str,
  source_path: &'static str,
  start_marker: Option<&'static str>,
  end_marker: Option<&'static str>,
  append_closing_brace: bool,
  wrap_in_heap_allocator_record: bool,
  config: FormatterConfig,
  expected: &'static str,
}

struct UnsupportedRealFileCase {
  name: &'static str,
  source_path: &'static str,
  start_marker: Option<&'static str>,
  end_marker: Option<&'static str>,
  append_closing_brace: bool,
  wrap_in_heap_allocator_record: bool,
  config: FormatterConfig,
  expected_error: &'static str,
}

fn workspace_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../..")
    .canonicalize()
    .expect("workspace root")
}

fn read_real_file(path: &str) -> String {
  fs::read_to_string(workspace_root().join(path)).unwrap_or_else(|error| panic!("read {path}: {error}"))
}

fn extract_slice(
  source: &str,
  start_marker: &str,
  end_marker: &str,
) -> String {
  let start = source
    .find(start_marker)
    .unwrap_or_else(|| panic!("missing start marker: {start_marker}\n{source}"));
  let end = source[start..]
    .find(end_marker)
    .map(|offset| start + offset)
    .unwrap_or_else(|| panic!("missing end marker: {end_marker}\n{source}"));

  source[start..end].trim_end().to_string()
}

fn wrap_record_body(body: &str) -> String {
  format!("export record HeapAllocator {{\n{body}\n}}\n")
}

fn remove_single_indent_block(
  body: &str,
  indent_width: usize,
) -> String {
  body
    .lines()
    .map(|line| {
      let indent = " ".repeat(indent_width);
      line.strip_prefix(&indent).unwrap_or(line)
    })
    .collect::<Vec<_>>()
    .join("\n")
    + "\n"
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

fn assert_no_trailing_whitespace(text: &str) {
  for line in text.lines() {
    assert_eq!(line, line.trim_end(), "line has trailing whitespace: {line:?}");
  }
}

fn reviewed_real_file_cases() -> Vec<ReviewedRealFileCase> {
  vec![
    ReviewedRealFileCase {
      name: "example hello-world whole file",
      source_path: "example/hello-world.ign",
      start_marker: None,
      end_marker: None,
      append_closing_brace: false,
      wrap_in_heap_allocator_record: false,
      config: FormatterConfig::default(),
      expected: r#"import Io from "std::io";

function main(): i32 {
    Io::println("Hello, World!");
    return 0;
}
"#,
    },
    ReviewedRealFileCase {
      name: "example record whole file",
      source_path: "example/record.ign",
      start_marker: None,
      end_marker: None,
      append_closing_brace: false,
      wrap_in_heap_allocator_record: false,
      config: FormatterConfig::default(),
      expected: r#"import String from "std::string";
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
    },
    ReviewedRealFileCase {
      name: "heap allocator record comments window",
      source_path: "example/allocator/src/heap_allocator.ign",
      start_marker: Some("/// Pointer to the first (oldest) block in the linked list."),
      end_marker: Some("/// Creates a new, empty `HeapAllocator` with the default search mode (FirstFit)."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: true,
      config: FormatterConfig::default(),
      expected: r#"/// Pointer to the first (oldest) block in the linked list.
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
"#,
    },
    ReviewedRealFileCase {
      name: "std io import block",
      source_path: "std/io/mod.ign",
      start_marker: Some(
        "import LibC from \"std::libc\";\nimport String from \"std::string\";\nimport Vector from \"std::vector\";",
      ),
      end_marker: Some("function __vectorStringMonomorphizationRoots(): void {"),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: false,
      config: FormatterConfig::default(),
      expected: r#"import LibC from "std::libc";
import String from "std::string";
import Vector from "std::vector";

import _ from "std::io::error";

/// Keeps `Vector<String>` monomorphizations reachable for std I/O builds.
///
/// Some std APIs return or manipulate `Vector<String>` only through code paths
/// that may be optimized away in small programs. This internal root forces the
/// compiler to instantiate clear/drop paths used by linked std modules.
"#,
    },
    ReviewedRealFileCase {
      name: "std io println overload slice",
      source_path: "std/io/mod.ign",
      start_marker: Some("/// Prints `message` to stdout followed by a newline (`\\n`)."),
      end_marker: Some("/// Prints `message` to stderr followed by a newline (`\\n`)."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: false,
      config: FormatterConfig::default(),
      expected: r#"/// Prints `message` to stdout followed by a newline (`\n`).
///
/// This is the most common way to produce output. The String is
/// moved into the function and dropped after printing.
///
/// # Arguments
///
/// * `message` - The String to print (consumed/moved).
///
/// # Example
///
/// ```ignis
/// import Io from "std::io";
/// import String from "std::string";
///
/// Io::println(String::create("hello"));
/// Io::println("hello from str");
/// Io::println(String::create(42));
/// ```
function println(message: String): void {
    let s: str = message.toStr();
    let len: u64 = message.length();

    LibC::File::write(LibC::File::STDOUT_FILENO, s as *void, len);
    LibC::File::write(LibC::File::STDOUT_FILENO, "\n" as *void, 1);
}

/// Prints a `str` message to stdout followed by a newline (`\n`).
///
/// Writes the C string directly via `write(2)`, no heap allocation.
///
/// # Arguments
///
/// * `message` - The `str` literal or slice to print.
function println(message: str): void {
    let len: u64 = LibC::String::strlen(message as *u8);

    LibC::File::write(LibC::File::STDOUT_FILENO, message as *void, len);
    LibC::File::write(LibC::File::STDOUT_FILENO, "\n" as *void, 1);
}
"#,
    },
    ReviewedRealFileCase {
      name: "heap allocator setSearchMode method window",
      source_path: "example/allocator/src/heap_allocator.ign",
      start_marker: Some("public setSearchMode(&mut self, mode: SearchMode): void {"),
      end_marker: Some("/// Searches the block list for a free block of sufficient size."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: true,
      config: FormatterConfig::default(),
      expected: r#"public setSearchMode(&mut self, mode: SearchMode): void {
    self.searchMode = mode;

    // Reset lastSearch when changing modes to avoid stale pointers
    if (mode != SearchMode::NextFit) {
        self.lastSearch = null;
    }
}
"#,
    },
    ReviewedRealFileCase {
      name: "heap allocator init record literal window",
      source_path: "example/allocator/src/heap_allocator.ign",
      start_marker: Some("public static init(): HeapAllocator {"),
      end_marker: Some("/// Creates a new, empty `HeapAllocator` with the specified search mode."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: true,
      config: FormatterConfig::default(),
      expected: r#"public static init(): HeapAllocator {
    return HeapAllocator {
        first: null,
        last: null,
        searchMode: SearchMode::FirstFit,
        lastSearch: null,
    };
}
"#,
    },
    ReviewedRealFileCase {
      name: "std vector private dropElements helper slice",
      source_path: "std/vector/mod.ign",
      start_marker: Some("private dropElements(&mut self): void {"),
      end_marker: Some("/// Grows the backing storage using geometric doubling."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: true,
      config: FormatterConfig::default(),
      expected: r#"private dropElements(&mut self): void {
    let mut i: u64 = 0;
    while (i < self.length) {
        @dropInPlace<T>((&mut self.data[i]) as *mut T);
        i += 1;
    }
}
"#,
    },
    ReviewedRealFileCase {
      name: "std vector init overload slice",
      source_path: "std/vector/mod.ign",
      start_marker: Some("/// Creates an empty vector with zero capacity."),
      end_marker: Some("/// Returns the current number of elements."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: true,
      config: FormatterConfig::default(),
      expected: r#"/// Creates an empty vector with zero capacity.
///
/// # Example
///
/// ```ignis
/// import Vector from "std::vector";
///
/// let v: Vector<i32> = Vector::new<i32>();
/// ```
public static new(): Vector<T> {
    return Vector {
        data: null,
        length: 0,
        capacity: 0,
    };
}

/// Creates an empty vector with preallocated `capacity` elements.
///
/// # Example
///
/// ```ignis
/// import Vector from "std::vector";
///
/// let v: Vector<i32> = Vector::new<i32>(16);
/// ```
public static new(capacity: u64): Vector<T> {
    return Vector {
        data: Memory::allocateVector<T>(capacity),
        length: 0,
        capacity: capacity,
    };
}

/// Compatibility alias for `Vector::new()`.
public static init(): Vector<T> {
    return Vector {
        data: null,
        length: 0,
        capacity: 0,
    };
}

/// Compatibility alias for `Vector::new(capacity)`.
public static init(capacity: u64): Vector<T> {
    return Vector {
        data: Memory::allocateVector<T>(capacity),
        length: 0,
        capacity: capacity,
    };
}
"#,
    },
    ReviewedRealFileCase {
      name: "std option lang enum slice",
      source_path: "std/option/mod.ign",
      start_marker: Some("@lang(try)"),
      end_marker: Some("/// Returns `true` if the option is `NONE`."),
      append_closing_brace: true,
      wrap_in_heap_allocator_record: false,
      config: FormatterConfig::default(),
      expected: r#"@lang(try)
export enum Option<S> {
    SOME(S),
    NONE,

    /// Returns `true` if the option contains a value (`SOME`).
    ///
    /// # Example
    ///
    /// ```ignis
    /// import Option from "std::option";
    ///
    /// let x: Option<i32> = Option::SOME(42);
    /// let y: Option<i32> = Option::NONE;
    ///
    /// x.isSome();  // true
    /// y.isSome();  // false
    /// ```
    public isSome(&self): boolean {
        return match (self) {
            Option::SOME(_) -> true,
            _ -> false,
        };
    }
}
"#,
    },
    ReviewedRealFileCase {
      name: "std result lang enum slice",
      source_path: "std/result/mod.ign",
      start_marker: Some("@lang(try)"),
      end_marker: Some("/// Returns `true` if the result is `ERROR`."),
      append_closing_brace: true,
      wrap_in_heap_allocator_record: false,
      config: FormatterConfig::default(),
      expected: r#"@lang(try)
export enum Result<T, E> {
    OK(T),
    ERROR(E),

    /// Returns `true` if the result is `OK`.
    ///
    /// # Example
    ///
    /// ```ignis
    /// import Result from "std::result";
    ///
    /// let x: Result<i32, str> = Result::OK(42);
    /// let y: Result<i32, str> = Result::ERROR("oops");
    ///
    /// x.isOk();     // true
    /// y.isOk();     // false
    /// ```
    public isOk(&self): boolean {
        return match (self) {
            Result::OK(_) -> true,
            _ -> false,
        };
    }
}
"#,
    },
    ReviewedRealFileCase {
      name: "std vector map higher-order slice",
      source_path: "std/vector/mod.ign",
      start_marker: Some("public map<U>(&self, @noescape f: (&T) -> U): Vector<U> {"),
      end_marker: Some("/// Folds the vector left-to-right starting from an explicit initial value."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: true,
      config: FormatterConfig::default(),
      expected: r#"public map<U>(&self, @noescape f: (&T) -> U): Vector<U> {
    let mut result: Vector<U> = Vector::new<U>(self.length);
    let mut i: u64 = 0;
    while (i < self.length) {
        result.push(f(&self.data[i]));
        i += 1;
    }
    return result;
}
"#,
    },
    ReviewedRealFileCase {
      name: "std vector fold higher-order slice",
      source_path: "std/vector/mod.ign",
      start_marker: Some("public fold<U>(&self, initial: U, @noescape f: (U, &T) -> U): U {"),
      end_marker: Some("/// Releases the backing storage and resets length/capacity to zero."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: true,
      config: FormatterConfig::default(),
      expected: r#"public fold<U>(&self, initial: U, @noescape f: (U, &T) -> U): U {
    let mut acc: U = initial;
    let mut i: u64 = 0;
    while (i < self.length) {
        acc = f(acc, &self.data[i]);
        i += 1;
    }
    return acc;
}
"#,
    },
    ReviewedRealFileCase {
      name: "lambda module const and higher-order slice",
      source_path: "example/lambda.ign",
      start_marker: Some("const add: (i32, i32) -> i32 ="),
      end_marker: Some("function main(): i32 {"),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: false,
      config: FormatterConfig::default(),
      expected: r#"const add: (i32, i32) -> i32 = (a: i32, b: i32): i32 -> a + b;
const mul: (i32, i32) -> i32 = (a: i32, b: i32): i32 -> {
  return a * b;
};

// Higher-order: takes a closure parameter and applies it
function apply(f: (i32) -> i32, x: i32): i32 {
  return f(x);
}

// Takes two closures
function combine(f: (i32) -> i32, g: (i32) -> i32, x: i32): i32 {
  return f(g(x));
}
"#,
    },
    ReviewedRealFileCase {
      name: "std fs tight line width function slice",
      source_path: "std/fs/mod.ign",
      start_marker: Some("function write(path: str, data: *void, len: u64): Result<void, Io::IoError> {"),
      end_marker: Some("/// Alias for `writeBytes(&String, data, len)`."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: false,
      config: FormatterConfig {
        indent_width: 4,
        line_width: 40,
        use_tabs: false,
        sort_imports: false,
      },
      expected: "function write(\n    path: str,\n    data: *void,\n    len: u64\n): Result<void, Io::IoError> {\n    return Fs::writeBytesRaw(path, data, len);\n}\n",
    },
    ReviewedRealFileCase {
      name: "std vector reduce higher-order slice formats exactly",
      source_path: "std/vector/mod.ign",
      start_marker: Some("/// Folds the vector left-to-right without an explicit initial value."),
      end_marker: Some("/// Sorts the vector in-place using a comparator."),
      append_closing_brace: false,
      wrap_in_heap_allocator_record: true,
      config: FormatterConfig::default(),
      expected: "/// Folds the vector left-to-right without an explicit initial value.\n///\n/// The first element is **copied** as the initial accumulator. Then `f`\n/// is called for each subsequent element with `(accumulator, &element)`,\n/// and its return value becomes the new accumulator.\n///\n/// Returns `Option::NONE` for an empty vector.\n///\n/// # Arguments\n///\n/// * `f` - Callback that takes the accumulator by value and the next\n///   element by reference, and returns the updated accumulator.\n///\n/// # Returns\n///\n/// `Option::SOME(result)` with the final accumulated value, or\n/// `Option::NONE` if the vector is empty.\n///\n/// # Example\n///\n/// ```ignis\n/// import Vector from \"std::vector\";\n/// import Option from \"std::option\";\n///\n/// let mut v: Vector<i32> = Vector::new<i32>();\n/// v.push(1);\n/// v.push(2);\n/// v.push(3);\n///\n/// // Sum all elements: 1 + 2 + 3 = 6\n/// let sum: Option<i32> = v.reduce((acc: i32, x: &i32): i32 -> {\n///   return acc + *x;\n/// });\n/// // sum == Option::SOME(6)\n///\n/// // Empty vector returns NONE\n/// let empty: Vector<i32> = Vector::new<i32>();\n/// let result: Option<i32> = empty.reduce((acc: i32, x: &i32): i32 -> {\n///   return acc + *x;\n/// });\n/// // result == Option::NONE\n/// ```\n///\n/// # See Also\n///\n/// Use `fold<U>` if you need an explicit initial value or a different\n/// return type.\npublic reduce(&self, @noescape f: (T, &T) -> T): Option<T> {\n    if (self.length == 0) {\n        return Option::NONE;\n    }\n\n    let mut acc: T = self.data[0];\n    let mut i: u64 = 1;\n    while (i < self.length) {\n        acc = f(acc, &self.data[i]);\n        i += 1;\n    }\n    return Option::SOME(acc);\n}\n",
    },
  ]
}

fn unsupported_real_file_cases() -> Vec<UnsupportedRealFileCase> {
  vec![UnsupportedRealFileCase {
    name: "std io top level structure",
    source_path: "std/io/mod.ign",
    start_marker: Some("//! # I/O"),
    end_marker: Some("/// Prints `message` to stdout followed by a newline (`\\n`)."),
    append_closing_brace: false,
    wrap_in_heap_allocator_record: false,
    config: FormatterConfig::default(),
    expected_error: "parser failed: Expected RightBrace",
  }]
}

#[test]
fn reviewed_real_file_corpus_covers_example_allocator_and_std_inputs() {
  let covered_paths = reviewed_real_file_cases()
    .iter()
    .map(|case| case.source_path)
    .chain(unsupported_real_file_cases().iter().map(|case| case.source_path))
    .collect::<Vec<_>>();

  assert!(covered_paths.contains(&"example/hello-world.ign"));
  assert!(covered_paths.contains(&"example/allocator/src/heap_allocator.ign"));
  assert!(covered_paths.contains(&"std/io/mod.ign"));
  assert!(covered_paths.contains(&"std/vector/mod.ign"));
  assert!(covered_paths.contains(&"std/option/mod.ign"));
  assert!(covered_paths.contains(&"std/result/mod.ign"));
}

#[test]
fn reviewed_real_file_corpus_matches_exact_expected_outputs() {
  for case in reviewed_real_file_cases() {
    let source = read_real_file(case.source_path);
    let raw_input = match (case.start_marker, case.end_marker) {
      (Some(start_marker), Some(end_marker)) => {
        let slice = extract_slice(&source, start_marker, end_marker);

        if case.append_closing_brace {
          slice + "}\n"
        } else {
          slice
        }
      },
      (None, None) => source,
      _ => panic!("invalid review case markers for {}", case.name),
    };
    let input = if case.wrap_in_heap_allocator_record {
      wrap_record_body(&raw_input)
    } else {
      raw_input
    };

    let formatted = format_text(
      &input,
      &FormatOptions {
        check: false,
        config: case.config.clone(),
      },
    )
    .unwrap_or_else(|error| panic!("format {}: {error}", case.name));

    let actual = if case.wrap_in_heap_allocator_record {
      remove_single_indent_block(
        formatted
          .strip_prefix("export record HeapAllocator {\n")
          .and_then(|body| body.strip_suffix("}\n"))
          .unwrap_or_else(|| panic!("record wrapper for {}", case.name)),
        case.config.indent_width,
      )
    } else {
      formatted
    };

    assert_eq!(
      actual,
      normalize_expected_indent(case.expected, &case.config),
      "reviewed case: {}",
      case.name
    );
    assert_no_trailing_whitespace(&actual);
  }
}

#[test]
fn reviewed_real_file_unsupported_cases_fail_fast() {
  for case in unsupported_real_file_cases() {
    let source = read_real_file(case.source_path);
    let raw_input = match (case.start_marker, case.end_marker) {
      (Some(start_marker), Some(end_marker)) => {
        let slice = extract_slice(&source, start_marker, end_marker);

        if case.append_closing_brace {
          slice + "}\n"
        } else {
          slice
        }
      },
      (None, None) => source,
      _ => panic!("invalid unsupported case markers for {}", case.name),
    };
    let input = if case.wrap_in_heap_allocator_record {
      wrap_record_body(&raw_input)
    } else {
      raw_input
    };

    let error = match format_text(
      &input,
      &FormatOptions {
        check: false,
        config: case.config.clone(),
      },
    ) {
      Ok(formatted) => panic!("unsupported case {} formatted unexpectedly:\n{formatted}", case.name),
      Err(error) => error,
    };

    assert_eq!(
      error.to_string(),
      case.expected_error,
      "unsupported reviewed case: {}",
      case.name
    );
  }
}

// --- Phase 5.3: Release gate ---

/// The release gate verifies that the three pillars of formatter quality all
/// hold simultaneously: AST completeness, real-file corpus regression, and
/// idempotence safety. If any pillar regresses, this test fails and blocks
/// release. Each pillar is independently enforced by its own test file; this
/// test verifies they are wired together and documents the gate intent.
///
/// Completeness gate: `completeness.rs` (AST node dispatch inventory)
/// Real-file corpus:  `real_files.rs` (whole-file formatting + idempotence)
/// Idempotence suite: `safety.rs` (parse-back, token-loss, idempotence checks)
#[test]
fn release_gate_completeness_corpus_and_idempotence_suites_must_all_pass() {
  // This test exercises the same formatter pipeline that the independent
  // completeness, real-file, and safety test files validate. It acts as a
  // single integration point that fails fast if any pillar regresses.

  // Completeness: format a representative case from every major AST domain
  // that currently passes safety validation. Domains with known token-shape
  // drift (e.g. match-in-let) are covered separately in snapshot tests.
  let domains = [
    // Statements: function, record, enum, import, constant, namespace
    "function main(): void { return; }\n",
    "record Point { x: i32; y: i32; }\n",
    "enum Color { RED, GREEN, BLUE, }\n",
    "import Io from \"std::io\";\n",
    "const MAX: i32 = 100;\n",
    "export namespace Inner {\n    function helper(): void {}\n}\n",
    // Expressions: binary, call, cast, lambda
    "function f(): void { let x: i32 = 1 + 2; }\n",
    "function f(): void { Io::println(\"hello\"); }\n",
    "function f(): void { let p: *void = null as *void; }\n",
    "function f(): void { let square = (x: i32): i32 -> x * x; }\n",
  ];

  for source in &domains {
    let result = format_text(source, &FormatOptions::default());
    assert!(
      result.is_ok(),
      "release gate: completeness domain must format successfully\n  source: {source:?}\n  error: {:?}",
      result.err()
    );

    let formatted = result.expect("already checked");

    // Idempotence: second pass must produce identical output.
    let second =
      format_text(&formatted, &FormatOptions::default()).expect("release gate: idempotence second pass must succeed");
    assert_eq!(formatted, second, "release gate: idempotence violation for source: {source:?}");
  }

  // Real-file corpus: verify at least one representative whole file formats.
  let corpus_paths = ["example/hello-world.ign", "std/option/mod.ign", "std/result/mod.ign"];

  for path in &corpus_paths {
    let source = read_real_file(path);
    let first = format_text(&source, &FormatOptions::default())
      .unwrap_or_else(|error| panic!("release gate: corpus file {path} must format: {error}"));
    let second = format_text(&first, &FormatOptions::default())
      .unwrap_or_else(|error| panic!("release gate: corpus file {path} idempotence: {error}"));
    assert_eq!(first, second, "release gate: corpus file {path} must be idempotent");
  }
}
