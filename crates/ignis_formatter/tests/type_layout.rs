use ignis_formatter::{FormatterConfig, FormatOptions, format_text};
use insta::assert_snapshot;

fn normalize_expected_indent(text: &str) -> String {
  let config = FormatterConfig::default();
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

#[test]
fn formats_type_alias_modifier_syntax_without_operator_spacing() {
  let formatted =
    format_text("type Borrowed = * mut Node;\n", &FormatOptions::default()).expect("format type alias modifiers");

  assert_eq!(formatted, "type Borrowed = *mut Node;\n");
}

#[test]
fn formats_exported_record_members_with_pointer_and_reference_types_and_method_ownership() {
  let source = "export record Holder { ptr: * mut Node; public borrow(& mut self, target: & mut Node): * mut Node { return self.ptr; } }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("record members with inline method modifiers should now format successfully");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "export record Holder {\n    ptr: *mut Node;\n\n    public borrow(&mut self, target: &mut Node): *mut Node {\n        return self.ptr;\n    }\n}\n"
    )
  );
}

#[test]
fn formats_callable_and_vector_value_slices() {
  let source = "type  Mapper = (i32)->i32;\nfunction  build( mapper : Mapper, value : i32 ) : i32[] {return [mapper(value), value];}\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("callable and vector slices should format successfully");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "type Mapper = (i32) -> i32;\nfunction build(mapper: Mapper, value: i32): i32[] {\n    return [mapper(value), value];\n}\n"
    )
  );
}

#[test]
fn formats_const_callable_tuple_value_slices() {
  let source = "const  add : (i32,i32)->i32 = (a: i32, b: i32): i32 -> {return a + b;};\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("const callable tuple slices should format without failing safety validation");

  assert_eq!(
    formatted,
    normalize_expected_indent("const add: (i32, i32) -> i32 = (a: i32, b: i32): i32 -> {\n    return a + b;\n};\n")
  );
}

#[test]
fn formats_generic_const_value_slices() {
  let source = "const  defaultResult : Result<i32,Error> = other;\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("generic const slices should format without safety failure");

  assert_eq!(formatted, "const defaultResult: Result<i32, Error> = other;\n");
}

#[test]
fn formats_qualified_generic_path_types_without_dropping_args() {
  let source = "record Module { public items: Ast::Store<Ast::ItemTag,Ast::Item::Node>; }\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("qualified generic path type arguments should be preserved");

  assert_eq!(
    formatted,
    normalize_expected_indent("record Module {\n    public items: Ast::Store<Ast::ItemTag, Ast::Item::Node>;\n}\n")
  );
}

#[test]
fn formats_generic_record_members_with_vector_callable_types_and_record_initializer_values() {
  let source = "record  ReducerBox<T> { public values: T[]; public reducer: (T,&T)->T; }\nfunction  build<T>( values : T[], reducer : (T,&T)->T ) : ReducerBox<T> { return ReducerBox<T> { values: values, reducer: reducer, }; }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("generic record members and record initializer values should format successfully");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "record ReducerBox<T> {\n    public values: T[];\n    public reducer: (T, &T) -> T;\n}\nfunction build<T>(values: T[], reducer: (T, &T) -> T): ReducerBox<T> {\n    return ReducerBox<T> { values: values, reducer: reducer };\n}\n"
    )
  );
}

#[test]
fn formats_record_members_with_visibility_modifiers_and_tuple_types() {
  let source = "record  Cache<T> { public value: T; private touch(&mut self, value: T): void { self.hits += 1; } }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("record member visibility and tuple type slices should format successfully");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "record Cache<T> {\n    public value: T;\n\n    private touch(&mut self, value: T): void {\n        self.hits += 1;\n    }\n}\n"
    )
  );
}

#[test]
fn formats_let_bound_record_initializer_values_multiline() {
  let source = "record Household { public city: str; public price: i32; }\nfunction main(): i32 { let home: Household = Household { city: \"Barcelona\", price: 109 }; return home.price; }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("let-bound record initializer values should format as stable multiline slices");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "record Household {\n    public city: str;\n    public price: i32;\n}\nfunction main(): i32 {\n    let home: Household = Household { city: \"Barcelona\", price: 109 };\n    return home.price;\n}\n"
    )
  );
}

#[test]
fn formats_export_namespace_with_single_function() {
  let formatted = format_text(
    "export namespace Io { function println(message: str): void {} }\n",
    &FormatOptions::default(),
  )
  .expect("export namespace with single function should format successfully");

  assert_eq!(
    formatted,
    normalize_expected_indent("export namespace Io {\n    function println(message: str): void {\n    }\n}\n")
  );
}

#[test]
fn formats_empty_high_level_blocks_inline() {
  let formatted = format_text(
    "export namespace Fs::Sys {\n}\nrecord Empty {\n}\ntrait Marker {\n}\nextern __fs_rt {\n}\n",
    &FormatOptions::default(),
  )
  .expect("empty high-level blocks should format inline");

  assert_eq!(
    formatted,
    "export namespace Fs::Sys {}\nrecord Empty {}\ntrait Marker {}\nextern __fs_rt {}\n"
  );
}

#[test]
fn removes_trailing_comma_when_record_initializer_collapses_inline() {
  let formatted = format_text(
    "record String { data: *mut u8; len: u64; cap: u64; }\nfunction build(): void { let mut result: String = String { data: null, len: 0, cap: 0, }; }\n",
    &FormatOptions { config: FormatterConfig { indent_width: 2, line_width: 200, use_tabs: false, sort_imports: false }, check: false },
  )
  .expect("inline record initializer should drop trailing comma");

  assert!(
    formatted.contains("let mut result: String = String { data: null, len: 0, cap: 0 };"),
    "expected inline record initializer without trailing comma\nformatted:\n{formatted}"
  );
}

#[test]
fn adds_trailing_comma_when_record_initializer_breaks_multiline() {
  let formatted = format_text(
    "record String { data: *mut u8; len: u64; cap: u64; }\nfunction build(): void { let mut result: String = String { data: null, len: 0, cap: 0 }; }\n",
    &FormatOptions { config: FormatterConfig { indent_width: 2, line_width: 40, use_tabs: false, sort_imports: false }, check: false },
  )
  .expect("multiline record initializer should add trailing comma");

  assert!(
    formatted.contains("cap: 0,\n  };"),
    "expected multiline record initializer with trailing comma\nformatted:\n{formatted}"
  );
}

#[test]
fn formats_namespace_with_two_functions_no_comments() {
  let source = "namespace Io {\n  function println(msg: str): void {\n    let x: i32 = 1;\n  }\n\n  function eprint(msg: str): void {\n    let y: i32 = 2;\n  }\n}\n";
  let result = format_text(source, &FormatOptions::default());
  match result {
    Ok(formatted) => assert!(!formatted.is_empty(), "namespace with two functions should format"),
    Err(e) => panic!("namespace with two functions failed: {}", e),
  }
}

#[test]
fn formats_namespace_with_multiple_functions_and_doc_comments() {
  let source = "namespace Io {\n  /// Prints to stdout.\n  function println(msg: str): void {\n    let x: i32 = 1;\n  }\n\n  /// Prints to stderr.\n  function eprint(msg: str): void {\n    let y: i32 = 2;\n  }\n}\n";
  let result = format_text(source, &FormatOptions::default());
  match result {
    Ok(formatted) => assert!(!formatted.is_empty(), "namespace with doc comments should format"),
    Err(e) => panic!("namespace with doc comments failed: {}", e),
  }
}

#[test]
fn preserves_lang_attribute_on_exported_generic_enum_slices() {
  let source = "@lang(try)\nexport enum  Option<T> {\n  SOME(T),\n  NONE,\n}\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("enum slices with lang attributes should keep the attribute while formatting generic variants");

  assert_eq!(
    formatted,
    normalize_expected_indent("@lang(try)\nexport enum Option<T> {\n    SOME(T),\n    NONE,\n}\n")
  );
}

#[test]
fn preserves_noescape_parameter_attributes_in_generic_function_slices() {
  let source = "function  foldValues<T,U>( values : Vector<T>, @noescape reducer : (U,&T)->U, initial : U ) : U { return reducer(initial, &values[0]); }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("generic function slices should preserve @noescape parameter attributes");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "function foldValues<T, U>(values: Vector<T>, @noescape reducer: (U, &T) -> U, initial: U): U {\n    return reducer(initial, &values[0]);\n}\n"
    )
  );
}

#[test]
fn formats_nested_record_initializer_values_inside_generic_record_initializers() {
  let source = "record Child {\npublic left: i32;\npublic right: i32;\n}\nrecord Parent {\npublic child: Child;\npublic count: i32;\n}\nfunction  init( value : i32 ) : Parent { return Parent { child: Child { left: value, right: value + 1, }, count: 2, }; }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("nested record initializer values should format as stable multiline slices");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "record Child {\n    public left: i32;\n    public right: i32;\n}\nrecord Parent {\n    public child: Child;\n    public count: i32;\n}\nfunction init(value: i32): Parent {\n    return Parent { child: Child { left: value, right: value + 1 }, count: 2 };\n}\n"
    )
  );
}

// Phase 2: Task 2.2 — Type/member snapshots

#[test]
fn formats_enum_with_mixed_variants_and_fields() {
  let source = "enum  Status  {  OK(i32),  ERR,  }\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("enum with mixed variants should format successfully");

  assert_snapshot!("formats_enum_with_mixed_variants_and_fields", formatted);
}

#[test]
fn formats_trait_with_multiple_methods_and_blank_line_separation() {
  let source = "trait  Comparable  {  compare(&self, other: &Self): i32;  equals(&self, other: &Self): boolean; }\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("trait with multiple methods should format with blank lines");

  assert_snapshot!("formats_trait_with_multiple_methods_and_blank_line_separation", formatted);
}

#[test]
fn formats_record_with_static_fields_and_methods() {
  let source =
    "record  Counter  {  public  static  count: i32;  public  static  increment(): void { Counter::count += 1; }  }\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("record with static members should format successfully");

  assert_snapshot!("formats_record_with_static_fields_and_methods", formatted);
}

#[test]
fn formats_enum_with_fields_and_methods() {
  let source =
    "enum  Color  {  RED,  GREEN,  BLUE,  public static fromName(name: str): Color { return Color::RED; } }\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("enum with fields and methods should format successfully");

  assert_snapshot!("formats_enum_with_fields_and_methods", formatted);
}

#[test]
fn formats_record_with_default_field_values() {
  let source = "record  Config  {  host: str = \"localhost\";  port: i32 = 8080;  }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("record with default field values should format successfully");

  assert_snapshot!("formats_record_with_default_field_values", formatted);
}

#[test]
fn formats_generic_record_with_multiple_methods() {
  let source = "record  Container<T>  {  value: T;  get(&self): T { return self.value; }  set(&mut self, value: T): void { self.value = value; }  }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("generic record with multiple methods should format with blank lines between methods");

  assert_snapshot!("formats_generic_record_with_multiple_methods", formatted);
}

#[test]
fn formats_trait_with_default_method_body() {
  let source = "trait  Describable  {  describe(&self): str;  defaultName(&self): str { return \"unknown\"; }  }\n";

  let formatted =
    format_text(source, &FormatOptions::default()).expect("trait with default method body should format successfully");

  assert_snapshot!("formats_trait_with_default_method_body", formatted);
}
