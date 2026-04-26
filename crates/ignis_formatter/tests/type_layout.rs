use ignis_formatter::{FormatterConfig, FormatOptions, format_text};

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
      "type Mapper = (i32) -> i32;\n\nfunction build(mapper: Mapper, value: i32): i32[] {\n    return [mapper(value), value];\n}\n"
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
fn formats_generic_record_members_with_vector_callable_types_and_record_initializer_values() {
  let source = "record  ReducerBox<T> { public values: T[]; public reducer: (T,&T)->T; }\nfunction  build<T>( values : T[], reducer : (T,&T)->T ) : ReducerBox<T> { return ReducerBox<T> { values: values, reducer: reducer, }; }\n";

  let formatted = format_text(source, &FormatOptions::default())
    .expect("generic record members and record initializer values should format successfully");

  assert_eq!(
    formatted,
    normalize_expected_indent(
      "record ReducerBox<T> {\n    public values: T[];\n    public reducer: (T, &T) -> T;\n}\n\nfunction build<T>(values: T[], reducer: (T, &T) -> T): ReducerBox<T> {\n    return ReducerBox<T> {\n        values: values,\n        reducer: reducer,\n    };\n}\n"
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
      "record Household {\n    public city: str;\n    public price: i32;\n}\n\nfunction main(): i32 {\n    let home: Household = Household {\n        city: \"Barcelona\",\n        price: 109\n    };\n    return home.price;\n}\n"
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
      "record Child {\n    public left: i32;\n    public right: i32;\n}\n\nrecord Parent {\n    public child: Child;\n    public count: i32;\n}\n\nfunction init(value: i32): Parent {\n    return Parent {\n        child: Child {\n            left: value,\n            right: value + 1,\n        },\n        count: 2,\n    };\n}\n"
    )
  );
}
