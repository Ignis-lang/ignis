mod common;

use insta::assert_snapshot;

#[test]
fn basic_function() {
  let result = common::analyze(
    r#"
function main(): void {
    return;
}
"#,
  );

  assert_snapshot!("basic_function_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("basic_function_hir", common::format_hir(&result));
}

#[test]
fn function_with_params() {
  let result = common::analyze(
    r#"
function add(a: i32, b: i32): i32 {
    return a + b;
}
"#,
  );

  assert_snapshot!(
    "function_with_params_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("function_with_params_hir", common::format_hir(&result));
}

#[test]
fn extern_function() {
  let result = common::analyze(
    r#"
extern C {
    function puts(s: str): i32;
}
"#,
  );

  assert_snapshot!("extern_function_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("extern_function_hir", common::format_hir(&result));
}

#[test]
fn mutable_variable() {
  let result = common::analyze(
    r#"
function main(): void {
    let mut x: i32 = 1;
    x = 2;
    return;
}
"#,
  );

  assert_snapshot!("mutable_variable_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("mutable_variable_hir", common::format_hir(&result));
}

#[test]
fn mutable_reference() {
  let result = common::analyze(
    r#"
function modify(x: &mut i32): void {
    x = 42;
    return;
}
"#,
  );

  assert_snapshot!(
    "mutable_reference_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("mutable_reference_hir", common::format_hir(&result));
}

#[test]
fn if_statement() {
  let result = common::analyze(
    r#"
function check(x: i32): i32 {
    if (x > 0) {
        return 1;
    } else {
        return 0;
    }
}
"#,
  );

  assert_snapshot!("if_statement_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("if_statement_hir", common::format_hir(&result));
}

#[test]
fn while_loop() {
  let result = common::analyze(
    r#"
function count(): i32 {
    let mut i: i32 = 0;
    while (i < 10) {
        i = i + 1;
    }
    return i;
}
"#,
  );

  assert_snapshot!("while_loop_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("while_loop_hir", common::format_hir(&result));
}

#[test]
fn test_attribute_marks_top_level_function() {
  let result = common::analyze(
    r#"
@test
function smoke(): void {
    return;
}
"#,
  );

  let smoke_name = result.output.symbols.borrow_mut().intern("smoke");
  let test_def = result
    .output
    .defs
    .iter()
    .find_map(|(_, def)| (def.name == smoke_name).then_some(def))
    .expect("smoke definition");

  let has_test_attr = match &test_def.kind {
    ignis_type::definition::DefinitionKind::Function(function) => function
      .attrs
      .iter()
      .any(|attr| matches!(attr, ignis_type::attribute::FunctionAttr::Test)),
    other => panic!("expected function definition, got {:?}", other),
  };

  assert!(has_test_attr, "expected @test to be recorded on the function definition");
  assert_snapshot!(
    "test_attribute_marks_top_level_function_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
}

#[test]
fn legacy_attributes_remain_compatible_after_attribute_ast_changes() {
  let result = common::analyze(
    r#"
trait Greetable {
    greet(&self): i32;
}

@implements(Greetable)
record Person {
    public age: i32;

    greet(&self): i32 {
        return self.age;
    }
}

@allow(unused_variable)
@test
function smoke(): void {
    let usedValue: i32 = Person { age: 3 }.greet();
    let unusedValue: i32 = usedValue + 1;
    return;
}
"#,
  );

  let smoke_name = result.output.symbols.borrow_mut().intern("smoke");
  let test_def = result
    .output
    .defs
    .iter()
    .find_map(|(_, def)| (def.name == smoke_name).then_some(def))
    .expect("smoke definition");

  let has_test_attr = match &test_def.kind {
    ignis_type::definition::DefinitionKind::Function(function) => function
      .attrs
      .iter()
      .any(|attr| matches!(attr, ignis_type::attribute::FunctionAttr::Test)),
    other => panic!("expected function definition, got {:?}", other),
  };

  assert!(has_test_attr, "expected @test to be recorded on the function definition");
  assert_snapshot!(
    "legacy_attributes_remain_compatible_after_attribute_ast_changes_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!(
    "legacy_attributes_remain_compatible_after_attribute_ast_changes_hir",
    common::format_hir(&result)
  );
}

#[test]
fn directive_attribute_records_function_metadata() {
  let result = common::analyze(
    r#"
@directive(target: "record", phase: expand, effect: emit, group: serde, capabilities: diagnostics)
function deriveRecord(): void {
    return;
}
"#,
  );

  assert_eq!(
    common::format_diagnostics(&result.output.diagnostics),
    "(no diagnostics)",
    "expected a valid @directive declaration to analyze cleanly"
  );

  let derive_record_name = result.output.symbols.borrow_mut().intern("deriveRecord");
  let directive_def = result
    .output
    .defs
    .iter()
    .find_map(|(_, def)| (def.name == derive_record_name).then_some(def))
    .expect("deriveRecord definition");

  let directive_attr = match &directive_def.kind {
    ignis_type::definition::DefinitionKind::Function(function) => function
      .attrs
      .iter()
      .find_map(|attr| match attr {
        ignis_type::attribute::FunctionAttr::Directive(metadata) => Some(metadata),
        _ => None,
      })
      .expect("expected @directive metadata on the function definition"),
    other => panic!("expected function definition, got {:?}", other),
  };

  assert_eq!(directive_attr.target, ignis_type::attribute::DirectiveTarget::Record);
  assert_eq!(directive_attr.phase, ignis_type::attribute::DirectivePhase::Expand);
  assert_eq!(directive_attr.effect, ignis_type::attribute::DirectiveEffect::Emit);
  assert_eq!(directive_attr.group.as_deref(), Some("serde"));
  assert_eq!(
    directive_attr.capabilities,
    vec![ignis_type::attribute::DirectiveCapability::Diagnostics],
    "parser named values currently accept identifier|string|int only, so this test locks the scalar identifier form"
  );
}

#[test]
fn directive_attribute_accepts_minimal_required_metadata() {
  let result = common::analyze(
    r#"
@directive(target: "function", phase: check, effect: diagnose)
function validate(): void {
    return;
}
"#,
  );

  assert_eq!(
    common::format_diagnostics(&result.output.diagnostics),
    "(no diagnostics)",
    "expected minimal required @directive metadata to analyze cleanly"
  );

  let validate_name = result.output.symbols.borrow_mut().intern("validate");
  let validate_def = result
    .output
    .defs
    .iter()
    .find_map(|(_, def)| (def.name == validate_name).then_some(def))
    .expect("validate definition");

  let directive_attr = match &validate_def.kind {
    ignis_type::definition::DefinitionKind::Function(function) => function
      .attrs
      .iter()
      .find_map(|attr| match attr {
        ignis_type::attribute::FunctionAttr::Directive(metadata) => Some(metadata),
        _ => None,
      })
      .expect("expected @directive metadata on the function definition"),
    other => panic!("expected function definition, got {:?}", other),
  };

  assert_eq!(directive_attr.target, ignis_type::attribute::DirectiveTarget::Function);
  assert_eq!(directive_attr.phase, ignis_type::attribute::DirectivePhase::Check);
  assert_eq!(directive_attr.effect, ignis_type::attribute::DirectiveEffect::Diagnose);
  assert_eq!(directive_attr.group, None);
  assert!(
    directive_attr.capabilities.is_empty(),
    "expected optional metadata to stay absent"
  );
}

#[test]
fn directive_registry_exposes_definition_metadata_and_provenance() {
  let result = common::analyze(
    r#"
@directive(target: "record", phase: expand, effect: emit, group: serde, capabilities: diagnostics)
function deriveRecord(): void {
    return;
}

@directive(target: "function", phase: check, effect: diagnose)
function validateFunction(): void {
    return;
}
"#,
  );

  assert_eq!(
    common::format_diagnostics(&result.output.diagnostics),
    "(no diagnostics)",
    "expected valid @directive declarations to analyze cleanly"
  );

  let symbols = result.output.symbols.borrow();
  let registry = &result.output.directive_registry;

  assert_eq!(registry.defs.len(), 2, "expected both directive definitions to be discoverable");
  assert_eq!(
    registry.uses.len(),
    0,
    "task 2.2 should not invent directive use resolution yet"
  );

  let derive_record = registry
    .defs
    .iter()
    .find(|directive| symbols.get(&directive.name) == "deriveRecord")
    .expect("deriveRecord directive registry entry");

  assert_eq!(derive_record.target, ignis_type::attribute::DirectiveTarget::Record);
  assert_eq!(derive_record.phase, ignis_type::attribute::DirectivePhase::Expand);
  assert_eq!(derive_record.effect, ignis_type::attribute::DirectiveEffect::Emit);
  assert_eq!(derive_record.group.as_deref(), Some("serde"));
  assert!(
    derive_record.provenance.origin_attr_span.is_valid() && !derive_record.provenance.origin_attr_span.is_empty(),
    "expected directive provenance to preserve the originating attribute span"
  );

  let validate_function = registry
    .defs
    .iter()
    .find(|directive| symbols.get(&directive.name) == "validateFunction")
    .expect("validateFunction directive registry entry");

  assert_eq!(validate_function.target, ignis_type::attribute::DirectiveTarget::Function);
  assert_eq!(validate_function.phase, ignis_type::attribute::DirectivePhase::Check);
  assert_eq!(validate_function.effect, ignis_type::attribute::DirectiveEffect::Diagnose);
  assert_eq!(validate_function.group, None);

  let serde_group = registry.groups.get("serde").expect("serde directive group entry");
  assert_eq!(serde_group, &vec![derive_record.id]);
}

#[test]
fn qualified_local_directive_use_registers_a_record_use() {
  let result = common::analyze(
    r#"
namespace Serde {
    @directive(target: "record", phase: expand, effect: emit)
    function serializable(): void {
        return;
    }
}

@Serde::serializable
record User {
    public id: i32;
}
"#,
  );

  assert_eq!(
    common::format_diagnostics(&result.output.diagnostics),
    "(no diagnostics)",
    "expected a same-file qualified directive use to stay analyzable in the minimal 3.1 slice"
  );

  let symbols = result.output.symbols.borrow();
  let registry = &result.output.directive_registry;

  assert_eq!(registry.defs.len(), 1, "expected the directive declaration to stay registered");
  assert_eq!(
    registry.uses.len(),
    1,
    "expected the qualified record attribute to be collected as a directive use"
  );

  let directive_use = &registry.uses[0];
  let directive_def = registry
    .defs
    .iter()
    .find(|directive| directive.id == directive_use.directive)
    .expect("directive definition for collected use");

  assert_eq!(symbols.get(&directive_def.name), "serializable");
  assert_eq!(directive_def.target, ignis_type::attribute::DirectiveTarget::Record);
  assert_eq!(
    directive_use.provenance.origin_attr_span,
    directive_def.provenance.origin_attr_span
  );
}

#[test]
fn legacy_and_directive_function_attributes_stay_separated() {
  let result = common::analyze(
    r#"
@test
function smoke(): void {
    return;
}

@directive(target: "function", phase: check, effect: diagnose)
function validateFunction(): void {
    return;
}
"#,
  );

  assert_eq!(
    common::format_diagnostics(&result.output.diagnostics),
    "(no diagnostics)",
    "expected legacy and directive attributes to coexist without changing behavior"
  );

  let mut symbols = result.output.symbols.borrow_mut();
  let smoke_name = symbols.intern("smoke");
  let validate_name = symbols.intern("validateFunction");
  drop(symbols);

  let smoke_def = result
    .output
    .defs
    .iter()
    .find_map(|(_, def)| (def.name == smoke_name).then_some(def))
    .expect("smoke definition");

  let validate_def = result
    .output
    .defs
    .iter()
    .find_map(|(_, def)| (def.name == validate_name).then_some(def))
    .expect("validateFunction definition");

  let smoke_attrs = match &smoke_def.kind {
    ignis_type::definition::DefinitionKind::Function(function) => &function.attrs,
    other => panic!("expected smoke function definition, got {:?}", other),
  };

  assert!(
    smoke_attrs
      .iter()
      .any(|attr| matches!(attr, ignis_type::attribute::FunctionAttr::Test)),
    "expected @test to stay on legacy function attributes"
  );
  assert!(
    !smoke_attrs
      .iter()
      .any(|attr| matches!(attr, ignis_type::attribute::FunctionAttr::Directive(_))),
    "expected legacy @test function to avoid directive metadata"
  );

  let validate_attrs = match &validate_def.kind {
    ignis_type::definition::DefinitionKind::Function(function) => &function.attrs,
    other => panic!("expected validateFunction definition, got {:?}", other),
  };

  assert!(
    validate_attrs
      .iter()
      .any(|attr| matches!(attr, ignis_type::attribute::FunctionAttr::Directive(_))),
    "expected @directive metadata to stay on directive-marked functions"
  );
  assert!(
    !validate_attrs
      .iter()
      .any(|attr| matches!(attr, ignis_type::attribute::FunctionAttr::Test)),
    "expected directive-marked function to avoid legacy @test tagging"
  );

  assert_eq!(
    result.output.directive_registry.defs.len(),
    1,
    "expected only the directive-marked function to be registered"
  );
}

#[test]
fn directive_declarations_and_uses_coexist_with_implements_and_test() {
  let result = common::analyze(
    r#"
trait Greetable {
    greet(&self): i32;
}

@directive(target: "record", phase: expand, effect: emit)
function serializable(): void {
    return;
}

@serializable
record User {
    public id: i32;
}

@implements(Greetable)
record Person {
    public age: i32;

    greet(&self): i32 {
        return self.age;
    }
}

@test
function smoke(): void {
    let person: Person = Person { age: 7 };
    let user: User = User { id: person.greet() };

    if (user.id == 0) {
        return;
    }

    return;
}
"#,
  );

  assert_eq!(
    common::format_diagnostics(&result.output.diagnostics),
    "(no diagnostics)",
    "expected directive collection to coexist with @implements and @test"
  );

  let mut symbols = result.output.symbols.borrow_mut();
  let serializable_name = symbols.intern("serializable");
  let person_name = symbols.intern("Person");
  let smoke_name = symbols.intern("smoke");
  drop(symbols);

  let registry = &result.output.directive_registry;
  assert_eq!(registry.defs.len(), 1, "expected one directive declaration to be registered");
  assert_eq!(registry.uses.len(), 1, "expected one directive use to be collected");

  let directive_def = registry
    .defs
    .iter()
    .find(|directive| directive.name == serializable_name)
    .expect("serializable directive definition");

  assert_eq!(directive_def.target, ignis_type::attribute::DirectiveTarget::Record);
  assert_eq!(registry.uses[0].directive, directive_def.id);
  assert_eq!(
    registry.uses[0].provenance.origin_attr_span,
    directive_def.provenance.origin_attr_span
  );

  let person_def = result
    .output
    .defs
    .iter()
    .find_map(|(_, def)| (def.name == person_name).then_some(def))
    .expect("Person definition");

  match &person_def.kind {
    ignis_type::definition::DefinitionKind::Record(record) => {
      assert_eq!(record.implemented_traits.len(), 1, "expected @implements to stay bound");
    },
    other => panic!("expected Person record definition, got {:?}", other),
  }

  let smoke_def = result
    .output
    .defs
    .iter()
    .find_map(|(_, def)| (def.name == smoke_name).then_some(def))
    .expect("smoke definition");

  match &smoke_def.kind {
    ignis_type::definition::DefinitionKind::Function(function) => {
      assert!(
        function
          .attrs
          .iter()
          .any(|attr| matches!(attr, ignis_type::attribute::FunctionAttr::Test)),
        "expected @test to stay bound on the legacy function"
      );
    },
    other => panic!("expected smoke function definition, got {:?}", other),
  }
}

#[test]
fn generated_attached_method_metadata_can_attach_to_real_definition_ids() {
  let result = common::analyze(
    r#"
@directive(target: "record", phase: expand, effect: emit)
function serializable(): void {
    return;
}

@serializable
record User {
    public id: i32;

    describe(&self): i32 {
        return self.id;
    }
}
"#,
  );

  assert_eq!(common::format_diagnostics(&result.output.diagnostics), "(no diagnostics)");

  let mut symbols = result.output.symbols.borrow_mut();
  let user_name = symbols.intern("User");
  let describe_name = symbols.intern("describe");
  drop(symbols);

  let user_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == user_name).then_some(def_id))
    .expect("User definition id");
  let describe_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == describe_name).then_some(def_id))
    .expect("describe definition id");

  let mut registry = result.output.directive_registry.clone();
  let generated_metadata = ignis_type::definition::GeneratedItemMetadata::attached_method(
    registry.uses[0].generated_provenance(3),
    user_def_id,
    false,
  );

  registry.attach_generated_item(describe_def_id, generated_metadata.clone());

  assert_eq!(registry.generated_item_metadata(&describe_def_id), Some(&generated_metadata));
  assert_eq!(
    registry.generated_attached_method_items_for_owner(user_def_id),
    vec![ignis_analyzer::directive_registry::GeneratedSemanticItem {
      definition: describe_def_id,
      metadata: generated_metadata.clone(),
    }]
  );
  assert!(
    registry
      .generated_attached_method_items_for_owner(describe_def_id)
      .is_empty()
  );
}

#[test]
fn generated_implemented_trait_metadata_can_attach_without_changing_implements_behavior() {
  let result = common::analyze(
    r#"
trait EqLike {
}

@directive(target: "record", phase: expand, effect: emit)
function deriveEq(): void {
    return;
}

@deriveEq
@implements(EqLike)
record User {
    public id: i32;
}
"#,
  );

  assert_eq!(common::format_diagnostics(&result.output.diagnostics), "(no diagnostics)");

  let mut symbols = result.output.symbols.borrow_mut();
  let user_name = symbols.intern("User");
  let eq_like_name = symbols.intern("EqLike");
  drop(symbols);

  let user_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == user_name).then_some(def_id))
    .expect("User definition id");
  let eq_like_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == eq_like_name).then_some(def_id))
    .expect("EqLike definition id");

  let mut registry = result.output.directive_registry.clone();
  let generated_metadata = ignis_type::definition::GeneratedItemMetadata::implemented_trait(
    registry.uses[0].generated_provenance(7),
    user_def_id,
    eq_like_def_id,
  );

  registry.attach_generated_item(user_def_id, generated_metadata.clone());

  assert_eq!(registry.generated_item_metadata(&user_def_id), Some(&generated_metadata));
  assert_eq!(
    registry.generated_implemented_trait_items_for_owner(user_def_id),
    vec![ignis_analyzer::directive_registry::GeneratedSemanticItem {
      definition: user_def_id,
      metadata: generated_metadata.clone(),
    }]
  );
  assert!(
    registry
      .generated_implemented_trait_items_for_owner(eq_like_def_id)
      .is_empty()
  );

  let user_record = result.output.defs.get(&user_def_id);
  match &user_record.kind {
    ignis_type::definition::DefinitionKind::Record(record) => {
      assert_eq!(record.implemented_traits, vec![eq_like_def_id]);
    },
    other => panic!("expected User record definition, got {:?}", other),
  }
}

#[test]
fn for_loop() {
  let result = common::analyze(
    r#"
function sum(): i32 {
    let mut total: i32 = 0;
    for (let i = 0; i < 10; i = i + 1) {
        total = total + i;
    }
    return total;
}
"#,
  );

  assert_snapshot!("for_loop_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("for_loop_hir", common::format_hir(&result));
}

#[test]
fn numeric_cast() {
  let result = common::analyze(
    r#"
function convert(): f64 {
    let x: i32 = 42;
    return x as f64;
}
"#,
  );

  assert_snapshot!("numeric_cast_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("numeric_cast_hir", common::format_hir(&result));
}

#[test]
fn multiple_functions() {
  let result = common::analyze(
    r#"
function helper(x: i32): i32 {
    return x * 2;
}

function main(): i32 {
    return helper(21);
}
"#,
  );

  assert_snapshot!(
    "multiple_functions_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("multiple_functions_hir", common::format_hir(&result));
}

#[test]
fn extern_const() {
  let result = common::analyze(
    r#"
extern C {
    const PI: f64;
}

function area(radius: f64): f64 {
    return C::PI * radius * radius;
}
"#,
  );

  assert_snapshot!("extern_const_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("extern_const_hir", common::format_hir(&result));
}

#[test]
fn export_function() {
  let result = common::analyze(
    r#"
export function add(a: i32, b: i32): i32 {
    return a + b;
}
"#,
  );

  assert_snapshot!("export_function_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("export_function_hir", common::format_hir(&result));
}

#[test]
fn staged_export_function_matches_golden() {
  let result = common::analyze_staged(
    r#"
export function add(a: i32, b: i32): i32 {
    return a + b;
}
"#,
  );

  assert_snapshot!(
    "staged_export_function_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("staged_export_function_hir", common::format_hir(&result));
}

#[test]
fn literal_adapts_to_expected_type() {
  let result = common::analyze(
    r#"
function test(): i8 {
    let x: i8 = 42;
    let y: i16 = 1000;
    let z: f32 = 3.14;
    return x;
}
"#,
  );

  assert_snapshot!("literal_adapts_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("literal_adapts_hir", common::format_hir(&result));
}

#[test]
fn const_array() {
  let result = common::analyze(
    r#"
const PRIMES: i32[4] = [2, 3, 5, 7];

function get_second(): i32 {
    return PRIMES[1];
}
"#,
  );

  assert_snapshot!("const_array_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("const_array_hir", common::format_hir(&result));
}

// ============================================================================
// Type Alias Tests
// ============================================================================

#[test]
fn type_alias_basic() {
  let result = common::analyze(
    r#"
type Int = i32;

function main(): void {
    let x: Int = 42;
    return;
}
"#,
  );

  assert_snapshot!("type_alias_basic_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("type_alias_basic_hir", common::format_hir(&result));
}

#[test]
fn type_alias_in_function() {
  let result = common::analyze(
    r#"
type Number = i32;

function add(a: Number, b: Number): Number {
    return a + b;
}
"#,
  );

  assert_snapshot!(
    "type_alias_in_function_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("type_alias_in_function_hir", common::format_hir(&result));
}

// ============================================================================
// Record Tests
// ============================================================================

#[test]
fn record_basic() {
  let result = common::analyze(
    r#"
record Point {
    x: i32;
    y: i32;
}

function main(): void {
    return;
}
"#,
  );

  assert_snapshot!("record_basic_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("record_basic_hir", common::format_hir(&result));
}

#[test]
fn record_field_access() {
  let result = common::analyze(
    r#"
record Point {
    public x: i32;
    public y: i32;
}

function main(): i32 {
    let p: Point = Point { x: 10, y: 20 };
    return p.x + p.y;
}
"#,
  );

  assert_snapshot!(
    "record_field_access_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("record_field_access_hir", common::format_hir(&result));
}

#[test]
fn record_instance_method() {
  let result = common::analyze(
    r#"
record Counter {
    value: i32;

    public get(): i32 {
        return self.value;
    }
}

function main(): i32 {
    let c: Counter = Counter { value: 42 };
    return c.get();
}
"#,
  );

  assert_snapshot!(
    "record_instance_method_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("record_instance_method_hir", common::format_hir(&result));
}

#[test]
fn record_static_method() {
  let result = common::analyze(
    r#"
record Point {
    public x: i32;
    public y: i32;

    public static origin(): Point {
        return Point { x: 0, y: 0 };
    }
}

function main(): i32 {
    let p: Point = Point::origin();
    return p.x;
}
"#,
  );

  assert_snapshot!(
    "record_static_method_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("record_static_method_hir", common::format_hir(&result));
}

// ============================================================================
// Enum Tests
// ============================================================================

#[test]
fn enum_unit_variants() {
  let result = common::analyze(
    r#"
enum Color {
    Red,
    Green,
    Blue,
}

function main(): Color {
    return Color::Red;
}
"#,
  );

  assert_snapshot!(
    "enum_unit_variants_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("enum_unit_variants_hir", common::format_hir(&result));
}

#[test]
fn enum_with_payload() {
  let result = common::analyze(
    r#"
enum Option {
    Some(i32),
    None,
}

function main(): Option {
    return Option::Some(42);
}
"#,
  );

  assert_snapshot!(
    "enum_with_payload_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("enum_with_payload_hir", common::format_hir(&result));
}

#[test]
fn extension_method_basic() {
  let result = common::analyze(
    r#"
@extension(i32)
function double(value: i32): i32 {
    return value * 2;
}

function main(): i32 {
    let x: i32 = 21;
    return x.double();
}
"#,
  );

  assert_snapshot!(
    "extension_method_basic_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("extension_method_basic_hir", common::format_hir(&result));
}

#[test]
fn extension_method_with_args() {
  let result = common::analyze(
    r#"
@extension(i32)
function add(value: i32, other: i32): i32 {
    return value + other;
}

function main(): i32 {
    let x: i32 = 10;
    return x.add(5);
}
"#,
  );

  assert_snapshot!(
    "extension_method_with_args_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("extension_method_with_args_hir", common::format_hir(&result));
}

#[test]
fn extension_method_str() {
  let result = common::analyze(
    r#"
@extension(str)
function isEmpty(value: str): boolean {
    return false;
}

function main(): void {
    let s: str = "hello";
    let result: boolean = s.isEmpty();
    return;
}
"#,
  );

  assert_snapshot!(
    "extension_method_str_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("extension_method_str_hir", common::format_hir(&result));
}

#[test]
fn extension_method_multiple_for_same_type() {
  let result = common::analyze(
    r#"
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

@extension(i32)
function isPositive(value: i32): boolean {
    return value > 0;
}

function main(): void {
    let x: i32 = 21;
    let d: i32 = x.doubled();
    let p: boolean = x.isPositive();
    return;
}
"#,
  );

  assert_snapshot!(
    "extension_method_multiple_for_same_type_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("extension_method_multiple_for_same_type_hir", common::format_hir(&result));
}

#[test]
fn extension_method_f64() {
  let result = common::analyze(
    r#"
@extension(f64)
function halved(value: f64): f64 {
    return value / 2.0;
}

function main(): void {
    let x: f64 = 10.0;
    let h: f64 = x.halved();
    return;
}
"#,
  );

  assert_snapshot!(
    "extension_method_f64_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("extension_method_f64_hir", common::format_hir(&result));
}

#[test]
fn extension_method_mut_on_mutable() {
  let result = common::analyze(
    r#"
@extension(i32, mut)
function increment(value: i32): i32 {
    return value + 1;
}

function main(): void {
    let mut x: i32 = 5;
    let y: i32 = x.increment();
    return;
}
"#,
  );

  assert_snapshot!(
    "extension_method_mut_on_mutable_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("extension_method_mut_on_mutable_hir", common::format_hir(&result));
}

// ============================================================================
// Trait Tests
// ============================================================================

#[test]
fn trait_basic_required_method() {
  let result = common::analyze(
    r#"
trait Greetable {
    greet(&self): i32;
}

@implements(Greetable)
record Person {
    public age: i32;

    greet(&self): i32 {
        return self.age;
    }
}

function main(): void {
    let p: Person = Person { age: 30 };
    let _x: i32 = p.greet();
    return;
}
"#,
  );

  assert_snapshot!(
    "trait_basic_required_method_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("trait_basic_required_method_hir", common::format_hir(&result));
}

#[test]
fn trait_default_method() {
  let result = common::analyze(
    r#"
trait Describable {
    describe(&self): i32 {
        return 0;
    }
}

@implements(Describable)
record Item {
    public value: i32;
}

function main(): void {
    let item: Item = Item { value: 5 };
    let _x: i32 = item.describe();
    return;
}
"#,
  );

  assert_snapshot!(
    "trait_default_method_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("trait_default_method_hir", common::format_hir(&result));
}

#[test]
fn trait_default_method_override() {
  let result = common::analyze(
    r#"
trait Describable {
    describe(&self): i32 {
        return 0;
    }
}

@implements(Describable)
record Item {
    public value: i32;

    describe(&self): i32 {
        return self.value;
    }
}

function main(): void {
    let item: Item = Item { value: 42 };
    let _x: i32 = item.describe();
    return;
}
"#,
  );

  assert_snapshot!(
    "trait_default_method_override_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("trait_default_method_override_hir", common::format_hir(&result));
}

#[test]
fn trait_multiple_implements() {
  let result = common::analyze(
    r#"
trait Greetable {
    greet(&self): i32;
}

trait Describable {
    describe(&self): i32;
}

@implements(Greetable)
@implements(Describable)
record Person {
    public age: i32;

    greet(&self): i32 {
        return self.age;
    }

    describe(&self): i32 {
        return self.age + 1;
    }
}

function main(): void {
    let p: Person = Person { age: 25 };
    let _a: i32 = p.greet();
    let _b: i32 = p.describe();
    return;
}
"#,
  );

  assert_snapshot!(
    "trait_multiple_implements_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("trait_multiple_implements_hir", common::format_hir(&result));
}

#[test]
fn trait_in_namespace() {
  let result = common::analyze(
    r#"
namespace Graphics {
    trait Renderable {
        render(&self): i32;
    }
}

function main(): void {
    return;
}
"#,
  );

  assert_snapshot!(
    "trait_in_namespace_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("trait_in_namespace_hir", common::format_hir(&result));
}

#[test]
fn generic_bounds_allow_trait_method_lookup() {
  let result = common::analyze(
    r#"
trait Hash {
    hash(&self): i32;
}

@implements(Hash)
record Key {
    public value: i32;

    hash(&self): i32 {
        return self.value;
    }
}

function hashValue<T: Hash>(value: T): i32 {
    return value.hash();
}

function main(): i32 {
    let key: Key = Key { value: 42 };
    return hashValue<Key>(key);
}
"#,
  );

  assert_snapshot!(
    "generic_bounds_allow_trait_method_lookup_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("generic_bounds_allow_trait_method_lookup_hir", common::format_hir(&result));
}

// ── Type Inference ──────────────────────────────────────────

#[test]
fn infer_from_initializer() {
  let result = common::analyze(
    r#"
function main(): i32 {
    let x = 42;
    return x;
}
"#,
  );

  assert_snapshot!(
    "infer_from_initializer_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("infer_from_initializer_hir", common::format_hir(&result));
}

#[test]
fn infer_deferred_from_assignment() {
  let result = common::analyze(
    r#"
function main(): i32 {
    let mut x;
    x = 10;
    return x;
}
"#,
  );

  assert_snapshot!(
    "infer_deferred_from_assignment_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("infer_deferred_from_assignment_hir", common::format_hir(&result));
}

#[test]
fn infer_multiple_variables() {
  let result = common::analyze(
    r#"
function main(): i32 {
    let a = 10;
    let b = 20;
    let c = a + b;
    return c;
}
"#,
  );

  assert_snapshot!(
    "infer_multiple_variables_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("infer_multiple_variables_hir", common::format_hir(&result));
}

// ============================================================================
// Literal Coercion Tests
// ============================================================================

#[test]
fn match_u8_with_literal() {
  let result = common::analyze(
    r#"
function main(): void {
    let ch: u8 = 40;
    match (ch) {
        40 -> {},
        _ -> {}
    };
    return;
}
"#,
  );

  assert_snapshot!(
    "match_u8_with_literal_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("match_u8_with_literal_hir", common::format_hir(&result));
}

#[test]
fn compare_u8_with_literal() {
  let result = common::analyze(
    r#"
function main(): void {
    let ch: u8 = 40;
    if (ch == 40) {
        return;
    }
    return;
}
"#,
  );

  assert_snapshot!(
    "compare_u8_with_literal_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("compare_u8_with_literal_hir", common::format_hir(&result));
}

#[test]
fn widening_implicit_u8_to_u32() {
  let result = common::analyze(
    r#"
function main(): void {
    let a: u8 = 10;
    let b: u32 = a;  // Widening u8 -> u32
    return;
}
"#,
  );

  assert_snapshot!(
    "widening_implicit_u8_to_u32_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("widening_implicit_u8_to_u32_hir", common::format_hir(&result));
}

#[test]
fn widening_implicit_i8_to_i64() {
  let result = common::analyze(
    r#"
function main(): void {
    let a: i8 = 10;
    let b: i64 = a;  // Widening i8 -> i64
    return;
}
"#,
  );

  assert_snapshot!(
    "widening_implicit_i8_to_i64_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("widening_implicit_i8_to_i64_hir", common::format_hir(&result));
}

#[test]
fn arithmetic_u8_plus_u32() {
  let result = common::analyze(
    r#"
function main(): u32 {
    let a: u8 = 10;
    let b: u32 = 20;
    let c: u32 = a + b;  // Should widen u8 to u32
    return c;
}
"#,
  );

  assert_snapshot!(
    "arithmetic_u8_plus_u32_diags",
    common::format_diagnostics(&result.output.diagnostics)
  );
  assert_snapshot!("arithmetic_u8_plus_u32_hir", common::format_hir(&result));
}
