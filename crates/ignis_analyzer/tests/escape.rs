mod common;

use ignis_diagnostics::diagnostic_report::Severity;
use ignis_hir::HIRKind;

/// The `escapes` flag of every closure in the program, in source order.
fn escape_flags(source: &str) -> Vec<bool> {
  let result = common::analyze(source);

  assert!(
    result.output.diagnostics.iter().all(|d| d.severity != Severity::Error),
    "analysis reported errors:\n{}",
    common::format_diagnostics(&result.output.diagnostics)
  );

  let mut closures: Vec<_> = result
    .output
    .hir
    .nodes
    .iter()
    .filter_map(|(_, node)| match &node.kind {
      HIRKind::Closure { escapes, .. } => Some((node.span.start, *escapes)),
      _ => None,
    })
    .collect();

  closures.sort_by_key(|(start, _)| *start);

  closures.into_iter().map(|(_, escapes)| escapes).collect()
}

#[test]
fn closure_returned_directly_escapes() {
  assert_eq!(
    escape_flags(
      r#"
function makeAdder(base: i32): (i32) -> i32 {
    return (x: i32): i32 -> x + base;
}
"#
    ),
    vec![true]
  );
}

#[test]
fn closure_returned_through_an_alias_escapes() {
  assert_eq!(
    escape_flags(
      r#"
function makeAdder(base: i32): (i32) -> i32 {
    let add = (x: i32): i32 -> x + base;
    let alias = add;

    return alias;
}
"#
    ),
    vec![true]
  );
}

#[test]
fn closure_in_a_returned_branch_escapes() {
  assert_eq!(
    escape_flags(
      r#"
function pick(flag: boolean, base: i32): (i32) -> i32 {
    if (flag) {
        return (x: i32): i32 -> x + base;
    }

    return (x: i32): i32 -> x - base;
}
"#
    ),
    vec![true, true]
  );
}

#[test]
fn closure_in_a_closure_tail_expression_escapes() {
  // The outer closure is only called locally, so it keeps a stack environment;
  // the inner one is the outer's result and leaves the outer's frame.
  assert_eq!(
    escape_flags(
      r#"
function makeAdder(base: i32): (i32) -> i32 {
    let make = (): (i32) -> i32 -> (x: i32): i32 -> x + base;

    return make();
}
"#
    ),
    vec![false, true]
  );
}

#[test]
fn closure_written_into_a_record_initializer_escapes() {
  assert_eq!(
    escape_flags(
      r#"
record Adder {
    public apply: (i32) -> i32;
}

function main(): i32 {
    let base: i32 = 40;
    let add = (x: i32): i32 -> x + base;
    let holder: Adder = Adder { apply: add };
    let apply = holder.apply;

    return apply(2);
}
"#
    ),
    vec![true]
  );
}

#[test]
fn a_closure_literal_written_as_a_bare_argument_keeps_its_stack_environment() {
  // Nothing on the other side of the call owns the environment, so heap-allocating
  // it would only leak it; the frame that built it outlives the call either way.
  assert_eq!(
    escape_flags(
      r#"
function apply(f: (i32) -> i32, x: i32): i32 {
    return f(x);
}

function main(): i32 {
    let base: i32 = 40;

    return apply((x: i32): i32 -> x + base, 2);
}
"#
    ),
    vec![false]
  );
}

#[test]
fn closure_written_into_a_vector_literal_escapes() {
  assert_eq!(
    escape_flags(
      r#"
type Adder = (i32) -> i32;

function main(): i32 {
    let base: i32 = 40;
    let add = (x: i32): i32 -> x + base;
    let sub = (x: i32): i32 -> x - base;
    let callbacks: Adder[2] = [add, sub];
    let first = callbacks[0];

    return first(2);
}
"#
    ),
    vec![true, true]
  );
}

#[test]
fn closure_passed_to_a_plain_parameter_escapes() {
  assert_eq!(
    escape_flags(
      r#"
function apply(f: (i32) -> i32, x: i32): i32 {
    return f(x);
}

function main(): i32 {
    let base: i32 = 40;
    let add = (x: i32): i32 -> x + base;

    return apply(add, 2);
}
"#
    ),
    vec![true]
  );
}

#[test]
fn closure_passed_to_a_noescape_parameter_keeps_its_stack_environment() {
  assert_eq!(
    escape_flags(
      r#"
function apply(@noescape f: (i32) -> i32, x: i32): i32 {
    return f(x);
}

function main(): i32 {
    let base: i32 = 40;
    let add = (x: i32): i32 -> x + base;

    return apply(add, 2);
}
"#
    ),
    vec![false]
  );
}

#[test]
fn closure_only_called_locally_keeps_its_stack_environment() {
  assert_eq!(
    escape_flags(
      r#"
function main(): i32 {
    let base: i32 = 40;
    let add = (x: i32): i32 -> x + base;

    return add(2);
}
"#
    ),
    vec![false]
  );
}

#[test]
fn closure_captured_by_an_escaping_closure_escapes() {
  assert_eq!(
    escape_flags(
      r#"
function makeChain(base: i32): (i32) -> i32 {
    let inner = (x: i32): i32 -> x + base;

    return (x: i32): i32 -> inner(x) + 1;
}
"#
    ),
    vec![true, true]
  );
}
