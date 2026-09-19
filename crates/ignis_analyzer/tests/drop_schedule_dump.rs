//! Pins the exact text of `--dump-drop-schedule`.
//!
//! The dump is a debugging aid for ownership bugs, so its value is in being stable enough
//! to diff: between two runs, between two compilers, and between two commits. These tests
//! therefore compare whole renderings, not fragments.

use std::cell::RefCell;
use std::rc::Rc;

use ignis_analyzer::{Analyzer, HirOwnershipChecker};
use ignis_hir::drop_schedule_dump::DropScheduleDumper;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::{file::SourceMap, symbol::SymbolTable};

fn dump(src: &str) -> String {
  let mut source_map = SourceMap::new();
  let file_id = source_map.add_file("test.ign", src.to_string());

  let mut lexer = IgnisLexer::new(file_id, source_map.get(&file_id).text.as_str());
  lexer.scan_tokens();
  assert!(lexer.diagnostics.is_empty(), "lexer errors: {:?}", lexer.diagnostics);

  let symbols = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
  let (nodes, roots) = parser.parse().expect("parse failed");

  let output = Analyzer::analyze(&nodes, &roots, symbols.clone());
  let symbols = symbols.borrow();

  let (schedules, _) = HirOwnershipChecker::new(&output.hir, &output.types, &output.defs, &symbols)
    .with_source_map(&source_map)
    .check();

  DropScheduleDumper::new(&output.hir, &output.defs, &symbols, &schedules)
    .with_source_map(&source_map)
    .render()
}

/// The raw `on_overwrite` lists, read straight off the schedule rather than through the
/// dump, so the pin fails on a second entry even if the renderer changes.
fn overwrite_schedule(src: &str) -> Vec<Vec<String>> {
  let mut source_map = SourceMap::new();
  let file_id = source_map.add_file("test.ign", src.to_string());

  let mut lexer = IgnisLexer::new(file_id, source_map.get(&file_id).text.as_str());
  lexer.scan_tokens();
  assert!(lexer.diagnostics.is_empty(), "lexer errors: {:?}", lexer.diagnostics);

  let symbols = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
  let (nodes, roots) = parser.parse().expect("parse failed");

  let output = Analyzer::analyze(&nodes, &roots, symbols.clone());
  let symbols = symbols.borrow();

  let (schedules, _) = HirOwnershipChecker::new(&output.hir, &output.types, &output.defs, &symbols)
    .with_source_map(&source_map)
    .check();

  let mut entries: Vec<Vec<String>> = schedules
    .on_overwrite
    .values()
    .map(|defs| {
      defs
        .iter()
        .map(|def_id| symbols.get(&output.defs.get(def_id).name).to_string())
        .collect()
    })
    .collect();

  entries.sort();
  entries
}

#[test]
fn an_overwrite_inside_a_loop_is_scheduled_once() {
  // `check_loop` walks the body a second time to simulate another iteration. One
  // assignment drops one value, so the second walk must not add a second drop —
  // for a closure-environment owner that was a double free.
  assert_eq!(
    overwrite_schedule(
      "function make(limit: i32): (i32) -> i32 {
  let base: i32 = 40;
  let mut f = (x: i32): i32 -> x + base;
  let mut i: i32 = 0;
  while (i < limit) {
    f = (x: i32): i32 -> x + base;
    i += 1;
  }
  return f;
}
"
    ),
    vec![vec!["f".to_string()]]
  );
}

const RESOURCE: &str = r#"@implements(Drop)
record Resource {
  public value: i32;

  drop(&mut self): void {
    return;
  }
}
"#;

#[test]
fn match_over_a_temporary_with_an_early_return_in_an_arm() {
  let source = format!(
    "{RESOURCE}
enum Slot {{
  Filled(Resource),
  Empty,
}}

function make(): Slot {{
  return Slot::Filled(Resource {{ value: 1 }});
}}

function take(): i32 {{
  match (make()) {{
    Slot::Filled(inner) -> {{
      return inner.value;
    }},
    Slot::Empty -> {{
      return 0;
    }},
  }};

  return -1;
}}
"
  );

  // The arm binding is dropped both at the end of the arm and on the arm's early
  // return: two sites for one binding, which is what the dump makes readable.
  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function make at test.ign:15:23
  <no owned values>
function take at test.ign:19:22
  value inner kind=binding declared at test.ign:21:28
    drop at test.ign:21:28 reason=arm-end
    drop at test.ign:22:7 reason=return
"
  );
}

#[test]
fn a_short_circuited_let_condition_lists_the_skipped_move_drop() {
  let source = format!(
    "{RESOURCE}
enum Slot {{
  Filled(Resource),
  Empty,
}}

function take(flag: boolean): i32 {{
  let mut seen: i32 = 0;
  let slot: Slot = Slot::Filled(Resource {{ value: 1 }});

  if (flag && let Slot::Filled(inner) = slot) {{
    seen = inner.value;
  }}

  return seen;
}}
"
  );

  // `slot` is moved by the `let`, but only on the path where `flag` was true. The other
  // path skips the `let` altogether and is the only site left that can free it.
  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function take at test.ign:15:35
  value slot kind=local declared at test.ign:17:3
    drop at test.ign:19:7 reason=condition-skip
    moved at test.ign:19:15
  value inner kind=binding declared at test.ign:19:15
    drop at test.ign:19:47 reason=scope-end
"
  );
}

#[test]
fn chained_let_conditions_list_the_short_circuit_drop() {
  let source = format!(
    "{RESOURCE}
enum Slot {{
  Filled(Resource),
  Empty,
}}

function make(): Slot {{
  return Slot::Filled(Resource {{ value: 1 }});
}}

function take(): i32 {{
  let mut seen: i32 = 0;

  if (let Slot::Filled(first) = make() && let Slot::Filled(second) = make()) {{
    seen = first.value + second.value;
  }}

  return seen;
}}
"
  );

  // `first` is bound before the second `let` runs, so a failure there has to free it:
  // the guarded branch that would otherwise own it never runs. `second` is bound last,
  // so nothing can fail after it and it only ever falls out of the branch.
  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function make at test.ign:15:23
  <no owned values>
function take at test.ign:19:22
  value first kind=binding declared at test.ign:22:7
    drop at test.ign:22:7 reason=condition-fail
    drop at test.ign:22:78 reason=scope-end
  value second kind=binding declared at test.ign:22:43
    drop at test.ign:22:78 reason=scope-end
"
  );
}

#[test]
fn a_break_lists_the_drop_its_loop_condition_will_not_reach() {
  let source = format!(
    "{RESOURCE}
enum Slot {{
  Filled(Resource),
  Empty,
}}

function make(): Slot {{
  return Slot::Filled(Resource {{ value: 1 }});
}}

function drain(): i32 {{
  let mut seen: i32 = 0;
  let mut slot: Slot = make();

  while (let Slot::Filled(inner) = slot) {{
    seen += inner.value;
    slot = make();

    if (seen >= 2) {{
      break;
    }}
  }}

  return seen;
}}
"
  );

  // The condition consumes `slot` on every round and frees whatever the last round put
  // back — on the round that ends the loop. A `break` never reaches that round, so it
  // owes the value the body just assigned, alongside the binding it leaves behind.
  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function make at test.ign:15:23
  <no owned values>
function drain at test.ign:19:23
  value slot kind=local declared at test.ign:21:3
    drop at test.ign:28:7 reason=break
    moved at test.ign:23:10
  value inner kind=binding declared at test.ign:23:10
    drop at test.ign:23:42 reason=scope-end
    drop at test.ign:28:7 reason=break
"
  );
}

#[test]
fn an_inner_break_leaves_the_outer_loop_condition_alone() {
  let source = format!(
    "{RESOURCE}
enum Slot {{
  Filled(Resource),
  Empty,
}}

function make(): Slot {{
  return Slot::Filled(Resource {{ value: 1 }});
}}

function drain(): i32 {{
  let mut seen: i32 = 0;
  let mut outer: Slot = make();

  while (let Slot::Filled(outerInner) = outer) {{
    seen += outerInner.value;
    outer = make();

    let mut inner: Slot = make();

    while (let Slot::Filled(innerInner) = inner) {{
      seen += innerInner.value;
      inner = make();
      break;
    }}
  }}

  return seen;
}}
"
  );

  // The inner `break` returns to the outer body, where `outer` is still live and still
  // the outer condition's to free, so only `inner` is listed against it.
  //
  // `inner` is listed as moved twice at the same site: the ownership walk goes through
  // the inner loop body a second time to simulate another iteration and records the same
  // move again. The dump prints what the schedule holds, so the repeat is visible here.
  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function make at test.ign:15:23
  <no owned values>
function drain at test.ign:19:23
  value outer kind=local declared at test.ign:21:3
    moved at test.ign:23:10
  value outerInner kind=binding declared at test.ign:23:10
    drop at test.ign:23:48 reason=scope-end
  value inner kind=local declared at test.ign:27:5
    drop at test.ign:32:7 reason=break
    moved at test.ign:29:12
    moved at test.ign:29:12
  value innerInner kind=binding declared at test.ign:29:12
    drop at test.ign:29:50 reason=scope-end
    drop at test.ign:32:7 reason=break
"
  );
}

#[test]
fn a_partially_moved_field_keeps_its_owner_scheduled() {
  let source = format!(
    "{RESOURCE}
record Pair {{
  public left: Resource;
  public right: i32;
}}

function consume(resource: Resource): i32 {{
  return resource.value;
}}

function main(): i32 {{
  let pair: Pair = Pair {{ left: Resource {{ value: 1 }}, right: 2 }};
  let moved: Resource = pair.left;
  return consume(moved) + pair.right;
}}
"
  );

  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function consume at test.ign:15:43
  value resource kind=parameter declared at test.ign:15:43
    drop at test.ign:15:43 reason=fn-end
    drop at test.ign:16:3 reason=return
function main at test.ign:19:22
  value pair kind=local declared at test.ign:20:3
    drop at test.ign:19:22 reason=scope-end
    drop at test.ign:22:3 reason=return
  value moved kind=local declared at test.ign:21:3
    moved at test.ign:22:10
"
  );
}

#[test]
fn a_match_arm_that_yields_an_owning_parameter_moves_it_and_pays_on_the_other_arms() {
  let source = format!(
    "{RESOURCE}
enum Slot {{
  Filled(Resource),
  Empty,
}}

function takeOr(slot: Slot, fallback: Resource): Resource {{
  return match (slot) {{
    Slot::Filled(inner) -> inner,
    Slot::Empty -> fallback,
  }};
}}
"
  );

  // `fallback` leaves through the `Empty` arm, so it is moved — but only on that path.
  // The `Filled` arm still owns it and is the last point where it is live and unowned,
  // which is why its single drop sits at that arm's end and not at function exit.
  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function takeOr at test.ign:15:59
  value fallback kind=parameter declared at test.ign:15:59
    drop at test.ign:17:28 reason=arm-end
    moved at test.ign:16:10
  value slot kind=parameter declared at test.ign:15:59
    moved at test.ign:16:10
  value inner kind=binding declared at test.ign:17:28
    moved at test.ign:16:10
"
  );
}

#[test]
fn a_match_nested_in_a_discarded_arm_hands_nothing_over() {
  let source = format!(
    "{RESOURCE}
enum Pick {{
  Left,
  Right,
}}

function drain(outer: Pick, inner: Pick, one: Resource, two: Resource, three: Resource): void {{
  match (outer) {{
    Pick::Left -> match (inner) {{
      Pick::Left -> one,
      Pick::Right -> two,
    }},
    Pick::Right -> three,
  }};

  return;
}}
"
  );

  // The enclosing match is in statement position, so its result goes nowhere — and the
  // nested match occupies that same result position. Neither hands a parameter over, so
  // all three keep the ordinary drop they owe at function exit. Reading the nested match
  // as used consumed `one` and `two` there, drained those moves into the enclosing
  // match, and left the taken one with no drop anywhere.
  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function drain at test.ign:15:95
  value one kind=parameter declared at test.ign:15:95
    drop at test.ign:15:95 reason=fn-end
    drop at test.ign:24:3 reason=return
  value three kind=parameter declared at test.ign:15:95
    drop at test.ign:15:95 reason=fn-end
    drop at test.ign:24:3 reason=return
  value two kind=parameter declared at test.ign:15:95
    drop at test.ign:15:95 reason=fn-end
    drop at test.ign:24:3 reason=return
"
  );
}

#[test]
fn a_binding_from_a_reference_scrutinee_owes_no_drop() {
  let source = format!(
    "{RESOURCE}
enum Slot {{
  Filled(Resource),
  Empty,
}}

function peek(slot: &Slot): i32 {{
  return match (slot) {{
    Slot::Filled(inner) -> inner.value,
    Slot::Empty -> 0,
  }};
}}
"
  );

  // The binding names a place inside the referent, which the caller still owns: nothing
  // here is owed a drop, and lowering reads the same decision to bind it by address.
  assert_eq!(
    dump(&source),
    "drop-schedule v1
function drop at test.ign:5:25
  <no owned values>
function peek at test.ign:15:33
  <no owned values>
"
  );
}

#[test]
fn a_program_without_owned_values_still_lists_every_function() {
  assert_eq!(
    dump("function main(): i32 {\n  return 0;\n}\n"),
    "drop-schedule v1
function main at test.ign:1:22
  <no owned values>
"
  );
}

#[test]
fn a_closure_received_from_a_call_is_owned_by_the_binding_that_takes_it() {
  // The environment of the closure `makeAdder` returns is heap-allocated, so the
  // binding that receives it is scheduled for a drop just like the closure
  // literal bound inside `makeAdder` would be.
  assert_eq!(
    dump(
      "function makeAdder(base: i32): (i32) -> i32 {
  return (x: i32): i32 -> x + base;
}

function main(): i32 {
  let add = makeAdder(40);
  return add(2);
}
"
    ),
    "drop-schedule v1
function makeAdder at test.ign:1:45
  <no owned values>
function __closure_drop_0 at test.ign:2:10
  <no owned values>
function __closure_thunk_0 at test.ign:2:10
  <no owned values>
function main at test.ign:5:22
  value add kind=local declared at test.ign:6:3
    drop at test.ign:5:22 reason=scope-end
    drop at test.ign:7:3 reason=return
"
  );
}

#[test]
fn returning_a_closure_local_cancels_its_drop() {
  // The environment goes to the caller, so `makeAdder` must not free it on the
  // way out: `add` is transferred, not dropped.
  assert_eq!(
    dump(
      "function makeAdder(base: i32): (i32) -> i32 {
  let add = (x: i32): i32 -> x + base;
  return add;
}

function main(): i32 {
  let f = makeAdder(40);
  return f(2);
}
"
    ),
    "drop-schedule v1
function makeAdder at test.ign:1:45
  <no owned values>
function __closure_drop_0 at test.ign:2:13
  <no owned values>
function __closure_thunk_0 at test.ign:2:13
  <no owned values>
function main at test.ign:6:22
  value f kind=local declared at test.ign:7:3
    drop at test.ign:6:22 reason=scope-end
    drop at test.ign:8:3 reason=return
"
  );
}
