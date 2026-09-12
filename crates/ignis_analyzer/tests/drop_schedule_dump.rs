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

  // The arm binding is dropped at the end of the arm, and the `return` inside the arm
  // schedules nothing of its own: that asymmetry is exactly what the dump is for.
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
fn a_program_without_owned_values_still_lists_every_function() {
  assert_eq!(
    dump("function main(): i32 {\n  return 0;\n}\n"),
    "drop-schedule v1
function main at test.ign:1:22
  <no owned values>
"
  );
}
