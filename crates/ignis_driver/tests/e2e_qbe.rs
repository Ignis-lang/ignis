mod common;

use common::{compile_to_qbe, compile_and_run_qbe};

fn qbe_available() -> bool {
  std::process::Command::new("qbe")
    .arg("-h")
    .output()
    .is_ok()
}

#[test]
fn qbe_emit_return_literal() {
  let qbe = compile_to_qbe(
    r#"
    function main(): i32 {
      return 42;
    }
    "#,
  )
  .unwrap();

  assert!(qbe.contains("export function"), "should have exported function");
  assert!(qbe.contains("ret 42") || qbe.contains("ret "), "should have return");
}

#[test]
fn qbe_emit_add() {
  let qbe = compile_to_qbe(
    r#"
    function add(a: i32, b: i32): i32 {
      return a + b;
    }

    function main(): i32 {
      return add(1, 2);
    }
    "#,
  )
  .unwrap();

  assert!(qbe.contains("add "), "should contain add instruction");
  assert!(qbe.contains("call $"), "should contain function call");
}

#[test]
fn qbe_emit_if_else() {
  let qbe = compile_to_qbe(
    r#"
    function main(): i32 {
      let x: i32 = 10;
      if (x > 5) {
        return 1;
      } else {
        return 0;
      }
    }
    "#,
  )
  .unwrap();

  assert!(qbe.contains("jnz "), "should contain conditional branch");
  assert!(qbe.contains("ret "), "should contain return");
}

#[test]
fn qbe_emit_while_loop() {
  let qbe = compile_to_qbe(
    r#"
    function main(): i32 {
      let mut i: i32 = 0;
      let mut sum: i32 = 0;
      while (i < 10) {
        sum = sum + i;
        i = i + 1;
      }
      return sum;
    }
    "#,
  )
  .unwrap();

  assert!(qbe.contains("jnz "), "should contain conditional branch for while");
  assert!(qbe.contains("jmp @"), "should contain unconditional jump for loop back-edge");
}

#[test]
fn qbe_emit_nested_calls() {
  let qbe = compile_to_qbe(
    r#"
    function double(x: i32): i32 {
      return x * 2;
    }

    function add_doubled(a: i32, b: i32): i32 {
      return double(a) + double(b);
    }

    function main(): i32 {
      return add_doubled(3, 4);
    }
    "#,
  )
  .unwrap();

  assert!(qbe.contains("mul "), "should contain multiply");
  assert!(qbe.contains("call $"), "should contain calls");
}

#[test]
fn qbe_emit_boolean_ops() {
  let qbe = compile_to_qbe(
    r#"
    function main(): i32 {
      let a: boolean = true;
      let b: boolean = false;
      if (a && !b) {
        return 1;
      }
      return 0;
    }
    "#,
  )
  .unwrap();

  assert!(qbe.contains("ceq"), "should contain comparison for boolean not");
}

#[test]
#[ignore = "requires qbe binary"]
fn qbe_e2e_return_literal() {
  if !qbe_available() {
    return;
  }

  let result = compile_and_run_qbe(
    r#"
    function main(): i32 {
      return 42;
    }
    "#,
  )
  .unwrap();

  assert_eq!(result.exit_code, 42);
}

#[test]
#[ignore = "requires qbe binary"]
fn qbe_e2e_arithmetic() {
  if !qbe_available() {
    return;
  }

  let result = compile_and_run_qbe(
    r#"
    function add(a: i32, b: i32): i32 {
      return a + b;
    }

    function main(): i32 {
      return add(17, 25);
    }
    "#,
  )
  .unwrap();

  assert_eq!(result.exit_code, 42);
}

#[test]
#[ignore = "requires qbe binary"]
fn qbe_e2e_if_else() {
  if !qbe_available() {
    return;
  }

  let result = compile_and_run_qbe(
    r#"
    function main(): i32 {
      let x: i32 = 10;
      if (x > 5) {
        return 1;
      } else {
        return 2;
      }
    }
    "#,
  )
  .unwrap();

  assert_eq!(result.exit_code, 1);
}

#[test]
#[ignore = "requires qbe binary"]
fn qbe_e2e_while_loop() {
  if !qbe_available() {
    return;
  }

  let result = compile_and_run_qbe(
    r#"
    function main(): i32 {
      let mut i: i32 = 0;
      let mut sum: i32 = 0;
      while (i < 10) {
        sum = sum + i;
        i = i + 1;
      }
      return sum;
    }
    "#,
  )
  .unwrap();

  assert_eq!(result.exit_code, 45);
}

#[test]
fn qbe_emit_record_with_drop() {
  let qbe = compile_to_qbe(
    r#"
@implements(Drop)
record Resource {
  public value: i32;

  drop(&mut self): void {
    return;
  }
}

function main(): i32 {
  let r: Resource = Resource { value: 42 };
  return r.value;
}
    "#,
  )
  .unwrap();

  assert!(qbe.contains("call $"), "should contain function calls");
  assert!(qbe.contains("ret "), "should contain return");
}

#[test]
fn qbe_emit_closure_capture() {
  let qbe = compile_to_qbe(
    r#"
function apply(f: (i32) -> i32, x: i32): i32 {
    return f(x);
}

function main(): i32 {
    let offset: i32 = 10;
    let adder = (n: i32): i32 -> n + offset;
    return apply(adder, 32);
}
    "#,
  )
  .unwrap();

  assert!(qbe.contains("alloc8"), "should allocate closure env or locals");
}

#[test]
#[ignore = "requires qbe binary"]
fn qbe_e2e_record_field_access() {
  if !qbe_available() {
    return;
  }

  let result = compile_and_run_qbe(
    r#"
record Point {
  public x: i32;
  public y: i32;
}

function sum(p: *Point): i32 {
  return p.x + p.y;
}

function main(): i32 {
  let p: Point = Point { x: 17, y: 25 };
  return sum(&p);
}
    "#,
  )
  .unwrap();

  assert_eq!(result.exit_code, 42);
}

#[test]
#[ignore = "requires qbe binary"]
fn qbe_e2e_nested_calls_result() {
  if !qbe_available() {
    return;
  }

  let result = compile_and_run_qbe(
    r#"
    function double(x: i32): i32 {
      return x * 2;
    }

    function add_doubled(a: i32, b: i32): i32 {
      return double(a) + double(b);
    }

    function main(): i32 {
      return add_doubled(3, 4);
    }
    "#,
  )
  .unwrap();

  assert_eq!(result.exit_code, 14);
}
