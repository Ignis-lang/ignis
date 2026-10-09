---
title: Inline assembly
description: The asm statement and expression, their operands, clobbers, and the checks the compiler runs on them.
section: language
order: 16
status: stable
---

Ignis ships inline assembly for x86-64. An `asm` at the start of a statement is a statement;
anywhere else it is an expression whose value is its output. Bodies are Intel syntax, and a
translation unit that contains any `asm` is compiled with `-masm=intel`.

```ignis
asm () -> (low: u32 in eax, high: u32 in edx) { rdtsc }

let cycles: u64 = ((high as u64) << 32) | (low as u64);
```

A statement `asm` binds each output as a local for the rest of the block. An `asm` in
expression position has at most one output; without an output, its type is `void`.
When an output is present, that output is the value:

```ignis
function double(value: u64): u64 {
    return asm pure (value in reg) -> (result: u64 in reg) { lea {result}, [{value} + {value}] };
}
```

## Operands

Inputs are written `expression in location` or `expression inout location`; outputs are
`name: type in location` inside a `-> (...)` list. An `inout` operand is read by the
instructions and written back to the local it names.

```ignis
function addThenMultiply(value: u64, addend: u64, factor: u64): u64 {
    let mut result: u64 = value;

    asm (result inout rax, addend in reg, factor in reg) clobber(flags) {
        add {result}, {addend}
        imul {result}, {factor}
    }

    return result;
}
```

A location is either a named register — any width of the general-purpose x86-64 registers,
such as `rax`, `eax` or `r8b` — or one of the classes `reg` (the compiler picks a register),
`mem` (a memory operand, typically a dereferenced pointer) and `imm` (an immediate). A
clobber list names registers the instructions overwrite, `memory`, or `flags`.

```ignis
let mut value: u64 = 40;
let pointer: *mut u64 = (&mut value) as *mut u64;

asm (*pointer inout mem) clobber(flags) {
    add qword ptr {pointer}, 2
}
```

`pure` declares that the asm has no side effects besides its outputs; the optimizer may
drop it when the outputs are unused. `pure` without outputs is an error.

## Holes

`{name}` in the body refers to the operand with that name. `{{` and `}}` are literal braces,
and comments inside the body stay in the text.

## Checks

The analyzer validates each `asm` rather than passing it through:

- A location or clobber must name a register asm can use or a valid class; the stack and
  frame pointers are reserved (`A0220`).
- Only integers and raw pointers go in registers, and only integers in `imm` (`A0221`); the
  operand type must be as wide as the register it names (`A0222`).
- Every `{name}` hole must name exactly one operand (`A0223`).
- Two inputs or two outputs cannot share a register; an input and an output may share one.
  A clobber cannot take an operand's register or be written twice (`A0224`).
- An `imm` input must be a constant, a `mem` input must be a place, and an `inout` input
  must be a place the asm may write (`A0225`).
- `pure` requires outputs, and an expression `asm` takes at most one output (`A0226`).

C compilers accept at most 30 operands per `asm`.

## Limits

The grammar only carries the header and the body text; the compiler checks names and
placements, but it does not verify the instructions themselves. A body that is valid x86-64
but wrong for its operands assembles and links into a program that misbehaves at run time.
