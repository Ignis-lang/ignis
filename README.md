# Ignis Lang

> [!CAUTION]
> The outlook for Ignis lang may change as the language is under development and evolving;
> it is currently in its early stages and, for now, is an experimental language.

Ignis is a general-purpose programming language with strong, static typing, and immutability by default for variables. 
The languages that inspired Ignis are TypeScript and Rust.
The goal of Ignis is not to be used by everyone but to be a language that allows me to learn how programming languages
are created and work. And, if in the process I can use it to create scripts that replace code written in Bash or other 
languages, especially Lua and Python, that would be ideal.

## Installation

### Requirements

- Rust/Cargo version 1.74 or higher
- Lua >= 5.2.4 (for running transpiled scripts) 

```bash
cargo install --git https://github.com/Ignis/ignis.git --branch dev
# or
git clone https://github.com/Ignis/ignis.git
cd ignis
cargo install --path . ignis
# Or
cargo build --release
```

## Usage

> [!IMPORTANT]
> The only backend that is currently running is the Lua backend.

```bash
# Transpile an Ignis file to Lua and execute it
ignis build main.ign

lua ./build/main.lua
```

## Example Code

> [!Note]
> The Typescript code block is used, because as the syntax is the same as TS, we take advantage of
> it to have syntax highlighting 

```ignis
import { println } from "std:io";

function sum(nums: i64[]): i64 {
  let mut total: i64 = 0;

  for (let i = 0; i < nums.length; i++) {
    total = total + nums[i];
  }

  return total;
}

function main(): void {
  let array: i64[] = [1, 2, 3, 4, 5];

  println("Lenght: " + array.length.toString());

  println(sum(array));
}
```

## Roadmap

> [!NOTE]
> A brief roadmap

- [ ] Development of a basic standard library (File/IO, String tools, Array tools).
- [ ] NeoVim API to allow writing configurations and plugins in Ignis, with direct transpilation to Lua.
- [ ] Binary compilation using C and GCC as a backend (in development, basic structure present).
- [ ] Binary compilation using LLVM (planned).
- [ ] Implementation of a memory management system inspired by Rust, seeking a balance between safety and flexibility.
- [ ] Compilation to bytecode and development of the IVM (Ignis Virtual Machine).

## Contributions

Ignis lang is an open project for contributions. If you're interested in collaborating, you can:

- Report bugs or issues.
- Propose new features.
- Submit pull requests with improvements or new features.

