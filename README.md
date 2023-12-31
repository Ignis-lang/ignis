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
- Lua (for running transpiled scripts)

```bash
git clone https://github.com/BasileusErwin/ignis-lang.git
cd ignis-lang
cargo install --path . ignisc
```

## Usage

> [!IMPORTANT]
> The only backend that is currently running is the Lua backend.

```bash
# Transpile an Ignis file to Lua and execute it
ignisc -b lua build main.ign

lua ./build/main.lua
```

## Example Code

> [!Note]
> The Typescript code block is used, because as the syntax is the same as TS, we take advantage of
> it to have syntax highlighting 

```Typescript
import { toString } from "std:string";
import { println } from "std:io";

function printFactorial(num: int, fact: int): void {
  println("The factorial of " + toString(num) + " is: " + toString(fact));
}

function main(): void {
  let x: int = 10;
  let y: int = 20;

  let num: int = 5;
  let fact: int = factorial(num);

  printFactorial(num, fact);

  let z: int = x + y;

  if (isEven(z)) {
    println("z is even");
  } else {
    println("z is odd");
  }

  let mut count: int = 0;

  while (count < 5) {
    println(count);
    count = count + 1;
  }

  let result: int = sum(x, y);

  println("The sum is: " + toString(result));
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

