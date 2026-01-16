# Ignis Lang

> [!CAUTION]
> Ignis is under active development and evolving. It is currently in its early stages
> and is an experimental language. APIs and syntax may change between versions.

Ignis is a general-purpose programming language with strong, static typing, and immutability by default.
Inspired by TypeScript and Rust, Ignis compiles to C and produces native executables via GCC.

## Features (v0.2)

- **Strong static typing**: `i8`-`i64`, `u8`-`u64`, `f32`, `f64`, `boolean`, `char`, `string`
- **Immutability by default**: `let` for immutable, `let mut` for mutable
- **Generics**: Type parameters for functions, records, enums, and type aliases
- **Records and enums**: User-defined types with fields, methods, and variants
- **Namespaces**: Module-level organization with `::` access
- **References and pointers**: `&T`, `&mut T`, `*T`
- **Control flow**: `if`/`else`, `while`, `for`, `for-of`, `break`, `continue`
- **Function overloading**: Multiple functions with the same name, different signatures
- **Modules**: `import`/`export` for multi-file projects
- **FFI**: `extern` blocks for C interop
- **Borrow checking**: Rust-style borrow analysis

See [docs/LANGUAGE_REFERENCE_v0.2.md](docs/LANGUAGE_REFERENCE_v0.2.md) for full language documentation.

## Installation

### Requirements

- Rust/Cargo 1.74+
- GCC (for compiling generated C code)

```bash
# From source
git clone https://github.com/ignis-lang/ignis.git
cd ignis
cargo install --path crates/ignis

# Or build without installing
cargo build --release
```

## Quick Start

Create a file `hello.ign`:

```ignis
import Io from "std::io";

function main(): void {
    Io::println("Hello, Ignis!");
    return;
}
```

Build and run:

```bash
# Build the standard library (first time only)
ignis build-std

# Compile and run
ignis build hello.ign
./build/hello
```

## Usage

```bash
# Compile a single file
ignis build main.ign

# Compile a project (uses ignis.toml)
ignis build

# Build the standard library
ignis build-std

# Additional options
ignis build main.ign --emit-c out.c          # Output generated C code
ignis build main.ign --dump hir              # Dump HIR for debugging
ignis build main.ign --dump hir --dump-hir main  # Dump HIR for a specific function
ignis build main.ign --debug --debug-trace analyzer
ignis build main.ign --debug                 # Enables -vv logs + debug traces
ignis build main.ign -O                      # Enable optimizations
```

## Example

```ignis
import Io from "std::io";
import String from "std::string";

record Box<T> {
    value: T;
}

function identity<T>(x: T): T {
    return x;
}

function main(): void {
    let box: Box<i32> = Box { value: 42 };
    let result: i32 = identity<i32>(box.value);
    
    Io::println(String::toString(result));
    
    return;
}
```

## Project Structure

For multi-file projects, create an `ignis.toml`:

```toml
name = "myproject"
version = "0.1.0"

[build]
source_dir = "src"
output_dir = "build"
```

## Contributions

Ignis is open for contributions. You can:

- Report bugs or issues
- Propose new features
- Submit pull requests with improvements
