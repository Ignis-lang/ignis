# Ignis Lang

> [!CAUTION]
> Ignis is under active development and evolving. It is currently in its early stages
> and is an experimental language. APIs and syntax may change between versions.

Ignis is a general-purpose programming language with strong, static typing, and immutability by default.
Inspired by TypeScript and Rust, Ignis compiles to C and produces native executables via GCC.

## Features (v0.1)

- **Strong static typing**: `i8`-`i64`, `u8`-`u64`, `f32`, `f64`, `boolean`, `char`, `string`
- **Immutability by default**: `let` for immutable, `let mut` for mutable
- **References and pointers**: `&T`, `&mut T`, `*T`
- **Control flow**: `if`/`else`, `while`, `for`, `break`, `continue`
- **Functions**: with type annotations and return types
- **Modules**: `import`/`export` for multi-file projects
- **FFI**: `extern` declarations for C interop
- **Borrow checking**: Basic Rust-style borrow analysis

See [docs/LANGUAGE_REFERENCE_v0.1.md](docs/LANGUAGE_REFERENCE_v0.1.md) for full language documentation.

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
import println from "std::io";

function main(): void {
    println("Hello, Ignis!");
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
ignis build main.ign --emit-c out.c    # Output generated C code
ignis build main.ign --dump-hir main   # Dump HIR for debugging
ignis build main.ign -O                # Enable optimizations
```

## Example

```ignis
import println from "std::io";

function factorial(n: i32): i32 {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}

function main(): void {
    let result: i32 = factorial(5);
    println("5! = 120");
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
