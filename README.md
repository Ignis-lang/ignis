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

```bash
cargo install --git https://gitlab.com/Ignis/ignis.git --branch dev
# or
git clone https://gitlab.com/Ignis/ignis.git
cd ignis
cargo install --path . ignis
# Or
cargo build --release
```

## Usage

> [!IMPORTANT]
> The only backend that is currently running is the Lua backend.

```bash
ignis build main.ign
```

## Contributions

Ignis lang is an open project for contributions. If you're interested in collaborating, you can:

- Report bugs or issues.
- Propose new features.
- Submit pull requests with improvements or new features.
