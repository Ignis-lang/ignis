---
title: Installation
description: Install the Ignis compiler from a release archive or build it from source.
section: getting-started
order: 1
status: stable
---

Ignis compiles to C and links native executables, so the toolchain it needs is the one your
system already uses to build C programs.

## Requirements

- Linux on amd64. Release archives are published for that target only.
- GCC.
- `ar`, from binutils.
- `make`.

## Install a release

The installer script resolves the latest release, downloads the archive for your architecture and
places the `ignis` binary along with the standard library.

```bash
curl -fsSL https://raw.githubusercontent.com/Ignis-lang/ignis/main/scripts/install.sh | sudo bash
```

Installing into `/usr/local` needs `sudo`. To install without it, choose a prefix inside your home
directory and make sure it is on your `PATH`:

```bash
curl -fsSL https://raw.githubusercontent.com/Ignis-lang/ignis/main/scripts/install.sh | bash -s -- --prefix ~/.local
```

## Build from source

The compiler is a Cargo workspace. The installer can clone and build it for you:

```bash
curl -fsSL https://raw.githubusercontent.com/Ignis-lang/ignis/main/scripts/install.sh | bash -s -- --build
```

Or do it by hand, which is what you want if you plan to work on the compiler itself:

```bash
git clone https://github.com/Ignis-lang/ignis.git
cd ignis
cargo build --release -p ignis
./target/release/ignis --help
```

## Verify

```bash
ignis --help
```

If the command is not found after a prefix install, the prefix's `bin` directory is missing from
your `PATH`. Add it and open a new shell.
