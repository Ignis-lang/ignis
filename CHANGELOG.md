# Changelog

All notable changes to the Ignis compiler will be documented in this file.

## [0.2.2] - 2026-01-18

This release focuses on LSP server stability and crash prevention.

### Bug Fixes

- **Panic Recovery**: Added global panic hook and `catch_unwind` wrappers around analysis and completion to prevent panics from killing the LSP server
- **Invalid ModuleId Crash**: Fixed crash when a file fails to parse by cleaning up placeholder ModuleId entries from the lookup table
- **UTF-8 Boundary Panics**: Fixed panics when cursor offset lands in the middle of a multi-byte UTF-8 character by clamping to valid boundaries
- **Empty Std Module Name**: Added validation to reject `"std::"` imports without a module name
- **Duplicate Import Prefix**: Fixed import completion inserting `std::io` when user already typed `std::` (now correctly inserts just `io`)

### Improvements

- **Fallback Import Detection**: Added raw text-based import path detection for unclosed quotes where the lexer fails to produce a String token
- **Dynamic Std Modules**: Import path completion now reads available modules from the project manifest instead of a hardcoded list
- **Version Mismatch Guard**: Completion now checks document version to avoid race conditions between `did_change` and completion requests
- Cleaned up verbose debug logging throughout the completion system

## [0.2.1] - 2026-01-18

This release brings significant improvements to the Language Server Protocol (LSP) support and adds new language features for visibility control and mutable pointers.

### Language Features

#### Visibility Control

Fields and methods in records are now **private by default**. Use the `public` keyword to make them accessible from outside the record:

```ignis
record Person {
    public name: string;   // Accessible from anywhere
    age: i32;              // Private - only accessible within Person methods
    
    public getName(): string {
        return self.name;
    }
    
    getAge(): i32 {        // Private method
        return self.age;
    }
}
```

Private members can be accessed within the record's own methods via `self`.

#### Mutable Pointers

Added explicit mutable pointer syntax `*mut T` to distinguish from immutable pointers `*T`:

```ignis
let x: i32 = 42;
let p: *i32 = &x as *i32;         // Immutable pointer
let mut y: i32 = 10;
let mp: *mut i32 = &mut y as *mut i32;  // Mutable pointer
```

#### Mutable Self in Methods

Methods can now explicitly declare mutable self to indicate they modify the receiver:

```ignis
record Counter {
    value: i32;
    
    public increment(&mut self): void {
        self.value = self.value + 1;
    }
    
    public get(&self): i32 {
        return self.value;
    }
}
```

#### Documentation Comments

Added support for documentation comments that appear in LSP hover:

```ignis
/// Adds two numbers together.
/// Returns the sum of a and b.
function add(a: i32, b: i32): i32 {
    return a + b;
}

/**
 * A person with a name and age.
 * Use Person::new() to create instances.
 */
record Person {
    public name: string;
    public age: i32;
}
```

### LSP Improvements

#### New Features

- **Code Completion**: Context-aware suggestions for identifiers, member access (`.`), static access (`::`), and import paths
- **Document Symbols**: Navigate to functions, records, enums, and namespaces in the current file
- **Workspace Symbols**: Search for symbols across all project files
- **Find All References**: Locate all usages of a symbol
- **Go to Definition on Imports**: Click on import path strings (e.g., `"std::io"`) to jump to the module file
- **File Watching**: Automatically re-analyze when files change outside the editor

#### Enhanced Hover Information

- Doc comments now display with proper Markdown formatting
- Hover works on all identifier positions including:
  - Record fields (in definitions and initializers)
  - Enum variants
  - Type annotations
  - Path segments (e.g., each part of `Outer::Inner::func`)
  - Import items
  - Static access targets (e.g., `Io` in `Io::println`)

#### Inlay Hints

- Parameter name hints at call sites (e.g., `foo(name: "Alice", age: 30)`)
- Hints skip obvious cases: single-char params, underscore prefixes, matching variable names

#### Performance

- Analysis results are cached per document
- Tokens are cached for completion context detection
- Last good analysis preserved during typing for better completion

### Internal Improvements

- Added `name_span` to Definition for accurate declaration detection
- Added `FileId::SYNTHETIC` for internal definitions that shouldn't appear in user-facing features
- Added `AnalysisStage` enum to track how far analysis progressed
- Improved semantic token classification accuracy
- Fixed visibility enforcement for field and method access

### Bug Fixes

- Fixed extern functions incorrectly consuming ownership of arguments
- Fixed NodeId collisions in multi-module analysis
- Improved diagnostic span accuracy for various error types

## [0.2.0] - 2026-01-15

Initial v0.2 release with generics, records, enums, and basic LSP support.
