## Version 0.2

```
<program> ::= (<declaration>)* <EOF>

<declaration> ::= <function>
  | <import>
  | <export>
  | <inline>
  | <const>
  | <record>
  | <enum>
  | <type-alias>
  | <extern>
  | <namespace>
  | <use> 

<function> ::= "function" <identifier> (<generic-type>)?
               "(" <parameters>? ")" ":" <type> <block>

<parameters> ::= <parameter> ("," <parameter>)*
<parameter> ::= "..."? <identifier> "?"? ":" <variable-modifiers>? <type>

<import> ::= "import" <import-list> "from" <string> ";"
<import-list> ::= <import-item> ("," <import-item>)*
<import-item> ::= "_" | <identifier> | <identifier> "as" <identifier>

<export> ::= "export" (<function> | <const> | <record> | <enum> | <type-alias> )
  | "export" <identifier> ";"
  | "export" <import-list> "from" <string> ";"
<inline> ::= "inline" (<function> | <const>)

<const> ::= "const" <identifier> ":" <type> "=" <expression> ";"

<type-alias> ::= "type" <identifier> <generic-type>? "=" <type> ";"

<qualified-identifier> ::= <identifier> ("::" <identifier>)*

<namespace> ::= "namespace" <qualified-identifier> "{" <namespace-item>* "}"
<namespace-item> ::= <function> | <const> | <record> | <enum> | <type-alias> | <extern> | <use> | <namespace>

<extern> ::= "extern" <qualified-identifier> "{" <extern-item>* "}"
<extern-item> ::= <extern-function> | <extern-const>

<extern-function> ::= "function" <identifier> (<generic-type>)?
                      "(" <parameters>? ")" ":" <type> ";"

<extern-const> ::= "const" <identifier> ":" <type> ";"
                      
<use> ::= "use" <use-path> <use-alias>? ";"

<use-path> ::= <qualified-identifier>
<use-alias> ::= "as" <identifier>

<record> ::= "record" <identifier> <generic-type>? "{" <record-item>* "}"

<record-item> ::= <record-property> | <record-method>

<record-property> ::= <property-modifier>* <identifier>
                      <variable-modifiers>? "?"? ":" <type>
                      ("=" <expression>)? ";"

<record-method> ::= <method-modifier>* <identifier> <generic-type>?
                    "(" <parameters>? ")" "?"? ":" <type>
                    (<block> | ";")

<property-modifier> ::= "public" | "private" | "static" | "mut" | "abstract"
<method-modifier> ::= "public" | "private" | "static" | "final" | "abstract" | "inline"

<statement> ::= <declaration>
  | <if>
  | <for>
  | <for-of>
  | <while>
  | <return>
  | <break>
  | <continue>
  | <block>
  | <variable>
  | <expression> ";"

<if> ::= "if" "(" <expression> ")" <block>
         ("else if" "(" <expression> ")" <block>)*
         ("else" <block>)?

<for> ::= "for" "(" "let" <identifier> "=" <expression> ";"
          <expression> ";" <expression> ")" <block>

<for-of> ::= "for" "(" ("let" <identifier> | <identifier>) "of" <expression> ")" <block>

<while> ::= "while" "(" <expression> ")" <block>

<return> ::= "return" <expression>? ";"
<break> ::= "break" ";"
<continue> ::= "continue" ";"

<block> ::= "{" <statement>* "}"

<variable> ::= "let" "mut"? <identifier> ":" <type> ("=" <expression>)? ";"

<expression> ::= <assignment>

<assignment> ::= <ternary-expression> ( <assignment-operators> <assignment> )?

<ternary-expression> ::= <or-expression> ( "?" <expression> ":" <expression> )?

<or-expression> ::= <and-expression> ( "||" <and-expression> )*
<and-expression> ::= <bitwise-or-expression> ( "&&" <bitwise-or-expression> )*

<bitwise-or-expression> ::= <bitwise-xor-expression> ( "|" <bitwise-xor-expression> )*
<bitwise-xor-expression> ::= <bitwise-and-expression> ( "^" <bitwise-and-expression> )*
<bitwise-and-expression> ::= <equality> ( "&" <equality> )*

<equality> ::= <comparison> ( ( "==" | "!=" ) <comparison> )*
<comparison> ::= <shift> ( ( "<" | ">" | "<=" | ">=" ) <shift> )*
<shift> ::= <term> ( ( "<<" | ">>" ) <term> )*
<term> ::= <factor> ( ( "+" | "-" ) <factor> )*
<factor> ::= <cast> ( ( "*" | "/" | "%" ) <cast> )*
<cast> ::= <unary> ( "as" <type> )?

<unary> ::= ( "++" | "--" | "-" | "!" | "~" )* <postfix>

<postfix> ::= <primary> ( ("++" | "--") | <call-suffix> )*

<call-suffix> ::= <arguments>
  | "[" <expression> "]"
  | <member-call>
  | <member-access>
  
<member-call> ::= "." <identifier> <generic-args>? <arguments>
               | "::" <identifier> <generic-args>? <arguments>

<arguments> ::= "(" (<expression> ("," <expression>)*)? ")"

<member-access> ::= "." <identifier>
                  | "::" <identifier>

<primary> ::= <path>
  | <identifier>
  | <literal>
  | <group>
  | <this>
  | <self>

<path> ::= <qualified-identifier>

<group> ::= "(" <expression> ")"
<this> ::= "this"
<self> ::= "self"

<enum> ::= "enum" <identifier> <generic-type>? "{" (<enum-variant> | <enum-method>)* "}"

<enum-variant> ::= <identifier> ( "=" <expression> | "(" <type-list>? ")" )? ","

<enum-method> ::= <method-modifier>* <identifier> <generic-type>?
                  "(" <parameters>? ")" ":" <type> <block>

<literal> ::= <integer>
  | <float>
  | <hex>
  | <binary>
  | <string>
  | <char>
  | <boolean>
  | <null>
  | <vector>
  | <tuple>

<vector> ::= "[" <expression> ("," <expression>)* "]"
<tuple> ::= "(" <expression> ("," <expression>)+ ")"

<type> ::= <type-modifier>? <type-core> <type-suffix>*

<type-core> ::= <primitive>
  | <type-path>
  | <tuple-type>
  | <function-type>

<type-suffix> ::= "[]"
  | "[" <number>? "]"

<tuple-type> ::= "(" <type> ("," <type>)+ ")"
<function-type> ::= "(" <type-list>? ")" "->" <type>
<type-list> ::= <type> ("," <type>)*

<type-modifier> ::= ("mut" | "&" | "*")+

<variable-modifiers> ::= ("mut" | "&" | "*")+

<generic-type> ::= "<" <type-parameter> ("," <type-parameter>)* ">"
<type-parameter> ::= <type> ("as" <type>)?

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*
<number> ::= [0-9]+

<digits> ::= [0-9]+

<integer> ::= <digits> ("_" <digits>)*
<float> ::= <digits>? "." <digits> | <digits> "." <digits>?

<hex> ::= "0x" [0-9a-fA-F]+
<binary> ::= "0b" [01]+

<string> ::= "\"" (<string-char> | <escape-sequence>)* "\""
<string-char> ::= [^"\\] | <escape-sequence>
<char> ::= "'" ( [^'\\] | <escape-sequence> ) "'"
<escape-sequence> ::= "\\" [abfnrtv'"\\]

<boolean> ::= "true" | "false"
<null> ::= "null"

<primitive> ::= "void"
  | "boolean"
  | "char"
  | "str"
  | "i8" | "i16" | "i32" | "i64"
  | "u8" | "u16" | "u32" | "u64"
  | "f32" | "f64"
  | "hex"
  | "binary"

<assignment-operators> ::=
    "=" | "+=" | "-=" | "*=" | "/=" | "%="
  | "&=" | "|=" | "^=" | "<<=" | ">>="
```


## Version 0.3

```
<program> ::= (<declaration>)* <EOF>

<declaration> ::= <function>
  | <import>
  | <export>
  | <inline>
  | <const>
  | <record>
  | <enum>
  | <trait>
  | <type-alias>
  | <extern>
  | <namespace>

<function> ::= <directive-attrs>? "function" <identifier> (<generic-type>)?
               "(" <parameters>? ")" ":" <type> <block>

<parameters> ::= <parameter> ("," <parameter>)* ","?
<parameter> ::= <param-attr>* <identifier> ":" <variable-modifiers>? <type>

<param-attr> ::= "@" "takes" | "@" "noescape" | <directive-attr>

// `@takes` marks a parameter that takes ownership of its argument. `@noescape`
// marks a closure parameter that does not outlive the call, which stops escape
// propagation through that call site and lets the caller pass a by-reference
// closure: `function apply(@noescape f: (i32) -> i32, @takes v: i32): i32`.

<import> ::= "import" <import-list> "from" <string> ";"
<import-list> ::= <import-item> ("," <import-item>)*
<import-item> ::= "_" | <identifier>

<export> ::= "export" (<function> | <const> | <record> | <enum> | <type-alias>)
  | "export" <identifier> ";"
  | "export" <import-list> "from" <string> ";"
<inline> ::= <inline-modifier> (<function> | <const>)

<inline-modifier> ::= "inline" ("(" ("always" | "never") ")")?
// `inline` is a modifier keyword, not an attribute: `inline function f(): void {}`,
// `inline(always) function f(): void {}`, `inline(never) getX(&self): i32 { ... }`.
// The same modifier is available on record and enum methods via <method-modifier>.

<const> ::= <directive-attrs>? "const" <identifier> ":" <type> "=" <expression> ";"

<directive-attrs> ::= (<directive-attr>)+

<directive-attr> ::= "@" <qualified-identifier> <attribute-args>?

<attribute-args> ::= "(" (<attribute-arg-item> ("," <attribute-arg-item>)* ","?)? ")"
<attribute-arg-item> ::= <attribute-arg> | <identifier> ":" <attribute-arg>
<attribute-arg> ::= <integer> | <string> | <identifier> | "mut" | <primitive>

// `@directive(...)` also accepts named directive arguments such as
// `@directive(target: "record", phase: expand, effect: emit)`.
// Named directive arguments use `identifier: <attribute-arg>` alongside
// positional attribute arguments.

// Attribute forms recognized by later phases. Each one is an instance of
// <directive-attr>; the shapes below record which argument lists are accepted.
<known-attribute> ::= "@" "packed"
  | "@" "aligned" "(" <integer> ")"
  | "@" "cold"
  | "@" "test"
  | "@" "externName" "(" <string> ")"
  | "@" "deprecated" ("(" <string> ")")?
  | "@" "extension" "(" (<identifier> | <primitive>) ("," "mut")? ")"
  | "@" "implements" "(" <identifier> ("," <identifier>)* ")"
  | "@" "lang" "(" "try" ")"
  | "@" "langHook" "(" <string> ")"
  | "@" "takes"
  | "@" "noescape"
  | "@" ("allow" | "warn" | "deny") "(" <identifier> ")"

// `@extension(T)` and `@extension(T, mut)` apply to a free function whose first
// parameter is the receiver; the `mut` form allows a mutable receiver.
// `@implements(...)` takes one or more trait names, including the lang traits
// `Drop`, `Clone` and `Copy`. `@lang(try)` marks the `Result`/`Option` enums.
// `@langHook("name")` applies to a namespace. `@takes` and `@noescape` are
// parameter attributes (see <parameter>).

// Compile-time selection. These are resolved by the parser: the discarded
// branch or item is skipped and never reaches the AST.

<config-flag> ::= "@" "configFlag" "(" <compile-condition> ")" <config-flag-target>
<config-flag-target> ::= <declaration> | <namespace-item> | <statement>

<compile-if> ::= "@" "if" "(" <compile-condition> ")" <compile-branch>
                 ("@" "else" <compile-branch>)?
  | "@" "ifelse" "(" <compile-condition> ")" <compile-branch>
    ("@" "else")? <compile-branch>

<compile-branch> ::= "{" (<declaration> | <namespace-item> | <statement>)* "}"

<compile-condition> ::= <compile-condition-and> ( "||" <compile-condition-and> )*
<compile-condition-and> ::= <compile-condition-unary> ( "&&" <compile-condition-unary> )*
<compile-condition-unary> ::= "!"* <compile-condition-primary>
<compile-condition-primary> ::= "(" <compile-condition> ")" | <compile-predicate>

<compile-predicate> ::= "@" ("debug" | "release") "(" ")"
  | "@" ("platform" | "arch" | "abi" | "target" | "feature") "(" <string> ")"

// `@configFlag(<compile-condition>)` is also valid in expression position, where
// it evaluates to a boolean literal at parse time.

<record> ::= <directive-attrs>? "record" <generic-type>? <identifier> "{" <record-item>* "}"

<record-item> ::= <directive-attrs>? (<record-property> | <record-method>)

<record-property> ::= <property-modifier>* <identifier>
                      <variable-modifiers>? "?"? ":" <type>
                      ("=" <expression>)? ";"

// A `static` property is a namespaced constant on the type and requires an
// initializer: `static MAX: i32 = 10;`. It is read as `TypeName::MAX`.

<record-method> ::= <method-modifier>* <identifier> <generic-type>?
                    "(" <method-parameters>? ")" "?"? ":" <type>
                    (<block> | ";")

<method-parameters> ::= <self-parameter> ("," <parameters>)? | <parameters>
<self-parameter> ::= "self" | "&" "mut"? "self"

// A bare `self` is the consuming receiver: `x.method()` moves `x`, and inside the
// body `self` is an owned local dropped at scope end unless it is moved out.
// Reaching a consuming method through a reference moves out of a borrow and is
// rejected. There is no `mut self` form, because parameters carry no `mut`
// modifier; rebind with `let mut` inside the body instead.

<property-modifier> ::= "public" | "private" | "static"
<method-modifier> ::= "public" | "private" | "static" | <inline-modifier>

<enum> ::= <directive-attrs>? "enum" <identifier> <generic-type>?
           "{" (<enum-item>)* "}"

<enum-item> ::= <directive-attrs>? (<enum-variant> | <enum-method> | <enum-field>)

<enum-variant> ::= <identifier> ("=" <expression> | "(" <type-list>? ")" )? ","

<enum-method> ::= <method-modifier>* <identifier> <generic-type>?
                  "(" <method-parameters>? ")" ":" <type> <block>

<enum-field> ::= <property-modifier>* <identifier> ":" <type> ("=" <expression>)? ";"

// Like record properties, an enum field declared `static` is a namespaced
// constant on the enum and requires an initializer: `static COUNT: i32 = 2;`.
// `static` is rejected on a variant, not on a field.

<type-alias> ::= <directive-attrs>? "type" <identifier> <generic-type>? "=" <type> ";"

<extern> ::= <directive-attrs>? "extern" <qualified-identifier> "{" <extern-item>* "}"

<extern-item> ::= <directive-attrs>? ( <extern-function> | <record> | <enum> | <type-alias> )

<extern-function> ::= "function" <identifier> (<generic-type>)?
                      "(" <parameters>? ")" ":" <type> ";"

<namespace> ::= <directive-attrs>? "namespace" <qualified-identifier> "{" <namespace-item>* "}"
<namespace-item> ::= <function> | <const> | <record> | <enum> | <trait> | <type-alias> | <extern> | <namespace>

<trait> ::= <directive-attrs>? "trait" <identifier> <generic-type>?
            "{" <trait-method>* "}"

<trait-method> ::= <method-modifier>* "function"? <identifier> <generic-type>?
                   "(" <method-parameters>? ")" (":" <type>)?
                   (<block> | ";")

// `;` declares a required method that every implementor must provide.
// A block declares a default body that implementors may override:
//   trait Greet {
//     name(&self): i32;
//     greet(&self): i32 { return self.name() + 1; }
//   }
// An omitted return type means `void`.

<statement> ::= <declaration>
  | <if>
  | <for>
  | <for-of>
  | <while>
  | <return>
  | <break>
  | <continue>
  | <defer>
  | <block>
  | <variable>
  | <let-else>
  | <expression> ";"

<if> ::= "if" "(" <condition> ")" <block>
         ("else if" "(" <condition> ")" <block>)*
         ("else" <block>)?

<condition> ::= <condition-operand> ( "&&" <condition-operand> )*
              | <expression>

<condition-operand> ::= <let-condition> | <expression>

<let-condition> ::= "let" <pattern> "=" <expression>

// A `let` condition is an expression form, so `if` and `while` accept chains that
// mix pattern bindings with ordinary tests, in any order and any number:
//   if (let Option::SOME(x) = a && let Option::SOME(y) = b) { ... }
//   while (let Option::SOME(x) = next(&mut it) && x >= 0) { ... }
// The bound value is parsed above `&&`, so the chain splits at each `&&`, and a
// binding is visible to the conditions that follow it and inside the body.

<for> ::= "for" "(" "let" <identifier> (":" <type>)? "=" <expression> ";" <expression> ";" <expression> ")" <block>
<for-of> ::= "for" "(" "let" <identifier> (":" <type>)? "of" <expression> ")" <block>

// The optional annotation in `for ... of` selects how each element is bound:
// `for (let x of arr)` binds by value, `for (let x: &i32 of arr)` binds a shared
// reference and `for (let x: &mut i32 of arr)` binds a mutable one.

<while> ::= "while" "(" <condition> ")" <block>

<return> ::= "return" <expression>? ";"
<break> ::= "break" ";"
<continue> ::= "continue" ";"
<defer> ::= "defer" <expression> ";"

<block> ::= "{" <statement>* "}"
<variable> ::= <directive-attrs>? "let" "mut"? <identifier> ":" <type> ("=" <expression>)? ";"
<let-else> ::= "let" "mut"? <pattern> "=" <expression> "else" <block> ";"
  | "let" "mut"? <identifier> (":" <type>)? "=" <expression> "else" <block> ";"

// The first form destructures an explicit pattern and runs the `else` block when
// the pattern does not match: `let Option::SOME(x) = a else { return 1; };`.
// The second form is the shorthand over a try type (`Result` or `Option`): the
// identifier binds the payload of the success variant and the `else` block runs
// on the failure variant: `let v = a else { return 1; };`.
// The `else` block must diverge (`return`, `break`, `continue` or `@panic`).
// A plain identifier followed by `::` or `(` is read as a pattern, so
// `let Option::NONE = a else { ... };` takes the first form.

<expression> ::= <assignment> | <match>

<match> ::= "match" "(" <expression> ")" "{" <match-arm>+ "}"
<match-arm> ::= <match-pattern> "->" (<expression> | <block>) ","?

<match-pattern> ::= <pattern> ( "|" <pattern> )* <guard-clause>?

<guard-clause> ::= "if" <expression>

<pattern> ::= "_"
  | <literal-pattern>
  | <binding-pattern>
  | <qualified-variant-pattern>
  | <tuple-pattern>

<literal-pattern> ::= <integer> | <float> | <hex> | <binary> | <string> | <char> | <boolean> | <null> | <atom>
<binding-pattern> ::= <identifier>
<qualified-variant-pattern> ::= <qualified-identifier> ("(" <pattern-list>? ")")?
<tuple-pattern> ::= "(" <pattern-list> ")"
<pattern-list> ::= <pattern> ("," <pattern>)* ","?

<assignment> ::= <ternary-expression> ( <assignment-operators> <assignment> )?

<ternary-expression> ::= <pipe-expression> ( "?" <expression> ":" <expression> )?

<pipe-expression> ::= <or-expression> ( "|>" <pipe-rhs> )*

<pipe-rhs> ::= <or-expression>

// `_` in expression position is parsed as a pipe placeholder and is only valid
// inside the RHS of `|>`. Accepted RHS shapes and their arity rules:
//
//   <identifier> | <qualified-identifier>   bare callee; the LHS becomes its only
//                                           argument, so the callee takes 1 param
//                                             21 |> twice
//                                             99 |> Util::unwrap
//   <postfix> "." <identifier>              bare method; the LHS becomes the first
//                                           argument after the receiver
//                                             5 |> a.add
//   <call>                                  the LHS is prepended as the first
//                                           argument unless `_` says otherwise
//                                             10 |> add3(5, 3)
//                                             7  |> m.sub(3, _)
//   <lambda>                                the LHS becomes its only argument
//                                             10 |> (x: i32): i32 -> x * 3
//   <record-init> | <vector> | <builtin>    require exactly one `_`
//                                             42 |> Wrapper { value: _ }
//                                             10 |> [_, 20, 30]
//
// A call RHS may use zero or one `_` (a placeholder in any argument position
// replaces the implicit prepend); more than one `_` in the same RHS is rejected,
// and a bare `_` as the whole RHS is rejected. `_` does not cross a nested lambda
// or a nested `|>`, which each open a fresh placeholder scope.

<or-expression> ::= <and-expression> ( "||" <and-expression> )*
<and-expression> ::= <bitwise-or-expression> ( "&&" <bitwise-or-expression> )*

<bitwise-or-expression> ::= <bitwise-xor-expression> ( "|" <bitwise-xor-expression> )*
<bitwise-xor-expression> ::= <bitwise-and-expression> ( "^" <bitwise-and-expression> )*
<bitwise-and-expression> ::= <equality> ( "&" <equality> )*

<equality> ::= <comparison> ( ( "==" | "!=" ) <comparison> )*
<comparison> ::= <shift> ( ( "<" | ">" | "<=" | ">=" ) <shift> )*

<shift> ::= <term> ( ( "<<" | ">>" ) <term> )*
<term> ::= <factor> ( ( "+" | "-" ) <factor> )*
<factor> ::= <cast> ( ( "*" | "/" | "%" ) <cast> )*
<cast> ::= <unary> ( "as" <type> )?

<unary> ::= <prefix-operator>* <postfix>

<prefix-operator> ::= "++" | "--" | "-" | "!" | "~" | "&" "mut"? | "*"
// Every prefix operator binds tighter than `as`: the cast applies to the whole
// prefix expression, so `-a as i32`, `&mut a as *mut i32` and `*p as i32` are
// casts of a negation, of a mutable reference and of a dereference.

<postfix> ::= <primary> ( <postfix-suffix> )*

<postfix-suffix> ::= ("++" | "--" | "!")
  | <arguments>
  | "[" <expression> "]"
  | <member-access>

// `++` and `--` exist in both positions and are two different expressions:
// `++i` and `--i` are <prefix-operator>s, `i++` and `i--` are postfix suffixes.
// Postfix `!` is the try operator on a `Result`/`Option` value.

<arguments> ::= <generic-type>? "(" <expression-list>? ")"
<expression-list> ::= <expression> ("," <expression>)* ","?

<member-access> ::= ("." | "::") <identifier> <generic-type>?

<primary> ::= <record-init>
  | <identifier>
  | <literal>
  | <group>
  | <self>
  | <directive-expression>
  | <capture-override>
  | <pipe-placeholder>
  | <lambda>
  
<record-init> ::= <type-path> "{" <record-init-fields>? "}"
<record-init-fields> ::= <record-init-field> ("," <record-init-field>)* ","?
<record-init-field> ::= <identifier> ":" <expression>

<type-path> ::= <qualified-identifier> <generic-args>?

<generic-args> ::= "<" <type-list> ">"

<group> ::= "(" <expression> ")"
<self> ::= "self"

<directive-expression> ::= "@" <qualified-identifier> ("(" <expression-list>? ")")?

<capture-override> ::= ("@move" | "@ref" | "@refMut") <expression>

// A capture override is a prefix on a closure body expression that forces how the
// named outer variable is captured, overriding the inferred capture mode:
//   let getX = (): i32 -> @move x;
//   let getX = (): i32 -> @ref x;
// It is written without parentheses. `@move(x)` is parsed as a builtin call, not
// as a capture override.

<pipe-placeholder> ::= "_"

<lambda> ::= (<generic-type>)? "(" <parameters>? ")" ":" <type> "->" (<expression> | <block>)

<digits> ::= [0-9]+
<integer> ::= <digits> ("_" <digits>)*
<float> ::= <digits>? "." <digits> | <digits> "." <digits>?

<hex> ::= <hex-numbers>
<binary> ::= <binary-numbers>

<string> ::= "\"" (<string-char> | <escape-sequence>)* "\""
<string-char> ::= [^"\\] | <escape-sequence>
<char> ::= "'" ( [^'\\] | <escape-sequence> ) "'"
<escape-sequence> ::= "\\" [abfnrtv'"\\]

<template> ::= <template-no-substitution>
  | <template-head> <expression> (<template-middle> <expression>)* <template-tail>
<template-no-substitution> ::= "`" <template-char>* "`"
<template-head> ::= "`" <template-char>* "${"
<template-middle> ::= "}" <template-char>* "${"
<template-tail> ::= "}" <template-char>* "`"
<template-char> ::= [^`\\] | <template-escape>
<template-escape> ::= <escape-sequence> | "\\`" | "\\$"

<boolean> ::= "true" | "false"
<null> ::= "null"

<vector> ::= "[" <expression-list>? "]"

<atom> ::= ":" <identifier>

<object> ::= "{" <object-item>* "}"
<object-item> ::= (<object-property> | <object-method>) ","?
<object-property> ::= <identifier> ":" <expression>
<object-method> ::= <identifier> <generic-type>? "(" <parameters>? ")" ":" <type> <block>

<tuple> ::= "(" <expression> "," <expression> ("," <expression>)* ","? ")"

<literal> ::= <integer>
  | <float>
  | <hex>
  | <binary>
  | <string>
  | <template>
  | <char>
  | <boolean>
  | <null>
  | <atom>
  | <vector>
  | <object>
  | <tuple>

<type-modifier> ::= ("mut" | "&" | "*")+
<type-parameter> ::= <type> ("as" <type>)?

<qualified-identifier> ::= <identifier> ("::" <identifier>)*

<type> ::= <function-type> | <vector-type> | <type-identifier>

<type-identifier> ::= <type-modifier>? (<primitive> | <qualified-identifier>) <generic-type>? "[]"?

<function-type> ::= "(" <type-list>? ")" "->" <type>
<type-list> ::= <type> ("," <type>)* ","?

<vector-type> ::= <type-modifier>? <type-identifier> "[" <number>? "]"

<generic-type> ::= "<" <generic-param> ("," <generic-param>)* ">"
<generic-param> ::= <identifier> ("as" <type>)?

<primitive> ::= "void"
  | "boolean"
  | "char"
  | "str"
  | "atom"
  | "i8" | "i16" | "i32" | "i64"
  | "u8" | "u16" | "u32" | "u64"
  | "f32" | "f64"

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*
<number> ::= [0-9]+

<hex-numbers> ::= "0x" [0-9a-fA-F]+
<binary-numbers> ::= "0b" [01]+

<assignment-operators> ::= "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
<variable-modifiers> ::= ("mut" | "&" | "*")+
```

### Reserved syntax not yet parsed

- `@[attr, attr]` — bracketed attribute lists (`@[...]`), both as a declaration attribute and in expression position.
- `this` — the `this` keyword; only `self` is accepted.
- `use <path> (as <name>)?;` — the `use` declaration.
- `directive <name> (...)?;` and `directive <path> (...)? (; | { ... })` — directive declarations.
- `..` and `..=` as a range operator between expressions.
- Tuple types `(T, T, ...)`, union types `T | T`, and intersection types `T & T`.
- `hex` and `binary` as primitive type keywords.
- `<identifier> as <identifier>` in an import item.
- `...` (variadic) and `?` (optional) on a function parameter.
- `mut` and `abstract` as property modifiers; `final` and `abstract` as method modifiers.
