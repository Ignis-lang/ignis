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
<import-item> ::= <identifier> | <identifier> "as" <identifier>

<export> ::= "export" (<function> | <const> | <record> | <enum> | <type-alias> )
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
  | "string"
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
  | <type-alias>
  | <extern>
  | <namespace>
  | <directive-statement>
  | <directive>
  | <use>

<function> ::= <directive-attrs>? "function" <identifier> (<generic-type>)?
               "(" <parameters>? ")" ":" <type> <block>

<parameters> ::= <parameter> ("," <parameter>)*
<parameter> ::= <directive-attrs>? "..."? <identifier> "?"? ":" <variable-modifiers>? <type>

<import> ::= "import" <import-list> "from" <string> ";"
<import-list> ::= <import-item> ("," <import-item>)*
<import-item> ::= <identifier> | <identifier> "as" <identifier>

<export> ::= "export" (<function> | <const> | <record> | <enum> | <type-alias> | <directive-statement> | <directive>)
<inline> ::= "inline" (<function> | <const>)

<const> ::= <directive-attrs>? "const" <identifier> ":" <type> "=" <expression> ";"

<directive-attrs> ::= (<directive-attr>)+

<directive-attr> ::= "@[" <directive-attr-list>? "]"
                   | "@" <qualified-identifier> ("(" <expression-list>? ")")?

<directive-attr-list> ::= <directive-attr-item> ("," <directive-attr-item>)* ","?
<directive-attr-item> ::= <qualified-identifier> ("(" <expression-list>? ")")?

<directive-statement> ::= "directive" <qualified-identifier>
                          ("(" <expression-list>? ")")?
                          (";" | <block>)

<directive> ::= "directive" <identifier> ("(" <parameters>? ")")? ";"

<record> ::= <directive-attrs>? "record" <generic-type>? <identifier> "{" <record-item>* "}"

<record-item> ::= <directive-attrs>? (<record-property> | <record-method>)

<record-property> ::= <property-modifier>* <identifier>
                      <variable-modifiers>? "?"? ":" <type>
                      ("=" <expression>)? ";"

<record-method> ::= <method-modifier>* <identifier> <generic-type>?
                    "(" <parameters>? ")" "?"? ":" <type>
                    (<block> | ";")

<property-modifier> ::= "public" | "private" | "static" | "mut" | "abstract"
<method-modifier> ::= "public" | "private" | "static" | "final" | "abstract" | "inline"

<enum> ::= <directive-attrs>? "enum" <identifier> <generic-type>?
           "{" (<enum-item>)* "}"

<enum-item> ::= <directive-attrs>? (<enum-variant> | <enum-method>)

<enum-variant> ::= <identifier> ("=" <expression> | "(" <type-list>? ")" )? ","

<enum-method> ::= <method-modifier>* <identifier> <generic-type>?
                  "(" <parameters>? ")" ":" <type> <block>

<type-alias> ::= <directive-attrs>? "type" <identifier> <generic-type>? "=" <type> ";"

<extern> ::= <directive-attrs>? "extern" <qualified-identifier> "{" <extern-item>* "}"

<extern-item> ::= <directive-attrs>? ( <extern-function> | <record> | <enum> | <type-alias> )

<extern-function> ::= "function" <identifier> (<generic-type>)?
                      "(" <parameters>? ")" ":" <type> ";"

<namespace> ::= <directive-attrs>? "namespace" <qualified-identifier> "{" <namespace-item>* "}"
<namespace-item> ::= <function> | <const> | <record> | <enum> | <type-alias> | <extern> | <directive-statement> | <directive> | <use> | <namespace>

<use> ::= "use" <use-path> <use-alias>? ";"

<use-path> ::= <qualified-identifier>
<use-alias> ::= "as" <identifier>

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

<for> ::= "for" "(" "let" <identifier> "=" <expression> ";" <expression> ";" <expression> ")" <block>
<for-of> ::= "for" "(" ("let" <identifier> | <identifier>) "of" <expression> ")" <block>
<while> ::= "while" "(" <expression> ")" <block>

<return> ::= "return" <expression>? ";"
<break> ::= "break" ";"
<continue> ::= "continue" ";"

<block> ::= "{" <statement>* "}"
<variable> ::= <directive-attrs>? "let" "mut"? <identifier> ":" <type> ("=" <expression>)? ";"

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

<pipe-expression> ::= <or-expression> ( "|>" <or-expression> )*

<or-expression> ::= <and-expression> ( "||" <and-expression> )*
<and-expression> ::= <bitwise-or-expression> ( "&&" <bitwise-or-expression> )*

<bitwise-or-expression> ::= <bitwise-xor-expression> ( "|" <bitwise-xor-expression> )*
<bitwise-xor-expression> ::= <bitwise-and-expression> ( "^" <bitwise-and-expression> )*
<bitwise-and-expression> ::= <equality> ( "&" <equality> )*

<equality> ::= <comparison> ( ( "==" | "!=" ) <comparison> )*
<comparison> ::= <range> ( ( "<" | ">" | "<=" | ">=" ) <range> )*

<range> ::= <shift> ( ( ".." | "..=" ) <shift> )?

<shift> ::= <term> ( ( "<<" | ">>" ) <term> )*
<term> ::= <factor> ( ( "+" | "-" ) <factor> )*
<factor> ::= <cast> ( ( "*" | "/" | "%" ) <cast> )*
<cast> ::= <unary> ( "as" <type> )?

<unary> ::= ( "++" | "--" | "-" | "!" | "~" )* <postfix>

<postfix> ::= <primary> ( <postfix-suffix> )*

<postfix-suffix> ::= ("++" | "--")
  | <arguments>
  | "[" <expression> "]"
  | <member-access>

<arguments> ::= <generic-type>? "(" <expression-list>? ")"
<expression-list> ::= <expression> ("," <expression>)* ","?

<member-access> ::= ("." | "::") <identifier> <generic-type>?

<primary> ::= <record-init>
  | <identifier>
  | <literal>
  | <group>
  | <this>
  | <self>
  | <directive-expression>
  | <lambda>
  
<record-init> ::= <type-path> "{" <record-init-fields>? "}"
<record-init-fields> ::= <record-init-field> ("," <record-init-field>)* ","?
<record-init-field> ::= <identifier> ":" <expression>

<type-path> ::= <qualified-identifier> <generic-args>?

<generic-args> ::= "<" <type-list> ">"

<group> ::= "(" <expression> ")"
<this> ::= "this"
<self> ::= "self"

<directive-expression> ::= "@" <qualified-identifier> ("(" <expression-list>? ")")?
                         | "@[" <expression-list>? "]"

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

<type> ::= <union-type> | <intersection-type> | <function-type> | <vector-type> | <tuple-type> | <type-identifier>

<type-identifier> ::= <type-modifier>? (<primitive> | <qualified-identifier>) <generic-type>? "[]"?

<tuple-type> ::= "(" <type> "," <type> ("," <type>)* ","? ")"

<function-type> ::= "(" <type-list>? ")" "->" <type>
<type-list> ::= <type> ("," <type>)* ","?

<vector-type> ::= <type-modifier>? <type-identifier> "[" <number>? "]"

<generic-type> ::= "<" <generic-param> ("," <generic-param>)* ">"
<generic-param> ::= <identifier> ("as" <type>)?

<primitive> ::= "void"
  | "boolean"
  | "char"
  | "string"
  | "atom"
  | "i8" | "i16" | "i32" | "i64"
  | "u8" | "u16" | "u32" | "u64"
  | "f32" | "f64"
  | "hex"
  | "binary"

<union-type> ::= <type-identifier> ("|" <type-identifier>)+
<intersection-type> ::= <type-identifier> ("&" <type-identifier>)+

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*
<number> ::= [0-9]+

<hex-numbers> ::= "0x" [0-9a-fA-F]+
<binary-numbers> ::= "0b" [01]+

<assignment-operators> ::= "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
<variable-modifiers> ::= ("mut" | "&" | "*")+
```
