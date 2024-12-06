use std::collections::HashMap;

use colored::Colorize;
use ignis_ast::{
  expressions::{
    assign::ASTAssignment, binary::ASTBinary, call::ASTCall, cast::ASTCast, grouping::ASTGrouping, lambda::ASTLambda, literal::{ASTLiteral, ASTLiteralValue}, logical::ASTLogical, match_expression::{ASTMatchCase, ASTMatchExpression}, member_access::ASTMemberAccess, meta::{ASTMeta, ASTMetaEntity}, object_literal::ASTObject, spread::ASTSpread, ternary::ASTTernary, this::ASTThis, unary::ASTUnary, variable::ASTVariableExpression, vector::ASTVector, vector_access::ASTVectorAccess, ASTExpression
  },
  metadata::{ASTMetadata, ASTMetadataFlags},
  statements::{
    block::ASTBlock,
    comment::{ASTComment, ASTCommentType},
    constant::ASTConstant,
    r#extern::ASTExtern,
    for_of_statement::ASTForOf,
    for_statement::ASTFor,
    function::ASTFunction,
    if_statement::ASTIf,
    import::{ASTImport, ASTImportSource, ASTImportSymbol},
    method::ASTMethod,
    namespace::ASTNamespace,
    property::ASTProperty,
    record::ASTRecord,
    return_::ASTReturn,
    variable::ASTVariable,
    while_statement::ASTWhile,
    ASTStatement,
  },
};
use ignis_config::IgnisConfig;
use ignis_data_type::DataType;
use ignis_token::{token_types::TokenType, token::Token};

use super::diagnostics::{ParserDiagnostic, ParserDiagnosticError};

type IgnisParserResult<T> = Result<T, Box<ParserDiagnostic>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum IgnisParserContext {
  Function,
  Class,
  VectorAccess,
  Enum,
  Const,
  Extern,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ParserDeclaration {
  Import,
  Enum,
  Record,
}

#[derive(Debug, Clone)]
enum ParserDeclarationList {
  Name(String),
  Record(String, Vec<(String, DataType)>),
}

type StructDeclaration = HashMap<ParserDeclaration, Vec<ParserDeclarationList>>;

/// # Ignis Parser
///
/// ## Ignis BNF:
///
/// ### Types
/// ```bnf
/// <type-modifier> ::= ("mut" | "&" | "*")*
/// <type-parameter> ::= <type> ("as" <type>)?
/// <qualified-identifier> ::= <identifier> ( ("::" | "." ) <identifier> )*
///
/// <type> ::= <type-modifier>? (<qualified-identifier> | <primitive> | <function-type> | <vector-type>)
///
/// <function-type> ::= "(" <type-list>? ")" "->" <type>
/// <type-list> ::= <type> ("," <type>)*
///
/// <vector-type> ::= <type> "[" <number>? "]"
/// <primitive> ::= "void"
///   | "boolean"
///   | "char"
///   | "string"
///   | "i8"
///   | "i16"
///   | "i32"
///   | "i64"
///   | "u8"
///   | "u16"
///   | "u32"
///   | "u64"
///   | "f32"
///   | "f64"
///   | "unknown"
///   | "hex"
///   | "binary"
///
/// <identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*
/// <numbers> ::= [0-9]+
/// <hex-numbers> ::= "0x" [0-9a-fA-F]+
/// <binary-numbers> ::= "0b" [01]+
/// ```
///
/// ```bnf
/// <program> ::= (<declaration>)* <EOF>
///
/// <declaration> ::= <function>
///   | <import>
///   | <export>
///   | <const>
///   | <record>
///   | <extern>
///   | <declare>
///   | <meta>
///
/// <function> ::= "function" <identifier> "(" <parameters>? ")" ":" <type> (<block> | ";")
/// <parameters> ::= <parameter> ("," <parameter>)*
/// <parameter> ::= "..."? <identifier> "?"? ":" <type>
///
/// <import> ::= "import" "{" <import-list> "}" "from" <string> ";"
/// <import-list> ::= <import-item> | <import-item> "," <import-list>
/// <import-item> ::= <identifier> | <identifier> "as" <identifier>
///
/// <export> ::= "export" <declaration> | "export" <identifier> ";"
///
/// <const> ::= "const" <identifier> ":" <type> "=" <expression> ";"
///
/// # Record syntax
/// <record> ::= "record" <identifier> "{" <record-item>* "}"
/// <record-item> ::= <record-property> | <record-method>
/// <record-property> ::= <identifier> "?"? ":" <type> ("=" <expression>)? ";"
/// <record-method> ::= <identifier> "?"? "(" <parameters> ")" ":" <type> ";"
///
/// # Extern syntax
/// <extern> ::= "extern" (<qualified-identifier>) "{" <extern-item>* "}"
/// <extern-item> ::= <declaration> | "include" <string> ";" | "source" <string> ";"
///
/// # Namespace syntax
/// <namespace> ::= "namespace" <qualified-identifier> "{" <namespace-item>* "}"
/// <namespace-item> ::= <function> | <const> | <record> | <class> | <enum> | <type-alias> | <interface> | <declare>
///
/// # Declare syntax
/// <declare> ::= "declare" <identifier> ":" <type>
///
/// # Meta syntax
/// <meta> ::= "meta" <identifier> ("(" <parameters> ")")? ";"
///
/// # Statements
/// <statement> ::= <declaration>
///   | <if>
///   | <for>
///   | <for-of>
///   | <while>
///   | <return>
///   | <break>
///   | <continue>
///   | <block>
///   | <variable>
///   | <expression> ";"
///
/// <if> ::= "if" "(" <expression> ")" <block> ("else if" "(" <expression> ")" <block>)* ("else" <block>)?
/// <for> ::= "for" "(" "let" <identifier> "=" <expression> ";" <expression> ";" <expression> ")" <block>
/// <for-of> ::= "for" "(" "let" <identifier> "of" <expression> ")" <block>
/// <while> ::= "while" "(" <expression> ")" <block>
///
/// <return> ::= "return" <expression>? ";"
/// <break> ::= "break" ";"
/// <continue> ::= "continue" ";"
///
/// <block> ::= "{" <statement>* "}"
/// <variable> ::= "let" "mut"? <identifier> ":" <type> ("=" <expression>)? ";"
///
/// <expression> ::= <assignment> | <match>
///
/// ## Match expression
/// <match> ::= "match" <expression> "{" <match-case>* "}"
/// <match-case> ::= <match-pattern> "->" <statement> ","?
/// <match-pattern> ::= <pattern> ( "|" <pattern> )*
/// <pattern> ::= <literal> | <identifier> | "_" | <pattern> "when" <expression>
///
/// <assignment> ::= <ternary-expression> ( ( <assignment-operators> ) <assignment> )?
/// <ternary-expression> ::= <or-expression> ( "?" <expression> ":" <expression> )?
/// <or-expression> ::= <and-expression> ( "||" <and-expression> )*
/// <and-expression> ::= <bitwise-or-expression> ( "&&" <bitwise-or-expression> )*
///
/// <bitwise-or-expression> ::= <bitwise-xor-expression> ( "|" <bitwise-xor-expression> )*
/// <bitwise-xor-expression> ::= <bitwise-and-expression> ( "^" <bitwise-and-expression> )*
/// <bitwise-and-expression> ::= <equality> ( "&" <equality> )*
///
/// <equality> ::= <comparison> ( ( "==" | "!=" ) <comparison> )*
/// <comparison> ::= <range> ( ( "<" | ">" | "<=" | ">=" ) <range> )*
/// <range> ::= <shift> ( ( ".." | "..=" ) <shift> )*
/// <shift> ::= <term> ( ( "<<" | ">>" ) <term> )*
/// <term> ::= <factor> ( ( "+" | "-" ) <factor> )*
/// <factor> ::= <cast> ( ( "*" | "/" | "%" ) <cast> )*
/// <cast> ::= <unary> ( "as" <type> )?
/// <unary> ::= ( "++" | "--" | "-" | "!" | "~" )* <postfix>
/// <postfix> ::= <primary> ( ("++" | "--") | <call-suffix> )*
/// <call-suffix> ::= <arguments> | <member-access>
/// <arguments> ::= "(" <expression> ("," <expression>)* ")"
/// <member-access> ::= ("." | "::") <identifier>
///
/// <primary> ::= <identifier>
///   | <literal>
///   | <group>
///   | <meta-expression>
///   | <this>
///   | "..." <expression>
///
/// # literal
/// <integer> ::= <numbers> ("_" <numbers>)*
/// <float> ::= (<integer>? "." <numbers>) | (<integer> "." <numbers>?)
/// <hex> ::= <hex-numbers>
/// <binary> ::= <binary-numbers>
/// <string> ::= "\"" (<string-char> | <escape-sequence>)* "\""
/// <string-char> ::= [^"\\] | <escape-sequence>
/// <char> ::= "'" ( [^'\\] | <escape-sequence> ) "'"
/// <escape-sequence> ::= "\\" [abfnrtv'"\\]
/// <boolean> ::= "true" | "false"
/// <null> ::= "null"
/// <vector> ::= "[" <expression> ("," <expression>)* "]"
/// <vector-access> ::= <identifier> "[" <expression> "]"
/// <object> ::= "{" <object-item>* "}"
/// <object-item> ::= (<object-property> | <object-method>) ","?
/// <object-property> ::= <identifier> ":" <expression>
/// <object-method> ::= <identifier> "(" <expression>? ")" ":" <block>
///
/// <group> ::= "(" <expression> ")"
/// <new> ::= "new" <qualified-identifier> "(" <arguments>? ")"
/// <this> ::= "this"
/// <literal> ::= <integer> | <float> | <hex> | <binary> | <string> | <boolean> | <null> | <vector> | <object>
/// <decorator-expression> ::= "@" <qualified-identifier> "(" <expression>? ")"
/// <meta-expression> ::= "#" <qualified-identifier> ("(" <expression>*? ")")? ";"? | "#" "[" (<expression>? ","?)* "]"
/// <lambda> ::= "(" <parameters>? ")" ":" <type> "->" (<expression> | <block>)
///
/// <comment> ::= "//" ([a-zA-Z_][a-zA-Z0-9_]*)?
/// <multiline-comment> ::= "/*" ([a-zA-Z_][a-zA-Z0-9_]*)?
/// <documentation-comment> ::= "/**" ([a-zA-Z_][a-zA-Z0-9_]*)? "**/"
/// <assignment-operators> ::= "="
///   | "+="
///   | "-="
///   | "*="
///   | "/="
///   | "%="
///   | "&="
///   | "|="
///   | "^="
///   | "<<="
///   | ">>="
/// <separator> ::= "," | ";" | ":"
/// <delimiter> ::= <separator> | "[" | "]" | "(" | ")" | "{" | "}"
/// <symbol> ::= <assignment-operators>
///   | <delimiter>
///   | "."
///   | "+"
///   | "-"
///   | "*"
///   | "/"
///   | "%"
///   | "&"
///   | "|"
///   | "^"
///   | "<<"
///   | ">>"
///   | "<"
///   | ">"
///   | "<="
///   | ">="
///   | "=="
///   | "!="
///   | "!"
///   | "~"
///   | "?"
///   | ":"
///   | "||"
///   | "&&"
///   | "++"
///   | "--"
///   | "->"
/// <keyword> ::= "abstract"
///   | "as"
///   | "async"
///   | "await"
///   | "break"
///   | "continue"
///   | "class"
///   | "const"
///   | "declare"
///   | "else"
///   | "enum"
///   | "export"
///   | "extends"
///   | "false"
///   | "for"
///   | "from"
///   | "function"
///   | "is"
///   | "if"
///   | "implements"
///   | "import"
///   | "in"
///   | "interface"
///   | "let"
///   | "meta"
///   | "namespace"
///   | "new"
///   | "null"
///   | "private"
///   | "public"
///   | "record"
///   | "return"
///   | "static"
///   | "this"
///   | "true"
///   | "type"
///   | "typeof"
///   | "void"
///   | "when"
///   | "while"
///   | "with"
///   | "mut"
///   | "override"
///   | "super"
///   | "include"
///   | "source"
/// ```
#[derive(Debug, Clone)]
pub struct IgnisParser {
  config: Box<IgnisConfig>,
  context: Vec<IgnisParserContext>,
  declarations: StructDeclaration,
  tokens: Vec<Token>,
  current: usize,
  diagnostics: Vec<ParserDiagnostic>,
}

impl IgnisParser {
  pub fn new(
    config: Box<IgnisConfig>,
    tokens: Vec<Token>,
  ) -> Self {
    let declarations: StructDeclaration = HashMap::from([
      (ParserDeclaration::Import, vec![]),
      (ParserDeclaration::Enum, vec![]),
      (ParserDeclaration::Record, vec![]),
    ]);

    Self {
      config,
      context: vec![],
      declarations,
      tokens,
      current: 0,
      diagnostics: vec![],
    }
  }

  pub fn parse(
    &mut self,
    std: bool,
  ) -> (Vec<ASTStatement>, Vec<ParserDiagnostic>) {
    if !self.config.quiet {
      let file = &self.tokens.last();

      if let Some(last_token) = file {
        if std {
          println!(
            "{:indent$}{} Parsing... {}",
            " ",
            "-->".bright_yellow().bold(),
            last_token.file_name.bold(),
            indent = 6
          );
        } else {
          println!(
            "{:indent$}Parsing... {}",
            "-->".bright_green().bold(),
            last_token.file_name.bold(),
            indent = 4
          );
        }
      }
    }

    let mut statements: Vec<ASTStatement> = vec![];

    while !self.is_at_end() {
      match self.declaration() {
        Ok(declaration) => statements.push(declaration),
        Err(error) => self.diagnostics.push(*error),
      };
    }

    (statements, self.diagnostics.clone())
  }

  fn synchronize(&mut self) {
    loop {
      let token = self.advance();

      // Save point for continue with parsing
      if matches!(token.type_, TokenType::SemiColon | TokenType::RightBrace) || self.is_at_end() {
        break;
      }

      match self.peek().type_ {
        TokenType::Class
        | TokenType::Const
        | TokenType::Declare
        | TokenType::Enum
        | TokenType::Export
        | TokenType::Extern
        | TokenType::For
        | TokenType::Function
        | TokenType::If
        | TokenType::Import
        | TokenType::Let
        | TokenType::Meta
        | TokenType::Return
        | TokenType::Type
        | TokenType::While => return,
        _ => (),
      };
    }
  }

  /// <declaration> ::= <function> | <import> | <export> | <const> | <record> | <extern> | <declare> | <meta> | <namespace>
  fn declaration(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.peek();

    match token.type_ {
      TokenType::Function => self.function(false),
      TokenType::Import => self.import(),
      TokenType::Export => self.export(),
      TokenType::Const => self.const_(false),
      TokenType::Record => self.record(false),
      TokenType::Extern => self.extern_(false),
      TokenType::Declare => self.declare(false),
      TokenType::Meta => self.meta(false),
      TokenType::Namespace => self.namespace(false),
      _ => match self.statement() {
        Ok(statement) => Ok(statement),
        Err(error) => {
          self.synchronize();
          Err(error)
        },
      },
    }
  }

  /// <function> ::= "function" <identifier> "(" <parameters>? ")" ":" <type> (<block> | ";")
  fn function(
    &mut self,
    is_exported: bool,
  ) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Function)?;

    let name: Token = self.consume(TokenType::Identifier)?;

    let parameters = self.parameters(&name)?;

    self.consume(TokenType::Colon)?;

    let return_type = self.resolve_type()?;

    let mut body: Vec<ASTStatement> = Vec::new();

    if self.check(TokenType::LeftBrace) {
      body.push(self.block()?);
    } else if !self.match_token(&[TokenType::SemiColon]) {
      return Err(Box::new(ParserDiagnostic::new(
        ParserDiagnosticError::ExpectedSemicolonAfterExpression(self.peek()),
      )));
    }

    let mut metadata = ASTMetadata::default();

    if is_exported {
      metadata.push(ASTMetadataFlags::Export);
    }

    Ok(ASTStatement::Function(Box::new(ASTFunction::new(
      name,
      parameters,
      body,
      return_type,
      metadata,
    ))))
  }

  /// <import> ::= "import" "{" <import-list> "}" "from" <string> ";"
  fn import(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Import)?;

    self.consume(TokenType::LeftBrace)?;

    let imports: Vec<ASTImportSymbol> = self.import_list()?;

    self.consume(TokenType::RightBrace)?;

    self.consume(TokenType::From)?;

    let module = self.consume(TokenType::String)?;

    self.consume(TokenType::SemiColon)?;

    let mut source_type: ASTImportSource = ASTImportSource::FileSystem;
    let mut is_std: bool = false;

    if module.lexeme.starts_with("std::") {
      source_type = ASTImportSource::StandardLibrary;
      is_std = true;
    }

    if module.lexeme.starts_with("package::") {
      source_type = ASTImportSource::Package;
    }

    Ok(ASTStatement::Import(Box::new(ASTImport::new(
      module,
      imports,
      is_std,
      source_type,
    ))))
  }

  /// <import-list> ::= <import-item> | <import-item> "," <import-list>
  fn import_list(&mut self) -> IgnisParserResult<Vec<ASTImportSymbol>> {
    let mut imports: Vec<ASTImportSymbol> = vec![];

    if !self.check(TokenType::RightBrace) {
      loop {
        if self.match_token(&[TokenType::Comma]) {
          continue;
        }

        imports.push(self.import_item()?);

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    return Ok(imports);
  }

  /// <import-item> ::= <identifier> | <identifier> "as" <identifier>
  fn import_item(&mut self) -> IgnisParserResult<ASTImportSymbol> {
    let mut alias: Option<Token> = None;

    let name = self.consume(TokenType::Identifier)?;

    if self.match_token(&[TokenType::As]) {
      alias = Some(self.consume(TokenType::Identifier)?);
    }

    Ok(ASTImportSymbol::new(name, alias))
  }

  /// <export> ::= "export" <declaration> | "export" <identifier> ";"
  fn export(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Export)?;

    match self.peek().type_ {
      TokenType::Function => self.function(true),
      TokenType::Const => self.const_(true),
      TokenType::Record => self.record(true),
      TokenType::Extern => self.extern_(true),
      TokenType::Declare => self.declare(true),
      TokenType::Meta => self.meta(true),
      TokenType::Namespace => self.namespace(true),
      _ => Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(
        TokenType::Function,
        self.peek(),
      )))),
    }
  }

  /// <const> ::= "const" <identifier> ":" <type> "=" <expression> ";"
  fn const_(
    &mut self,
    is_exported: bool,
  ) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Const)?;

    self.context.push(IgnisParserContext::Const);

    let name = self.consume(TokenType::Identifier)?;

    self.consume(TokenType::Colon)?;

    let data_type = self.resolve_type()?;

    self.consume(TokenType::Equal)?;

    let value = self.expression()?;

    self.consume(TokenType::SemiColon)?;

    self.context.pop();

    let mut metadata = ASTMetadata::new(vec![]);

    if is_exported {
      metadata.push(ASTMetadataFlags::Public);
    }

    Ok(ASTStatement::Constant(Box::new(ASTConstant::new(
      name,
      Box::new(value),
      data_type,
      metadata,
    ))))
  }

  /// <record> ::= "record" <identifier> "{" <record-item>* "}"
  fn record(
    &mut self,
    is_exported: bool,
  ) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Record)?;

    let name = self.consume(TokenType::Identifier)?;

    self.consume(TokenType::LeftBrace)?;

    let mut items: Vec<ASTStatement> = vec![];

    while !self.check(TokenType::RightBrace) {
      items.push(self.record_item(&name)?);
    }

    self.consume(TokenType::RightBrace)?;

    let mut metadata: ASTMetadata = ASTMetadata::new(vec![]);

    if is_exported {
      metadata.push(ASTMetadataFlags::Export);
    }

    Ok(ASTStatement::Record(Box::new(ASTRecord::new(name, items, metadata))))
  }

  /// <record-item> ::= <record-property> | <record-method>
  fn record_item(
    &mut self,
    record: &Token,
  ) -> IgnisParserResult<ASTStatement> {
    let name = self.consume(TokenType::Identifier)?;

    let is_optional = self.match_token(&[TokenType::QuestionMark]);
    if self.match_token(&[TokenType::Colon]) {
      return self.record_property(name, is_optional);
    }

    if self.check(TokenType::LeftParen) {
      return self.record_method(record, name, is_optional);
    }

    Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(
      TokenType::Colon,
      self.peek(),
    ))))
  }

  /// <record-property> ::= <identifier> "?"? ":" <type> ("=" <expression>)? ";"
  fn record_property(
    &mut self,
    name: Token,
    is_optional: bool,
  ) -> IgnisParserResult<ASTStatement> {
    let mut data_type = self.resolve_type()?;

    if is_optional {
      data_type = DataType::Optional(Box::new(data_type));
    }

    self.consume(TokenType::SemiColon)?;

    let mut metadata = ASTMetadata::new(vec![ASTMetadataFlags::Property]);

    if is_optional {
      metadata.push(ASTMetadataFlags::Optional);
    }

    Ok(ASTStatement::Variable(Box::new(ASTVariable::new(
      name, None, data_type, metadata,
    ))))
  }

  /// <record-method> ::= <identifier> "(" <parameters> ")" "?"? ":" <type> ";"
  fn record_method(
    &mut self,
    record: &Token,
    name: Token,
    is_optional: bool,
  ) -> IgnisParserResult<ASTStatement> {
    let parameters = self.parameters(&name)?;

    self.consume(TokenType::Colon)?;

    let return_type = self.resolve_type()?;

    self.consume(TokenType::SemiColon)?;

    let mut metadata = ASTMetadata::new(vec![ASTMetadataFlags::Method]);

    if is_optional {
      metadata.push(ASTMetadataFlags::Optional);
    }

    Ok(ASTStatement::Method(Box::new(ASTMethod::new(
      name,
      parameters,
      ASTBlock::new(vec![]),
      return_type,
      metadata,
      record.clone(),
    ))))
  }

  /// <extern> ::= "extern" (<qualified-identifier>) "{" <extern-item>* "}"
  fn extern_(
    &mut self,
    is_exported: bool,
  ) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Extern)?;

    self.context.push(IgnisParserContext::Extern);

    let name = self.expression()?;

    self.consume(TokenType::LeftBrace)?;

    let mut items: Vec<ASTStatement> = vec![];

    while !self.check(TokenType::RightBrace) {
      items.push(self.extern_item()?);
    }

    self.consume(TokenType::RightBrace)?;

    let mut metadata = ASTMetadata::new(vec![]);

    if is_exported {
      metadata.push(ASTMetadataFlags::Export);
    }

    self.context.pop();

    Ok(ASTStatement::Extern(Box::new(ASTExtern::new(Box::new(name), items, metadata))))
  }

  /// <extern-item> ::= <declaration> | "include" <string> ";" | "source" <string> ";"
  fn extern_item(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.peek();

    match token.type_ {
      TokenType::Function => self.function(false),
      TokenType::Const => self.const_(false),
      TokenType::Record => self.record(false),
      TokenType::Declare => self.declare(false),
      TokenType::Hash | TokenType::At => self.statement(),
      TokenType::Include => {
        self.consume(TokenType::Include)?;
        let path = self.consume(TokenType::String)?;

        self.consume(TokenType::SemiColon)?;

        Ok(ASTStatement::Include(Box::new(path)))
      },
      TokenType::Source => {
        self.consume(TokenType::Source)?;
        let path = self.consume(TokenType::String)?;

        self.consume(TokenType::SemiColon)?;

        Ok(ASTStatement::Source(Box::new(path)))
      },
      _ => Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(
        TokenType::Function,
        self.peek(),
      )))),
    }
  }

  /// <declare> ::= "declare" <identifier> ":" <type>
  fn declare(
    &mut self,
    is_exported: bool,
  ) -> IgnisParserResult<ASTStatement> {
    todo!()
  }

  /// <meta> ::= "meta" <identifier> ("(" <parameters> ")")? ";"
  fn meta(
    &mut self,
    is_exported: bool,
  ) -> IgnisParserResult<ASTStatement> {
    todo!()
  }

  /// <namespace> ::= "namespace" <qualified-identifier> "{" <namespace-item>* "}"
  fn namespace(
    &mut self,
    is_exported: bool,
  ) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Namespace)?;

    let name = self.expression()?;

    self.consume(TokenType::LeftBrace)?;

    let mut members: Vec<ASTStatement> = vec![];

    while !self.check(TokenType::RightBrace) {
      members.push(self.namespace_item()?);
    }

    self.consume(TokenType::RightBrace)?;
    let mut metadata = ASTMetadata::new(vec![]);

    if is_exported {
      metadata.push(ASTMetadataFlags::Export);
    }

    Ok(ASTStatement::Namespace(Box::new(ASTNamespace::new(
      Box::new(name),
      members,
      metadata,
    ))))
  }

  /// <namespace-item> ::= <function> | <const> | <record> | <class> | <enum> | <type-alias> | <interface> | <declare>
  fn namespace_item(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.peek();

    match token.type_ {
      TokenType::Function => self.function(false),
      TokenType::Const => self.const_(false),
      TokenType::Record => self.record(false),
      TokenType::Declare => self.declare(false),
      TokenType::Identifier => {
        if TokenType::get_keyword_from_string(token.lexeme.as_str()).is_some() {
          return Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::UnexpectedKeyword(
            token.clone(),
          ))));
        }

        self.statement()
      },
      _ => Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(
        TokenType::Function,
        self.peek(),
      )))),
    }
  }

  /// <statement> ::= <declaration> | <if> | <for> | <for-of> | <while> | <return> | <break> | <continue> | <block> | <variable> | <expression> ";"
  fn statement(&mut self) -> IgnisParserResult<ASTStatement> {
    match self.peek().type_ {
      TokenType::If => self.if_(),
      TokenType::For => self.for_(),
      TokenType::While => self.while_(),
      TokenType::Return => self.return_(),
      TokenType::Break => self.break_(),
      TokenType::Continue => self.continue_(),
      TokenType::LeftBrace => self.block(),
      TokenType::Let => self.variable(),
      TokenType::Comment => self.comment(),
      TokenType::MultiLineComment => self.multiline_comment(),
      TokenType::DocComment => self.documentation_comment(),
      _ => {
        let expression = self.expression()?;

        if !matches!(expression, ASTExpression::Meta(_) | ASTExpression::MetaEntity(_)) {
          self.consume(TokenType::SemiColon)?;
        }

        Ok(ASTStatement::Expression(Box::new(expression)))
      },
    }
  }

  /// <comment> ::= "//" ([a-zA-Z_][a-zA-Z0-9_]*)?
  fn comment(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.consume(TokenType::Comment)?;

    Ok(ASTStatement::Comment(Box::new(ASTComment::new(
      token,
      ASTCommentType::SingleLine,
    ))))
  }

  /// <multiline-comment> ::= "/*" ([a-zA-Z_][a-zA-Z0-9_]*)?
  fn multiline_comment(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.consume(TokenType::MultiLineComment)?;
    Ok(ASTStatement::Comment(Box::new(ASTComment::new(
      token,
      ASTCommentType::MultiLine,
    ))))
  }

  /// <documentation-comment> ::= "/**" ([a-zA-Z_][a-zA-Z0-9_]*)? "**/"
  fn documentation_comment(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.consume(TokenType::DocComment)?;
    Ok(ASTStatement::Comment(Box::new(ASTComment::new(
      token,
      ASTCommentType::Documentation,
    ))))
  }

  /// <if> ::= "if" "(" <expression> ")" <block> ("else if" "(" <expression> ")" <block>)* ("else" <block>)?
  fn if_(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::If)?;

    self.consume(TokenType::LeftParen)?;

    let condition: ASTExpression = self.expression()?;

    self.consume(TokenType::RightParen)?;

    let then_branch: ASTStatement = self.statement()?;

    let else_branch: Option<Box<ASTStatement>> = if self.match_token(&[TokenType::Else]) {
      Some(Box::new(self.statement()?))
    } else {
      None
    };

    Ok(ASTStatement::If(Box::new(ASTIf::new(
      Box::new(condition),
      Box::new(then_branch),
      else_branch,
    ))))
  }

  /// <for> ::= "for" "(" "let" <identifier> "=" <expression> ";" <expression> ";" <expression> ")" <block>
  /// <for-of> ::= "for" "(" "let" <identifier> "in" <expression> ")" <block>
  fn for_(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::For)?;

    self.consume(TokenType::LeftParen)?;

    self.consume(TokenType::Let)?;
    let item: Token = self.consume(TokenType::Identifier)?;

    let mut variable =
      ASTVariable::new(item, None, DataType::Pending, ASTMetadata::new(vec![ASTMetadataFlags::Mutable]));

    if self.check(TokenType::Of) {
      return self.for_of_statement(variable);
    }

    self.consume(TokenType::Equal)?;

    let initializer = self.expression()?;

    variable.initializer = Some(Box::new(initializer));
    variable.type_annotation = DataType::UnsignedInt32;

    self.consume(TokenType::SemiColon)?;

    let condition: ASTExpression = self.expression()?;

    self.consume(TokenType::SemiColon)?;

    let increment_decrement: ASTExpression = self.expression()?;

    self.consume(TokenType::RightParen)?;

    let body = self.block()?;

    Ok(ASTStatement::For(Box::new(ASTFor::new(
      Box::new(variable),
      Box::new(condition),
      Box::new(increment_decrement),
      Box::new(body),
    ))))
  }

  /// <for-of> ::= "for" "(" "let" <identifier> "of" <expression> ")" <block>
  fn for_of_statement(
    &mut self,
    variable: ASTVariable,
  ) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Of)?;

    let iterable: ASTExpression = self.expression()?;

    self.consume(TokenType::RightParen)?;

    let body: ASTStatement = self.statement()?;

    let statement = ASTForOf::new(Box::new(variable), Box::new(iterable), Box::new(body), self.previous());

    Ok(ASTStatement::ForOf(Box::new(statement)))
  }

  /// <while> ::= "while" "(" <expression> ")" <block>
  fn while_(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::While)?;

    let condition = self.expression()?;
    let body = self.block()?;

    Ok(ASTStatement::While(Box::new(ASTWhile::new(
      Box::new(condition),
      Box::new(body),
    ))))
  }

  /// <return> ::= "return" <expression>? ";"
  fn return_(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.consume(TokenType::Return)?;

    if self.match_token(&[TokenType::SemiColon]) {
      return Ok(ASTStatement::Return(Box::new(ASTReturn::new(None, token))));
    }

    let value = self.expression()?;

    self.consume(TokenType::SemiColon)?;

    Ok(ASTStatement::Return(Box::new(ASTReturn::new(Some(Box::new(value)), token))))
  }

  /// <break> ::= "break" ";"
  fn break_(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.consume(TokenType::Break)?;

    self.consume(TokenType::SemiColon)?;
    Ok(ASTStatement::Break { token })
  }

  /// <continue> ::= "continue" ";"
  fn continue_(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.consume(TokenType::Continue)?;

    self.consume(TokenType::SemiColon)?;
    Ok(ASTStatement::Continue { token })
  }

  /// <block> ::= "{" <statement>* "}"
  fn block(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::LeftBrace)?;

    let mut statements: Vec<ASTStatement> = Vec::new();

    while !self.check(TokenType::RightBrace) && !self.is_at_end() {
      statements.push(self.declaration()?);
    }

    self.consume(TokenType::RightBrace)?;

    Ok(ASTStatement::Block(Box::new(ASTBlock::new(statements))))
  }

  /// <variable> ::= "let" "mut"? <identifier> ":" <type> ("=" <expression>)? ";"
  fn variable(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Let)?;

    let mutable: bool = self.match_token(&[TokenType::Mut]);

    let name: Token = self.consume(TokenType::Identifier)?;

    let mut initializer: Option<ASTExpression> = None;

    self.consume(TokenType::Colon)?;

    let mut type_annotation = self.resolve_type()?;

    let mut flags: Vec<ASTMetadataFlags> = vec![];

    if mutable {
      flags.push(ASTMetadataFlags::Mutable);
    }

    if self.match_token(&[TokenType::SemiColon]) {
      return Ok(ASTStatement::Variable(Box::new(ASTVariable::new(
        name,
        None,
        type_annotation,
        ASTMetadata::new(flags),
      ))));
    }

    if self.match_token(&[TokenType::LeftBrack]) {
      let mut size: Option<Token> = None;
      if self.match_token(&[TokenType::Int]) {
        size.clone_from(&Some(self.consume(TokenType::Int)?));
      }

      self.consume(TokenType::RightBrack)?;

      type_annotation = DataType::Vector(Box::new(type_annotation), size);
    }

    if self.match_token(&[TokenType::Equal]) {
      let mut value = self.expression()?;

      if let ASTExpression::Vector(v) = value {
        value = ASTExpression::Vector(Box::new(ASTVector::new(v.token.clone(), v.elements, type_annotation.clone())));
      };

      initializer = Some(value);
    }
    self.consume(TokenType::SemiColon)?;

    if let Some(ini) = initializer {
      Ok(ASTStatement::Variable(Box::new(ASTVariable::new(
        name,
        Some(Box::new(ini)),
        type_annotation,
        ASTMetadata::new(flags),
      ))))
    } else {
      let token = self.peek();
      Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedExpression(
        token.clone(),
      ))))
    }
  }

  /// <expression> ::= <assignment> | <match>
  fn expression(&mut self) -> IgnisParserResult<ASTExpression> {
    match self.peek().type_ {
      TokenType::Match => self.match_(),
      _ => self.assignment(),
    }
  }

  /// <match> ::= "match" <expression> "{" <match-case>* "}"
  fn match_(&mut self) -> IgnisParserResult<ASTExpression> {
    self.consume(TokenType::Match)?;

    let expression = self.expression()?;

    self.consume(TokenType::LeftBrace)?;

    let mut cases: Vec<ASTMatchCase> = vec![];

    while !self.check(TokenType::RightBrace) {
      cases.push(self.match_case()?);

      if !self.match_token(&[TokenType::Comma]) {
        break;
      }
    }

    self.consume(TokenType::RightBrace)?;

    Ok(ASTExpression::Match(Box::new(ASTMatchExpression::new(
      Box::new(expression),
      cases,
    ))))
  }

  /// <match-case> ::= <match-pattern> "->" <statement> ","?
  fn match_case(&mut self) -> IgnisParserResult<ASTMatchCase> {
    let pattern = self.match_pattern()?;

    let when = if self.match_token(&[TokenType::When]) {
      Some(self.expression()?)
    } else {
      None
    };

    self.consume(TokenType::Arrow)?;

    let block = if self.check(TokenType::LeftBrace) {
      self.block()?
    } else {
      ASTStatement::Expression(Box::new(self.expression()?))
    };

    Ok(ASTMatchCase::new(pattern, when, Box::new(block)))
  }

  /// <match-pattern> ::= <pattern> ( "|" <pattern> )*
  fn match_pattern(&mut self) -> IgnisParserResult<Vec<ASTExpression>> {
    let mut patterns: Vec<ASTExpression> = vec![self.pattern()?];

    while self.match_token(&[TokenType::Pipe]) {
      patterns.push(self.pattern()?);
    }

    Ok(patterns)
  }

  /// <pattern> ::= <literal> | <identifier> | "_" | <pattern> "when" <expression>
  fn pattern(&mut self) -> IgnisParserResult<ASTExpression> {
    let token = self.peek();

    match token.type_ {
      TokenType::Identifier => {
        let token = self.consume(TokenType::Identifier)?;

        Ok(ASTExpression::Variable(Box::new(ASTVariableExpression::new(
          token.clone(),
          DataType::from(&token.type_),
          ASTMetadata::new(vec![]),
        ))))
      },
      TokenType::Int
      | TokenType::Float
      | TokenType::String
      | TokenType::Hex
      | TokenType::Binary
      | TokenType::Char
      | TokenType::True
      | TokenType::False
      | TokenType::Null
      | TokenType::LeftBrace
      | TokenType::LeftBrack => self.literal(),
      _ => Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedPattern(
        token.clone(),
      )))),
    }
  }

  /// <assignment> ::= <ternary-expression> ( ( <assignment-operators> ) <assignment> )?
  fn assignment(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression = self.ternary_expression()?;

    if self.match_token(&[
      TokenType::Equal,
      TokenType::AddAssign,
      TokenType::SubtractAssign,
      TokenType::MulAssign,
      TokenType::DivAssign,
      TokenType::ModAssign,
      TokenType::LeftShiftAssign,
      TokenType::RightShiftAssign,
    ]) {
      let operator = self.previous();
      let right = self.assignment()?;

      expression = ASTExpression::Assigment(Box::new(ASTAssignment::new(
        Box::new(expression),
        Box::new(operator),
        Box::new(right),
      )));
    }

    Ok(expression)
  }

  /// <ternary-expression> ::= <or-expression> ( "?" <expression> ":" <expression> )?
  fn ternary_expression(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut children: Vec<ASTExpression> = vec![];

    children.push(self.or_expression()?);

    while self.match_token(&[TokenType::QuestionMark]) {
      children.push(self.expression()?);

      self.consume(TokenType::Colon)?;

      children.push(self.expression()?);
    }

    if children.len() == 1 {
      return Ok(children.pop().unwrap());
    }

    let else_branch = children.pop().unwrap();
    let then_branch = children.pop().unwrap();
    let condition = children.pop().unwrap();

    let mut expression: ASTExpression = ASTExpression::Ternary(Box::new(ASTTernary::new(
      Box::new(condition),
      Box::new(then_branch),
      Box::new(else_branch),
      Box::new(self.peek()),
      DataType::Pending,
    )));

    while !children.is_empty() {
      expression = ASTExpression::Ternary(Box::new(ASTTernary::new(
        Box::new(children.pop().unwrap()),
        Box::new(expression),
        Box::new(children.pop().unwrap()),
        Box::new(self.peek()),
        DataType::Pending,
      )));
    }

    Ok(expression)
  }

  /// <or-expression> ::= <and-expression> ( "||" <and-expression> )*
  fn or_expression(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.and_expression()?;

    while self.match_token(&[TokenType::Or]) {
      let operator: Token = self.previous();
      let right = self.and_expression()?;

      expression = ASTExpression::Logical(Box::new(ASTLogical::new(Box::new(expression), operator, Box::new(right))));
    }

    Ok(expression)
  }

  /// <and-expression> ::= <equality> ( "&&" <equality> )*
  fn and_expression(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.equality()?;

    while self.match_token(&[TokenType::And]) {
      let operator: Token = self.previous();
      let right = self.equality()?;

      expression = ASTExpression::Logical(Box::new(ASTLogical::new(Box::new(expression), operator, Box::new(right))));
    }

    Ok(expression)
  }

  /// <equality> ::= <comparison> ( ( "==" | "!=" ) <comparison> )*
  fn equality(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.comparison()?;

    while self.match_token(&[TokenType::EqualEqual, TokenType::BangEqual]) {
      let operator: Token = self.previous();
      let right = self.comparison()?;
      let type_: DataType = expression.clone().into();

      expression =
        ASTExpression::Binary(Box::new(ASTBinary::new(Box::new(expression), operator, Box::new(right), type_)));
    }

    Ok(expression)
  }

  /// <comparison> ::= <range> ( ( "<" | ">" | "<=" | ">=" ) <range> )*
  fn comparison(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.range()?;

    while self.match_token(&[
      TokenType::Less,
      TokenType::Greater,
      TokenType::LessEqual,
      TokenType::GreaterEqual,
    ]) {
      let operator: Token = self.previous();
      let right = self.range()?;
      let type_: DataType = expression.clone().into();

      expression =
        ASTExpression::Binary(Box::new(ASTBinary::new(Box::new(expression), operator, Box::new(right), type_)));
    }

    Ok(expression)
  }

  /// <range> ::= <shift> ( ( ".." | "..=" ) <shift> )*
  fn range(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.shift()?;

    while self.match_token(&[TokenType::Range, TokenType::RangeInclusive]) {
      let operator: Token = self.previous();
      let right = self.shift()?;
      let type_: DataType = expression.clone().into();

      expression =
        ASTExpression::Binary(Box::new(ASTBinary::new(Box::new(expression), operator, Box::new(right), type_)));
    }

    Ok(expression)
  }

  /// <shift> ::= <term> ( ( "<<" | ">>" ) <term> )*
  fn shift(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.term()?;

    while self.match_token(&[TokenType::LeftShift, TokenType::RightShift]) {
      let operator: Token = self.previous();
      let right = self.term()?;
      let type_: DataType = expression.clone().into();

      expression =
        ASTExpression::Binary(Box::new(ASTBinary::new(Box::new(expression), operator, Box::new(right), type_)));
    }

    Ok(expression)
  }

  /// <term> ::= <factor> ( ( "+" | "-" ) <factor> )*
  fn term(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.factor()?;

    while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
      let operator: Token = self.previous();
      let right = self.factor()?;
      let type_: DataType = expression.clone().into();

      expression =
        ASTExpression::Binary(Box::new(ASTBinary::new(Box::new(expression), operator, Box::new(right), type_)));
    }

    Ok(expression)
  }

  /// <factor> ::= <cast> ( ( "*" | "/" | "%" ) <cast> )*
  fn factor(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.cast()?;

    while self.match_token(&[TokenType::Asterisk, TokenType::Slash, TokenType::Mod]) {
      let operator: Token = self.previous();
      let right = self.cast()?;
      let type_: DataType = expression.clone().into();

      expression =
        ASTExpression::Binary(Box::new(ASTBinary::new(Box::new(expression), operator, Box::new(right), type_)));
    }

    Ok(expression)
  }

  /// <cast> ::= <unary> ( "as" <type> )?
  fn cast(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.unary()?;

    if self.match_token(&[TokenType::As]) {
      let operator: Token = self.previous();
      let type_: DataType = self.resolve_type()?;

      expression = ASTExpression::Cast(Box::new(ASTCast::new(operator, type_, Box::new(expression))));
    }

    Ok(expression)
  }

  /// <unary> ::= ( "++" | "--" | "-" | "!" | "~" )* <postfix>
  fn unary(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.postfix()?;

    while self.match_token(&[TokenType::Plus, TokenType::Minus, TokenType::Bang, TokenType::Tilde]) {
      let operator: Token = self.previous();
      let right = self.postfix()?;
      let type_: DataType = expression.clone().into();

      expression = ASTExpression::Unary(Box::new(ASTUnary::new(operator, Box::new(right), type_, false)));
    }

    Ok(expression)
  }

  /// <postfix> ::= <primary> ( ("++" | "--") | <call-suffix> )* | <vector-access>
  fn postfix(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.primary()?;

    while self.match_token(&[
      TokenType::Increment,
      TokenType::Decrement,
      TokenType::Dot,
      TokenType::DoubleColon,
      TokenType::LeftParen,
      TokenType::LeftBrack,
    ]) {
      let operator: Token = self.previous().clone();

      expression = match operator.type_ {
        TokenType::Dot => self.member_access(&expression, false)?,
        TokenType::DoubleColon => self.member_access(&expression, true)?,
        TokenType::LeftParen => self.call_suffix(&expression)?,
        TokenType::LeftBrack => self.vector_access(expression)?,
        TokenType::Increment | TokenType::Decrement => ASTExpression::Unary(Box::new(ASTUnary::new(
          operator,
          Box::new(expression.clone()),
          expression.clone().into(),
          true,
        ))),
        _ => unreachable!(),
      };
    }

    Ok(expression)
  }

  /// <call-suffix> ::= <arguments> | <member-access>
  fn call_suffix(
    &mut self,
    previous_expression: &ASTExpression,
  ) -> IgnisParserResult<ASTExpression> {
    let arguments: Vec<ASTExpression> = self.arguments()?;

    let mut expression = ASTExpression::Call(Box::new(ASTCall::new(Box::new(previous_expression.clone()), arguments)));

    while self.match_token(&[TokenType::Dot, TokenType::LeftParen, TokenType::DoubleColon]) {
      expression = match self.previous().type_ {
        TokenType::Dot => self.member_access(&expression, false)?,
        TokenType::DoubleColon => self.member_access(&expression, true)?,
        TokenType::LeftParen => self.call_suffix(&expression)?,
        _ => unreachable!(),
      };
    }

    return Ok(expression);
  }

  /// <arguments> ::= "(" <expression> ("," <expression>)* ")"
  fn arguments(&mut self) -> IgnisParserResult<Vec<ASTExpression>> {
    let mut arguments = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if arguments.len() >= 255 {
          return Err(Box::new(ParserDiagnostic::new(
            ParserDiagnosticError::InvalidNumberOfArguments(255, arguments.len(), self.peek().clone()),
          )));
        }

        arguments.push(self.expression()?);

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightParen)?;

    Ok(arguments)
  }

  /// <member-access> ::= ("." | "::") <identifier>
  fn member_access(
    &mut self,
    previous_expression: &ASTExpression,
    is_namespace: bool,
  ) -> IgnisParserResult<ASTExpression> {
    let member_name = self.consume(TokenType::Identifier)?;

    let mut metadata = ASTMetadata::new(vec![]);

    if is_namespace {
      metadata.push(ASTMetadataFlags::NamespaceMember);
    }

    let mut expression = ASTExpression::MemberAccess(Box::new(ASTMemberAccess::new(
      Box::new(previous_expression.clone()),
      Box::new(member_name),
      metadata,
    )));

    while self.match_token(&[TokenType::Dot, TokenType::LeftParen, TokenType::DoubleColon]) {
      expression = match self.previous().type_ {
        TokenType::Dot => self.member_access(&expression, false)?,
        TokenType::DoubleColon => self.member_access(&expression, true)?,
        TokenType::LeftParen => self.call_suffix(&expression)?,
        _ => unreachable!(),
      };
    }

    Ok(expression)
  }

  /// <primary> ::= <identifier> | <literal> | <group> | <meta-expression> | <lambda>
  fn primary(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut token = self.peek();
    let mut metadata = ASTMetadata::new(vec![]);

    if self.match_token(&[TokenType::Ampersand]) {
      token = self.peek();
      metadata.push(ASTMetadataFlags::Reference);
    }

    if self.match_token(&[TokenType::Asterisk]) {
      token = self.peek();
      metadata.push(ASTMetadataFlags::Pointer);
    }

    match token.type_ {
      TokenType::LeftParen => {
        if self.is_lambda() {
          return self.lambda();
        }

        self.group()
      },
      TokenType::Hash => self.meta_expression(),
      TokenType::Identifier => {
        self.consume(TokenType::Identifier)?;

        let kind = token.type_.clone();

        Ok(ASTExpression::Variable(Box::new(ASTVariableExpression::new(
          token.clone(),
          DataType::from(&kind),
          metadata,
        ))))
      },
      TokenType::This => {
        self.consume(TokenType::This)?;

        Ok(ASTExpression::This(Box::new(ASTThis::new(token))))
      },
      TokenType::Variadic => {
        self.consume(TokenType::Variadic)?;

        Ok(ASTExpression::Spread(Box::new(ASTSpread::new(
          Box::new(self.expression()?),
          token,
        ))))
      },
      _ => self.literal(),
    }
  }

  /// <vector-access> ::= <identifier> "[" <expression> "]"
  fn vector_access(
    &mut self,
    var: ASTExpression,
  ) -> IgnisParserResult<ASTExpression> {
    self.context.push(IgnisParserContext::VectorAccess);

    let index = self.expression()?;

    self.consume(TokenType::RightBrack)?;

    self.context.pop();

    Ok(ASTExpression::VectorAccess(Box::new(ASTVectorAccess::new(
      Box::new(self.previous()),
      Box::new(var),
      Box::new(index),
    ))))
  }

  /// <literal> ::= <integer> | <float> | <hex> | <binary> | <string> | <boolean> | <null> | <vector> | <object>
  fn literal(&mut self) -> IgnisParserResult<ASTExpression> {
    let token = &self.peek();
    match token.type_ {
      TokenType::LeftBrace => self.object(),
      TokenType::True
      | TokenType::False
      | TokenType::Null
      | TokenType::Int
      | TokenType::Float
      | TokenType::String
      | TokenType::Hex
      | TokenType::Binary
      | TokenType::Char => {
        self.advance();

        let value: ASTLiteralValue = ASTLiteralValue::from((token.type_.clone(), token.lexeme.clone()));

        Ok(ASTExpression::Literal(Box::new(ASTLiteral::new(value, token.clone()))))
      },
      TokenType::LeftBrack => self.vector(),
      _ => Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedExpression(
        token.clone(),
      )))),
    }
  }

  /// <vector> ::= "[" <expression> ("," <expression>)* "]"
  fn vector(&mut self) -> IgnisParserResult<ASTExpression> {
    let token: Token = self.consume(TokenType::LeftBrack)?;

    let mut elements: Vec<ASTExpression> = vec![];

    if !self.check(TokenType::RightBrack) {
      loop {
        elements.push(self.expression()?);

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightBrack)?;

    let type_: DataType = DataType::Vector(Box::new(DataType::Pending), None);

    Ok(ASTExpression::Vector(Box::new(ASTVector::new(token, elements, type_))))
  }

  /// <object> ::= "{" <object-item>* "}"
  fn object(&mut self) -> IgnisParserResult<ASTExpression> {
    let token = self.consume(TokenType::LeftBrace)?;

    let mut properties: Vec<ASTProperty> = vec![];
    let mut methods: Vec<ASTMethod> = vec![];

    loop {
      if self.check(TokenType::RightBrace) {
        break;
      }

      if self.is_current_a_comment() {
        self.advance();
        continue;
      }

      let _ = self.object_item(&mut properties, &mut methods)?;

      if !self.match_token(&[TokenType::Comma]) {
        break;
      }
    }

    self.consume(TokenType::RightBrace)?;

    let data_type = DataType::Object((
      properties.iter().map(|p| p.type_annotation.clone()).collect(),
      methods.iter().map(|m| m.return_type.clone()).collect(),
    ));

    Ok(ASTExpression::Object(Box::new(ASTObject::new(
      token, properties, methods, data_type,
    ))))
  }

  /// <object-item> ::= (<object-property> | <object-method>) ","?
  fn object_item(
    &mut self,
    properties: &mut Vec<ASTProperty>,
    methods: &mut Vec<ASTMethod>,
  ) -> IgnisParserResult<()> {
    let name = self.consume(TokenType::Identifier)?;

    if self.check(TokenType::LeftParen) {
      let method = self.object_method(name)?;

      methods.push(method);
      return Ok(());
    }

    let property = self.object_property(name)?;

    properties.push(property);

    return Ok(());
  }

  /// <object-property> ::= <identifier> ":" <expression>
  fn object_property(
    &mut self,
    name: Token,
  ) -> IgnisParserResult<ASTProperty> {
    let initializer = if self.match_token(&[TokenType::Colon]) {
      Some(Box::new(self.expression()?))
    } else {
      None
    };

    let type_annotation = initializer.clone().map_or(DataType::Pending, |e| (*e.clone()).into());

    let metadata = ASTMetadata::new(vec![]);

    Ok(ASTProperty::new(Box::new(name), initializer, type_annotation, metadata))
  }

  /// <object-method> ::= <identifier> "(" <parameters>? ")" ":" <type>  <block>
  fn object_method(
    &mut self,
    name: Token,
  ) -> IgnisParserResult<ASTMethod> {
    let arguments = self.parameters(&name)?;

    self.consume(TokenType::Colon)?;

    let return_type = self.resolve_type()?;

    let body = if let ASTStatement::Block(block) = self.block()? {
      block.clone()
    } else {
      Box::new(ASTBlock::new(vec![]))
    };

    let metadata = ASTMetadata::new(vec![]);

    Ok(ASTMethod::new(
      name.clone(),
      arguments,
      *body,
      return_type,
      metadata,
      name.clone(),
    ))
  }

  /// <group> ::= "(" <expression> ")"
  fn group(&mut self) -> IgnisParserResult<ASTExpression> {
    self.consume(TokenType::LeftParen)?;

    let expression = self.expression()?;
    self.consume(TokenType::RightParen)?;

    Ok(ASTExpression::Grouping(Box::new(ASTGrouping::new(Box::new(expression)))))
  }

  /// <meta-expression> ::= "#" <qualified-identifier> ("(" <expression>*? ")")? ";"? | "#" "[" (<expression>? ","?)* "]"
  fn meta_expression(&mut self) -> IgnisParserResult<ASTExpression> {
    self.consume(TokenType::Hash)?;

    let mut meta: Vec<ASTMeta> = vec![];

    if self.match_token(&[TokenType::LeftBrack]) {
      loop {
        let name = self.expression()?;

        meta.push(ASTMeta::new(Box::new(name)));

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }

      self.consume(TokenType::RightBrack)?;
    } else {
      let name = self.expression()?;

      meta.push(ASTMeta::new(Box::new(name)));
    }

    let entity: Option<ASTStatement> = if self.check(TokenType::SemiColon) {
      None
    } else {
      Some(self.meta_entity()?)
    };

    Ok(ASTExpression::MetaEntity(Box::new(ASTMetaEntity::new(meta, entity))))
  }

  fn meta_entity(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.peek();
    match token.type_ {
      TokenType::Function => self.function(false),
      TokenType::Const => self.const_(false),
      TokenType::Record => self.record(false),
      TokenType::Declare => self.declare(false),
      TokenType::Identifier => {
        if TokenType::get_keyword_from_string(token.lexeme.as_str()).is_some() {
          return Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::UnexpectedKeyword(
            token.clone(),
          ))));
        }

        self.statement()
      },
      _ => Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(
        TokenType::Function,
        self.peek(),
      )))),
    }
  }

  /// <lambda> ::= "(" <parameters>? ")" ":" <type> "->" (<expression> | <block>)
  fn lambda(&mut self) -> IgnisParserResult<ASTExpression> {
    let token = self.previous();
    let parameters = self.parameters(&token)?;

    self.consume(TokenType::Colon)?;

    let return_type = self.resolve_type()?;

    self.consume(TokenType::Arrow)?;

    let body = if self.check(TokenType::LeftBrace) {
      self.block()?
    } else {
      ASTStatement::Expression(Box::new(self.expression()?))
    };

    let lambda_type = DataType::Function(
      parameters.iter().map(|p| p.type_annotation.clone()).collect(),
      Box::new(return_type.clone()),
    );

    Ok(ASTExpression::Lambda(Box::new(ASTLambda::new(
      parameters,
      Box::new(body),
      return_type,
      lambda_type,
    ))))
  }

  /// <parameters> ::= <parameter> ("," <parameter>)*
  fn parameters(
    &mut self,
    name: &Token,
  ) -> IgnisParserResult<Vec<ASTVariable>> {
    self.consume(TokenType::LeftParen)?;

    let mut parameters: Vec<ASTVariable> = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if parameters.len() >= 255 {
          return Err(Box::new(ParserDiagnostic::new(
            ParserDiagnosticError::InvalidNumberOfArguments(255, parameters.len(), name.clone()),
          )));
        }

        let mut metadata = ASTMetadata::new(vec![]);

        if self.match_token(&[TokenType::Variadic]) {
          metadata.push(ASTMetadataFlags::Variadic);
        }

        let param = self.consume(TokenType::Identifier)?;

        self.consume(TokenType::Colon)?;

        let is_refence: bool = self.match_token(&[TokenType::Ampersand]);
        let is_mut: bool = self.match_token(&[TokenType::Mut]);
        let is_pointer: bool = self.match_token(&[TokenType::Asterisk]);

        let data_type = self.resolve_type()?;

        if is_refence {
          metadata.push(ASTMetadataFlags::Reference);
        }

        if is_mut {
          metadata.push(ASTMetadataFlags::Mutable);
        }

        if is_pointer {
          metadata.push(ASTMetadataFlags::Pointer);
        }

        parameters.push(ASTVariable::new(param, None, data_type, metadata));

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightParen)?;

    Ok(parameters)
  }

  fn is_lambda(&mut self) -> bool {
    let mut index = self.current + 1;

    if self.tokens[index].type_ == TokenType::RightParen {
      index += 1;

      if self.tokens.get(index).map_or(false, |t| t.type_ == TokenType::Colon) {
        index += 2;
      }

      return self.tokens.get(index).map_or(false, |t| t.type_ == TokenType::Arrow);
    }

    while let Some(token) = self.tokens.get(index) {
      match token.type_ {
        TokenType::Identifier => {
          index += 1;

          if self.tokens.get(index).map_or(false, |t| t.type_ != TokenType::Colon) {
            return false;
          }

          index += 2;

          if self.tokens.get(index).map_or(false, |t| t.type_ == TokenType::Comma) {
            index += 1;
            continue;
          }

          if self
            .tokens
            .get(index)
            .map_or(false, |t| t.type_ == TokenType::RightParen)
          {
            index += 1;
          }

          if self.tokens.get(index).map_or(false, |t| t.type_ == TokenType::Colon) {
            index += 2;
            break;
          }

          return false;
        },
        _ => return false,
      }
    }

    self.tokens.get(index).map_or(false, |t| t.type_ == TokenType::Arrow)
  }

  fn resolve_type(&mut self) -> IgnisParserResult<DataType> {
    let is_refence = self.match_token(&[TokenType::Ampersand]);
    let is_pointer = self.match_token(&[TokenType::Asterisk]);

    if self.match_token(&[TokenType::Void]) {
      if is_refence {
        return Ok(DataType::Reference(Box::new(DataType::Void)));
      }

      if is_pointer {
        return Ok(DataType::Pointer(Box::new(DataType::Void)));
      }

      return Ok(DataType::Void);
    }

    if self.match_token(&[TokenType::LeftParen]) {
      let mut value = self.function_type()?;

      if is_refence {
        value = DataType::Reference(Box::new(value));
      }

      if is_pointer {
        value = DataType::Pointer(Box::new(value));
      }

      return Ok(value);
    }

    if self.match_token(&[
      TokenType::Int8Type,
      TokenType::Int16Type,
      TokenType::Int32Type,
      TokenType::Int64Type,
      TokenType::UnsignedInt8Type,
      TokenType::UnsignedInt16Type,
      TokenType::UnsignedInt32Type,
      TokenType::UnsignedInt64Type,
      TokenType::Float32Type,
      TokenType::Float64Type,
      TokenType::StringType,
      TokenType::BooleanType,
      TokenType::CharType,
      TokenType::Unknown,
      TokenType::Identifier,
      TokenType::HexType,
      TokenType::BinaryType,
    ]) {
      let token: Token = self.previous();
      let mut data_type: DataType = DataType::from(&token.type_);

      if data_type == DataType::Pending {
        data_type = self.resolve_pending_type(&token);
      }

      if self.match_token(&[TokenType::LeftBrack]) {
        let mut size: Option<Token> = None;

        if self.check(TokenType::Int) {
          size.clone_from(&Some(self.consume(TokenType::Int)?));
        }

        self.consume(TokenType::RightBrack)?;
        data_type = DataType::Vector(Box::new(data_type), size);
      }

      if is_refence {
        data_type = DataType::Reference(Box::new(data_type));
      }

      if is_pointer {
        data_type = DataType::Pointer(Box::new(data_type));
      }

      return Ok(data_type);
    }

    let token = &self.peek();

    Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedType(
      token.clone(),
    ))))
  }

  fn resolve_pending_type(
    &self,
    token: &Token,
  ) -> DataType {
    // TODO: Ignis v0.3.0
    // if self.is_declared(ParserDeclaration::Class, token) {
    //   DataType::ClassType(token.lexeme.clone())
    // } else if self.is_declared(ParserDeclaration::Interface, token) {
    //   DataType::Interface(token.lexeme.clone())
    // } else if self.is_declared(ParserDeclaration::Enum, token) {
    //   DataType::Enum(token.lexeme.clone())
    // } else
    if self.is_declared(ParserDeclaration::Import, token) {
      DataType::PendingImport(token.lexeme.clone())
    } else if self.is_declared(ParserDeclaration::Record, token) {
      let record = match self
        .declarations
        .get(&ParserDeclaration::Record)
        .unwrap()
        .iter()
        .find(|r| {
          if let ParserDeclarationList::Record(name, _) = r {
            name == &token.lexeme
          } else {
            false
          }
        })
        .unwrap()
      {
        ParserDeclarationList::Record(name, data_type) => (name.clone(), data_type.clone()),
        _ => (String::from(""), Vec::new()),
      };

      DataType::Record(record.0, record.1)

    // TODO: Ignis v0.2.0
    // } else if let Some(g) = generic_parameters
    //   .iter()
    //   .find(|g| g.name.lexeme == token.lexeme)
    // {
    //   DataType::GenericType(GenericType::new(
    //     Box::new(DataType::Variable(g.name.lexeme.clone())),
    //     Vec::new(),
    //   ))
    } else {
      DataType::Pending
    }
  }

  fn function_type(&mut self) -> IgnisParserResult<DataType> {
    let mut parameters: Vec<DataType> = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if parameters.len() >= 255 {
          let token = &self.peek();
          return Err(Box::new(ParserDiagnostic::new(
            ParserDiagnosticError::InvalidNumberOfArguments(255, parameters.len(), token.clone()),
          )));
        }

        let name = self.advance();

        if name.type_ != TokenType::Identifier {
          let token = &self.peek();
          return Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(
            TokenType::Identifier,
            token.clone(),
          ))));
        }

        if !self.match_token(&[TokenType::Colon]) {
          let token = &self.peek();
          return Err(Box::new(ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(
            TokenType::Colon,
            self.peek(),
          ))));
        }

        parameters.push(self.resolve_type()?);

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightParen)?;
    self.consume(TokenType::Arrow)?;

    let return_type = self.resolve_type()?;

    Ok(DataType::Function(parameters, Box::new(return_type)))
  }

  fn consume(
    &mut self,
    kind: TokenType,
  ) -> IgnisParserResult<Token> {
    let token: Token = self.peek();

    if token.type_ == kind {
      return Ok(self.advance());
    }

    let token_previous = self.previous();

    let error = match kind {
      TokenType::Equal => {
        if self.context.contains(&IgnisParserContext::Const) {
          let name = self.get_previous_token_by_token_type(TokenType::Identifier);
          ParserDiagnostic::new(ParserDiagnosticError::UninitializedConstant(name.clone()))
        } else {
          ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(TokenType::Equal, token_previous.clone()))
        }
      },
      TokenType::SemiColon => {
        ParserDiagnostic::new(ParserDiagnosticError::ExpectedSemicolonAfterExpression(token_previous.clone()))
      },
      TokenType::Colon => ParserDiagnostic::new(ParserDiagnosticError::ExpectedType(token_previous.clone())),
      TokenType::Identifier => {
        if TokenType::get_keywords().contains(&token_previous.type_) {
          ParserDiagnostic::new(ParserDiagnosticError::UnexpectedKeyword(token_previous.clone()))
        } else {
          ParserDiagnostic::new(ParserDiagnosticError::ExpectedIdentifier(token.clone()))
        }
      },
      TokenType::QuestionMark => ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(
        TokenType::QuestionMark,
        token_previous.clone(),
      )),
      TokenType::Greater => {
        ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(TokenType::Greater, token_previous.clone()))
      },
      TokenType::LeftParen | TokenType::RightParen => {
        let expression = self.previous();

        ParserDiagnostic::new(ParserDiagnosticError::ExpectedAfterExpression(
          Box::new(kind.clone()),
          expression.clone(),
          Box::new(token.clone()),
        ))
      },
      TokenType::RightBrace => ParserDiagnostic::new(ParserDiagnosticError::ExpectedDelimiter(token)),
      _ => ParserDiagnostic::new(ParserDiagnosticError::ExpectedToken(kind.clone(), token_previous.clone())),
    };

    self.synchronize();

    Err(Box::new(error))
  }

  fn get_previous_token_by_token_type(
    &mut self,
    token_type: TokenType,
  ) -> Token {
    let mut index = 0;
    loop {
      if let Some(pos) = self.current.checked_sub(index) {
        let token = self.tokens[pos].clone();

        if token.type_ == token_type {
          return token;
        }
      } else {
        return self.tokens[0].clone();
      }

      index += 1;

      if self.current - index == 0 {
        return self.tokens[0].clone();
      }
    }
  }

  fn match_token(
    &mut self,
    kinds: &[TokenType],
  ) -> bool {
    for kind in kinds {
      if self.check(kind.clone()) {
        self.advance();
        return true;
      }
    }

    false
  }

  fn check(
    &mut self,
    kind: TokenType,
  ) -> bool {
    if self.is_at_end() {
      return false;
    }

    self.peek().type_ == kind
  }

  fn advance(&mut self) -> Token {
    if self.is_at_end() {
      return self.tokens[self.current - 1].clone();
    }

    self.current += 1;

    self.tokens[self.current - 1].clone()
  }

  fn previous(&mut self) -> Token {
    if self.current == 0 {
      self.tokens[self.current].clone()
    } else {
      self.tokens[self.current - 1].clone()
    }
  }

  fn peek(&mut self) -> Token {
    self.tokens[self.current].clone()
  }

  fn is_current_a_comment(&mut self) -> bool {
    let token = self.peek();

    matches!(
      token.type_,
      TokenType::Comment | TokenType::MultiLineComment | TokenType::DocComment
    )
  }

  fn is_at_end(&mut self) -> bool {
    if self.current >= self.tokens.len() {
      return true;
    }

    self.peek().type_ == TokenType::Eof
  }

  fn is_declared(
    &self,
    declaration: ParserDeclaration,
    token: &Token,
  ) -> bool {
    self.declarations.get(&declaration).unwrap().iter().any(|d| match d {
      ParserDeclarationList::Name(name) | ParserDeclarationList::Record(name, _) => name == &token.lexeme,
    })
  }
}
