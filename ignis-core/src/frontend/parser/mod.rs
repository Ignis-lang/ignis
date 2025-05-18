mod meta;

use std::collections::HashMap;

use colored::Colorize;
use ignis_ast::{
  expressions::{
    assign::ASTAssignment,
    binary::ASTBinary,
    call::ASTCall,
    cast::ASTCast,
    grouping::ASTGrouping,
    lambda::ASTLambda,
    literal::ASTLiteral,
    logical::ASTLogical,
    match_expression::{ASTMatchCase, ASTMatchExpression},
    member_access::ASTMemberAccess,
    meta::{ASTMeta, ASTMetaEntity},
    object_literal::ASTObject,
    spread::ASTSpread,
    ternary::ASTTernary,
    this::ASTThis,
    unary::ASTUnary,
    variable::ASTVariableExpression,
    vector::ASTVector,
    vector_access::ASTVectorAccess,
    ASTExpression,
  },
  metadata::{ASTMetadata, ASTMetadataFlags},
  statements::{
    block::ASTBlock,
    comment::{ASTComment, ASTCommentType},
    constant::ASTConstant,
    enum_statement::{ASTEnum, ASTEnumItem},
    r#extern::ASTExtern,
    for_of_statement::ASTForOf,
    for_statement::ASTFor,
    function::{ASTFunction, ASTGenericParameter},
    if_statement::ASTIf,
    import::{ASTImport, ASTImportSource, ASTImportSymbol},
    meta::ASTMetaStatement,
    method::ASTMethod,
    namespace::ASTNamespace,
    record::ASTRecord,
    return_::ASTReturn,
    type_alias::ASTTypeAlias,
    variable::ASTVariable,
    while_statement::ASTWhile,
    ASTStatement,
  },
};
use ignis_config::{DebugPrint, IgnisConfig};
use ignis_data_type::{DataType, GenericType, value::IgnisLiteralValue};
use ignis_token::{token_types::TokenType, token::Token};

use crate::diagnostics::{diagnostic_report::DiagnosticReport, message::DiagnosticMessage};

type IgnisParserResult<T> = Result<T, Box<DiagnosticMessage>>;

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
  Enum(String),
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
/// <generic-type> ::= "<" <type-parameter> ("," <type-parameter>)* ">"
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
/// <function> ::= "function" <identifier> <generic-type>? "(" <parameters>? ")" ":" <type> (<block> | ";")
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
/// <record> ::= "record" <identifier> <generic-type>? "{" <record-item>* "}"
/// <record-item> ::= <record-property> | <record-method>
/// <record-property> ::= <identifier> "?"? ":" <type> ("=" <expression>)? ";"
/// <record-method> ::= <identifier> <generic-type>? "?"? "(" <parameters> ")" ":" <type> ";"
///
/// # Enum syntax
/// <enum> ::= "enum" <identifier> <generic-type>? "{" (<enum-item> ","?)* "}"
/// <enum-item> ::= <identifier> | <identifier> "=" <expression> | <enum-complex-item>
/// <enum-complex-item> ::= <identifier> "(" <expression> ")"
///
/// # Type alias syntax
/// <type-alias> ::= "type" <identifier> <generic-type>? "=" <type> ";"
///
/// # Extern syntax
/// <extern> ::= "extern" (<qualified-identifier>) "{" <extern-item>* "}"
/// <extern-item> ::= <declaration>
///
/// # Namespace syntax
/// <namespace> ::= "namespace" <qualified-identifier> "{" <namespace-item>* "}"
/// <namespace-item> ::= <function> | <const> | <record> | <class> | <enum> | <type-alias> | <interface> | <declare>
///
/// # Declare syntax
/// <declare> ::= "declare" <identifier> ":" <type>
///
/// # Meta syntax
/// <meta> ::= "meta" <identifier> <generic-type>? ("(" <parameters> ")")? ";"
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
/// <call-suffix> ::= <generics>? <arguments> | <generics>? <member-access>
/// <generics> ::= "<" <type-list> ("," <types-lits>)* ">"
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
/// <lambda> ::= <generic-type>? "(" <parameters>? ")" ":" <type> "->" (<expression> | <block>)
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
pub struct IgnisParser<'a> {
  config: &'a IgnisConfig,
  context: Vec<IgnisParserContext>,
  declarations: StructDeclaration,
  tokens: &'a Vec<Token>,
  current: usize,
  pub diagnostics: Vec<DiagnosticMessage>,
}

impl<'a> IgnisParser<'a> {
  pub fn new(
    config: &'a IgnisConfig,
    tokens: &'a Vec<Token>,
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
  ) -> (Vec<ASTStatement>, Vec<DiagnosticReport>) {
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

    if self.config.debug.contains(&DebugPrint::Ast) && !self.config.quiet {
      println!("Pre-Meta AST: {:#?}", statements);
    }

    let mut meta_processor = meta::IgnisMetaProcessor::new(statements);
    meta_processor.process();

    if self.config.debug.contains(&DebugPrint::Ast) && !self.config.quiet {
      println!("Post-Meta AST: {:#?}", meta_processor.new_ast);
    }

    let mut diagnostics: Vec<DiagnosticReport> = vec![];

    for diagnostic in &self.diagnostics {
      diagnostics.push(diagnostic.report());
    }

    for diagnostic in &meta_processor.diagnostics {
      diagnostics.push(diagnostic.report());
    }

    (meta_processor.new_ast, diagnostics)
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
        | TokenType::Inline
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

  /// <declaration> ::= <function> | <import> | <export> | <inline> | <const> | <record> | <extern> | <declare> | <meta> | <namespace> | <type-alias>
  fn declaration(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.peek();

    match token.type_ {
      TokenType::Function => self.function(),
      TokenType::Import => self.import(),
      TokenType::Export => self.export(),
      TokenType::Inline => self.inline(),
      TokenType::Const => self.const_(),
      TokenType::Record => self.record(),
      TokenType::Extern => self.extern_(),
      TokenType::Declare => self.declare(),
      TokenType::Meta => self.meta(),
      TokenType::Namespace => self.namespace(),
      TokenType::Type => self.type_alias(),
      TokenType::Enum => self.enum_statement(),
      _ => match self.statement() {
        Ok(statement) => Ok(statement),
        Err(error) => {
          self.synchronize();
          Err(error)
        },
      },
    }
  }

  fn inline(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Inline)?;

    let mut result = match self.peek().type_ {
      TokenType::Function => self.function(),
      TokenType::Const => self.const_(),
      _ => Err(Box::new(DiagnosticMessage::ExpectedToken(TokenType::Function, self.peek()))),
    };

    if let Ok(ok) = &mut result {
      ok.push_flag(ASTMetadataFlags::Inline);
    }

    result
  }

  /// <function> ::= "function" <identifier> <generic-type>? "(" <parameters>? ")" ":" <type> (<block> | ";")
  fn function(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Function)?;

    let name: Token = self.consume(TokenType::Identifier)?;

    let generic_parameters = self.resolve_generic_params()?;

    let parameters = self.parameters(&name, &generic_parameters)?;

    self.consume(TokenType::Colon)?;

    let return_type = self.resolve_type(&generic_parameters)?;

    let mut body: Vec<ASTStatement> = Vec::new();

    if self.check(TokenType::LeftBrace) {
      body.push(self.block()?);
    } else if !self.match_token(&[TokenType::SemiColon]) {
      return Err(Box::new(DiagnosticMessage::ExpectedSemicolonAfterExpression(self.peek())));
    }

    let mut metadata = ASTMetadata::default();

    Ok(ASTStatement::Function(Box::new(ASTFunction::new(
      name,
      parameters,
      body,
      return_type,
      metadata,
      generic_parameters,
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
      let value = self.consume(TokenType::Identifier)?;

      self
        .declarations
        .get_mut(&ParserDeclaration::Import)
        .unwrap()
        .push(ParserDeclarationList::Name(value.lexeme.clone()));

      alias = Some(value);
    } else {
      self
        .declarations
        .get_mut(&ParserDeclaration::Import)
        .unwrap()
        .push(ParserDeclarationList::Name(name.lexeme.clone()));
    }

    Ok(ASTImportSymbol::new(name, alias))
  }

  /// <export> ::= "export" <declaration> | "export" <identifier> ";"
  fn export(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Export)?;

    let mut result = match self.peek().type_ {
      TokenType::Function => self.function(),
      TokenType::Const => self.const_(),
      TokenType::Record => self.record(),
      TokenType::Extern => self.extern_(),
      TokenType::Declare => self.declare(),
      TokenType::Meta => self.meta(),
      TokenType::Namespace => self.namespace(),
      TokenType::Type => self.type_alias(),
      TokenType::Enum => self.enum_statement(),
      _ => Err(Box::new(DiagnosticMessage::ExpectedToken(TokenType::Function, self.peek()))),
    };

    if let Ok(ok) = &mut result {
      ok.push_flag(ASTMetadataFlags::Export);
    }

    result
  }

  /// <const> ::= "const" <identifier> ":" <type> "=" <expression> ";"
  fn const_(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Const)?;

    self.context.push(IgnisParserContext::Const);

    let name = self.consume(TokenType::Identifier)?;

    self.consume(TokenType::Colon)?;

    let data_type = self.resolve_type(&[])?;

    self.consume(TokenType::Equal)?;

    let value = self.expression()?;

    self.consume(TokenType::SemiColon)?;

    self.context.pop();

    let mut metadata = ASTMetadata::new(vec![]);

    Ok(ASTStatement::Constant(Box::new(ASTConstant::new(
      name,
      Box::new(value),
      data_type,
      metadata,
    ))))
  }

  /// <record> ::= "record" <identifier> "{" <record-item>* "}"
  fn record(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Record)?;
    let mut generic_parameters: Vec<ASTGenericParameter> = Vec::new();

    if self.match_token(&[TokenType::Less]) {
      loop {
        let name = self.consume(TokenType::Identifier)?;
        let mut constraints: Vec<DataType> = Vec::new();

        if self.match_token(&[TokenType::As]) {
          let token = self.advance();
          let mut kind = DataType::from(&token.type_);

          if matches!(kind, DataType::Pending) {
            kind = self.resolve_pending_type(&token, &generic_parameters);
          }

          constraints.push(kind);
        }

        generic_parameters.push(ASTGenericParameter::new(name, constraints));
        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }

      self.consume(TokenType::Greater)?;
    }

    let name: Token = if self.check_if_type() {
      self.advance()
    } else {
      self.consume(TokenType::Identifier)?
    };

    let mut data_type: Vec<(String, DataType)> = vec![];

    self.consume(TokenType::LeftBrace)?;

    let mut items: Vec<ASTStatement> = vec![];

    while !self.check(TokenType::RightBrace) {
      if self.is_current_a_comment() {
        self.advance();
        continue;
      }

      let mut meta = None;

      if self.check(TokenType::Hash) {
        meta = Some(self.meta_expression(false)?);
      }

      let mut item = self.record_item(&name, &generic_parameters, &mut data_type)?;

      if let Some(meta) = &mut meta {
        if let ASTExpression::MetaEntity(meta) = meta {
          meta.entity = Some(item);
          item = ASTStatement::Expression(Box::new(ASTExpression::MetaEntity(meta.clone())));
        }
      }

      items.push(item);
    }

    self.consume(TokenType::RightBrace)?;

    let mut metadata: ASTMetadata = ASTMetadata::default();

    self
      .declarations
      .get_mut(&ParserDeclaration::Record)
      .unwrap()
      .push(ParserDeclarationList::Record(name.lexeme.clone(), data_type));

    Ok(ASTStatement::Record(Box::new(ASTRecord::new(
      name,
      items,
      metadata,
      generic_parameters,
    ))))
  }

  /// <record-item> ::= <meta-expression>? <record-property> | <record-method>
  fn record_item(
    &mut self,
    record: &Token,
    generic_parameters: &[ASTGenericParameter],
    record_type: &mut Vec<(String, DataType)>,
  ) -> IgnisParserResult<ASTStatement> {
    let name = self.consume(TokenType::Identifier)?;

    let mut item_generics: Vec<ASTGenericParameter> = self.resolve_generic_params()?;
    item_generics.append(&mut generic_parameters.to_vec());

    let is_optional = self.match_token(&[TokenType::QuestionMark]);
    if self.match_token(&[TokenType::Colon]) {
      return self.record_property(name, is_optional, record_type, &item_generics);
    }

    if self.check(TokenType::LeftParen) {
      return self.record_method(record, name, is_optional, record_type, &item_generics);
    }

    Err(Box::new(DiagnosticMessage::ExpectedToken(TokenType::Colon, self.peek())))
  }

  /// <record-property> ::= <identifier> "?"? ":" <type> ("=" <expression>)? ";"
  fn record_property(
    &mut self,
    name: Token,
    is_optional: bool,
    record_type: &mut Vec<(String, DataType)>,
    generic_parameters: &[ASTGenericParameter],
  ) -> IgnisParserResult<ASTStatement> {
    let mut data_type = self.resolve_type(generic_parameters)?;

    if is_optional {
      data_type = DataType::Optional(Box::new(data_type));
    }

    self.consume(TokenType::SemiColon)?;

    let mut metadata = ASTMetadata::new(vec![ASTMetadataFlags::Property]);

    if is_optional {
      metadata.push(ASTMetadataFlags::Optional);
    }

    record_type.push((name.lexeme.clone(), data_type.clone()));

    Ok(ASTStatement::Variable(Box::new(ASTVariable::new(
      name, None, data_type, metadata,
    ))))
  }

  /// <record-method> ::= <identifier> <generic-type>? "(" <parameters> ")" "?"? ":" <type> ";"
  fn record_method(
    &mut self,
    record: &Token,
    name: Token,
    is_optional: bool,
    record_type: &mut Vec<(String, DataType)>,
    generic_parameters: &[ASTGenericParameter],
  ) -> IgnisParserResult<ASTStatement> {
    let mut method_generic_parameters = self.resolve_generic_params()?;
    method_generic_parameters.append(&mut generic_parameters.to_vec());

    let parameters = self.parameters(&name, &method_generic_parameters)?;

    self.consume(TokenType::Colon)?;

    let return_type = self.resolve_type(&method_generic_parameters)?;

    self.consume(TokenType::SemiColon)?;

    let mut metadata = ASTMetadata::new(vec![ASTMetadataFlags::Method]);

    if is_optional {
      metadata.push(ASTMetadataFlags::Optional);
    }

    record_type.push((name.lexeme.clone(), return_type.clone()));

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
  fn extern_(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Extern)?;

    self.context.push(IgnisParserContext::Extern);

    let name = self.consume(TokenType::Identifier)?;

    self.consume(TokenType::LeftBrace)?;

    let mut items: Vec<ASTStatement> = vec![];

    while !self.check(TokenType::RightBrace) {
      if self.check(TokenType::Comment) || self.check(TokenType::DocComment) || self.check(TokenType::MultiLineComment) {
        self.advance();
        continue;
      }

      items.push(self.extern_item()?);
    }

    self.consume(TokenType::RightBrace)?;

    let mut metadata = ASTMetadata::new(vec![]);

    self.context.pop();

    Ok(ASTStatement::Extern(Box::new(ASTExtern::new(name, items, metadata))))
  }

  /// <extern-item> ::= <declaration>
  fn extern_item(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.peek();

    let mut result = match token.type_ {
      TokenType::Function => self.function(),
      TokenType::Const => self.const_(),
      TokenType::Record => self.record(),
      TokenType::Declare => self.declare(),
      TokenType::Hash | TokenType::At => self.statement(),
      TokenType::Type => self.type_alias(),
      TokenType::Enum => self.enum_statement(),
      _ => Err(Box::new(DiagnosticMessage::ExpectedToken(TokenType::Function, self.peek()))),
    };

    if let Ok(ok) = &mut result {
      ok.push_flag(ASTMetadataFlags::ExternMember);
    }

    result
  }

  /// <declare> ::= "declare" <identifier> ":" <type>
  fn declare(&mut self) -> IgnisParserResult<ASTStatement> {
    todo!()
  }

  /// <meta> ::= "meta" <identifier> ("(" <parameters> ")")? ";"
  fn meta(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Meta)?;
    let meta = self.consume(TokenType::Identifier)?;

    let generic_parameters = self.resolve_generic_params()?;

    let mut parameters = vec![];

    if self.check(TokenType::LeftParen) {
      parameters = self.parameters(&meta, &generic_parameters)?;
    }

    self.consume(TokenType::SemiColon)?;

    let metadata = ASTMetadata::default();

    Ok(ASTStatement::Meta(Box::new(ASTMetaStatement::new(
      meta,
      parameters,
      metadata,
      generic_parameters,
    ))))
  }

  /// <namespace> ::= "namespace" <qualified-identifier> "{" <namespace-item>* "}"
  fn namespace(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Namespace)?;

    let name = self.consume(TokenType::Identifier)?;

    self.consume(TokenType::LeftBrace)?;

    let mut members: Vec<ASTStatement> = vec![];

    while !self.check(TokenType::RightBrace) {
      if self.check(TokenType::Comment) || self.check(TokenType::MultiLineComment) || self.check(TokenType::DocComment) {
        self.consume(TokenType::Comment)?;
        continue;
      }

      members.push(self.namespace_item()?);
    }

    self.consume(TokenType::RightBrace)?;

    let metadata = ASTMetadata::default();

    Ok(ASTStatement::Namespace(Box::new(ASTNamespace::new(name, members, metadata))))
  }

  /// <namespace-item> ::= <function> | <const> | <record> | <class> | <enum> | <type-alias> | <interface> | <declare>
  fn namespace_item(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.peek();

    let mut result = match token.type_ {
      TokenType::Function => self.function(),
      TokenType::Const => self.const_(),
      TokenType::Record => self.record(),
      TokenType::Declare => self.declare(),
      TokenType::Identifier => {
        if TokenType::get_keyword_from_string(token.lexeme.as_str()).is_some() {
          return Err(Box::new(DiagnosticMessage::UnexpectedKeyword(token.clone())));
        }

        self.statement()
      },
      TokenType::Enum => self.enum_statement(),
      _ => Err(Box::new(DiagnosticMessage::ExpectedToken(TokenType::Function, self.peek()))),
    };

    if let Ok(ok) = &mut result {
      ok.push_flag(ASTMetadataFlags::NamespaceMember);
    }

    result
  }

  // <type-alias> ::= "type" <identifier> <generic-type>? "=" <type> ";"
  fn type_alias(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Type)?;

    let name = self.consume(TokenType::Identifier)?;

    let generic_parameters = self.resolve_generic_params()?;

    self.consume(TokenType::Equal)?;

    let value = self.resolve_type(&generic_parameters)?;

    self.consume(TokenType::SemiColon)?;

    let mut metadata = ASTMetadata::new(vec![]);

    Ok(ASTStatement::TypeAlias(Box::new(ASTTypeAlias::new(
      name,
      Box::new(value),
      metadata,
      generic_parameters,
    ))))
  }

  /// <enum> ::= "enum" <identifier> <generic-type>? "{" (<enum-item> ","?)* "}"
  fn enum_statement(&mut self) -> IgnisParserResult<ASTStatement> {
    self.consume(TokenType::Enum)?;

    let name = self.consume(TokenType::Identifier)?;

    let generic_parameters = self.resolve_generic_params()?;

    self.consume(TokenType::LeftBrace)?;

    let mut members: Vec<ASTEnumItem> = vec![];

    while !self.check(TokenType::RightBrace) {
      members.push(self.enum_item()?);

      if !self.match_token(&[TokenType::Comma]) {
        break;
      }
    }

    self.consume(TokenType::RightBrace)?;

    let metadata = ASTMetadata::new(vec![]);

    self
      .declarations
      .get_mut(&ParserDeclaration::Enum)
      .unwrap()
      .push(ParserDeclarationList::Enum(name.lexeme.clone()));

    Ok(ASTStatement::Enum(Box::new(ASTEnum::new(
      name,
      members,
      metadata,
      generic_parameters,
    ))))
  }

  /// <enum-item> ::= <identifier> | <identifier> "=" <expression> | <enum-complex-item>
  fn enum_item(&mut self) -> IgnisParserResult<ASTEnumItem> {
    let name = self.consume(TokenType::Identifier)?;
    let metadata = ASTMetadata::new(vec![]);
    let mut value = None;
    let mut data_type = DataType::Int32;

    if self.match_token(&[TokenType::Equal]) {
      let expression = self.expression()?;

      data_type = expression.clone().into();

      value = Some(expression);
    }

    Ok(ASTEnumItem::new(name, value, data_type, metadata))
  }

  /// <enum-complex-item> ::= <identifier> "(" <expression> ")"

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

    self.consume(TokenType::LeftParen)?;

    let condition = self.expression()?;

    self.consume(TokenType::RightParen)?;

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

    let mut type_annotation = self.resolve_type(&[])?;

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
      let mut size: Option<usize> = None;
      if self.match_token(&[TokenType::Int]) {
        let value = self.consume(TokenType::Int)?;
        size.clone_from(&Some(value.lexeme.parse().unwrap()));
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
      Err(Box::new(DiagnosticMessage::ExpectedExpression(token.clone())))
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
      _ => Err(Box::new(DiagnosticMessage::ExpectedPattern(token.clone()))),
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
      let mut operator = self.previous();
      let mut left = expression.clone();
      let mut right = self.assignment()?;

      match operator.type_.clone() {
        TokenType::AddAssign => {
          let token = Token::new(
            TokenType::Plus,
            "+".to_string(),
            operator.line,
            operator.column,
            operator.file_name.clone(),
          );
          let type_: DataType = expression.clone().into();

          right = ASTExpression::Binary(Box::new(ASTBinary::new(
            Box::new(expression.clone()),
            token,
            Box::new(right.clone()),
            type_,
          )));
        },
        TokenType::SubtractAssign => {
          let token = Token::new(
            TokenType::Minus,
            "-".to_string(),
            operator.line,
            operator.column,
            operator.file_name.clone(),
          );
          let type_: DataType = expression.clone().into();

          right = ASTExpression::Binary(Box::new(ASTBinary::new(
            Box::new(expression.clone()),
            token,
            Box::new(right.clone()),
            type_,
          )));
        },
        TokenType::MulAssign => {
          let token = Token::new(
            TokenType::Asterisk,
            "*".to_string(),
            operator.line,
            operator.column,
            operator.file_name.clone(),
          );
          let type_: DataType = expression.clone().into();

          right = ASTExpression::Binary(Box::new(ASTBinary::new(
            Box::new(expression.clone()),
            token,
            Box::new(right.clone()),
            type_,
          )));
        },
        TokenType::DivAssign => {
          let token = Token::new(
            TokenType::Slash,
            "/".to_string(),
            operator.line,
            operator.column,
            operator.file_name.clone(),
          );
          let type_: DataType = expression.clone().into();

          right = ASTExpression::Binary(Box::new(ASTBinary::new(
            Box::new(expression.clone()),
            token,
            Box::new(right.clone()),
            type_,
          )));
        },
        TokenType::ModAssign => {
          let token = Token::new(
            TokenType::Mod,
            "%".to_string(),
            operator.line,
            operator.column,
            operator.file_name.clone(),
          );
          let type_: DataType = expression.clone().into();

          right = ASTExpression::Binary(Box::new(ASTBinary::new(
            Box::new(expression.clone()),
            token,
            Box::new(right.clone()),
            type_,
          )));
        },
        TokenType::LeftShiftAssign => {
          let token = Token::new(
            TokenType::LeftShift,
            "<<".to_string(),
            operator.line,
            operator.column,
            operator.file_name.clone(),
          );
          let type_: DataType = expression.clone().into();

          right = ASTExpression::Binary(Box::new(ASTBinary::new(
            Box::new(expression.clone()),
            token,
            Box::new(right.clone()),
            type_,
          )));
        },
        TokenType::RightShiftAssign => {
          let token = Token::new(
            TokenType::RightShift,
            ">>".to_string(),
            operator.line,
            operator.column,
            operator.file_name.clone(),
          );
          let type_: DataType = expression.clone().into();

          right = ASTExpression::Binary(Box::new(ASTBinary::new(
            Box::new(expression.clone()),
            token,
            Box::new(right.clone()),
            type_,
          )));
        },
        _ => (),
      };

      expression = ASTExpression::Assigment(Box::new(ASTAssignment::new(operator, Box::new(left), Box::new(right))));
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
      TokenType::Greater,
      TokenType::GreaterEqual,
      TokenType::Less,
      TokenType::LessEqual,
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
      let type_: DataType = self.resolve_type(&[])?;

      expression = ASTExpression::Cast(Box::new(ASTCast::new(operator, type_, Box::new(expression))));
    }

    Ok(expression)
  }

  /// <unary> ::= ( "++" | "--" | "-" | "!" | "~" )* <postfix>
  fn unary(&mut self) -> IgnisParserResult<ASTExpression> {
    let mut expression: ASTExpression = self.postfix()?;

    while self.match_token(&[
      TokenType::Increment,
      TokenType::Decrement,
      TokenType::Minus,
      TokenType::Bang,
      TokenType::Tilde,
    ]) {
      let operator: Token = self.previous();
      let right = self.postfix()?;
      let type_: DataType = expression.clone().into();

      expression = ASTExpression::Unary(Box::new(ASTUnary::new(operator, Box::new(right), type_, false)));
    }

    Ok(expression)
  }

  /// <postfix> ::= <primary> <generics>? ( ("++" | "--") | <call-suffix> )* | <vector-access>
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
      let mut generics: Vec<DataType> = self.generic_arguments()?;

      expression = match operator.type_ {
        TokenType::Dot => self.member_access(&expression, &mut generics, false)?,
        TokenType::DoubleColon => self.member_access(&expression, &mut generics, true)?,
        TokenType::LeftParen => self.call_suffix(&expression, &mut generics)?,
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

  /// <call-suffix> ::= <generics>? <arguments> | <generics>? <member-access>
  fn call_suffix(
    &mut self,
    previous_expression: &ASTExpression,
    generics: &mut Vec<DataType>,
  ) -> IgnisParserResult<ASTExpression> {
    let mut new_generics = self.generic_arguments()?;
    generics.append(&mut new_generics);

    let arguments: Vec<ASTExpression> = self.arguments()?;

    let mut expression = ASTExpression::Call(Box::new(ASTCall::new(
      Into::into(previous_expression),
      Box::new(previous_expression.clone()),
      arguments,
      generics.clone(),
    )));

    while self.match_token(&[TokenType::Dot, TokenType::LeftParen, TokenType::DoubleColon]) {
      expression = match self.previous().type_ {
        TokenType::Dot => self.member_access(&expression, generics, false)?,
        TokenType::DoubleColon => self.member_access(&expression, generics, true)?,
        TokenType::LeftParen => self.call_suffix(&expression, generics)?,
        _ => unreachable!(),
      };
    }

    return Ok(expression);
  }

  fn generic_arguments(&mut self) -> Result<Vec<DataType>, Box<DiagnosticMessage>> {
    let mut generics: Vec<DataType> = vec![];

    if self.match_token(&[TokenType::Less]) {
      loop {
        if self.check(TokenType::Greater) {
          break;
        }

        generics.push(self.resolve_type(&[])?);

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }

      self.consume(TokenType::Greater)?;
    }

    return Ok(generics);
  }

  /// <arguments> ::= "(" <expression> ("," <expression>)* ")"
  fn arguments(&mut self) -> IgnisParserResult<Vec<ASTExpression>> {
    let mut arguments = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if arguments.len() >= 255 {
          return Err(Box::new(DiagnosticMessage::InvalidNumberOfArguments(
            255,
            arguments.len(),
            self.peek().clone(),
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

  /// <member-access> ::= ("." | "::") <identifier> ("<" <type-list> ("," <types-lits>)* ">")?
  fn member_access(
    &mut self,
    previous_expression: &ASTExpression,
    generics: &mut Vec<DataType>,
    is_namespace: bool,
  ) -> IgnisParserResult<ASTExpression> {
    let member_name = self.consume(TokenType::Identifier)?;

    let mut new_generics = self.generic_arguments()?;

    let mut metadata = ASTMetadata::new(vec![]);

    if is_namespace {
      metadata.push(ASTMetadataFlags::NamespaceMember);
    }

    let mut expression = ASTExpression::MemberAccess(Box::new(ASTMemberAccess::new(
      Box::new(previous_expression.clone()),
      Box::new(member_name),
      metadata,
      generics.clone(),
    )));

    while self.match_token(&[TokenType::Dot, TokenType::LeftParen, TokenType::DoubleColon]) {
      let token = self.previous();

      if new_generics.len() > 0 && token.type_ == TokenType::Dot {
        return Err(Box::new(DiagnosticMessage::UnexpectedGenerics(token)));
      }

      expression = match token.type_ {
        TokenType::Dot => self.member_access(&expression, &mut new_generics, false)?,
        TokenType::DoubleColon => self.member_access(&expression, &mut new_generics, true)?,
        TokenType::LeftParen => self.call_suffix(&expression, &mut new_generics)?,
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
      TokenType::Hash => self.meta_expression(true),
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

        let value: IgnisLiteralValue = IgnisLiteralValue::from((token.type_.clone(), token.lexeme.clone()));

        Ok(ASTExpression::Literal(Box::new(ASTLiteral::new(value, token.clone()))))
      },
      TokenType::LeftBrack => self.vector(),
      _ => Err(Box::new(DiagnosticMessage::ExpectedExpression(token.clone()))),
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

    let mut properties: Vec<(Token, ASTExpression)> = vec![];
    let mut methods: Vec<ASTMethod> = vec![];
    let mut data_type: Vec<(String, DataType)> = vec![];

    loop {
      if self.check(TokenType::RightBrace) {
        break;
      }

      if self.is_current_a_comment() {
        self.advance();
        continue;
      }

      let _ = self.object_item(&mut properties, &mut methods, &mut data_type)?;

      if !self.match_token(&[TokenType::Comma]) {
        break;
      }
    }

    self.consume(TokenType::RightBrace)?;

    Ok(ASTExpression::Object(Box::new(ASTObject::new(
      token,
      properties,
      methods,
      DataType::Object(data_type),
    ))))
  }

  /// <object-item> ::= (<object-property> | <object-method>) ","?
  fn object_item(
    &mut self,
    properties: &mut Vec<(Token, ASTExpression)>,
    methods: &mut Vec<ASTMethod>,
    data_type: &mut Vec<(String, DataType)>,
  ) -> IgnisParserResult<()> {
    let name = self.consume(TokenType::Identifier)?;

    let mut generic_parameters: Vec<ASTGenericParameter> = Vec::new();

    if self.match_token(&[TokenType::Less]) {
      loop {
        let name = self.consume(TokenType::Identifier)?;
        let mut constraints: Vec<DataType> = Vec::new();

        if self.match_token(&[TokenType::As]) {
          loop {
            constraints.push(DataType::from(&self.peek().type_));

            if !self.match_token(&[TokenType::Comma]) {
              break;
            }
          }
        }

        generic_parameters.push(ASTGenericParameter::new(name, constraints));

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }

      self.consume(TokenType::Greater)?;
    };

    if self.check(TokenType::LeftParen) {
      let method = self.object_method(&name, &generic_parameters)?;
      let type_ = DataType::Function(
        method
          .parameters
          .iter()
          .map(|p| p.type_annotation.clone())
          .collect::<Vec<DataType>>(),
        Box::new(method.return_type.clone()),
      );

      data_type.push((name.lexeme.clone(), type_));

      methods.push(method);
      return Ok(());
    }

    let property = self.object_property(&name)?;

    data_type.push((name.lexeme.clone(), property.2.clone()));

    properties.push((property.0, property.1));

    return Ok(());
  }

  /// <object-property> ::= <identifier> ":" <expression>
  fn object_property(
    &mut self,
    name: &Token,
  ) -> IgnisParserResult<(Token, ASTExpression, DataType)> {
    let initializer = if self.match_token(&[TokenType::Colon]) {
      self.expression()?
    } else {
      ASTExpression::Variable(Box::new(ASTVariableExpression::new(
        name.clone(),
        DataType::Variable(name.lexeme.clone(), Box::new(DataType::Pending)),
        ASTMetadata::new(vec![]),
      )))
    };

    let type_annotation = initializer.clone().into();

    Ok((name.clone(), initializer, type_annotation))
  }

  /// <object-method> ::= <identifier> "(" <parameters>? ")" ":" <type>  <block>
  fn object_method(
    &mut self,
    name: &Token,
    generic_parameters: &[ASTGenericParameter],
  ) -> IgnisParserResult<ASTMethod> {
    let arguments = self.parameters(&name, generic_parameters)?;

    self.consume(TokenType::Colon)?;

    let return_type = self.resolve_type(generic_parameters)?;

    let body = if let ASTStatement::Block(block) = self.block()? {
      block.clone()
    } else {
      Box::new(ASTBlock::new(vec![]))
    };

    let metadata = ASTMetadata::new(vec![ASTMetadataFlags::ObjectMember]);

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
  fn meta_expression(
    &mut self,
    include_entity: bool,
  ) -> IgnisParserResult<ASTExpression> {
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

    let entity: Option<ASTStatement> = if !self.check(TokenType::SemiColon) && include_entity {
      Some(self.meta_entity()?)
    } else {
      None
    };

    Ok(ASTExpression::MetaEntity(Box::new(ASTMetaEntity::new(meta, entity))))
  }

  fn meta_entity(&mut self) -> IgnisParserResult<ASTStatement> {
    let token = self.peek();

    match token.type_ {
      TokenType::Function => self.function(),
      TokenType::Type => self.type_alias(),
      TokenType::Namespace => self.namespace(),
      TokenType::Const => self.const_(),
      TokenType::Record => self.record(),
      TokenType::Declare => self.declare(),
      TokenType::Identifier => {
        if TokenType::get_keyword_from_string(token.lexeme.as_str()).is_some() {
          return Err(Box::new(DiagnosticMessage::UnexpectedKeyword(token.clone())));
        }

        self.statement()
      },
      TokenType::Export => self.export(),
      TokenType::Extern => self.extern_(),
      _ => Err(Box::new(DiagnosticMessage::ExpectedToken(TokenType::Function, self.peek()))),
    }
  }

  /// <lambda> ::= <generic-type>? "(" <parameters>? ")" ":" <type> "->" (<expression> | <block>)
  fn lambda(&mut self) -> IgnisParserResult<ASTExpression> {
    let token = self.previous();
    let generic_parameters = self.resolve_generic_params()?;

    let parameters = self.parameters(&token, &generic_parameters)?;

    self.consume(TokenType::Colon)?;

    let return_type = self.resolve_type(&generic_parameters)?;

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
      generic_parameters.clone(),
    ))))
  }

  /// <parameters> ::= <parameter> ("," <parameter>)*
  fn parameters(
    &mut self,
    name: &Token,
    generic_parameters: &[ASTGenericParameter],
  ) -> IgnisParserResult<Vec<ASTVariable>> {
    self.consume(TokenType::LeftParen)?;

    let mut parameters: Vec<ASTVariable> = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if parameters.len() >= 255 {
          return Err(Box::new(DiagnosticMessage::InvalidNumberOfArguments(
            255,
            parameters.len(),
            name.clone(),
          )));
        }

        let mut metadata = ASTMetadata::new(vec![ASTMetadataFlags::Parameter, ASTMetadataFlags::Declaration]);

        if self.match_token(&[TokenType::Variadic]) {
          metadata.push(ASTMetadataFlags::Variadic);
        }

        let param = self.consume(TokenType::Identifier)?;

        if self.match_token(&[TokenType::QuestionMark]) {
          metadata.push(ASTMetadataFlags::Optional);
        }

        self.consume(TokenType::Colon)?;

        let is_refence: bool = self.match_token(&[TokenType::Ampersand]);
        let is_mut: bool = self.match_token(&[TokenType::Mut]);
        let is_pointer: bool = self.match_token(&[TokenType::Asterisk]);

        let data_type = self.resolve_type(generic_parameters)?;

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

  fn resolve_type(
    &mut self,
    generic_parameters: &[ASTGenericParameter],
  ) -> IgnisParserResult<DataType> {
    let is_refence = self.match_token(&[TokenType::Ampersand]);
    let is_pointer = self.match_token(&[TokenType::Asterisk]);

    for generic in generic_parameters {
      if self.peek().lexeme == generic.name.lexeme {
        let value = DataType::GenericType(GenericType::new(
          Box::new(DataType::Variable(generic.name.lexeme.clone(), Box::new(DataType::Unknown))),
          generic.constraints.clone(),
        ));
        self.advance();

        if is_refence {
          return Ok(DataType::Reference(Box::new(value)));
        }

        if is_pointer {
          return Ok(DataType::Pointer(Box::new(value)));
        }

        if self.match_token(&[TokenType::LeftBrack]) {
          let mut size: Option<usize> = None;

          if self.match_token(&[TokenType::Int]) {
            let value = self.consume(TokenType::Int)?;
            size.clone_from(&Some(value.lexeme.parse().unwrap()));
          }

          self.consume(TokenType::RightBrack)?;

          return Ok(DataType::Vector(Box::new(value), size));
        }

        return Ok(value);
      }
    }

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
      let mut value = self.function_type(generic_parameters)?;

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
        data_type = self.resolve_pending_type(&token, generic_parameters);
      }

      if self.match_token(&[TokenType::LeftBrack]) {
        let mut size: Option<usize> = None;

        if self.check(TokenType::Int) {
          let value = self.consume(TokenType::Int)?;
          size.clone_from(&Some(value.lexeme.parse().unwrap()));
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

      if matches!(self.peek().type_, TokenType::Pipe | TokenType::Ampersand) {
        let is_union: bool = self.match_token(&[TokenType::Pipe]);

        let mut members: Vec<DataType> = vec![data_type];

        loop {
          let token: Token = self.peek();

          let mut type_: DataType = DataType::from(&token.type_);

          if self.match_token(&[TokenType::LeftBrack]) {
            self.consume(TokenType::RightBrack)?;
            type_ = DataType::Vector(Box::new(type_), None);
          }

          self.advance();
          members.push(type_);

          if !self.match_token(&[TokenType::Pipe, TokenType::Ampersand]) {
            break;
          }
        }

        if is_union {
          data_type = DataType::UnionType(members);
        } else {
          data_type = DataType::IntersectionType(members);
        }
      }

      return Ok(data_type);
    }

    let token = &self.peek();

    Err(Box::new(DiagnosticMessage::ExpectedType(token.clone())))
  }

  fn resolve_pending_type(
    &self,
    token: &Token,
    generic_parameters: &[ASTGenericParameter],
  ) -> DataType {
    // TODO: Ignis v0.3.0
    // if self.is_declared(ParserDeclaration::Class, token) {
    //   DataType::ClassType(token.lexeme.clone())
    // } else if self.is_declared(ParserDeclaration::Interface, token) {
    //   DataType::Interface(token.lexeme.clone())
    if self.is_declared(ParserDeclaration::Enum, token) {
      DataType::Enum(token.lexeme.clone(), Box::new(DataType::Pending))
    } else if self.is_declared(ParserDeclaration::Import, token) {
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
    } else if let Some(g) = generic_parameters.iter().find(|g| g.name.lexeme == token.lexeme) {
      DataType::GenericType(GenericType::new(
        Box::new(DataType::Variable(g.name.lexeme.clone(), Box::new(DataType::Unknown))),
        Vec::new(),
      ))
    } else {
      DataType::Pending
    }
  }

  fn function_type(
    &mut self,
    generic_parameters: &[ASTGenericParameter],
  ) -> IgnisParserResult<DataType> {
    let mut parameters: Vec<DataType> = Vec::new();

    if !self.check(TokenType::RightParen) {
      loop {
        if parameters.len() >= 255 {
          let token = &self.peek();
          return Err(Box::new(DiagnosticMessage::InvalidNumberOfArguments(
            255,
            parameters.len(),
            token.clone(),
          )));
        }

        let type_: DataType = self.resolve_type(generic_parameters)?;

        parameters.push(type_);

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RightParen)?;
    self.consume(TokenType::Arrow)?;

    let return_type = self.resolve_type(generic_parameters)?;

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
          DiagnosticMessage::UninitializedConstant(name.clone())
        } else {
          DiagnosticMessage::ExpectedToken(TokenType::Equal, token_previous.clone())
        }
      },
      TokenType::SemiColon => DiagnosticMessage::ExpectedSemicolonAfterExpression(token_previous.clone()),
      TokenType::Colon => DiagnosticMessage::ExpectedType(token_previous.clone()),
      TokenType::Identifier => {
        if TokenType::get_keywords().contains(&token_previous.type_) {
          DiagnosticMessage::UnexpectedKeyword(token_previous.clone())
        } else {
          DiagnosticMessage::ExpectedIdentifier(token.clone())
        }
      },
      TokenType::QuestionMark => DiagnosticMessage::ExpectedToken(TokenType::QuestionMark, token_previous.clone()),
      TokenType::Greater => DiagnosticMessage::ExpectedToken(TokenType::Greater, token_previous.clone()),
      TokenType::LeftParen | TokenType::RightParen => {
        let expression = self.previous();

        DiagnosticMessage::ExpectedAfterExpression(Box::new(kind.clone()), expression.clone(), Box::new(token.clone()))
      },
      TokenType::RightBrace => DiagnosticMessage::ExpectedDelimiter(token),
      _ => DiagnosticMessage::ExpectedToken(kind.clone(), token_previous.clone()),
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
      ParserDeclarationList::Name(name)
      | ParserDeclarationList::Record(name, _)
      | ParserDeclarationList::Enum(name) => name == &token.lexeme,
    })
  }

  fn resolve_generic_params(&mut self) -> Result<Vec<ASTGenericParameter>, Box<DiagnosticMessage>> {
    let mut generic_parameters: Vec<ASTGenericParameter> = Vec::new();

    if self.check(TokenType::Less) {
      self.advance();

      loop {
        let name = self.consume(TokenType::Identifier)?;
        let mut constraints: Vec<DataType> = Vec::new();

        if self.match_token(&[TokenType::As]) {
          let data_type = self.resolve_type(&[])?;
          constraints.push(data_type);
        }

        generic_parameters.push(ASTGenericParameter::new(name, constraints));

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }

      self.consume(TokenType::Greater)?;
    }

    Ok(generic_parameters)
  }

  fn check_if_type(&mut self) -> bool {
    matches!(
      self.peek().type_,
      TokenType::Int8Type
        | TokenType::Int16Type
        | TokenType::Int32Type
        | TokenType::Int64Type
        | TokenType::UnsignedInt8Type
        | TokenType::UnsignedInt16Type
        | TokenType::UnsignedInt32Type
        | TokenType::UnsignedInt64Type
        | TokenType::Float32Type
        | TokenType::Float64Type
        | TokenType::BooleanType
        | TokenType::StringType
        | TokenType::CharType
        | TokenType::Void
        | TokenType::Unknown
        | TokenType::BinaryType
        | TokenType::HexType
    )
  }
}
