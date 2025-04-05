use std::fmt::Display;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenType {
  // Single-character tokens
  Plus,         // +
  Minus,        // -
  Asterisk,     // *
  Slash,        // /
  LeftParen,    // (
  RightParen,   // )
  LeftBrace,    // {
  RightBrace,   // }
  LeftBrack,    // [
  RightBrack,   // ]
  Colon,        // :
  Pipe,         // |
  Ampersand,    // &
  Mod,          // %
  QuestionMark, // ?
  At,           // @
  Hash,         // #
  Caret,        // ^
  Tilde,        // ~

  // One or two character tokens
  Equal,            // =
  EqualEqual,       // ==
  Bang,             // !
  BangEqual,        // !=
  Greater,          // >
  GreaterEqual,     // >=
  Less,             // <
  LessEqual,        // <=
  Or,               // ||
  And,              // &&
  Arrow,            // ->
  Increment,        // ++
  Decrement,        // --
  AddAssign,        // +=
  SubtractAssign,   // -=
  MulAssign,        // *=
  DivAssign,        // /=
  ModAssign,        // %=
  LeftShift,        // <<
  RightShift,       // >>
  NotAssign,        // ~=
  XorAssign,        // ^=
  OrAssign,         // |=
  AndAssign,        // &=
  LeftShiftAssign,  // <<=
  RightShiftAssign, // >>=
  DoubleColon,      // ::
  Variadic,         // ...
  Range,            // ..
  RangeInclusive,   // ..=

  // Separator
  Comma,     // ,
  SemiColon, // ;
  Dot,       // .

  // Literals
  Int,    // 1-10
  Float,  // 1.3
  Char,   // a-z A-Z 0-9
  String, // "abc"
  Hex,    // 0xABC
  Binary, // 0b0101

  // Types
  StringType,        // string
  Int8Type,          // i8
  Int16Type,         // i16
  Int32Type,         // i32
  Int64Type,         // i64
  UnsignedInt8Type,  // u8
  UnsignedInt16Type, // u16
  UnsignedInt32Type, // u32
  UnsignedInt64Type, // u64
  BooleanType,       // boolean
  Float32Type,       // f32
  Float64Type,       // f64
  CharType,          // char
  HexType,           // hex
  BinaryType,        // binary

  // Keywords
  Class,
  Super,
  Static,
  New,
  Final,
  ReadOnly,
  Public,
  Private,
  Else,
  False,
  True,
  Function,
  For,
  In,
  Of,
  If,
  Null,
  Return,
  This,
  Let,
  Const,
  Mut,
  As,
  Is,
  While,
  Break,
  Enum,
  Export,
  Import,
  Type,
  From,
  Extends,
  Implements,
  Interface,
  Void,
  Extern,
  Continue,
  Unknown,
  Record,
  Decorator,
  Namespace,
  Meta,
  Match,
  When,
  Declare,

  Bad,
  Identifier,
  Comment,
  MultiLineComment,
  DocComment,
  Eof,
}

impl TokenType {
  pub fn get_keyword_from_string(key: &str) -> Option<TokenType> {
    match key {
      "as" => Some(TokenType::As),
      "binary" => Some(TokenType::BinaryType),
      "boolean" => Some(TokenType::BooleanType),
      "break" => Some(TokenType::Break),
      "char" => Some(TokenType::CharType),
      "class" => Some(TokenType::Class),
      "const" => Some(TokenType::Const),
      "continue" => Some(TokenType::Continue),
      "declare" => Some(TokenType::Declare),
      "decorator" => Some(TokenType::Decorator),
      "else" => Some(TokenType::Else),
      "enum" => Some(TokenType::Enum),
      "export" => Some(TokenType::Export),
      "extends" => Some(TokenType::Extends),
      "extern" => Some(TokenType::Extern),
      "f32" => Some(TokenType::Float32Type),
      "f64" => Some(TokenType::Float64Type),
      "false" => Some(TokenType::False),
      "final" => Some(TokenType::Final),
      "for" => Some(TokenType::For),
      "from" => Some(TokenType::From),
      "function" => Some(TokenType::Function),
      "hex" => Some(TokenType::HexType),
      "i16" => Some(TokenType::Int16Type),
      "i32" => Some(TokenType::Int32Type),
      "i64" => Some(TokenType::Int64Type),
      "i8" => Some(TokenType::Int8Type),
      "if" => Some(TokenType::If),
      "implements" => Some(TokenType::Implements),
      "import" => Some(TokenType::Import),
      "in" => Some(TokenType::In),
      "interface" => Some(TokenType::Interface),
      "is" => Some(TokenType::Is),
      "let" => Some(TokenType::Let),
      "match" => Some(TokenType::Match),
      "meta" => Some(TokenType::Meta),
      "mut" => Some(TokenType::Mut),
      "namespace" => Some(TokenType::Namespace),
      "new" => Some(TokenType::New),
      "null" => Some(TokenType::Null),
      "of" => Some(TokenType::Of),
      "private" => Some(TokenType::Private),
      "public" => Some(TokenType::Public),
      "readonly" => Some(TokenType::ReadOnly),
      "record" => Some(TokenType::Record),
      "return" => Some(TokenType::Return),
      "static" => Some(TokenType::Static),
      "string" => Some(TokenType::StringType),
      "super" => Some(TokenType::Super),
      "this" => Some(TokenType::This),
      "true" => Some(TokenType::True),
      "type" => Some(TokenType::Type),
      "u16" => Some(TokenType::UnsignedInt16Type),
      "u32" => Some(TokenType::UnsignedInt32Type),
      "u64" => Some(TokenType::UnsignedInt64Type),
      "u8" => Some(TokenType::UnsignedInt8Type),
      "unknown" => Some(TokenType::Unknown),
      "void" => Some(TokenType::Void),
      "when" => Some(TokenType::When),
      "while" => Some(TokenType::While),
      _ => None,
    }
  }

  pub fn get_keywords() -> Vec<TokenType> {
    vec![
      TokenType::As,
      TokenType::BinaryType,
      TokenType::BooleanType,
      TokenType::Break,
      TokenType::CharType,
      TokenType::Class,
      TokenType::Const,
      TokenType::Continue,
      TokenType::Declare,
      TokenType::Decorator,
      TokenType::Else,
      TokenType::Enum,
      TokenType::Export,
      TokenType::Extends,
      TokenType::Extern,
      TokenType::False,
      TokenType::Final,
      TokenType::Float32Type,
      TokenType::Float64Type,
      TokenType::For,
      TokenType::From,
      TokenType::Function,
      TokenType::HexType,
      TokenType::If,
      TokenType::Implements,
      TokenType::Import,
      TokenType::In,
      TokenType::Int16Type,
      TokenType::Int32Type,
      TokenType::Int64Type,
      TokenType::Int8Type,
      TokenType::Interface,
      TokenType::Is,
      TokenType::Let,
      TokenType::Match,
      TokenType::Meta,
      TokenType::Mut,
      TokenType::Namespace,
      TokenType::New,
      TokenType::Null,
      TokenType::Of,
      TokenType::Private,
      TokenType::Public,
      TokenType::ReadOnly,
      TokenType::Record,
      TokenType::Return,
      TokenType::Static,
      TokenType::Super,
      TokenType::This,
      TokenType::True,
      TokenType::Type,
      TokenType::Unknown,
      TokenType::UnsignedInt16Type,
      TokenType::UnsignedInt32Type,
      TokenType::UnsignedInt64Type,
      TokenType::UnsignedInt8Type,
      TokenType::Void,
      TokenType::When,
      TokenType::While,
    ]
  }
}

impl Display for TokenType {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      TokenType::AddAssign => write!(f, "AddAssign"),
      TokenType::Ampersand => write!(f, "Ampersand"),
      TokenType::And => write!(f, "And"),
      TokenType::AndAssign => write!(f, "AndAssign"),
      TokenType::Arrow => write!(f, "Arrow"),
      TokenType::As => write!(f, "As"),
      TokenType::Asterisk => write!(f, "Asterisk"),
      TokenType::At => write!(f, "At"),
      TokenType::Bad => write!(f, "Bad"),
      TokenType::Bang => write!(f, "Bang"),
      TokenType::BangEqual => write!(f, "BangEqual"),
      TokenType::Binary => write!(f, "Binary"),
      TokenType::BinaryType => write!(f, "BinaryType"),
      TokenType::BooleanType => write!(f, "BooleanType"),
      TokenType::Break => write!(f, "Break"),
      TokenType::Caret => write!(f, "Caret"),
      TokenType::Char => write!(f, "Char"),
      TokenType::CharType => write!(f, "CharType"),
      TokenType::Class => write!(f, "Class"),
      TokenType::Colon => write!(f, "Colon"),
      TokenType::Comma => write!(f, "Comma"),
      TokenType::Comment => write!(f, "Comment"),
      TokenType::Const => write!(f, "Const"),
      TokenType::Continue => write!(f, "Continue"),
      TokenType::Declare => write!(f, "Declare"),
      TokenType::Decorator => write!(f, "Decorator"),
      TokenType::Decrement => write!(f, "Decrement"),
      TokenType::DivAssign => write!(f, "DivAssign"),
      TokenType::DocComment => write!(f, "DocComment"),
      TokenType::Dot => write!(f, "Dot"),
      TokenType::DoubleColon => write!(f, "DoubleColon"),
      TokenType::Else => write!(f, "Else"),
      TokenType::Enum => write!(f, "Enum"),
      TokenType::Eof => write!(f, "Eof"),
      TokenType::Equal => write!(f, "Equal"),
      TokenType::EqualEqual => write!(f, "EqualEqual"),
      TokenType::Export => write!(f, "Export"),
      TokenType::Extends => write!(f, "Extends"),
      TokenType::Extern => write!(f, "Extern"),
      TokenType::False => write!(f, "False"),
      TokenType::Final => write!(f, "Final"),
      TokenType::Float => write!(f, "Float"),
      TokenType::Float32Type => write!(f, "Float32Type"),
      TokenType::Float64Type => write!(f, "Float64Type"),
      TokenType::For => write!(f, "For"),
      TokenType::From => write!(f, "From"),
      TokenType::Function => write!(f, "Function"),
      TokenType::Greater => write!(f, "Greater"),
      TokenType::GreaterEqual => write!(f, "GreaterEqual"),
      TokenType::Hash => write!(f, "Hash"),
      TokenType::Hex => write!(f, "Hex"),
      TokenType::HexType => write!(f, "HexType"),
      TokenType::Identifier => write!(f, "Identifier"),
      TokenType::If => write!(f, "If"),
      TokenType::Implements => write!(f, "Implements"),
      TokenType::Import => write!(f, "Import"),
      TokenType::In => write!(f, "In"),
      TokenType::Increment => write!(f, "Increment"),
      TokenType::Int => write!(f, "Int"),
      TokenType::Int16Type => write!(f, "Int16Type"),
      TokenType::Int32Type => write!(f, "Int32Type"),
      TokenType::Int64Type => write!(f, "Int64Type"),
      TokenType::Int8Type => write!(f, "Int8Type"),
      TokenType::Interface => write!(f, "Interface"),
      TokenType::Is => write!(f, "Is"),
      TokenType::LeftBrace => write!(f, "LeftBrace"),
      TokenType::LeftBrack => write!(f, "LeftBrack"),
      TokenType::LeftParen => write!(f, "LeftParen"),
      TokenType::LeftShift => write!(f, "LeftShift"),
      TokenType::LeftShiftAssign => write!(f, "LeftShiftAssign"),
      TokenType::Less => write!(f, "Less"),
      TokenType::LessEqual => write!(f, "LessEqual"),
      TokenType::Let => write!(f, "Let"),
      TokenType::Match => write!(f, "Match"),
      TokenType::Meta => write!(f, "Meta"),
      TokenType::Minus => write!(f, "Minus"),
      TokenType::Mod => write!(f, "Mod"),
      TokenType::ModAssign => write!(f, "ModAssign"),
      TokenType::MulAssign => write!(f, "MulAssign"),
      TokenType::MultiLineComment => write!(f, "MultiLineComment"),
      TokenType::Mut => write!(f, "Mut"),
      TokenType::Namespace => write!(f, "Namespace"),
      TokenType::New => write!(f, "New"),
      TokenType::NotAssign => write!(f, "NotAssign"),
      TokenType::Null => write!(f, "Null"),
      TokenType::Of => write!(f, "Of"),
      TokenType::Or => write!(f, "Or"),
      TokenType::OrAssign => write!(f, "OrAssign"),
      TokenType::Pipe => write!(f, "Pipe"),
      TokenType::Plus => write!(f, "Plus"),
      TokenType::Private => write!(f, "Private"),
      TokenType::Public => write!(f, "Public"),
      TokenType::QuestionMark => write!(f, "QuestionMark"),
      TokenType::Range => write!(f, "Range"),
      TokenType::RangeInclusive => write!(f, "RangeInclusive"),
      TokenType::ReadOnly => write!(f, "ReadOnly"),
      TokenType::Record => write!(f, "Record"),
      TokenType::Return => write!(f, "Return"),
      TokenType::RightBrace => write!(f, "RightBrace"),
      TokenType::RightBrack => write!(f, "RightBrack"),
      TokenType::RightParen => write!(f, "RightParen"),
      TokenType::RightShift => write!(f, "RightShift"),
      TokenType::RightShiftAssign => write!(f, "RightShiftAssign"),
      TokenType::SemiColon => write!(f, "SemiColon"),
      TokenType::Slash => write!(f, "Slash"),
      TokenType::Static => write!(f, "Static"),
      TokenType::String => write!(f, "String"),
      TokenType::StringType => write!(f, "StringType"),
      TokenType::SubtractAssign => write!(f, "SubtractAssign"),
      TokenType::Super => write!(f, "Super"),
      TokenType::This => write!(f, "This"),
      TokenType::Tilde => write!(f, "Tilde"),
      TokenType::True => write!(f, "True"),
      TokenType::Type => write!(f, "Type"),
      TokenType::Unknown => write!(f, "Unknown"),
      TokenType::UnsignedInt16Type => write!(f, "UnsignedInt16Type"),
      TokenType::UnsignedInt32Type => write!(f, "UnsignedInt32Type"),
      TokenType::UnsignedInt64Type => write!(f, "UnsignedInt64Type"),
      TokenType::UnsignedInt8Type => write!(f, "UnsignedInt8Type"),
      TokenType::Variadic => write!(f, "Variadic"),
      TokenType::Void => write!(f, "Void"),
      TokenType::When => write!(f, "When"),
      TokenType::While => write!(f, "While"),
      TokenType::XorAssign => write!(f, "XorAssign"),
    }
  }
}
