use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
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
  Declare,

  Bad,
  Identifier,
  Comment,
  MultiLineComment,
  DocComment,
  Eof,
}

impl TokenType {
  pub fn get_keywords() -> Vec<TokenType> {
    vec![
      TokenType::Class,
      TokenType::Super,
      TokenType::Static,
      TokenType::New,
      TokenType::Final,
      TokenType::ReadOnly,
      TokenType::Public,
      TokenType::Private,
      TokenType::Else,
      TokenType::False,
      TokenType::True,
      TokenType::Function,
      TokenType::For,
      TokenType::In,
      TokenType::Of,
      TokenType::If,
      TokenType::Null,
      TokenType::Return,
      TokenType::This,
      TokenType::Let,
      TokenType::Const,
      TokenType::Mut,
      TokenType::As,
      TokenType::Is,
      TokenType::While,
      TokenType::Break,
      TokenType::Enum,
      TokenType::Export,
      TokenType::Import,
      TokenType::Type,
      TokenType::From,
      TokenType::Extends,
      TokenType::Implements,
      TokenType::Interface,
      TokenType::Void,
      TokenType::Extern,
      TokenType::Continue,
      TokenType::Unknown,
      TokenType::Record,
      TokenType::Decorator,
      TokenType::Namespace,
      TokenType::Meta,
      TokenType::Declare,
      TokenType::Int8Type,
      TokenType::Int16Type,
      TokenType::Int32Type,
      TokenType::Int64Type,
      TokenType::UnsignedInt8Type,
      TokenType::UnsignedInt16Type,
      TokenType::UnsignedInt32Type,
      TokenType::UnsignedInt64Type,
      TokenType::BooleanType,
      TokenType::Float32Type,
      TokenType::Float64Type,
      TokenType::CharType,
      TokenType::HexType,
      TokenType::BinaryType,
    ]
  }
}

impl Display for TokenType {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      TokenType::Plus => write!(f, "Plus"),
      TokenType::Minus => write!(f, "Minus"),
      TokenType::Asterisk => write!(f, "Asterisk"),
      TokenType::Slash => write!(f, "Slash"),
      TokenType::LeftParen => write!(f, "LeftParen"),
      TokenType::RightParen => write!(f, "RightParen"),
      TokenType::LeftBrace => write!(f, "LeftBrace"),
      TokenType::RightBrace => write!(f, "RightBrace"),
      TokenType::LeftBrack => write!(f, "LeftBrack"),
      TokenType::RightBrack => write!(f, "RightBrack"),
      TokenType::Colon => write!(f, "Colon"),
      TokenType::Pipe => write!(f, "Pipe"),
      TokenType::Ampersand => write!(f, "Ampersand"),
      TokenType::Mod => write!(f, "Mod"),
      TokenType::QuestionMark => write!(f, "QuestionMark"),
      TokenType::At => write!(f, "At"),
      TokenType::Hash => write!(f, "Hash"),
      TokenType::Caret => write!(f, "Caret"),
      TokenType::Tilde => write!(f, "Tilde"),
      TokenType::Equal => write!(f, "Equal"),
      TokenType::EqualEqual => write!(f, "EqualEqual"),
      TokenType::Bang => write!(f, "Bang"),
      TokenType::BangEqual => write!(f, "BangEqual"),
      TokenType::Greater => write!(f, "Greater"),
      TokenType::GreaterEqual => write!(f, "GreaterEqual"),
      TokenType::Less => write!(f, "Less"),
      TokenType::LessEqual => write!(f, "LessEqual"),
      TokenType::Or => write!(f, "Or"),
      TokenType::And => write!(f, "And"),
      TokenType::Arrow => write!(f, "Arrow"),
      TokenType::AddAssign => write!(f, "AddAssign"),
      TokenType::SubtractAssign => write!(f, "SubtractAssign"),
      TokenType::MulAssign => write!(f, "MulAssign"),
      TokenType::DivAssign => write!(f, "DivAssign"),
      TokenType::ModAssign => write!(f, "ModAssign"),
      TokenType::LeftShift => write!(f, "LeftShift"),
      TokenType::RightShift => write!(f, "RightShift"),
      TokenType::NotAssign => write!(f, "NotAssign"),
      TokenType::XorAssign => write!(f, "XorAssign"),
      TokenType::OrAssign => write!(f, "OrAssign"),
      TokenType::AndAssign => write!(f, "AndAssign"),
      TokenType::LeftShiftAssign => write!(f, "LeftShiftAssign"),
      TokenType::RightShiftAssign => write!(f, "RightShiftAssign"),
      TokenType::Comma => write!(f, "Comma"),
      TokenType::SemiColon => write!(f, "SemiColon"),
      TokenType::Dot => write!(f, "Dot"),
      TokenType::Int => write!(f, "Int"),
      TokenType::Float => write!(f, "Float"),
      TokenType::Char => write!(f, "Char"),
      TokenType::String => write!(f, "String"),
      TokenType::StringType => write!(f, "StringType"),
      TokenType::Int8Type => write!(f, "Int8Type"),
      TokenType::Int16Type => write!(f, "Int16Type"),
      TokenType::Int32Type => write!(f, "Int32Type"),
      TokenType::Int64Type => write!(f, "Int64Type"),
      TokenType::UnsignedInt8Type => write!(f, "UnsignedInt8Type"),
      TokenType::UnsignedInt16Type => write!(f, "UnsignedInt16Type"),
      TokenType::UnsignedInt32Type => write!(f, "UnsignedInt32Type"),
      TokenType::UnsignedInt64Type => write!(f, "UnsignedInt64Type"),
      TokenType::Float32Type => write!(f, "Float32Type"),
      TokenType::Float64Type => write!(f, "Float64Type"),
      TokenType::BooleanType => write!(f, "BooleanType"),
      TokenType::CharType => write!(f, "CharType"),
      TokenType::Class => write!(f, "Class"),
      TokenType::Super => write!(f, "Super"),
      TokenType::Static => write!(f, "Static"),
      TokenType::Final => write!(f, "Final"),
      TokenType::ReadOnly => write!(f, "ReadOnly"),
      TokenType::Public => write!(f, "Public"),
      TokenType::Private => write!(f, "Private"),
      TokenType::Else => write!(f, "Else"),
      TokenType::False => write!(f, "False"),
      TokenType::True => write!(f, "True"),
      TokenType::Function => write!(f, "Function"),
      TokenType::For => write!(f, "For"),
      TokenType::In => write!(f, "In"),
      TokenType::Of => write!(f, "Of"),
      TokenType::If => write!(f, "If"),
      TokenType::Null => write!(f, "Null"),
      TokenType::Return => write!(f, "Return"),
      TokenType::This => write!(f, "This"),
      TokenType::Let => write!(f, "Let"),
      TokenType::Const => write!(f, "Const"),
      TokenType::Mut => write!(f, "Mut"),
      TokenType::As => write!(f, "As"),
      TokenType::While => write!(f, "While"),
      TokenType::Break => write!(f, "Break"),
      TokenType::Enum => write!(f, "Enum"),
      TokenType::Export => write!(f, "Export"),
      TokenType::Import => write!(f, "Import"),
      TokenType::Type => write!(f, "Type"),
      TokenType::From => write!(f, "From"),
      TokenType::Extends => write!(f, "Extends"),
      TokenType::Implements => write!(f, "Implements"),
      TokenType::Interface => write!(f, "Interface"),
      TokenType::Void => write!(f, "Void"),
      TokenType::Extern => write!(f, "Extern"),
      TokenType::Continue => write!(f, "Continue"),
      TokenType::Unknown => write!(f, "Unknown"),
      TokenType::Record => write!(f, "Record"),
      TokenType::Decorator => write!(f, "Decorator"),
      TokenType::Namespace => write!(f, "Namespace"),
      TokenType::Meta => write!(f, "Meta"),
      TokenType::Declare => write!(f, "Declare"),
      TokenType::Bad => write!(f, "Bad"),
      TokenType::Identifier => write!(f, "Identifier"),
      TokenType::Comment => write!(f, "Comment"),
      TokenType::MultiLineComment => write!(f, "MultiLineComment"),
      TokenType::DocComment => write!(f, "DocComment"),
      TokenType::Eof => write!(f, "Eof"),
      TokenType::Increment => write!(f, "Increment"),
      TokenType::Decrement => write!(f, "Decrement"),
      TokenType::Hex => write!(f, "Hex"),
      TokenType::Binary => write!(f, "Binary"),
      TokenType::HexType => write!(f, "HexType"),
      TokenType::BinaryType => write!(f, "BinaryType"),
      TokenType::New => write!(f, "New"),
      TokenType::Is => write!(f, "Is"),
    }
  }
}
