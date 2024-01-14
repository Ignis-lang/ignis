use std::fmt::{Display, Formatter, write};

/**
  TokenType
*/
#[derive(Debug, Clone, PartialEq, Eq)]
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

  // Template string tokens
  TemplateStringStart, // `
  TemplateStringEnd,   // `
  ExpressionStart,     // ${
  ExpressionEnd,       // }

  // One or two character tokens
  Equal,              // =
  EqualEqual,         // ==
  Bang,               // !
  BangEqual,          // !=
  Greater,            // >
  GreaterEqual,       // >=
  Less,               // <
  LessEqual,          // <=
  Or,                 // ||
  And,                // &&
  Arrow,              // ->
  IncrementAndAssign, // +=
  DecrementAndAssign, // -=
  Increment,          // ++
  Decrement,          // --

  // Separator
  Comma,     // ,
  SemiColon, // ;
  Dot,       // .

  // Literals
  Int,   // 1-10
  Float, // 1.3
  Char,  // a-z A-Z 0-9
  String,

  // Types
  StringType,
  IntType,
  BooleanType,
  FloatType,
  CharType,

  // Keywords
  Class,
  Super,
  Static,
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
  If,
  Null,
  Return,
  This,
  Let,
  Const,
  Mut,
  As,
  While,
  Break,
  Enum,
  Export,
  Import,
  From,
  Extends,
  Implements,
  Interface,
  Void,
  Extern,
  Continue,

  Bad,
  Identifier,
  Comment,
  Eof,
}

impl TokenType {
  fn to_string(&self) -> String {
    match self {
      TokenType::Plus => "+".to_string(),
      TokenType::Minus => "-".to_string(),
      TokenType::Ampersand => "&".to_string(),
      TokenType::Asterisk => "*".to_string(),
      TokenType::Slash => "/".to_string(),
      TokenType::LeftParen => "(".to_string(),
      TokenType::RightParen => ")".to_string(),
      TokenType::LeftBrace => "{".to_string(),
      TokenType::RightBrace => "}".to_string(),
      TokenType::LeftBrack => "[".to_string(),
      TokenType::RightBrack => "]".to_string(),
      TokenType::Colon => ":".to_string(),
      TokenType::Pipe => "|".to_string(),
      TokenType::Comma => ",".to_string(),
      TokenType::SemiColon => ";".to_string(),
      TokenType::Dot => ".".to_string(),
      TokenType::Mod => "%".to_string(),
      TokenType::Equal => "=".to_string(),
      TokenType::QuestionMark => "?".to_string(),
      TokenType::EqualEqual => "==".to_string(),
      TokenType::Bang => "!".to_string(),
      TokenType::BangEqual => "!=".to_string(),
      TokenType::Greater => ">".to_string(),
      TokenType::GreaterEqual => ">=".to_string(),
      TokenType::Less => "<".to_string(),
      TokenType::LessEqual => "<=".to_string(),
      TokenType::Or => "||".to_string(),
      TokenType::And => "&&".to_string(),
      TokenType::Arrow => "->".to_string(),
      TokenType::IncrementAndAssign => "+=".to_string(),
      TokenType::Increment => "++".to_string(),
      TokenType::DecrementAndAssign => "-=".to_string(),
      TokenType::Decrement => "--".to_string(),
      TokenType::Int => "int".to_string(),
      TokenType::Float => "Float".to_string(),
      TokenType::Char => "char".to_string(),
      TokenType::String => "string".to_string(),
      TokenType::StringType => "string".to_string(),
      TokenType::IntType => "int".to_string(),
      TokenType::BooleanType => "bool".to_string(),
      TokenType::FloatType => "float".to_string(),
      TokenType::CharType => "char".to_string(),
      TokenType::Class => "class".to_string(),
      TokenType::Super => "super".to_string(),
      TokenType::Static => "static".to_string(),
      TokenType::Final => "final".to_string(),
      TokenType::ReadOnly => "readonly".to_string(),
      TokenType::Public => "public".to_string(),
      TokenType::Private => "private".to_string(),
      TokenType::Else => "else".to_string(),
      TokenType::False => "false".to_string(),
      TokenType::True => "true".to_string(),
      TokenType::Function => "function".to_string(),
      TokenType::For => "for".to_string(),
      TokenType::In => "in".to_string(),
      TokenType::If => "if".to_string(),
      TokenType::Null => "null".to_string(),
      TokenType::Return => "return".to_string(),
      TokenType::This => "this".to_string(),
      TokenType::Let => "let".to_string(),
      TokenType::Const => "const".to_string(),
      TokenType::Mut => "mut".to_string(),
      TokenType::As => "as".to_string(),
      TokenType::While => "while".to_string(),
      TokenType::Break => "break".to_string(),
      TokenType::Enum => "enum".to_string(),
      TokenType::Export => "export".to_string(),
      TokenType::Import => "import".to_string(),
      TokenType::From => "from".to_string(),
      TokenType::Extends => "extends".to_string(),
      TokenType::Implements => "implements".to_string(),
      TokenType::Interface => "interface".to_string(),
      TokenType::Void => "void".to_string(),
      TokenType::Bad => "bad".to_string(),
      TokenType::Identifier => "identifier".to_string(),
      TokenType::Eof => "eof".to_string(),
      TokenType::Extern => "extern".to_string(),
      TokenType::At => "comment".to_string(),
      TokenType::Continue => "continue".to_string(),
      _ => String::new(),
    }
  }
}

impl Display for TokenType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      TokenType::Plus => write!(f, "+"),
      TokenType::Minus => write!(f, "-"),
      TokenType::Asterisk => write!(f, "*"),
      TokenType::Slash => write!(f, "/"),
      TokenType::LeftParen => write!(f, "("),
      TokenType::RightParen => write!(f, ")"),
      TokenType::LeftBrace => write!(f, "{{"),
      TokenType::RightBrace => write!(f, "}}"),
      TokenType::LeftBrack => write!(f, "["),
      TokenType::RightBrack => write!(f, "]"),
      TokenType::Colon => write!(f, ":"),
      TokenType::Pipe => write!(f, "|"),
      TokenType::Comma => write!(f, ","),
      TokenType::SemiColon => write!(f, ";"),
      TokenType::Dot => write!(f, "."),
      TokenType::Ampersand => write!(f, "&"),
      TokenType::Mod => write!(f, "%"),
      TokenType::Equal => write!(f, "="),
      TokenType::EqualEqual => write!(f, "=="),
      TokenType::Bang => write!(f, "!"),
      TokenType::BangEqual => write!(f, "!="),
      TokenType::Greater => write!(f, ">"),
      TokenType::GreaterEqual => write!(f, ">="),
      TokenType::Less => write!(f, "<"),
      TokenType::LessEqual => write!(f, "<="),
      TokenType::Or => write!(f, "||"),
      TokenType::And => write!(f, "&&"),
      TokenType::Arrow => write!(f, "->"),
      TokenType::IncrementAndAssign => write!(f, "+="),
      TokenType::DecrementAndAssign => write!(f, "-="),
      TokenType::Int => write!(f, "int"),
      TokenType::Float => write!(f, "Float"),
      TokenType::Char => write!(f, "char"),
      TokenType::String => write!(f, "string"),
      TokenType::StringType => write!(f, "string"),
      TokenType::IntType => write!(f, "int"),
      TokenType::BooleanType => write!(f, "bool"),
      TokenType::FloatType => write!(f, "float"),
      TokenType::CharType => write!(f, "char"),
      TokenType::Class => write!(f, "class"),
      TokenType::Super => write!(f, "super"),
      TokenType::Static => write!(f, "static"),
      TokenType::Final => write!(f, "final"),
      TokenType::ReadOnly => write!(f, "readonly"),
      TokenType::Public => write!(f, "public"),
      TokenType::Private => write!(f, "private"),
      TokenType::Else => write!(f, "else"),
      TokenType::False => write!(f, "false"),
      TokenType::True => write!(f, "true"),
      TokenType::Function => write!(f, "function"),
      TokenType::For => write!(f, "for"),
      TokenType::In => write!(f, "in"),
      TokenType::If => write!(f, "if"),
      TokenType::Null => write!(f, "null"),
      TokenType::Return => write!(f, "return"),
      TokenType::This => write!(f, "this"),
      TokenType::Let => write!(f, "let"),
      TokenType::Const => write!(f, "const"),
      TokenType::Mut => write!(f, "mut"),
      TokenType::As => write!(f, "as"),
      TokenType::While => write!(f, "while"),
      TokenType::Break => write!(f, "break"),
      TokenType::Enum => write!(f, "enum"),
      TokenType::Export => write!(f, "export"),
      TokenType::Import => write!(f, "import"),
      TokenType::From => write!(f, "from"),
      TokenType::Extends => write!(f, "extends"),
      TokenType::Implements => write!(f, "implements"),
      TokenType::Interface => write!(f, "interface"),
      TokenType::Bad => write!(f, "bad"),
      TokenType::Identifier => write!(f, "identifier"),
      TokenType::Eof => write!(f, "eof"),
      TokenType::QuestionMark => write!(f, "?"),
      TokenType::Void => write!(f, "void"),
      TokenType::Comment => write!(f, "comment"),
      TokenType::Extern => write!(f, "extern"),
      TokenType::At => write!(f, "@"),
      TokenType::TemplateStringStart => write!(f, "`"),
      TokenType::TemplateStringEnd => write!(f, "`"),
      TokenType::ExpressionStart => write!(f, "${{"),
      TokenType::ExpressionEnd => write!(f, "}}"),
      TokenType::Continue => write!(f, "continue"),
      TokenType::Increment => write!(f, "++"),
      TokenType::Decrement => write!(f, "--"),
    }
  }
}
