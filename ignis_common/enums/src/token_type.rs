use std::fmt::{Display, Formatter};

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
