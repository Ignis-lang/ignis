//! Semantic token classification for LSP.
//!
//! This module provides semantic highlighting by classifying tokens based on
//! semantic analysis, not just syntax. This allows distinguishing between
//! functions, methods, types, variables, etc.

use std::collections::HashMap;

use ignis_token::token::Token;
use ignis_token::token_types::TokenType;
use ignis_type::definition::{Definition, DefinitionId, DefinitionKind, DefinitionStore};
use ignis_type::span::Span;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend};

use crate::convert::LineIndex;

/// Supported semantic token types.
///
/// Order matters - indices are used in the LSP protocol.
pub const SUPPORTED_TYPES: &[SemanticTokenType] = &[
  SemanticTokenType::NAMESPACE,
  SemanticTokenType::TYPE,
  SemanticTokenType::CLASS,
  SemanticTokenType::ENUM,
  SemanticTokenType::STRUCT,
  SemanticTokenType::TYPE_PARAMETER,
  SemanticTokenType::PARAMETER,
  SemanticTokenType::VARIABLE,
  SemanticTokenType::PROPERTY,
  SemanticTokenType::ENUM_MEMBER,
  SemanticTokenType::FUNCTION,
  SemanticTokenType::METHOD,
  SemanticTokenType::KEYWORD,
  SemanticTokenType::MODIFIER,
  SemanticTokenType::COMMENT,
  SemanticTokenType::STRING,
  SemanticTokenType::NUMBER,
  SemanticTokenType::OPERATOR,
];

/// Supported semantic token modifiers.
///
/// Order matters - bit positions are used in the LSP protocol.
pub const SUPPORTED_MODIFIERS: &[SemanticTokenModifier] = &[
  SemanticTokenModifier::DECLARATION,
  SemanticTokenModifier::DEFINITION,
  SemanticTokenModifier::READONLY,
  SemanticTokenModifier::STATIC,
  SemanticTokenModifier::MODIFICATION,
  SemanticTokenModifier::new("mutable"), // Custom modifier
];

/// Build the semantic tokens legend for capability registration.
pub fn build_legend() -> SemanticTokensLegend {
  SemanticTokensLegend {
    token_types: SUPPORTED_TYPES.to_vec(),
    token_modifiers: SUPPORTED_MODIFIERS.to_vec(),
  }
}

/// Index of token types for efficient lookup.
#[derive(Debug, Clone, Copy)]
#[repr(u32)]
enum TokenTypeIndex {
  Namespace = 0,
  Type = 1,
  Enum = 3,
  Struct = 4,
  TypeParameter = 5,
  Parameter = 6,
  Variable = 7,
  Property = 8,
  _EnumMember = 9,
  Function = 10,
  Method = 11,
  Keyword = 12,
  Modifier = 13,
  Comment = 14,
  String = 15,
  Number = 16,
  Operator = 17,
}

/// Index of token modifiers for efficient lookup.
#[derive(Debug, Clone, Copy)]
#[repr(u32)]
enum ModifierIndex {
  Declaration = 0,
  Definition = 1,
  Readonly = 2,
  Static = 3,
  _Modification = 4,
  Mutable = 5,
}

impl ModifierIndex {
  fn bit(self) -> u32 {
    1 << (self as u32)
  }
}

/// Classify tokens from lexer output using semantic analysis.
pub fn classify_tokens(
  tokens: &[Token],
  import_item_defs: &HashMap<Span, DefinitionId>,
  defs: &DefinitionStore,
  file_id: &ignis_type::file::FileId,
  line_index: &LineIndex,
) -> SemanticTokens {
  let mut classified = Vec::new();

  for token in tokens {
    // Only process tokens from the target file
    if &token.span.file != file_id {
      continue;
    }

    // Skip whitespace and EOF
    if matches!(token.type_, TokenType::Whitespace | TokenType::Eof) {
      continue;
    }

    if let Some(ct) = classify_token(token, import_item_defs, defs, line_index) {
      classified.push(ct);
    }
  }

  encode_tokens(classified)
}

/// Classify a single token.
fn classify_token(
  token: &Token,
  import_item_defs: &HashMap<Span, DefinitionId>,
  defs: &DefinitionStore,
  line_index: &LineIndex,
) -> Option<SemanticToken> {
  let (token_type, modifiers) = match token.type_ {
    // Keywords
    TokenType::Let
    | TokenType::Const
    | TokenType::Function
    | TokenType::If
    | TokenType::Else
    | TokenType::For
    | TokenType::In
    | TokenType::Of
    | TokenType::While
    | TokenType::Break
    | TokenType::Continue
    | TokenType::Return
    | TokenType::As
    | TokenType::Is
    | TokenType::Record
    | TokenType::Enum
    | TokenType::Type
    | TokenType::Import
    | TokenType::Export
    | TokenType::From
    | TokenType::Extern
    | TokenType::Namespace
    | TokenType::Match
    | TokenType::When
    | TokenType::Declare
    | TokenType::Inline => (TokenTypeIndex::Keyword as u32, 0),

    // Modifiers (also keywords, but highlight differently)
    TokenType::Static | TokenType::Mut | TokenType::Public | TokenType::Private => (TokenTypeIndex::Modifier as u32, 0),

    // Boolean literals (keywords)
    TokenType::True | TokenType::False => (TokenTypeIndex::Keyword as u32, 0),

    // Null (keyword)
    TokenType::Null => (TokenTypeIndex::Keyword as u32, 0),

    // Self/This (keyword, but could also be parameter)
    TokenType::Self_ | TokenType::This => (TokenTypeIndex::Keyword as u32, 0),

    // Void type (keyword)
    TokenType::Void => (TokenTypeIndex::Type as u32, ModifierIndex::Readonly.bit()),

    // Primitive types
    TokenType::StringType
    | TokenType::Int8Type
    | TokenType::Int16Type
    | TokenType::Int32Type
    | TokenType::Int64Type
    | TokenType::UnsignedInt8Type
    | TokenType::UnsignedInt16Type
    | TokenType::UnsignedInt32Type
    | TokenType::UnsignedInt64Type
    | TokenType::BooleanType
    | TokenType::Float32Type
    | TokenType::Float64Type
    | TokenType::CharType
    | TokenType::HexType
    | TokenType::BinaryType => (TokenTypeIndex::Type as u32, ModifierIndex::Readonly.bit()),

    // Literals
    TokenType::String => (TokenTypeIndex::String as u32, 0),
    TokenType::Int | TokenType::Float | TokenType::Hex | TokenType::Binary | TokenType::Char => {
      (TokenTypeIndex::Number as u32, 0)
    },

    // Comments
    TokenType::Comment | TokenType::MultiLineComment | TokenType::DocComment => (TokenTypeIndex::Comment as u32, 0),

    // Operators
    TokenType::Plus
    | TokenType::Minus
    | TokenType::Asterisk
    | TokenType::Slash
    | TokenType::Mod
    | TokenType::Equal
    | TokenType::EqualEqual
    | TokenType::Bang
    | TokenType::BangEqual
    | TokenType::Greater
    | TokenType::GreaterEqual
    | TokenType::Less
    | TokenType::LessEqual
    | TokenType::Or
    | TokenType::And
    | TokenType::Arrow
    | TokenType::Increment
    | TokenType::Decrement
    | TokenType::AddAssign
    | TokenType::SubtractAssign
    | TokenType::MulAssign
    | TokenType::DivAssign
    | TokenType::ModAssign
    | TokenType::LeftShift
    | TokenType::RightShift
    | TokenType::NotAssign
    | TokenType::XorAssign
    | TokenType::OrAssign
    | TokenType::AndAssign
    | TokenType::LeftShiftAssign
    | TokenType::RightShiftAssign
    | TokenType::DoubleColon
    | TokenType::Variadic
    | TokenType::Range
    | TokenType::RangeInclusive
    | TokenType::Ampersand
    | TokenType::Pipe
    | TokenType::Caret
    | TokenType::Tilde
    | TokenType::QuestionMark => (TokenTypeIndex::Operator as u32, 0),

    // Identifiers need semantic analysis
    TokenType::Identifier => {
      return classify_identifier(token, import_item_defs, defs, line_index);
    },

    // Skip punctuation and other tokens
    _ => return None,
  };

  let (line, start) = line_index.line_col_utf16(token.span.start);

  Some(SemanticToken {
    delta_line: line,
    delta_start: start,
    length: token.lexeme.encode_utf16().count() as u32,
    token_type,
    token_modifiers_bitset: modifiers,
  })
}

/// Classify an identifier token using semantic analysis.
fn classify_identifier(
  token: &Token,
  import_item_defs: &HashMap<Span, DefinitionId>,
  defs: &DefinitionStore,
  line_index: &LineIndex,
) -> Option<SemanticToken> {
  let def_id = import_item_defs.get(&token.span)?;
  let def = defs.get(def_id);

  let (token_type, modifiers) = classify_definition(def, &token.span, def);
  let (line, start) = line_index.line_col_utf16(token.span.start);

  Some(SemanticToken {
    delta_line: line,
    delta_start: start,
    length: token.lexeme.encode_utf16().count() as u32,
    token_type,
    token_modifiers_bitset: modifiers,
  })
}

/// Classify a definition into token type and modifiers.
fn classify_definition(
  def: &Definition,
  token_span: &Span,
  _original_def: &Definition,
) -> (u32, u32) {
  let is_declaration = token_span == &def.span;

  match &def.kind {
    DefinitionKind::Function(func_def) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit() | ModifierIndex::Definition.bit();
      }
      if func_def.is_extern {
        modifiers |= ModifierIndex::Readonly.bit();
      }
      (TokenTypeIndex::Function as u32, modifiers)
    },

    DefinitionKind::Method(method_def) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit() | ModifierIndex::Definition.bit();
      }
      if method_def.is_static {
        modifiers |= ModifierIndex::Static.bit();
      }
      (TokenTypeIndex::Method as u32, modifiers)
    },

    DefinitionKind::Record(_) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit() | ModifierIndex::Definition.bit();
      }
      (TokenTypeIndex::Struct as u32, modifiers)
    },

    DefinitionKind::Enum(_) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit() | ModifierIndex::Definition.bit();
      }
      (TokenTypeIndex::Enum as u32, modifiers)
    },

    DefinitionKind::Variable(var_def) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit();
      }
      if var_def.mutable {
        modifiers |= ModifierIndex::Mutable.bit();
      } else {
        modifiers |= ModifierIndex::Readonly.bit();
      }
      (TokenTypeIndex::Variable as u32, modifiers)
    },

    DefinitionKind::Constant(_) => {
      let mut modifiers = ModifierIndex::Readonly.bit();
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit() | ModifierIndex::Definition.bit();
      }
      (TokenTypeIndex::Variable as u32, modifiers)
    },

    DefinitionKind::Parameter(param_def) => {
      let mut modifiers = ModifierIndex::Declaration.bit();
      if param_def.mutable {
        modifiers |= ModifierIndex::Mutable.bit();
      } else {
        modifiers |= ModifierIndex::Readonly.bit();
      }
      (TokenTypeIndex::Parameter as u32, modifiers)
    },

    DefinitionKind::TypeParam(_) => {
      let modifiers = ModifierIndex::Declaration.bit();
      (TokenTypeIndex::TypeParameter as u32, modifiers)
    },

    DefinitionKind::Namespace(_) => (TokenTypeIndex::Namespace as u32, 0),

    DefinitionKind::TypeAlias(_) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit() | ModifierIndex::Definition.bit();
      }
      (TokenTypeIndex::Type as u32, modifiers)
    },

    DefinitionKind::Field(_) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit();
      }
      (TokenTypeIndex::Property as u32, modifiers)
    },

    DefinitionKind::Variant(_) => {
      // Enum variants are semantically similar to enum members
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit();
      }
      (TokenTypeIndex::_EnumMember as u32, modifiers)
    },

    DefinitionKind::Placeholder => (TokenTypeIndex::Variable as u32, 0),
  }
}

/// Encode classified tokens into LSP delta format.
fn encode_tokens(mut tokens: Vec<SemanticToken>) -> SemanticTokens {
  // Sort by position (line, then column)
  tokens.sort_by(|a, b| {
    a.delta_line
      .cmp(&b.delta_line)
      .then_with(|| a.delta_start.cmp(&b.delta_start))
  });

  // Convert absolute positions to deltas
  let mut prev_line = 0;
  let mut prev_start = 0;
  let mut data = Vec::new();

  for mut token in tokens {
    let abs_line = token.delta_line;
    let abs_start = token.delta_start;

    token.delta_line = abs_line.saturating_sub(prev_line);
    token.delta_start = if token.delta_line == 0 {
      abs_start.saturating_sub(prev_start)
    } else {
      abs_start
    };

    data.push(token);

    prev_line = abs_line;
    prev_start = abs_start;
  }

  SemanticTokens { result_id: None, data }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_modifier_bits() {
    assert_eq!(ModifierIndex::Declaration.bit(), 1 << 0);
    assert_eq!(ModifierIndex::Definition.bit(), 1 << 1);
    assert_eq!(ModifierIndex::Readonly.bit(), 1 << 2);
    assert_eq!(ModifierIndex::Static.bit(), 1 << 3);
    assert_eq!(ModifierIndex::_Modification.bit(), 1 << 4);
    assert_eq!(ModifierIndex::Mutable.bit(), 1 << 5);
  }

  #[test]
  fn test_combine_modifiers() {
    let combined = ModifierIndex::Declaration.bit() | ModifierIndex::Definition.bit() | ModifierIndex::Mutable.bit();
    assert_eq!(combined, 0b100011);
  }
}
