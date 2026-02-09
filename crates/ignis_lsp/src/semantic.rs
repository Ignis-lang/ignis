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
  SemanticTokenType::NAMESPACE,      // 0
  SemanticTokenType::TYPE,           // 1
  SemanticTokenType::CLASS,          // 2
  SemanticTokenType::ENUM,           // 3
  SemanticTokenType::STRUCT,         // 4
  SemanticTokenType::TYPE_PARAMETER, // 5
  SemanticTokenType::PARAMETER,      // 6
  SemanticTokenType::VARIABLE,       // 7
  SemanticTokenType::PROPERTY,       // 8
  SemanticTokenType::ENUM_MEMBER,    // 9
  SemanticTokenType::FUNCTION,       // 10
  SemanticTokenType::METHOD,         // 11
  SemanticTokenType::KEYWORD,        // 12
  SemanticTokenType::MODIFIER,       // 13
  SemanticTokenType::COMMENT,        // 14
  SemanticTokenType::STRING,         // 15
  SemanticTokenType::NUMBER,         // 16
  SemanticTokenType::OPERATOR,       // 17
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
  SemanticTokenModifier::new("mutable"),
];

/// Build the semantic tokens legend for capability registration.
pub fn build_legend() -> SemanticTokensLegend {
  SemanticTokensLegend {
    token_types: SUPPORTED_TYPES.to_vec(),
    token_modifiers: SUPPORTED_MODIFIERS.to_vec(),
  }
}

/// Token type indices matching SUPPORTED_TYPES array.
/// Some variants are unused but kept for completeness with the LSP spec.
#[derive(Debug, Clone, Copy)]
#[repr(u32)]
#[allow(dead_code)]
enum TokenTypeIndex {
  Namespace = 0,
  Type = 1,
  Class = 2,
  Enum = 3,
  Struct = 4,
  TypeParameter = 5,
  Parameter = 6,
  Variable = 7,
  Property = 8,
  EnumMember = 9,
  Function = 10,
  Method = 11,
  Keyword = 12,
  Modifier = 13,
  Comment = 14,
  String = 15,
  Number = 16,
  Operator = 17,
}

/// Modifier indices matching SUPPORTED_MODIFIERS array.
/// Some variants are unused but kept for completeness with the LSP spec.
#[derive(Debug, Clone, Copy)]
#[repr(u32)]
#[allow(dead_code)]
enum ModifierIndex {
  Declaration = 0,
  Definition = 1,
  Readonly = 2,
  Static = 3,
  Modification = 4,
  Mutable = 5,
}

impl ModifierIndex {
  fn bit(self) -> u32 {
    1 << (self as u32)
  }
}

/// Token with absolute position (before delta encoding).
struct AbsToken {
  line: u32,
  col: u32,
  len: u32,
  ty: u32,
  mods: u32,
}

/// Classify tokens from lexer output using semantic analysis.
pub fn classify_tokens(
  tokens: &[Token],
  ref_spans: &HashMap<Span, DefinitionId>,
  defs: &DefinitionStore,
  file_id: &ignis_type::file::FileId,
  line_index: &LineIndex,
) -> SemanticTokens {
  let mut abs_tokens = Vec::new();

  for token in tokens {
    if &token.span.file != file_id {
      continue;
    }

    if matches!(token.type_, TokenType::Whitespace | TokenType::Eof) {
      continue;
    }

    if let Some(abs) = classify_token(token, ref_spans, defs, line_index) {
      abs_tokens.push(abs);
    }
  }

  encode_tokens(abs_tokens)
}

/// Classify a single token.
fn classify_token(
  token: &Token,
  ref_spans: &HashMap<Span, DefinitionId>,
  defs: &DefinitionStore,
  line_index: &LineIndex,
) -> Option<AbsToken> {
  let (ty, mods) = match token.type_ {
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

    TokenType::Static | TokenType::Mut | TokenType::Public | TokenType::Private => (TokenTypeIndex::Modifier as u32, 0),

    TokenType::True | TokenType::False => (TokenTypeIndex::Keyword as u32, 0),

    TokenType::Null => (TokenTypeIndex::Keyword as u32, 0),

    TokenType::Self_ | TokenType::This => (TokenTypeIndex::Keyword as u32, 0),

    TokenType::Void => (TokenTypeIndex::Type as u32, ModifierIndex::Readonly.bit()),

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

    TokenType::String => (TokenTypeIndex::String as u32, 0),
    TokenType::Int | TokenType::Float | TokenType::Hex | TokenType::Binary | TokenType::Char => {
      (TokenTypeIndex::Number as u32, 0)
    },

    TokenType::Comment | TokenType::MultiLineComment | TokenType::DocComment => (TokenTypeIndex::Comment as u32, 0),

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

    TokenType::Identifier => {
      return classify_identifier(token, ref_spans, defs, line_index);
    },

    _ => return None,
  };

  let (line, col) = line_index.line_col_utf16(token.span.start);

  Some(AbsToken {
    line,
    col,
    len: token.lexeme.encode_utf16().count() as u32,
    ty,
    mods,
  })
}

/// Classify an identifier token using semantic analysis.
fn classify_identifier(
  token: &Token,
  ref_spans: &HashMap<Span, DefinitionId>,
  defs: &DefinitionStore,
  line_index: &LineIndex,
) -> Option<AbsToken> {
  let def_id = ref_spans.get(&token.span)?;
  let def = defs.get(def_id);
  let (ty, mods) = classify_definition(def, &token.span);
  let (line, col) = line_index.line_col_utf16(token.span.start);

  Some(AbsToken {
    line,
    col,
    len: token.lexeme.encode_utf16().count() as u32,
    ty,
    mods,
  })
}

/// Classify a definition into token type and modifiers.
fn classify_definition(
  def: &Definition,
  token_span: &Span,
) -> (u32, u32) {
  let is_declaration = token_span == &def.name_span;

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
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit();
      }
      if param_def.mutable {
        modifiers |= ModifierIndex::Mutable.bit();
      } else {
        modifiers |= ModifierIndex::Readonly.bit();
      }
      (TokenTypeIndex::Parameter as u32, modifiers)
    },

    DefinitionKind::TypeParam(_) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit();
      }
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
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit();
      }
      (TokenTypeIndex::EnumMember as u32, modifiers)
    },

    DefinitionKind::Trait(_) => {
      let mut modifiers = 0;
      if is_declaration {
        modifiers |= ModifierIndex::Declaration.bit();
      }
      (TokenTypeIndex::Type as u32, modifiers)
    },

    DefinitionKind::Placeholder => (TokenTypeIndex::Variable as u32, 0),
  }
}

/// Encode tokens with absolute positions into LSP delta format.
fn encode_tokens(mut tokens: Vec<AbsToken>) -> SemanticTokens {
  tokens.sort_by(|a, b| a.line.cmp(&b.line).then_with(|| a.col.cmp(&b.col)));

  let mut prev_line = 0;
  let mut prev_col = 0;
  let mut data = Vec::with_capacity(tokens.len());

  for tok in tokens {
    let delta_line = tok.line.saturating_sub(prev_line);
    let delta_start = if delta_line == 0 {
      tok.col.saturating_sub(prev_col)
    } else {
      tok.col
    };

    data.push(SemanticToken {
      delta_line,
      delta_start,
      length: tok.len,
      token_type: tok.ty,
      token_modifiers_bitset: tok.mods,
    });

    prev_line = tok.line;
    prev_col = tok.col;
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
    assert_eq!(ModifierIndex::Modification.bit(), 1 << 4);
    assert_eq!(ModifierIndex::Mutable.bit(), 1 << 5);
  }

  #[test]
  fn test_combine_modifiers() {
    let combined = ModifierIndex::Declaration.bit() | ModifierIndex::Definition.bit() | ModifierIndex::Mutable.bit();
    assert_eq!(combined, 0b100011);
  }

  #[test]
  fn test_token_type_indices() {
    assert_eq!(TokenTypeIndex::Namespace as u32, 0);
    assert_eq!(TokenTypeIndex::Type as u32, 1);
    assert_eq!(TokenTypeIndex::Class as u32, 2);
    assert_eq!(TokenTypeIndex::Enum as u32, 3);
    assert_eq!(TokenTypeIndex::Struct as u32, 4);
    assert_eq!(TokenTypeIndex::TypeParameter as u32, 5);
    assert_eq!(TokenTypeIndex::Parameter as u32, 6);
    assert_eq!(TokenTypeIndex::Variable as u32, 7);
    assert_eq!(TokenTypeIndex::Property as u32, 8);
    assert_eq!(TokenTypeIndex::EnumMember as u32, 9);
    assert_eq!(TokenTypeIndex::Function as u32, 10);
    assert_eq!(TokenTypeIndex::Method as u32, 11);
    assert_eq!(TokenTypeIndex::Keyword as u32, 12);
    assert_eq!(TokenTypeIndex::Modifier as u32, 13);
    assert_eq!(TokenTypeIndex::Comment as u32, 14);
    assert_eq!(TokenTypeIndex::String as u32, 15);
    assert_eq!(TokenTypeIndex::Number as u32, 16);
    assert_eq!(TokenTypeIndex::Operator as u32, 17);
  }
}
