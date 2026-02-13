use ignis_type::{definition::DefinitionId, value::IgnisLiteralValue};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HIRPattern {
  Wildcard,
  Literal {
    value: IgnisLiteralValue,
  },
  Binding {
    def_id: DefinitionId,
  },
  Variant {
    enum_def: DefinitionId,
    variant_tag: u32,
    args: Vec<HIRPattern>,
  },
  Tuple {
    elements: Vec<HIRPattern>,
  },
  Or {
    patterns: Vec<HIRPattern>,
  },
}
