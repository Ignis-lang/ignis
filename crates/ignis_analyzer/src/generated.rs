use ignis_ast::NodeId;
use ignis_type::attribute::DirectivePhase;
use ignis_type::definition::{DefinitionId, DirectiveDefId};
use ignis_type::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GeneratedFingerprint {
  Opaque(String),
}

impl GeneratedFingerprint {
  pub fn opaque(value: impl Into<String>) -> Self {
    Self::Opaque(value.into())
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GeneratedOrigin {
  pub directive: DirectiveDefId,
  pub directive_use_span: Span,
  pub target_node: NodeId,
  pub target_def: Option<DefinitionId>,
  pub source_order: usize,
  pub generation_id: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GeneratedItemDeltaKind {
  Record,
  AttachedMethod {
    owner: DefinitionId,
    is_static: bool,
  },
  Implements {
    owner: DefinitionId,
    trait_def: DefinitionId,
  },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GeneratedItemDelta {
  pub origin: GeneratedOrigin,
  pub kind: GeneratedItemDeltaKind,
  pub fingerprint: GeneratedFingerprint,
}

impl GeneratedItemDelta {
  pub fn new(
    origin: GeneratedOrigin,
    kind: GeneratedItemDeltaKind,
    fingerprint: GeneratedFingerprint,
  ) -> Self {
    Self {
      origin,
      kind,
      fingerprint,
    }
  }

  fn sort_key(&self) -> (u32, ignis_type::BytePosition, ignis_type::BytePosition, usize, u32) {
    (
      self.origin.directive_use_span.file.index(),
      self.origin.directive_use_span.start,
      self.origin.directive_use_span.end,
      self.origin.source_order,
      self.origin.generation_id,
    )
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratedBatch {
  pub iteration: usize,
  pub phase: DirectivePhase,
  pub entries: Vec<GeneratedItemDelta>,
}

impl GeneratedBatch {
  pub fn new(
    iteration: usize,
    phase: DirectivePhase,
    mut entries: Vec<GeneratedItemDelta>,
  ) -> Self {
    entries.sort_by_key(GeneratedItemDelta::sort_key);

    Self {
      iteration,
      phase,
      entries,
    }
  }
}

#[cfg(test)]
mod tests {
  use ignis_type::attribute::DirectivePhase;
  use ignis_type::file::FileId;
  use ignis_type::BytePosition;

  use super::*;

  fn span(
    start: u32,
    end: u32,
  ) -> Span {
    Span::new(FileId::SYNTHETIC, BytePosition(start), BytePosition(end))
  }

  #[test]
  fn generated_batch_orders_entries_by_origin_coordinates() {
    let batch = GeneratedBatch::new(
      0,
      DirectivePhase::Expand,
      vec![
        GeneratedItemDelta::new(
          GeneratedOrigin {
            directive: DirectiveDefId::new(1),
            directive_use_span: span(20, 25),
            target_node: NodeId::new(4),
            target_def: Some(DefinitionId::new(5)),
            source_order: 1,
            generation_id: 1,
          },
          GeneratedItemDeltaKind::AttachedMethod {
            owner: DefinitionId::new(5),
            is_static: false,
          },
          GeneratedFingerprint::opaque("late"),
        ),
        GeneratedItemDelta::new(
          GeneratedOrigin {
            directive: DirectiveDefId::new(1),
            directive_use_span: span(10, 15),
            target_node: NodeId::new(2),
            target_def: Some(DefinitionId::new(3)),
            source_order: 0,
            generation_id: 2,
          },
          GeneratedItemDeltaKind::Record,
          GeneratedFingerprint::opaque("first"),
        ),
        GeneratedItemDelta::new(
          GeneratedOrigin {
            directive: DirectiveDefId::new(1),
            directive_use_span: span(10, 15),
            target_node: NodeId::new(2),
            target_def: Some(DefinitionId::new(3)),
            source_order: 0,
            generation_id: 4,
          },
          GeneratedItemDeltaKind::Implements {
            owner: DefinitionId::new(3),
            trait_def: DefinitionId::new(9),
          },
          GeneratedFingerprint::opaque("second"),
        ),
      ],
    );

    assert_eq!(
      batch
        .entries
        .iter()
        .map(|entry| entry.fingerprint.clone())
        .collect::<Vec<_>>(),
      vec![
        GeneratedFingerprint::opaque("first"),
        GeneratedFingerprint::opaque("second"),
        GeneratedFingerprint::opaque("late"),
      ]
    );
  }
}
