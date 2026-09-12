use super::HIRId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LoopKind {
  Infinite,
  While {
    condition: HIRId,
  },
  For {
    init: Option<HIRId>,
    condition: Option<HIRId>,
    update: Option<HIRId>,
  },
}

impl LoopKind {
  /// Every direct child node of this loop header, in a stable order.
  pub fn child_ids(&self) -> Vec<HIRId> {
    match self {
      LoopKind::Infinite => Vec::new(),
      LoopKind::While { condition } => vec![*condition],
      LoopKind::For {
        init,
        condition,
        update,
      } => init
        .iter()
        .chain(condition.iter())
        .chain(update.iter())
        .copied()
        .collect(),
    }
  }

  /// Offset all HIRIds in this LoopKind by the given amount.
  pub fn offset_ids(
    &mut self,
    offset: u32,
  ) {
    match self {
      LoopKind::Infinite => {},
      LoopKind::While { condition } => {
        *condition = HIRId::new(condition.index() + offset);
      },
      LoopKind::For {
        init,
        condition,
        update,
      } => {
        if let Some(id) = init {
          *id = HIRId::new(id.index() + offset);
        }
        if let Some(id) = condition {
          *id = HIRId::new(id.index() + offset);
        }
        if let Some(id) = update {
          *id = HIRId::new(id.index() + offset);
        }
      },
    }
  }
}
