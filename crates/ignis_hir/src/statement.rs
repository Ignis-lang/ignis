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
