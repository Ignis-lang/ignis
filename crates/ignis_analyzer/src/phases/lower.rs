use ignis_ast::NodeId;
use ignis_hir::HIR;

use crate::Analyzer;

pub(crate) fn run(
  analyzer: &mut Analyzer<'_>,
  roots: &[NodeId],
) -> HIR {
  analyzer.lower_to_hir(roots)
}
