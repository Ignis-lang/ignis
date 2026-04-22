use ignis_ast::NodeId;

use crate::Analyzer;

pub(crate) fn run(
  analyzer: &mut Analyzer<'_>,
  roots: &[NodeId],
) {
  analyzer.typecheck_phase(roots);
}
