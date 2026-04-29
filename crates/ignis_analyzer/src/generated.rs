use ignis_ast::{
  ASTNode, NodeId,
  statements::{ASTRecord, ASTStatement},
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::attribute::DirectivePhase;
use ignis_type::definition::{
  Definition, DefinitionId, DefinitionKind, DirectiveDefId, GeneratedItemKind, GeneratedItemMetadata, MethodDefinition,
  SymbolEntry, Visibility,
};
use ignis_type::span::Span;

use crate::directive_registry::DirectiveUse;
use crate::Analyzer;

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
  pub target_span: Span,
  pub target_node: NodeId,
  pub target_def: Option<DefinitionId>,
  pub source_order: usize,
  pub generation_id: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GeneratedItemDeltaKind {
  Record {
    name: String,
  },
  AttachedMethod {
    owner: DefinitionId,
    name: String,
    is_static: bool,
  },
  Implements {
    owner: DefinitionId,
    trait_name: String,
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

  pub fn sort_key(&self) -> (u32, ignis_type::BytePosition, ignis_type::BytePosition, usize, u32) {
    (
      self.origin.target_span.file.index(),
      self.origin.target_span.start,
      self.origin.target_span.end,
      self.origin.source_order,
      self.origin.generation_id,
    )
  }

  pub fn fingerprint_value(&self) -> &str {
    match &self.fingerprint {
      GeneratedFingerprint::Opaque(value) => value.as_str(),
    }
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

#[derive(Clone)]
pub struct GeneratedIntegrationSnapshot {
  working_ast: ignis_type::Store<ASTNode>,
  defs: ignis_type::definition::DefinitionStore,
  types: ignis_type::types::TypeStore,
  namespaces: ignis_type::namespace::NamespaceStore,
  scopes: crate::ScopeTree,
  node_defs: std::collections::HashMap<NodeId, DefinitionId>,
  node_types: std::collections::HashMap<NodeId, ignis_type::types::TypeId>,
  directive_registry: crate::directive_registry::DirectiveRegistry,
  diagnostics: Vec<ignis_diagnostics::diagnostic_report::Diagnostic>,
  generated_roots: Vec<NodeId>,
}

impl GeneratedIntegrationSnapshot {
  pub fn capture(analyzer: &Analyzer<'_>) -> Self {
    Self {
      working_ast: analyzer.working_ast.clone(),
      defs: analyzer.defs.clone(),
      types: analyzer.types.clone(),
      namespaces: analyzer.namespaces.clone(),
      scopes: analyzer.scopes.clone(),
      node_defs: analyzer.node_defs.clone(),
      node_types: analyzer.node_types.clone(),
      directive_registry: analyzer.directive_registry.clone(),
      diagnostics: analyzer.diagnostics.clone(),
      generated_roots: analyzer.generated_roots.clone(),
    }
  }

  pub fn restore(
    self,
    analyzer: &mut Analyzer<'_>,
  ) {
    analyzer.working_ast = self.working_ast;
    analyzer.defs = self.defs;
    analyzer.types = self.types;
    analyzer.namespaces = self.namespaces;
    analyzer.scopes = self.scopes;
    analyzer.node_defs = self.node_defs;
    analyzer.node_types = self.node_types;
    analyzer.directive_registry = self.directive_registry;
    analyzer.diagnostics = self.diagnostics;
    analyzer.generated_roots = self.generated_roots;
  }
}

pub fn integrate_generated_batches(
  analyzer: &mut Analyzer<'_>,
  roots: &[NodeId],
) {
  let generated_batches = analyzer.directive_execution_report.generated_batches.clone();
  let mut seen_batches = std::collections::HashSet::new();

  for batch in generated_batches {
    let batch_key = batch
      .entries
      .iter()
      .map(|entry| (entry.origin.generation_id, entry.fingerprint_value().to_string()))
      .collect::<Vec<_>>();

    if !seen_batches.insert(batch_key) {
      continue;
    }

    let snapshot = GeneratedIntegrationSnapshot::capture(analyzer);
    let baseline_diagnostics_len = snapshot.diagnostics.len();
    let generated_roots = materialize_generated_batch(analyzer, &batch.entries);

    analyzer.bind_generated_phase(&generated_roots);
    let preserved_diagnostics = analyzer.diagnostics.split_off(baseline_diagnostics_len);
    replay_generated_semantics(analyzer, roots, &generated_roots, preserved_diagnostics);

    let mut new_diagnostics = analyzer.diagnostics.clone();
    dedupe_diagnostics(&mut new_diagnostics);
    let has_errors = !new_diagnostics.is_empty();

    if has_errors {
      snapshot.restore(analyzer);
      analyzer.diagnostics.extend(new_diagnostics);
      dedupe_diagnostics(&mut analyzer.diagnostics);
      continue;
    }
  }
}

fn replay_generated_semantics(
  analyzer: &mut Analyzer<'_>,
  roots: &[NodeId],
  generated_roots: &[NodeId],
  preserved_diagnostics: Vec<ignis_diagnostics::diagnostic_report::Diagnostic>,
) {
  let replay_roots = replay_roots(roots, &analyzer.generated_roots, generated_roots);
  analyzer.diagnostics = preserved_diagnostics;
  analyzer.node_types.clear();
  analyzer.resolved_calls.clear();
  analyzer.import_item_defs.clear();
  analyzer.extension_methods.clear();
  analyzer.scope_infer_vars.clear();
  analyzer.infer_ctx = ignis_type::inference::InferCtx::new();

  let replay_start = analyzer.diagnostics.len();

  analyzer.resolve_phase(&replay_roots);
  analyzer.typecheck_phase(&replay_roots);

  let replay_roots_vec = replay_roots.clone();
  analyzer.const_eval_phase(&replay_roots_vec);
  analyzer.extra_checks_phase(&replay_roots);

  attach_generated_replay_provenance(analyzer, &replay_roots, replay_start);
  dedupe_diagnostics(&mut analyzer.diagnostics);
}

fn replay_roots(
  roots: &[NodeId],
  committed_generated_roots: &[NodeId],
  batch_generated_roots: &[NodeId],
) -> Vec<NodeId> {
  let mut replay_roots = roots.to_vec();

  for generated_root in committed_generated_roots.iter().chain(batch_generated_roots.iter()) {
    if !replay_roots.contains(generated_root) {
      replay_roots.push(*generated_root);
    }
  }

  replay_roots
}

fn attach_generated_replay_provenance(
  analyzer: &mut Analyzer<'_>,
  replay_roots: &[NodeId],
  diagnostics_start: usize,
) {
  let replay_entries = replay_roots
    .iter()
    .filter_map(|root| {
      let root_span = analyzer.node_span(root).clone();
      let root_def_id = analyzer.node_defs.get(root).copied()?;
      let provenance_items = generated_provenance_items(analyzer, root_def_id);

      (!provenance_items.is_empty()).then_some((*root, root_span, provenance_items))
    })
    .collect::<Vec<_>>();

  for diagnostic in analyzer.diagnostics.iter_mut().skip(diagnostics_start) {
    for (root_node, root_span, provenance_items) in &replay_entries {
      if !span_contains(root_span, &diagnostic.primary_span) {
        continue;
      }

      for metadata in provenance_items {
        if !diagnostic
          .labels
          .iter()
          .any(|label| label.span == metadata.provenance.origin_attr_span && label.message == "directive declaration")
        {
          diagnostic.labels.push(ignis_diagnostics::diagnostic_report::Label {
            span: metadata.provenance.origin_attr_span.clone(),
            message: "directive declaration".to_string(),
          });
        }

        if let Some(directive_use) = analyzer.directive_registry.uses.iter().find(|directive_use| {
          directive_use.directive == metadata.provenance.directive && directive_use.target_node == *root_node
        }) && !diagnostic
          .labels
          .iter()
          .any(|label| label.span == directive_use.span && label.message == "directive use")
        {
          diagnostic.labels.push(ignis_diagnostics::diagnostic_report::Label {
            span: directive_use.span.clone(),
            message: "directive use".to_string(),
          });
        }

        let provenance_note = format!(
          "generated item #{}: {}",
          metadata.provenance.generation_id,
          generated_item_description(&metadata.kind)
        );

        if !diagnostic.notes.contains(&provenance_note) {
          diagnostic.notes.push(provenance_note);
        }
      }

      break;
    }
  }
}

fn dedupe_diagnostics(diagnostics: &mut Vec<ignis_diagnostics::diagnostic_report::Diagnostic>) {
  let mut seen = std::collections::HashSet::new();

  diagnostics.retain(|diagnostic| {
    let key = format!(
      "{}|{}|{}|{}|{}",
      diagnostic.error_code,
      diagnostic.message,
      diagnostic.primary_span.file.index(),
      diagnostic.primary_span.start.0,
      diagnostic.primary_span.end.0,
    );

    seen.insert(key)
  });
}

fn generated_provenance_items(
  analyzer: &Analyzer<'_>,
  root_def_id: DefinitionId,
) -> Vec<GeneratedItemMetadata> {
  let mut metadata = Vec::new();

  if let Some(item) = analyzer.directive_registry.generated_item_metadata(&root_def_id) {
    metadata.push(item.clone());
  }

  metadata.extend(
    analyzer
      .directive_registry
      .generated_attached_method_items_for_owner(root_def_id)
      .into_iter()
      .map(|item| item.metadata),
  );
  metadata.extend(
    analyzer
      .directive_registry
      .generated_implemented_trait_items_for_owner(root_def_id)
      .into_iter()
      .map(|item| item.metadata),
  );

  metadata.sort_by_key(|item| item.provenance.generation_id);
  metadata.dedup();
  metadata
}

fn generated_item_description(kind: &GeneratedItemKind) -> String {
  match kind {
    GeneratedItemKind::Record => "generated record".to_string(),
    GeneratedItemKind::AttachedMethod { owner_type, is_static } => {
      let static_label = if *is_static { "static " } else { "" };
      format!(
        "{}generated attached method for definition {}",
        static_label,
        owner_type.index()
      )
    },
    GeneratedItemKind::ImplementedTrait { owner_type, trait_def } => {
      format!(
        "generated implemented trait attachment for definition {} -> trait {}",
        owner_type.index(),
        trait_def.index()
      )
    },
  }
}

fn span_contains(
  container: &Span,
  inner: &Span,
) -> bool {
  container.file == inner.file && container.start <= inner.start && container.end >= inner.end
}

fn materialize_generated_batch(
  analyzer: &mut Analyzer<'_>,
  entries: &[GeneratedItemDelta],
) -> Vec<NodeId> {
  let directive_uses = analyzer.directive_registry.uses.clone();
  let mut generated_roots = Vec::new();

  for entry in entries {
    let Some(directive_use) = directive_uses.iter().find(|directive_use| {
      directive_use.directive == entry.origin.directive
        && directive_use.target_node == entry.origin.target_node
        && directive_use.span == entry.origin.directive_use_span
    }) else {
      continue;
    };

    match &entry.kind {
      GeneratedItemDeltaKind::Record { name } => {
        let record_node = materialize_generated_record(analyzer, directive_use, entry.origin.generation_id, name);
        generated_roots.push(record_node);
      },
      GeneratedItemDeltaKind::AttachedMethod { owner, name, is_static } => {
        materialize_generated_method(analyzer, directive_use, entry.origin.generation_id, *owner, name, *is_static);
      },
      GeneratedItemDeltaKind::Implements { owner, trait_name } => {
        materialize_generated_implements(analyzer, directive_use, entry.origin.generation_id, *owner, trait_name);
      },
    }
  }

  generated_roots
}

fn materialize_generated_record(
  analyzer: &mut Analyzer<'_>,
  directive_use: &DirectiveUse,
  generation_id: u32,
  name: &str,
) -> NodeId {
  let symbol = analyzer.symbols.borrow_mut().get_or_intern(name);
  let record_node = analyzer
    .working_ast
    .alloc(ASTNode::Statement(ASTStatement::Record(ASTRecord::new(
      symbol,
      None,
      Vec::new(),
      directive_use.span.clone(),
      None,
      Vec::new(),
    ))));

  analyzer.generated_roots.push(record_node);

  let generated_provenance = directive_use.generated_provenance(generation_id);
  let generated_metadata = GeneratedItemMetadata::record(generated_provenance);

  if let Some(definition_id) = analyzer.node_defs.get(&record_node).copied() {
    analyzer
      .directive_registry
      .attach_generated_item(definition_id, generated_metadata);
  } else {
    analyzer.pending_generated_items.push((record_node, generated_metadata));
  }

  record_node
}

fn materialize_generated_method(
  analyzer: &mut Analyzer<'_>,
  directive_use: &DirectiveUse,
  generation_id: u32,
  owner: DefinitionId,
  name: &str,
  is_static: bool,
) {
  let symbol = analyzer.symbols.borrow_mut().get_or_intern(name);
  let method_span = directive_use.span.clone();
  let visibility = Visibility::Private;

  let method_def = Definition {
    kind: DefinitionKind::Method(MethodDefinition {
      owner_type: owner,
      type_params: Vec::new(),
      params: Vec::new(),
      return_type: analyzer.types.void(),
      is_static,
      self_mutable: false,
      inline_mode: ignis_type::definition::InlineMode::None,
      attrs: Vec::new(),
    }),
    name: symbol,
    span: method_span.clone(),
    name_span: method_span.clone(),
    visibility,
    owner_module: analyzer.current_module,
    owner_namespace: analyzer.current_namespace,
    doc: None,
  };

  let method_def_id = analyzer.defs.alloc(method_def);

  register_generated_method_on_owner(analyzer, owner, symbol, method_def_id, is_static);

  let generated_metadata =
    GeneratedItemMetadata::attached_method(directive_use.generated_provenance(generation_id), owner, is_static);
  analyzer
    .directive_registry
    .attach_generated_item(method_def_id, generated_metadata);
}

fn materialize_generated_implements(
  analyzer: &mut Analyzer<'_>,
  directive_use: &DirectiveUse,
  generation_id: u32,
  owner: DefinitionId,
  trait_name: &str,
) {
  let Some(trait_def_id) = lookup_trait_definition(analyzer, trait_name) else {
    analyzer.add_diagnostic(
      DiagnosticMessage::UnknownTraitInImplements {
        name: trait_name.to_string(),
        span: directive_use.span.clone(),
      }
      .report(),
    );

    return;
  };

  attach_generated_trait_to_owner(analyzer, owner, trait_def_id);

  let generated_metadata =
    GeneratedItemMetadata::implemented_trait(directive_use.generated_provenance(generation_id), owner, trait_def_id);
  analyzer
    .directive_registry
    .attach_generated_item(owner, generated_metadata);
}

fn register_generated_method_on_owner(
  analyzer: &mut Analyzer<'_>,
  owner: DefinitionId,
  method_name: ignis_type::symbol::SymbolId,
  method_def_id: DefinitionId,
  is_static: bool,
) {
  let target_map = match &mut analyzer.defs.get_mut(&owner).kind {
    DefinitionKind::Record(record) => {
      if is_static {
        &mut record.static_methods
      } else {
        &mut record.instance_methods
      }
    },
    DefinitionKind::Enum(enum_def) => {
      if is_static {
        &mut enum_def.static_methods
      } else {
        &mut enum_def.instance_methods
      }
    },
    _ => return,
  };

  match target_map.get_mut(&method_name) {
    Some(SymbolEntry::Overload(group)) => group.push(method_def_id),
    Some(SymbolEntry::Single(existing)) => {
      let existing = *existing;
      target_map.insert(method_name, SymbolEntry::Overload(vec![existing, method_def_id]));
    },
    None => {
      target_map.insert(method_name, SymbolEntry::Single(method_def_id));
    },
  }
}

fn attach_generated_trait_to_owner(
  analyzer: &mut Analyzer<'_>,
  owner: DefinitionId,
  trait_def_id: DefinitionId,
) {
  match &mut analyzer.defs.get_mut(&owner).kind {
    DefinitionKind::Record(record) if !record.implemented_traits.contains(&trait_def_id) => {
      record.implemented_traits.push(trait_def_id);
    },
    DefinitionKind::Enum(enum_def) if !enum_def.implemented_traits.contains(&trait_def_id) => {
      enum_def.implemented_traits.push(trait_def_id);
    },
    _ => {},
  }
}

fn lookup_trait_definition(
  analyzer: &Analyzer<'_>,
  trait_name: &str,
) -> Option<DefinitionId> {
  let trait_symbol = analyzer.symbols.borrow_mut().get_or_intern(trait_name);

  analyzer.defs.iter().find_map(|(definition_id, definition)| {
    (definition.name == trait_symbol && matches!(definition.kind, DefinitionKind::Trait(_))).then_some(definition_id)
  })
}

pub fn commit_generated_metadata(analyzer: &mut Analyzer<'_>) {
  let pending_items = std::mem::take(&mut analyzer.pending_generated_items);

  for (node_id, metadata) in pending_items {
    if let Some(definition_id) = analyzer.node_defs.get(&node_id).copied() {
      analyzer
        .directive_registry
        .attach_generated_item(definition_id, metadata);
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
            target_span: span(40, 45),
            target_node: NodeId::new(4),
            target_def: Some(DefinitionId::new(5)),
            source_order: 1,
            generation_id: 1,
          },
          GeneratedItemDeltaKind::AttachedMethod {
            owner: DefinitionId::new(5),
            name: "method".to_string(),
            is_static: false,
          },
          GeneratedFingerprint::opaque("late"),
        ),
        GeneratedItemDelta::new(
          GeneratedOrigin {
            directive: DirectiveDefId::new(1),
            directive_use_span: span(10, 15),
            target_span: span(10, 15),
            target_node: NodeId::new(2),
            target_def: Some(DefinitionId::new(3)),
            source_order: 0,
            generation_id: 2,
          },
          GeneratedItemDeltaKind::Record {
            name: "First".to_string(),
          },
          GeneratedFingerprint::opaque("first"),
        ),
        GeneratedItemDelta::new(
          GeneratedOrigin {
            directive: DirectiveDefId::new(1),
            directive_use_span: span(10, 15),
            target_span: span(10, 15),
            target_node: NodeId::new(2),
            target_def: Some(DefinitionId::new(3)),
            source_order: 0,
            generation_id: 4,
          },
          GeneratedItemDeltaKind::Implements {
            owner: DefinitionId::new(3),
            trait_name: "Marker".to_string(),
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
