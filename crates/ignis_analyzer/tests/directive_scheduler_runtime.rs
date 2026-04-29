mod common;

use ignis_ast::{ASTNode, statements::ASTStatement};
use ignis_analyzer::generated::{
  GeneratedBatch, GeneratedFingerprint, GeneratedItemDelta, GeneratedItemDeltaKind, GeneratedOrigin,
};
use ignis_type::attribute::DirectivePhase;
use ignis_type::definition::{DefinitionId, DirectiveDefId};
use ignis_type::file::FileId;
use ignis_type::span::Span;
use ignis_type::BytePosition;

fn span(
  start: u32,
  end: u32,
) -> Span {
  Span::new(FileId::SYNTHETIC, BytePosition(start), BytePosition(end))
}

#[test]
fn generated_batches_sort_entries_deterministically_before_materialization() {
  let batch = GeneratedBatch::new(
    0,
    DirectivePhase::Expand,
    vec![
      GeneratedItemDelta::new(
        GeneratedOrigin {
          directive: DirectiveDefId::new(3),
          directive_use_span: span(20, 24),
          target_span: span(20, 24),
          target_node: ignis_ast::NodeId::new(9),
          target_def: Some(DefinitionId::new(11)),
          source_order: 2,
          generation_id: 1,
        },
        GeneratedItemDeltaKind::Implements {
          owner: DefinitionId::new(11),
          trait_name: "Serializable".to_string(),
        },
        GeneratedFingerprint::opaque("implements-b"),
      ),
      GeneratedItemDelta::new(
        GeneratedOrigin {
          directive: DirectiveDefId::new(3),
          directive_use_span: span(10, 14),
          target_span: span(10, 14),
          target_node: ignis_ast::NodeId::new(5),
          target_def: Some(DefinitionId::new(7)),
          source_order: 1,
          generation_id: 2,
        },
        GeneratedItemDeltaKind::AttachedMethod {
          owner: DefinitionId::new(7),
          name: "methodA".to_string(),
          is_static: false,
        },
        GeneratedFingerprint::opaque("method-a2"),
      ),
      GeneratedItemDelta::new(
        GeneratedOrigin {
          directive: DirectiveDefId::new(3),
          directive_use_span: span(10, 14),
          target_span: span(10, 14),
          target_node: ignis_ast::NodeId::new(5),
          target_def: Some(DefinitionId::new(7)),
          source_order: 1,
          generation_id: 0,
        },
        GeneratedItemDeltaKind::Record {
          name: "RecordA".to_string(),
        },
        GeneratedFingerprint::opaque("record-a0"),
      ),
    ],
  );

  let ordered_fingerprints = batch
    .entries
    .iter()
    .map(|entry| entry.fingerprint.clone())
    .collect::<Vec<_>>();

  assert_eq!(
    ordered_fingerprints,
    vec![
      GeneratedFingerprint::opaque("record-a0"),
      GeneratedFingerprint::opaque("method-a2"),
      GeneratedFingerprint::opaque("implements-b"),
    ]
  );
}

#[test]
fn staged_analysis_keeps_no_directive_behavior_unchanged() {
  let src = r#"
function main(): i32 {
    return 42;
}
"#;

  let legacy = common::analyze(src);
  let staged = common::analyze_staged(src);

  assert_eq!(
    common::format_hir(&staged),
    common::format_hir(&legacy),
    "directive scheduling should not perturb no-directive lowering"
  );
  assert_eq!(
    common::format_diagnostics(&staged.output.diagnostics),
    common::format_diagnostics(&legacy.output.diagnostics),
    "directive scheduling should not add diagnostics when no directives exist"
  );
  assert!(staged.output.directive_execution_report.executed_phases.is_empty());
  assert!(staged.output.directive_execution_report.completed_iterations.is_empty());
}

#[test]
fn staged_analysis_records_scheduler_execution_before_lowering() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function checkRecord(context: Compile::Context, target: Compile::ItemReference): void {
        return;
      }

      @directive(target: "record", phase: expand, effect: emit)
      function expandRecord(): void {
        return;
      }

      @directive(target: "record", phase: collect, effect: collect, group: routes)
      function collectRecord(): void {
        return;
      }

      @directive(target: "record", phase: finalize, effect: collect, group: routes)
      function finalizeRecord(): void {
        return;
      }

      @directive(target: "record", phase: transform, effect: transform)
      function transformRecord(): void {
        return;
      }

      @transformRecord
      @finalizeRecord
      @collectRecord
      @expandRecord
      @checkRecord
      record User {
        value: i32;
      }
    "#,
  );

  assert_eq!(common::format_diagnostics(&result.output.diagnostics), "(no diagnostics)");
  assert_eq!(
    result.output.directive_execution_report.executed_phases,
    vec![
      DirectivePhase::Check,
      DirectivePhase::Expand,
      DirectivePhase::Collect,
      DirectivePhase::Finalize,
      DirectivePhase::Transform,
      DirectivePhase::Check,
      DirectivePhase::Expand,
      DirectivePhase::Collect,
      DirectivePhase::Finalize,
      DirectivePhase::Transform,
    ]
  );
  assert_eq!(result.output.directive_execution_report.completed_iterations.len(), 2);
  assert_eq!(result.output.directive_execution_report.reanalysis_requests.len(), 1);
  assert!(result.output.directive_execution_report.failure.is_none());
}

#[test]
fn staged_analysis_reports_denied_directive_capability_as_diagnostic() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}
      }

      @directive(target: "record", phase: check, effect: diagnose, capabilities: filesystem)
      function inspectFilesystem(context: Compile::Context, target: Compile::ItemReference): void {
        return;
      }

      @inspectFilesystem
      record User {
        value: i32;
      }
    "#,
  );

  let diagnostics = common::format_diagnostics(&result.output.diagnostics);

  assert!(
    diagnostics.contains("[ERROR] A0196"),
    "expected denied-capability diagnostic, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("filesystem"),
    "expected denied capability name, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("label: directive use") && diagnostics.contains("label: target item"),
    "expected denied capability provenance labels, got: {diagnostics}"
  );
  assert!(result.output.directive_execution_report.failure.is_some());
}

#[test]
fn staged_analysis_reports_unsupported_generation_calls_with_call_site_provenance() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitRecord(context: Context, target: ItemReference): void {
          return;
        }
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function validateRecord(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitRecord(context, target);
      }

      @validateRecord
      record User {
        value: i32;
      }
    "#,
  );

  let diagnostics = common::format_diagnostics(&result.output.diagnostics);
  let unsupported = result
    .output
    .diagnostics
    .iter()
    .find(|diagnostic| diagnostic.error_code == "A0199")
    .expect("expected unsupported-generation diagnostic");

  assert!(
    diagnostics.contains("unsupported std::compile generation API 'emitRecord'"),
    "expected unsupported-generation message, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("label: directive use") && diagnostics.contains("label: target item"),
    "expected unsupported-generation provenance labels, got: {diagnostics}"
  );
  assert_eq!(common::diagnostic_line(&result, unsupported), 13);
  assert!(result.output.directive_execution_report.failure.is_some());
}

#[test]
fn staged_analysis_expand_phase_emits_bounded_generated_deltas() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitRecord(context: Context, target: ItemReference, name: str): void {
          return;
        }

        function emitMethod(context: Context, target: ItemReference, name: str, isStatic: boolean): void {
          return;
        }

        function emitImplements(context: Context, target: ItemReference, traitName: str): void {
          return;
        }
      }

      trait Serializable {
      }

      @directive(target: "record", phase: expand, effect: emit)
      function derive(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitMethod(context, target, "generatedMethod", false);
        Compile::emitRecord(context, target, "GeneratedUser");
        Compile::emitImplements(context, target, "Serializable");
      }

      @derive
      record User {
        value: i32;
      }
    "#,
  );

  assert_eq!(common::format_diagnostics(&result.output.diagnostics), "(no diagnostics)");
  assert!(result.output.directive_execution_report.failure.is_none());
  assert_eq!(result.output.directive_execution_report.generated_batches.len(), 2);

  let mut symbols = result.output.symbols.borrow_mut();
  let user_name = symbols.intern("User");
  drop(symbols);

  let user_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == user_name).then_some(def_id))
    .expect("User definition id");

  for batch in &result.output.directive_execution_report.generated_batches {
    assert_eq!(batch.phase, DirectivePhase::Expand);
    assert_eq!(batch.entries.len(), 3);
    assert_eq!(batch.entries[0].origin.generation_id, 0);
    assert_eq!(batch.entries[1].origin.generation_id, 1);
    assert_eq!(batch.entries[2].origin.generation_id, 2);
    assert_eq!(
      batch.entries[0].kind,
      GeneratedItemDeltaKind::AttachedMethod {
        owner: user_def_id,
        name: "generatedMethod".to_string(),
        is_static: false,
      }
    );
    assert_eq!(
      batch.entries[1].kind,
      GeneratedItemDeltaKind::Record {
        name: "GeneratedUser".to_string(),
      }
    );
    assert_eq!(
      batch.entries[2].kind,
      GeneratedItemDeltaKind::Implements {
        owner: user_def_id,
        trait_name: "Serializable".to_string(),
      }
    );
  }
}

#[test]
fn staged_analysis_rejects_invalid_vm_directive_signatures() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record Diagnostic {}
        record ItemReference {}
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function genericDirective<T>(context: Compile::Context, target: Compile::ItemReference): void {
        return;
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function nonVoidDirective(context: Compile::Context, target: Compile::ItemReference): i32 {
        return 1;
      }

      extern Meta {
        @directive(target: "record", phase: check, effect: diagnose)
        function externDirective(context: Compile::Context, target: Compile::ItemReference): void;
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function missingTarget(context: Compile::Context): void {
        return;
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function extraParam(
        context: Compile::Context,
        target: Compile::ItemReference,
        diagnostic: Compile::Diagnostic,
      ): void {
        return;
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function wrongFirstParam(target: Compile::ItemReference, context: Compile::Context): void {
        return;
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function wrongSecondParam(context: Compile::Context, diagnostic: Compile::Diagnostic): void {
        return;
      }
    "#,
  );

  let diagnostics = common::format_diagnostics(&result.output.diagnostics);

  assert!(
    diagnostics.contains("cannot declare generic parameters"),
    "expected generic directive signature diagnostic, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("must return void"),
    "expected non-void directive signature diagnostic, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("externDirective") && diagnostics.contains("must not be extern"),
    "expected extern directive signature diagnostic, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("externDirective") && diagnostics.contains("must declare a body"),
    "expected missing-body directive signature diagnostic, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("missingTarget") && diagnostics.contains("must accept exactly 2 parameters"),
    "expected wrong-arity directive signature diagnostic, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("extraParam") && diagnostics.contains("must accept exactly 2 parameters"),
    "expected extra-param directive signature diagnostic, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("wrongFirstParam") && diagnostics.contains("first parameter must be Compile::Context"),
    "expected wrong-first-parameter directive signature diagnostic, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("wrongSecondParam") && diagnostics.contains("second parameter must be Compile::ItemReference"),
    "expected wrong-second-parameter directive signature diagnostic, got: {diagnostics}"
  );
}

#[test]
fn staged_analysis_executes_check_phase_diagnostic_directive_bodies() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function error(context: Context, target: ItemReference, message: str): void {
          return;
        }
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function validateRecord(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::error(context, target, "record failed validation");
      }

      @validateRecord
      record User {
        value: i32;
      }
    "#,
  );

  let diagnostics = common::format_diagnostics(&result.output.diagnostics);

  assert!(
    diagnostics.contains("record failed validation"),
    "expected directive body diagnostic emission, got: {diagnostics}"
  );
  assert!(
    diagnostics.contains("directive use") || diagnostics.contains("target item"),
    "expected directive provenance labels, got: {diagnostics}"
  );
  assert_eq!(
    result.output.directive_execution_report.executed_phases,
    vec![DirectivePhase::Check]
  );
  assert!(result.output.directive_execution_report.failure.is_none());
}

#[test]
fn staged_analysis_executes_noop_check_phase_directive_bodies_deterministically() {
  let src = r#"
    namespace Compile {
      record Context {}
      record ItemReference {}

      function warning(context: Context, target: ItemReference, message: str): void {
        return;
      }
    }

    @directive(target: "record", phase: check, effect: diagnose)
    function validateRecord(context: Compile::Context, target: Compile::ItemReference): void {
      if (false) {
        Compile::warning(context, target, "should stay unreachable");
      }

      return;
    }

    @validateRecord
    record User {
      value: i32;
    }
  "#;

  let first = common::analyze_staged(src);
  let second = common::analyze_staged(src);

  assert_eq!(common::format_diagnostics(&first.output.diagnostics), "(no diagnostics)");
  assert_eq!(
    first.output.directive_execution_report.completed_iterations,
    second.output.directive_execution_report.completed_iterations,
    "expected deterministic no-op execution fingerprints"
  );
  assert_eq!(
    first.output.directive_execution_report.executed_phases,
    vec![DirectivePhase::Check]
  );
  assert!(first.output.directive_execution_report.failure.is_none());
}

#[test]
fn staged_analysis_keeps_generated_batches_empty_before_materialization() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function note(context: Context, target: ItemReference, message: str): void {
          return;
        }
      }

      @directive(target: "record", phase: check, effect: diagnose)
      function validateRecord(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::note(context, target, "still diagnostics-only");
      }

      @validateRecord
      record User {
        value: i32;
      }
    "#,
  );

  assert!(result.output.directive_execution_report.generated_batches.is_empty());
  assert!(result.output.directive_registry.generated_items.is_empty());
}

#[test]
fn staged_analysis_materializes_generated_records_into_owned_ast_and_definitions() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitRecord(context: Context, target: ItemReference, name: str): void {
          return;
        }
      }

      @directive(target: "record", phase: expand, effect: emit)
      function derive(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitRecord(context, target, "GeneratedUser");
      }

      @derive
      record User {
        value: i32;
      }
    "#,
  );

  assert_eq!(common::format_diagnostics(&result.output.diagnostics), "(no diagnostics)");

  let mut symbols = result.output.symbols.borrow_mut();
  let generated_name = symbols.intern("GeneratedUser");
  drop(symbols);

  let generated_record_def = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == generated_name).then_some((def_id, &def.kind)))
    .expect("generated record definition");

  assert!(
    matches!(generated_record_def.1, ignis_type::definition::DefinitionKind::Record(_)),
    "expected GeneratedUser to be a record definition"
  );

  let generated_record_nodes = result
    .output
    .ast
    .iter()
    .filter_map(|(node_id, node)| match node {
      ASTNode::Statement(ASTStatement::Record(record)) if record.name == generated_name => Some(node_id),
      _ => None,
    })
    .collect::<Vec<_>>();

  assert_eq!(
    generated_record_nodes.len(),
    1,
    "expected one materialized GeneratedUser AST node"
  );
  assert_eq!(
    result.output.node_defs.get(&generated_record_nodes[0]),
    Some(&generated_record_def.0),
    "expected generated record node to resolve to its definition"
  );
  assert!(
    result
      .output
      .directive_registry
      .generated_items
      .contains_key(&generated_record_def.0),
    "expected generated record metadata to point at the committed definition"
  );
}

#[test]
fn staged_analysis_rolls_back_failed_generated_record_materialization_without_leaks() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitRecord(context: Context, target: ItemReference, name: str): void {
          return;
        }
      }

      @directive(target: "record", phase: expand, effect: emit)
      function derive(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitRecord(context, target, "User");
      }

      @derive
      record User {
        value: i32;
      }
    "#,
  );

  let diagnostics = common::format_diagnostics(&result.output.diagnostics);

  assert!(
    diagnostics.contains("User") && diagnostics.contains("already defined"),
    "expected duplicate generated record diagnostic, got: {diagnostics}"
  );

  let mut symbols = result.output.symbols.borrow_mut();
  let user_name = symbols.intern("User");
  drop(symbols);

  let user_record_nodes = result
    .output
    .ast
    .iter()
    .filter(|(_, node)| matches!(node, ASTNode::Statement(ASTStatement::Record(record)) if record.name == user_name))
    .count();

  let user_record_defs = result
    .output
    .defs
    .iter()
    .filter(|(_, def)| def.name == user_name && matches!(def.kind, ignis_type::definition::DefinitionKind::Record(_)))
    .count();

  assert_eq!(
    user_record_nodes, 1,
    "failed generated record insertion must not leak extra AST nodes"
  );
  assert_eq!(
    user_record_defs, 1,
    "failed generated record insertion must not leak extra definitions"
  );
  assert!(
    result.output.directive_registry.generated_items.is_empty(),
    "failed generated record insertion must not leak generated registry metadata"
  );
}

#[test]
fn staged_analysis_materializes_generated_attached_methods_and_implements() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitMethod(context: Context, target: ItemReference, name: str, isStatic: boolean): void {
          return;
        }

        function emitImplements(context: Context, target: ItemReference, traitName: str): void {
          return;
        }
      }

      trait Serializable {
      }

      @directive(target: "record", phase: expand, effect: emit)
      function derive(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitMethod(context, target, "generatedDescribe", false);
        Compile::emitImplements(context, target, "Serializable");
      }

      @derive
      record User {
        value: i32;
      }
    "#,
  );

  assert_eq!(common::format_diagnostics(&result.output.diagnostics), "(no diagnostics)");

  let mut symbols = result.output.symbols.borrow_mut();
  let user_name = symbols.intern("User");
  let serializable_name = symbols.intern("Serializable");
  let generated_method_name = symbols.intern("generatedDescribe");
  drop(symbols);

  let user_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == user_name).then_some(def_id))
    .expect("User definition id");
  let serializable_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == serializable_name).then_some(def_id))
    .expect("Serializable definition id");
  let generated_method_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| {
      (def.name == generated_method_name && matches!(def.kind, ignis_type::definition::DefinitionKind::Method(_)))
        .then_some(def_id)
    })
    .expect("generated method definition id");

  let user_record = result.output.defs.get(&user_def_id);
  let ignis_type::definition::DefinitionKind::Record(record) = &user_record.kind else {
    panic!("expected User record definition");
  };

  assert!(
    matches!(record.instance_methods.get(&generated_method_name), Some(ignis_type::definition::SymbolEntry::Single(def_id)) if *def_id == generated_method_def_id),
    "expected generated attached method to be committed into the owner method map"
  );
  assert_eq!(record.implemented_traits, vec![serializable_def_id]);
  assert_eq!(
    result.output.effective_implemented_traits_for_owner(user_def_id),
    vec![serializable_def_id]
  );

  let generated_items = result
    .output
    .directive_registry
    .generated_attached_method_items_for_owner(user_def_id);
  assert_eq!(generated_items.len(), 1, "expected one generated attached method entry");
  assert_eq!(generated_items[0].definition, generated_method_def_id);

  let generated_traits = result
    .output
    .directive_registry
    .generated_implemented_trait_items_for_owner(user_def_id);
  assert_eq!(generated_traits.len(), 1, "expected one generated implemented-trait entry");
  assert_eq!(generated_traits[0].definition, user_def_id);
}

#[test]
fn staged_analysis_rolls_back_failed_generated_implements_materialization_without_method_leaks() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitMethod(context: Context, target: ItemReference, name: str, isStatic: boolean): void {
          return;
        }

        function emitImplements(context: Context, target: ItemReference, traitName: str): void {
          return;
        }
      }

      @directive(target: "record", phase: expand, effect: emit)
      function derive(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitMethod(context, target, "generatedDescribe", false);
        Compile::emitImplements(context, target, "MissingTrait");
      }

      @derive
      record User {
        value: i32;
      }
    "#,
  );

  let diagnostics = common::format_diagnostics(&result.output.diagnostics);

  assert!(
    diagnostics.contains("MissingTrait") && diagnostics.contains("implements"),
    "expected generated implements failure diagnostic, got: {diagnostics}"
  );

  let mut symbols = result.output.symbols.borrow_mut();
  let user_name = symbols.intern("User");
  let generated_method_name = symbols.intern("generatedDescribe");
  drop(symbols);

  let user_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| (def.name == user_name).then_some(def_id))
    .expect("User definition id");

  let generated_method_defs = result
    .output
    .defs
    .iter()
    .filter(|(_, def)| {
      def.name == generated_method_name && matches!(def.kind, ignis_type::definition::DefinitionKind::Method(_))
    })
    .count();

  let user_record = result.output.defs.get(&user_def_id);
  let ignis_type::definition::DefinitionKind::Record(record) = &user_record.kind else {
    panic!("expected User record definition");
  };

  assert_eq!(
    generated_method_defs, 0,
    "failed generated batch must not leak method definitions"
  );
  assert!(
    !record.instance_methods.contains_key(&generated_method_name),
    "failed generated batch must not leak owner method entries"
  );
  assert!(record.implemented_traits.is_empty());
  assert!(
    result.output.directive_registry.generated_items.is_empty(),
    "failed generated batch must not leak generated registry metadata"
  );
}

#[test]
fn staged_analysis_fingerprints_include_generated_reintegration_outcome() {
  let success = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitRecord(context: Context, target: ItemReference, name: str): void {
          return;
        }
      }

      @directive(target: "record", phase: expand, effect: emit)
      function derive(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitRecord(context, target, "GeneratedUser");
      }

      @derive
      record User {
        value: i32;
      }
    "#,
  );
  let rollback = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitRecord(context: Context, target: ItemReference, name: str): void {
          return;
        }
      }

      @directive(target: "record", phase: expand, effect: emit)
      function derive(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitRecord(context, target, "GeneratedUser");
      }

      @derive
      record User {
        value: i32;
      }

      record GeneratedUser {
        value: i32;
      }
    "#,
  );

  assert_eq!(common::format_diagnostics(&success.output.diagnostics), "(no diagnostics)");
  assert!(
    common::format_diagnostics(&rollback.output.diagnostics).contains("already defined"),
    "expected rollback case to fail reintegration"
  );

  assert_eq!(success.output.directive_execution_report.generated_batches.len(), 2);
  assert_eq!(rollback.output.directive_execution_report.generated_batches.len(), 2);
  assert_eq!(success.output.directive_execution_report.generated_batch_outcomes.len(), 2);
  assert_eq!(
    rollback
      .output
      .directive_execution_report
      .generated_batch_outcomes
      .len(),
    2
  );

  assert_ne!(
    success.output.directive_execution_report.generated_batch_outcomes,
    rollback.output.directive_execution_report.generated_batch_outcomes,
    "reintegration outcomes should distinguish committed and rolled-back batches"
  );
  assert_ne!(
    success.output.directive_execution_report.completed_iterations,
    rollback.output.directive_execution_report.completed_iterations,
    "iteration fingerprints must include reintegration outcomes, not just emitted deltas"
  );
}

#[test]
fn staged_analysis_replays_generated_roots_and_touched_owners_into_semantic_maps() {
  let result = common::analyze_staged(
    r#"
      namespace Compile {
        record Context {}
        record ItemReference {}

        function emitRecord(context: Context, target: ItemReference, name: str): void {
          return;
        }

        function emitMethod(context: Context, target: ItemReference, name: str, isStatic: boolean): void {
          return;
        }

        function emitImplements(context: Context, target: ItemReference, traitName: str): void {
          return;
        }
      }

      trait Serializable {
      }

      @directive(target: "record", phase: expand, effect: emit)
      function derive(context: Compile::Context, target: Compile::ItemReference): void {
        Compile::emitRecord(context, target, "GeneratedUser");
        Compile::emitMethod(context, target, "generatedDescribe", false);
        Compile::emitImplements(context, target, "Serializable");
      }

      @derive
      record User {
        value: i32;
      }

      function callGenerated(user: User): void {
        user.generatedDescribe();
        return;
      }

      function requireSerializable<T: Serializable>(value: T): void {
        return;
      }

      function smoke(): void {
        requireSerializable(User { value: 1 });
        return;
      }
    "#,
  );

  assert_eq!(common::format_diagnostics(&result.output.diagnostics), "(no diagnostics)");

  let mut symbols = result.output.symbols.borrow_mut();
  let generated_record_name = symbols.intern("GeneratedUser");
  let generated_method_name = symbols.intern("generatedDescribe");
  drop(symbols);

  let generated_record_node = result
    .output
    .ast
    .iter()
    .find_map(|(node_id, node)| {
      matches!(node, ASTNode::Statement(ASTStatement::Record(record)) if record.name == generated_record_name)
        .then_some(node_id)
    })
    .expect("GeneratedUser record node");

  assert!(
    result.output.node_defs.contains_key(&generated_record_node),
    "generated record roots should be replayed into node_defs"
  );
  assert!(
    result.output.node_types.contains_key(&generated_record_node),
    "generated record roots should be replayed into node_types"
  );

  let generated_method_def_id = result
    .output
    .defs
    .iter()
    .find_map(|(def_id, def)| {
      (def.name == generated_method_name && matches!(def.kind, ignis_type::definition::DefinitionKind::Method(_)))
        .then_some(def_id)
    })
    .expect("generated method definition id");

  assert!(
    result.output.defs.get(&generated_method_def_id).name == generated_method_name,
    "generated method definition should stay committed for replayed method calls"
  );
}
