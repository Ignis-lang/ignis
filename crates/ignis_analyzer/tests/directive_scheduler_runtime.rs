mod common;

use ignis_type::attribute::DirectivePhase;

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
      @directive(target: "record", phase: check, effect: diagnose)
      function checkRecord(): void {
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
    ]
  );
  assert!(result.output.directive_execution_report.failure.is_none());
}
