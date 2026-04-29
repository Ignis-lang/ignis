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
