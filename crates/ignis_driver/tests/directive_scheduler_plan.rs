use ignis_analyzer::directive_scheduler::{DirectiveExecutionState, DirectiveSchedulePlan, DirectiveStageResult};
use ignis_driver::analyze_text;
use ignis_type::attribute::DirectivePhase;

#[test]
fn analyze_text_builds_a_consumable_directive_schedule_plan_for_all_phases() {
  let output = analyze_text(
    "scheduler.ign",
    r#"
      @directive(target: "record", phase: check, effect: diagnose)
      function checkRecord(): void {
        return;
      }

      @directive(target: "record", phase: expand, effect: emit)
      function expandRecord(): void {
        return;
      }

      @directive(target: "record", phase: collect, effect: collect, group: "routes")
      function collectRecord(): void {
        return;
      }

      @directive(target: "record", phase: finalize, effect: collect, group: "routes")
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
    "#
    .to_string(),
  );

  assert!(
    !output.has_errors,
    "directive planning fixture should analyze cleanly: {:?}",
    output.diagnostics
  );

  let plan = DirectiveSchedulePlan::from_registry(&output.analyzer.directive_registry);
  let mut state = DirectiveExecutionState::new(plan, 2);
  let mut seen = Vec::new();

  while let Some(stage) = state.current_stage() {
    seen.push(stage.phase.clone());

    state
      .consume_current_stage(DirectiveStageResult::new(stage.phase.clone(), Vec::new(), false))
      .expect("driver-level plan should be consumable");
  }

  assert_eq!(
    seen,
    vec![
      DirectivePhase::Check,
      DirectivePhase::Expand,
      DirectivePhase::Collect,
      DirectivePhase::Finalize,
      DirectivePhase::Transform,
    ]
  );
  assert!(
    state.is_converged(),
    "consuming the analyzer-built schedule should finish cleanly"
  );
}
