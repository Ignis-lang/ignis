use ignis_formatter::{CommentPlacement, FormatFile, FormatItem, FormatOptions, format_text};

#[test]
fn builds_directive_tree_without_collapsing_inactive_branches() {
  let file = FormatFile::from_source(
    "@if(feature) {\n    function enabled(): void {}\n}\n@else {\n    function disabled(): void {}\n}\n",
  )
  .expect("build format file");

  assert_eq!(file.items.len(), 1);

  match &file.items[0] {
    FormatItem::Directive(block) => {
      assert_eq!(block.header, "@if(feature)");
      assert_eq!(block.then_items.len(), 1);
      assert_eq!(block.else_header.as_deref(), Some("@else"));
      assert_eq!(block.else_items.len(), 1);
    },
    other => panic!("expected directive block, got {:?}", other),
  }
}

#[test]
fn classifies_leading_trailing_and_detached_comments() {
  let file = FormatFile::from_source("// detached\n\n/// docs\nfunction main(): void {} // trailing\n")
    .expect("build format file");

  let placements: Vec<CommentPlacement> = file
    .comments()
    .iter()
    .map(|comment| comment.placement.clone())
    .collect();

  assert_eq!(
    placements,
    vec![
      CommentPlacement::Detached,
      CommentPlacement::Leading,
      CommentPlacement::Trailing,
    ]
  );
}

#[test]
fn format_text_is_a_canonical_no_op_for_simple_files() {
  let source = "function main(): void {\n  return;\n}\n";
  let options = FormatOptions::default();

  let once = format_text(source, &options).expect("first format");
  let twice = format_text(&once, &options).expect("second format");

  assert_eq!(once, source);
  assert_eq!(twice, source);
}

#[test]
fn keeps_top_level_doc_comments_owned_by_their_following_item() {
  let file = FormatFile::from_source("/// docs for mode\nexport enum Mode {\n    FIRST,\n}\n").expect("build file");

  assert_eq!(file.items.len(), 1);

  match &file.items[0] {
    FormatItem::Code(region) => {
      assert_eq!(region.leading.len(), 1);
      assert_eq!(region.leading[0].lines, vec!["/// docs for mode".to_string()]);
      assert!(region.trailing.is_empty());
    },
    other => panic!("expected owned code region, got {:?}", other),
  }
}
