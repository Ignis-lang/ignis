//! Textual rendering of [`DropSchedules`] for `--dump-drop-schedule`.
//!
//! This is a pure renderer: it reads the schedules ownership analysis already produced and
//! prints them. It never re-derives a drop, so a bug visible in the dump is a bug in the
//! analysis and not in this file.
//!
//! # Format
//!
//! ```text
//! drop-schedule v1
//! function main at example/a.ign:3:20
//!   value handle kind=local declared at example/a.ign:4:3
//!     drop at example/a.ign:8:3 reason=scope-end
//!     moved at example/a.ign:6:10
//!   defer declared at example/a.ign:5:3 runs at example/a.ign:8:3 reason=scope-end
//! function empty at example/a.ign:11:20
//!   <no owned values>
//! ```
//!
//! Every position is `<path>:<line>:<column>`, 1-based, where the column counts **bytes**
//! from the start of the line (not Unicode columns) so that the two compiler front ends
//! agree without sharing a grapheme table. `<path>` is the path the file was opened with,
//! made relative to the process working directory when it lies under it and with a leading
//! `./` removed, so that a dump does not depend on where the checkout lives; a span in a
//! synthetic file renders as `<synthetic>:0:0`.
//!
//! # Ordering
//!
//! Nothing here depends on hash-map iteration order:
//!
//! - functions are sorted by body position, then name, then definition index;
//! - values are sorted by declaration position, then name, then definition index;
//! - sites are sorted by position, then reason;
//! - duplicate sites are collapsed.
//!
//! # Attribution
//!
//! A drop site belongs to the function whose body contains the HIR node keyed by the
//! schedule, found by walking the body from `HIR::function_bodies`. A closure body is part
//! of the enclosing function's HIR tree, so drops inside a closure are listed under the
//! function that writes the closure. Move sites carry the function the ownership checker
//! was analysing, so they need no walk.
//!
//! # Normalization: everything is read from the HIR
//!
//! Positions and value kinds come from HIR nodes only, never from the definition store,
//! because the self-hosted compiler's `Definition` carries no span and has no `Parameter`
//! variant. Reading only what both front ends model identically is what lets them produce
//! the same bytes:
//!
//! - a function is positioned at its **body** node, not at its name;
//! - a `local` is positioned at the `Let` node that declares it;
//! - a pattern `binding` is positioned at the arm body (for a `match` arm) or at the
//!   `LetElse` node, since HIR patterns carry no span of their own;
//! - a `parameter` is positioned at the body node of its function, or at the `Closure`
//!   node for a closure parameter;
//! - anything the walk never reaches — a temporary, a compiler-introduced binding — is a
//!   `value` positioned at the function body.
//!
//! When a definition is reached more than once the earliest position wins, so the result
//! does not depend on traversal order.
//!
//! A synthesized closure thunk is positioned at the **closure expression** that produced
//! it rather than at its own body, because capture analysis hands the thunk whatever node
//! the front end had to hand and the two compilers do not agree on that choice. The
//! closure expression is the same source construct in both.
//!
//! # Residue: what the renderer cannot normalize
//!
//! Monomorphized names are mangled by each compiler's monomorphizer and reach the renderer
//! already flattened into one symbol, so no rendering rule can reconcile them: the host
//! qualifies a specialized method by its owner (`Pair__i32__fold__i32`) where the selfhost
//! does not (`fold__i32__i32`), and the selfhost still prints a placeholder
//! (`total__<record:DefId(1679)>`) where the host prints the record's name. Making those
//! agree means changing what the monomorphizers write, not what this file prints, so gate
//! G7 reports them rather than hiding them.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Write;

use ignis_type::{
  BytePosition,
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
  file::{FileId, SourceMap},
  span::Span,
  symbol::SymbolTable,
};

use crate::{DropSchedules, ExitKey, HIR, HIRId, HIRKind, HIRPattern};

/// Why a drop was scheduled at a given site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DropReason {
  /// End of the block that declared the value.
  ScopeEnd,
  /// End of a match arm that bound the value.
  ArmEnd,
  /// An explicit `return`.
  Return,
  /// A `break` out of a loop.
  Break,
  /// A `continue` to the next iteration.
  Continue,
  /// The synthetic return at the end of the function body.
  FnEnd,
  /// The value was overwritten by an assignment.
  Overwrite,
}

impl DropReason {
  pub fn as_str(self) -> &'static str {
    match self {
      DropReason::ScopeEnd => "scope-end",
      DropReason::ArmEnd => "arm-end",
      DropReason::Return => "return",
      DropReason::Break => "break",
      DropReason::Continue => "continue",
      DropReason::FnEnd => "fn-end",
      DropReason::Overwrite => "overwrite",
    }
  }
}

/// How a value entered the function it is listed under.
const KIND_LOCAL: &str = "local";
const KIND_BINDING: &str = "binding";
const KIND_PARAMETER: &str = "parameter";
const KIND_VALUE: &str = "value";

/// A rendered source position, pre-resolved so that sorting is textual and total.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Position {
  path: String,
  line: u32,
  column: u32,
}

impl Position {
  fn render(&self) -> String {
    format!("{}:{}:{}", self.path, self.line, self.column)
  }
}

/// Where and how a value entered the function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Declaration {
  position: Position,
  kind: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Site {
  Drop { position: Position, reason: DropReason },
  Move { position: Position },
}

/// Renders the drop schedules of a whole program.
pub struct DropScheduleDumper<'a> {
  hir: &'a HIR,
  defs: &'a DefinitionStore,
  symbols: &'a SymbolTable,
  schedules: &'a DropSchedules,
  source_map: Option<&'a SourceMap>,
  /// Prefix stripped from every rendered path, so two checkouts print the same bytes.
  working_directory: String,
}

impl<'a> DropScheduleDumper<'a> {
  pub fn new(
    hir: &'a HIR,
    defs: &'a DefinitionStore,
    symbols: &'a SymbolTable,
    schedules: &'a DropSchedules,
  ) -> Self {
    Self {
      hir,
      defs,
      symbols,
      schedules,
      source_map: None,
      working_directory: std::env::current_dir()
        .map(|path| path.to_string_lossy().to_string())
        .unwrap_or_default(),
    }
  }

  pub fn with_source_map(
    mut self,
    source_map: &'a SourceMap,
  ) -> Self {
    self.source_map = Some(source_map);
    self
  }

  pub fn render(&self) -> String {
    let mut output = String::new();
    writeln!(output, "drop-schedule v1").unwrap();

    let thunks = self.closure_thunks();

    for (function, body) in self.functions(&thunks) {
      writeln!(
        output,
        "function {} at {}",
        self.symbols.get(&self.defs.get(&function).name),
        self.function_position(function, body, &thunks).render()
      )
      .unwrap();

      let nodes = self.body_nodes(body);
      let declarations = self.declarations(function, body, &nodes);
      let values = self.values(function, body, &nodes, &declarations);
      let defers = self.defers(&nodes);

      if values.is_empty() && defers.is_empty() {
        writeln!(output, "  <no owned values>").unwrap();
        continue;
      }

      for ((declaration, name, _), sites) in &values {
        writeln!(
          output,
          "  value {} kind={} declared at {}",
          name,
          declaration.kind,
          declaration.position.render()
        )
        .unwrap();

        for site in sites {
          match site {
            Site::Drop { position, reason } => {
              writeln!(output, "    drop at {} reason={}", position.render(), reason.as_str()).unwrap()
            },
            Site::Move { position } => writeln!(output, "    moved at {}", position.render()).unwrap(),
          }
        }
      }

      for (declared, runs, reason) in &defers {
        writeln!(
          output,
          "  defer declared at {} runs at {} reason={}",
          declared.render(),
          runs.render(),
          reason.as_str()
        )
        .unwrap();
      }
    }

    output
  }

  /// Where each closure thunk's closure expression sits, keyed by the thunk.
  ///
  /// A thunk is synthesized by capture analysis, so the span of its body is
  /// whatever node the front end handed it, and the two compilers do not agree on
  /// that node. The closure expression that produced the thunk is the same source
  /// construct in both, so the dump positions a thunk there instead.
  fn closure_thunks(&self) -> HashMap<DefinitionId, Span> {
    let mut thunks: HashMap<DefinitionId, Span> = HashMap::new();

    for (id, node) in self.hir.nodes.iter() {
      let HIRKind::Closure {
        thunk_def: Some(thunk), ..
      } = &node.kind
      else {
        continue;
      };

      let span = self.span_of(id);

      thunks
        .entry(*thunk)
        .and_modify(|current| {
          if span.start < current.start {
            *current = span.clone();
          }
        })
        .or_insert(span);
    }

    thunks
  }

  fn function_position(
    &self,
    function: DefinitionId,
    body: HIRId,
    thunks: &HashMap<DefinitionId, Span>,
  ) -> Position {
    match thunks.get(&function) {
      Some(span) => self.position(span),
      None => self.position(&self.span_of(body)),
    }
  }

  /// Functions with a body, in the order they are printed.
  fn functions(
    &self,
    thunks: &HashMap<DefinitionId, Span>,
  ) -> Vec<(DefinitionId, HIRId)> {
    let mut functions: Vec<(Position, String, u32, DefinitionId, HIRId)> = self
      .hir
      .items
      .iter()
      .filter(|item| {
        matches!(
          self.defs.get(item).kind,
          DefinitionKind::Function(_) | DefinitionKind::Method(_)
        )
      })
      .filter_map(|item| self.hir.function_bodies.get(item).map(|body| (*item, *body)))
      .map(|(item, body)| {
        (
          self.function_position(item, body, thunks),
          self.symbols.get(&self.defs.get(&item).name).to_string(),
          item.index(),
          item,
          body,
        )
      })
      .collect();

    functions.sort_by(|left, right| (&left.0, &left.1, left.2).cmp(&(&right.0, &right.1, right.2)));
    functions.dedup_by_key(|entry| entry.2);
    functions
      .into_iter()
      .map(|(_, _, _, item, body)| (item, body))
      .collect()
  }

  /// Every HIR node reachable from a function body.
  fn body_nodes(
    &self,
    body: HIRId,
  ) -> HashSet<HIRId> {
    let mut seen = HashSet::new();
    let mut pending = vec![body];

    while let Some(id) = pending.pop() {
      if !seen.insert(id) {
        continue;
      }

      let Some(node) = self.hir.nodes.try_get(&id) else {
        continue;
      };

      pending.extend(node.kind.child_ids());
    }

    seen
  }

  /// Where each value of a function was introduced, keyed by definition.
  fn declarations(
    &self,
    function: DefinitionId,
    body: HIRId,
    nodes: &HashSet<HIRId>,
  ) -> HashMap<DefinitionId, Declaration> {
    let mut declarations: HashMap<DefinitionId, Declaration> = HashMap::new();

    let declare = |declarations: &mut HashMap<DefinitionId, Declaration>,
                   value: DefinitionId,
                   position: Position,
                   kind: &'static str| {
      let declaration = Declaration { position, kind };
      declarations
        .entry(value)
        .and_modify(|current| {
          if declaration < *current {
            *current = declaration.clone();
          }
        })
        .or_insert(declaration);
    };

    let body_position = self.position(&self.span_of(body));

    for parameter in self.parameters(function) {
      declare(&mut declarations, parameter, body_position.clone(), KIND_PARAMETER);
    }

    let mut ordered: Vec<HIRId> = nodes.iter().copied().collect();
    ordered.sort_by_key(|id| id.index());

    for id in ordered {
      let Some(node) = self.hir.nodes.try_get(&id) else {
        continue;
      };

      match &node.kind {
        HIRKind::Let { name, .. } => {
          declare(&mut declarations, *name, self.position(&node.span), KIND_LOCAL);
        },
        HIRKind::LetElse { pattern, .. } => {
          let position = self.position(&node.span);
          for binding in pattern_bindings(pattern) {
            declare(&mut declarations, binding, position.clone(), KIND_BINDING);
          }
        },
        HIRKind::Match { arms, .. } => {
          for arm in arms {
            let position = self.position(&self.span_of(arm.body));
            for binding in pattern_bindings(&arm.pattern) {
              declare(&mut declarations, binding, position.clone(), KIND_BINDING);
            }
          }
        },
        HIRKind::Closure { params, .. } => {
          let position = self.position(&node.span);
          for parameter in params {
            declare(&mut declarations, *parameter, position.clone(), KIND_PARAMETER);
          }
        },
        _ => {},
      }
    }

    declarations
  }

  fn parameters(
    &self,
    function: DefinitionId,
  ) -> Vec<DefinitionId> {
    match &self.defs.get(&function).kind {
      DefinitionKind::Function(definition) => definition.params.clone(),
      DefinitionKind::Method(definition) => definition.params.clone(),
      _ => Vec::new(),
    }
  }

  /// All owned values of one function, keyed so that the map iterates in print order.
  #[allow(clippy::type_complexity)]
  fn values(
    &self,
    function: DefinitionId,
    body: HIRId,
    nodes: &HashSet<HIRId>,
    declarations: &HashMap<DefinitionId, Declaration>,
  ) -> BTreeMap<(Declaration, String, u32), BTreeSet<Site>> {
    let mut values: BTreeMap<(Declaration, String, u32), BTreeSet<Site>> = BTreeMap::new();
    let fallback = Declaration {
      position: self.position(&self.span_of(body)),
      kind: KIND_VALUE,
    };

    let record = |dumper: &Self, values: &mut BTreeMap<_, BTreeSet<Site>>, value: DefinitionId, site: Site| {
      let declaration = declarations.get(&value).cloned().unwrap_or_else(|| fallback.clone());
      let key = (
        declaration,
        dumper.symbols.get(&dumper.defs.get(&value).name).to_string(),
        value.index(),
      );
      values.entry(key).or_default().insert(site);
    };

    let record_drops =
      |dumper: &Self, values: &mut BTreeMap<_, _>, span: &Span, reason: DropReason, dropped: &[DefinitionId]| {
        let position = dumper.position(span);
        for value in dropped {
          record(
            dumper,
            values,
            *value,
            Site::Drop {
              position: position.clone(),
              reason,
            },
          );
        }
      };

    for (block, dropped) in &self.schedules.on_scope_end {
      if nodes.contains(block) {
        record_drops(self, &mut values, &self.span_of(*block), DropReason::ScopeEnd, dropped);
      }
    }

    for (arm, dropped) in &self.schedules.on_match_arm_end {
      if nodes.contains(arm) {
        record_drops(self, &mut values, &self.span_of(*arm), DropReason::ArmEnd, dropped);
      }
    }

    for (assign, dropped) in &self.schedules.on_overwrite {
      if nodes.contains(assign) {
        record_drops(self, &mut values, &self.span_of(*assign), DropReason::Overwrite, dropped);
      }
    }

    for (exit, dropped) in &self.schedules.on_exit {
      let Some((span, reason)) = self.exit_site(exit, function, body, nodes) else {
        continue;
      };
      record_drops(self, &mut values, &span, reason, dropped);
    }

    for (value, moves) in &self.schedules.moves {
      for site in moves.iter().filter(|site| site.function == function) {
        let position = self.position(&site.span);
        record(self, &mut values, *value, Site::Move { position });
      }
    }

    values
  }

  /// Deferred bodies scheduled inside one function, in print order.
  fn defers(
    &self,
    nodes: &HashSet<HIRId>,
  ) -> BTreeSet<(Position, Position, DropReason)> {
    let mut defers = BTreeSet::new();

    for (block, bodies) in &self.schedules.on_scope_end_defers {
      if !nodes.contains(block) {
        continue;
      }

      let runs = self.position(&self.span_of(*block));
      for body in bodies {
        defers.insert((self.position(&self.span_of(*body)), runs.clone(), DropReason::ScopeEnd));
      }
    }

    for (exit, bodies) in &self.schedules.on_exit_defers {
      let (site, reason) = match exit {
        ExitKey::Return(id) => (*id, DropReason::Return),
        ExitKey::Break(id) => (*id, DropReason::Break),
        ExitKey::Continue(id) => (*id, DropReason::Continue),
        // A function-end defer is keyed by the function rather than by a node; its body is
        // already listed by the block that registered it.
        ExitKey::FnEnd(_) => continue,
      };

      if !nodes.contains(&site) {
        continue;
      }

      let runs = self.position(&self.span_of(site));
      for body in bodies {
        defers.insert((self.position(&self.span_of(*body)), runs.clone(), reason));
      }
    }

    defers
  }

  /// Where an early-exit schedule fires, or `None` when it belongs to another function.
  fn exit_site(
    &self,
    exit: &ExitKey,
    function: DefinitionId,
    body: HIRId,
    nodes: &HashSet<HIRId>,
  ) -> Option<(Span, DropReason)> {
    let (site, reason) = match exit {
      ExitKey::Return(id) => (*id, DropReason::Return),
      ExitKey::Break(id) => (*id, DropReason::Break),
      ExitKey::Continue(id) => (*id, DropReason::Continue),
      ExitKey::FnEnd(owner) => {
        return (*owner == function).then(|| (self.span_of(body), DropReason::FnEnd));
      },
    };

    nodes.contains(&site).then(|| (self.span_of(site), reason))
  }

  fn span_of(
    &self,
    id: HIRId,
  ) -> Span {
    self
      .hir
      .nodes
      .try_get(&id)
      .map(|node| node.span.clone())
      .unwrap_or_default()
  }

  fn position(
    &self,
    span: &Span,
  ) -> Position {
    let synthetic = Position {
      path: "<synthetic>".to_string(),
      line: 0,
      column: 0,
    };

    if span.file == FileId::SYNTHETIC {
      return synthetic;
    }

    let Some(source_map) = self.source_map else {
      return synthetic;
    };

    let (line, column) = source_map.byte_line_col(&span.file, BytePosition(span.start.0));

    Position {
      path: normalize_path(&source_map.get(&span.file).path.to_string_lossy(), &self.working_directory),
      line,
      column,
    }
  }
}

/// Every binding a pattern introduces, in pattern order.
fn pattern_bindings(pattern: &HIRPattern) -> Vec<DefinitionId> {
  match pattern {
    HIRPattern::Binding { def_id } => vec![*def_id],
    HIRPattern::Variant { args, .. } => args.iter().flat_map(pattern_bindings).collect(),
    HIRPattern::Tuple { elements } => elements.iter().flat_map(pattern_bindings).collect(),
    HIRPattern::Or { patterns } => patterns.iter().flat_map(pattern_bindings).collect(),
    HIRPattern::Wildcard | HIRPattern::Literal { .. } | HIRPattern::Constant { .. } => Vec::new(),
  }
}

/// Renders the path the way both front ends agree on: relative to the working directory
/// when it lies under it, and without a leading `./`.
fn normalize_path(
  path: &str,
  working_directory: &str,
) -> String {
  let relative = if working_directory.is_empty() {
    path
  } else {
    path
      .strip_prefix(working_directory)
      .and_then(|rest| rest.strip_prefix('/'))
      .unwrap_or(path)
  };

  relative.strip_prefix("./").unwrap_or(relative).to_string()
}
