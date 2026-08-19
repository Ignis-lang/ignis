//! Extraction of API documentation from an analyzed module graph.
//!
//! Documentation comments are already captured during binding and stored on each
//! [`Definition`], so this module does not parse anything: it walks the definition
//! store after analysis and turns the public surface into a serializable model.

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use ignis_analyzer::AnalyzerOutput;
use ignis_config::IgnisConfig;
use ignis_type::definition::{Definition, DefinitionId, DefinitionKind, DefinitionStore, SymbolEntry, Visibility};
use ignis_type::module::{ModuleId, ModulePath};
use ignis_type::symbol::{SymbolId, SymbolTable};
use ignis_type::types::{format_type_name, TypeStore};
use serde::Serialize;

use crate::context::CompilationContext;

/// Everything an API reference needs about one project or one standard-library module.
#[derive(Debug, Clone, Serialize)]
pub struct DocPackage {
  /// The entry the documentation was produced from.
  pub entry: String,
  /// The modules the items belong to, with their own `//!` documentation.
  pub modules: Vec<DocModule>,
  pub items: Vec<DocItem>,
}

/// A module and what it says about itself.
#[derive(Debug, Clone, Serialize)]
pub struct DocModule {
  /// Import path: `std::io`.
  pub name: String,
  /// The `//!` block at the top of the file.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub doc: Option<String>,
}

/// A single documented declaration.
#[derive(Debug, Clone, Serialize)]
pub struct DocItem {
  /// Module the declaration belongs to, as an import path (`std::io`, `app::lexer`).
  pub module: String,
  /// Fully qualified path, including any enclosing namespace.
  pub path: String,
  pub name: String,
  pub kind: DocKind,
  /// Whether the declaration is exported from its module.
  ///
  /// Namespace members are private unless exported, yet they are the public surface of
  /// a standard-library module, so this is reported rather than filtered on: what counts
  /// as the published API is a decision for whatever renders the package.
  pub visibility: DocVisibility,
  /// The declaration rendered as a signature, without its body.
  pub signature: String,
  /// The `///` block attached to the declaration, verbatim.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub doc: Option<String>,
  /// Members of a record, enum or trait, in declaration order.
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub members: Vec<DocMember>,
}

/// A member of a container declaration.
#[derive(Debug, Clone, Serialize)]
pub struct DocMember {
  pub name: String,
  pub kind: DocKind,
  pub signature: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub doc: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DocVisibility {
  Public,
  Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DocKind {
  Function,
  Record,
  Enum,
  Trait,
  TypeAlias,
  Constant,
  Namespace,
  Method,
  Field,
  Variant,
}

/// Analyze `entry_path` and collect its public documented surface.
///
/// Analysis stops after the semantic phases: no HIR lowering, no codegen, no linking.
/// Errors in the analyzed sources fail the extraction rather than producing a partial
/// package, because a reference built from a broken module graph would be wrong in ways
/// a reader cannot see.
pub fn document_project(
  config: Arc<IgnisConfig>,
  entry_path: &str,
) -> Result<DocPackage, ()> {
  let mut ctx = CompilationContext::new(&config);

  let entry_is_std = ctx.try_resolve_std_module_name(entry_path).is_some();

  if entry_is_std && config.std {
    ctx.discover_all_std_modules(&config);
  }

  let root_id = ctx.discover_modules(entry_path, &config)?;

  if config.std && config.auto_load_std {
    ctx.discover_prelude_modules_for_all(&config);
  }

  ctx.module_graph.root = Some(root_id);

  let output = ctx.compile(root_id, &config)?;
  let project_root = ctx.module_graph.project_root.clone();

  let module_names = ctx
    .module_graph
    .modules
    .iter()
    .map(|(id, module)| (id, module_display_name(&module.path, project_root.as_deref())))
    .collect::<HashMap<_, _>>();

  let mut modules = ctx
    .module_graph
    .modules
    .iter()
    .map(|(id, module)| DocModule {
      name: module_names.get(&id).cloned().unwrap_or_default(),
      doc: module.doc.clone(),
    })
    .collect::<Vec<_>>();

  modules.sort_by(|a, b| a.name.cmp(&b.name));

  Ok(collect(&output, &module_names, modules, entry_path))
}

fn collect(
  output: &AnalyzerOutput,
  module_names: &HashMap<ModuleId, String>,
  modules: Vec<DocModule>,
  entry_path: &str,
) -> DocPackage {
  let symbols = output.symbols.borrow();
  let mut items = Vec::new();

  for (id, def) in output.defs.iter() {
    let Some(kind) = doc_kind(&def.kind) else {
      continue;
    };

    // Members are emitted inside their container, never on their own.
    if matches!(kind, DocKind::Method | DocKind::Field | DocKind::Variant) {
      continue;
    }

    let module = module_names.get(&def.owner_module).cloned().unwrap_or_default();
    let name = symbols.get(&def.name).to_string();

    items.push(DocItem {
      path: qualified_path(&module, def, output, &symbols),
      module,
      signature: signature_of(&id, def, kind, &output.defs, &output.types, &symbols),
      name,
      kind,
      visibility: match def.visibility {
        Visibility::Public => DocVisibility::Public,
        Visibility::Private => DocVisibility::Private,
      },
      doc: def.doc.clone(),
      members: members_of(def, &output.defs, &output.types, &symbols),
    });
  }

  items.sort_by(|a, b| (&a.module, &a.path).cmp(&(&b.module, &b.path)));

  DocPackage {
    entry: entry_path.to_string(),
    modules,
    items,
  }
}

fn doc_kind(kind: &DefinitionKind) -> Option<DocKind> {
  match kind {
    DefinitionKind::Function(_) => Some(DocKind::Function),
    DefinitionKind::Record(_) => Some(DocKind::Record),
    DefinitionKind::Enum(_) => Some(DocKind::Enum),
    DefinitionKind::Trait(_) => Some(DocKind::Trait),
    DefinitionKind::TypeAlias(_) => Some(DocKind::TypeAlias),
    DefinitionKind::Constant(_) => Some(DocKind::Constant),
    DefinitionKind::Namespace(_) => Some(DocKind::Namespace),
    DefinitionKind::Method(_) => Some(DocKind::Method),
    DefinitionKind::Field(_) => Some(DocKind::Field),
    DefinitionKind::Variant(_) => Some(DocKind::Variant),
    DefinitionKind::Variable(_)
    | DefinitionKind::Parameter(_)
    | DefinitionKind::TypeParam(_)
    | DefinitionKind::Placeholder => None,
  }
}

fn qualified_path(
  module: &str,
  def: &Definition,
  output: &AnalyzerOutput,
  symbols: &SymbolTable,
) -> String {
  let name = symbols.get(&def.name);

  let Some(namespace) = def.owner_namespace else {
    return format!("{module}::{name}");
  };

  let segments: Vec<&str> = output
    .namespaces
    .full_path(namespace)
    .iter()
    .map(|segment| symbols.get(segment))
    .collect();

  if segments.is_empty() {
    return format!("{module}::{name}");
  }

  format!("{module}::{}::{name}", segments.join("::"))
}

fn signature_of(
  id: &DefinitionId,
  def: &Definition,
  kind: DocKind,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
) -> String {
  let name = symbols.get(&def.name);

  match &def.kind {
    DefinitionKind::Function(function) => {
      let params = render_params(&function.params, defs, types, symbols);
      let ret = render_type(&function.return_type, types, defs, symbols);
      let generics = render_type_params(&function.type_params, defs, symbols);

      format!("function {name}{generics}({params}): {ret}")
    },
    DefinitionKind::Method(method) => {
      // An instance method carries its receiver as the first parameter. A reader expects
      // to see it written the way the language writes it, so it is rendered as `&self`
      // rather than repeated as a typed parameter.
      let (receiver, rest) = if method.is_static {
        (None, method.params.as_slice())
      } else {
        let receiver = if method.self_mutable { "&mut self" } else { "&self" };
        (Some(receiver), method.params.split_first().map(|(_, rest)| rest).unwrap_or(&[]))
      };

      let mut params = render_params(rest, defs, types, symbols);

      if let Some(receiver) = receiver {
        params = if params.is_empty() {
          receiver.to_string()
        } else {
          format!("{receiver}, {params}")
        };
      }

      let ret = render_type(&method.return_type, types, defs, symbols);
      let prefix = if method.is_static { "static " } else { "" };

      format!("{prefix}{name}({params}): {ret}")
    },
    DefinitionKind::Record(record) => {
      format!("record {name}{}", render_type_params(&record.type_params, defs, symbols))
    },
    DefinitionKind::Enum(enumeration) => {
      format!("enum {name}{}", render_type_params(&enumeration.type_params, defs, symbols))
    },
    DefinitionKind::Trait(_) => format!("trait {name}"),
    DefinitionKind::TypeAlias(alias) => {
      let generics = render_type_params(&alias.type_params, defs, symbols);
      let target = render_type(&alias.target, types, defs, symbols);

      format!("type {name}{generics} = {target}")
    },
    DefinitionKind::Constant(constant) => {
      let ty = render_type(&constant.type_id, types, defs, symbols);

      format!("const {name}: {ty}")
    },
    DefinitionKind::Namespace(_) => format!("namespace {name}"),
    DefinitionKind::Field(field) => {
      let ty = render_type(&field.type_id, types, defs, symbols);

      format!("{name}: {ty}")
    },
    _ => {
      let _ = (id, kind);
      name.to_string()
    },
  }
}

/// Formats a type the way it is written in source.
///
/// The shared formatter is built for diagnostics, where naming the kind of a type helps
/// ("record String"). In a signature that noise is wrong: the reader is looking at the
/// declaration as it would be typed, so the kind prefixes go and `bool` recovers its real
/// spelling.
fn render_type(
  type_id: &ignis_type::types::TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  let formatted = format_type_name(type_id, types, defs, symbols);

  formatted
    .split_inclusive(|c: char| !c.is_alphanumeric() && c != '_')
    .map(|piece| {
      let (word, tail) = piece.split_at(piece.len() - piece.chars().last().map_or(0, |c| c.len_utf8()));
      let last_is_word = piece.chars().last().is_some_and(|c| c.is_alphanumeric() || c == '_');

      let (word, tail) = if last_is_word { (piece, "") } else { (word, tail) };

      match word {
        "record" | "enum" if tail == " " => String::new(),
        "bool" => format!("boolean{tail}"),
        _ => format!("{word}{tail}"),
      }
    })
    .collect()
}

fn render_params(
  params: &[DefinitionId],
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
) -> String {
  params
    .iter()
    .map(|param_id| {
      let param = defs.get(param_id);
      let name = symbols.get(&param.name);

      match &param.kind {
        DefinitionKind::Parameter(parameter) => {
          format!("{name}: {}", render_type(&parameter.type_id, types, defs, symbols))
        },
        _ => name.to_string(),
      }
    })
    .collect::<Vec<_>>()
    .join(", ")
}

fn render_type_params(
  type_params: &[DefinitionId],
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> String {
  if type_params.is_empty() {
    return String::new();
  }

  let names: Vec<&str> = type_params
    .iter()
    .map(|param_id| symbols.get(&defs.get(param_id).name))
    .collect();

  format!("<{}>", names.join(", "))
}

fn members_of(
  def: &Definition,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
) -> Vec<DocMember> {
  let mut members = Vec::new();

  match &def.kind {
    DefinitionKind::Record(record) => {
      for field in &record.fields {
        let field_def = defs.get(&field.def_id);

        members.push(DocMember {
          name: symbols.get(&field.name).to_string(),
          kind: DocKind::Field,
          signature: format!(
            "{}: {}",
            symbols.get(&field.name),
            render_type(&field.type_id, types, defs, symbols)
          ),
          doc: field_def.doc.clone(),
        });
      }

      collect_methods(&record.static_methods, defs, types, symbols, &mut members);
      collect_methods(&record.instance_methods, defs, types, symbols, &mut members);
    },
    DefinitionKind::Enum(enumeration) => {
      for variant in &enumeration.variants {
        let payload: Vec<String> = variant
          .payload
          .iter()
          .map(|ty| render_type(ty, types, defs, symbols))
          .collect();

        let name = symbols.get(&variant.name).to_string();
        let signature = if payload.is_empty() {
          name.clone()
        } else {
          format!("{name}({})", payload.join(", "))
        };

        members.push(DocMember {
          name,
          kind: DocKind::Variant,
          signature,
          doc: None,
        });
      }

      collect_methods(&enumeration.static_methods, defs, types, symbols, &mut members);
      collect_methods(&enumeration.instance_methods, defs, types, symbols, &mut members);
    },
    DefinitionKind::Trait(declaration) => {
      for method in &declaration.methods {
        let method_def = defs.get(&method.method_def_id);

        members.push(DocMember {
          name: symbols.get(&method.name).to_string(),
          kind: DocKind::Method,
          signature: signature_of(&method.method_def_id, method_def, DocKind::Method, defs, types, symbols),
          doc: method_def.doc.clone(),
        });
      }
    },
    _ => {},
  }

  members
}

/// Methods live in a hash map, so they are sorted by name to keep the emitted package
/// stable between runs.
fn collect_methods(
  methods: &HashMap<SymbolId, SymbolEntry>,
  defs: &DefinitionStore,
  types: &TypeStore,
  symbols: &SymbolTable,
  members: &mut Vec<DocMember>,
) {
  let mut ids: Vec<DefinitionId> = Vec::new();

  for entry in methods.values() {
    match entry {
      SymbolEntry::Single(id) => ids.push(*id),
      SymbolEntry::Overload(overloads) => ids.extend(overloads.iter().copied()),
    }
  }

  let mut collected: Vec<DocMember> = ids
    .into_iter()
    .map(|id| {
      let method = defs.get(&id);

      DocMember {
        name: symbols.get(&method.name).to_string(),
        kind: DocKind::Method,
        signature: signature_of(&id, method, DocKind::Method, defs, types, symbols),
        doc: method.doc.clone(),
      }
    })
    .collect();

  collected.sort_by(|a, b| (&a.name, &a.signature).cmp(&(&b.name, &b.signature)));
  members.extend(collected);
}

/// Renders a module path the way it would be written in an `import`.
fn module_display_name(
  path: &ModulePath,
  project_root: Option<&Path>,
) -> String {
  match path {
    ModulePath::Std(name) => format!("std::{name}"),
    ModulePath::Project(file) => {
      // Without a project root there is nothing to be relative to, and spelling out an
      // absolute path as a module name helps nobody. Single-file mode gets the stem.
      let Some(relative) = project_root.and_then(|root| file.strip_prefix(root).ok()) else {
        return file
          .file_stem()
          .map(|stem| stem.to_string_lossy().to_string())
          .unwrap_or_default();
      };

      relative
        .with_extension("")
        .components()
        .map(|component| component.as_os_str().to_string_lossy().to_string())
        .collect::<Vec<_>>()
        .join("::")
    },
  }
}
