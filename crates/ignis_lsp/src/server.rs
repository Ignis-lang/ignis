//! LSP server implementation.

use std::collections::HashMap;
use std::sync::Arc;

use ignis_type::definition::DefinitionKind;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use url::Url;

use crate::convert::{convert_diagnostic, LineIndex};
use crate::state::LspState;

/// The Ignis Language Server.
pub struct Server {
  /// LSP client for sending notifications.
  client: Client,

  /// Shared server state.
  state: Arc<LspState>,
}

impl Server {
  /// Create a new server instance.
  pub fn new(
    client: Client,
    state: Arc<LspState>,
  ) -> Self {
    Self { client, state }
  }

  /// Analyze a document and publish diagnostics.
  ///
  /// Uses project-level analysis to resolve imports properly.
  async fn analyze_and_publish(
    &self,
    uri: &Url,
  ) {
    // Extract data from state (behind RwLock)
    let (path_str, text, version) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return;
      };

      (doc.path.to_string_lossy().to_string(), doc.text.clone(), doc.version)
    };

    // Clone config for use in sync block
    let config = (*self.state.config).clone();

    // Run project-level analysis with in-memory text.
    // This uses the editor's current content instead of reading from disk,
    // which provides immediate feedback on unsaved changes.
    let diagnostics_by_uri = {
      let output = ignis_driver::analyze_project_with_text(&config, &path_str, Some(text));

      // Group diagnostics by file URI
      let mut by_uri: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

      for diag in &output.diagnostics {
        // Get the file path from the diagnostic's primary span
        let file_id = &diag.primary_span.file;
        let file_path = output.source_map.get(file_id).path.to_string_lossy().to_string();

        let file_uri = match Url::from_file_path(&file_path) {
          Ok(u) => u,
          Err(_) => continue,
        };

        // Build line index for this file
        let text = output.source_map.get(file_id).text.clone();
        let line_index = LineIndex::new(text);

        let lsp_diag = convert_diagnostic(diag, &line_index);

        by_uri.entry(file_uri).or_default().push(lsp_diag);
      }

      by_uri
    };

    // Publish diagnostics for the requested file
    let file_diagnostics = diagnostics_by_uri.get(uri).cloned().unwrap_or_default();

    self
      .client
      .publish_diagnostics(uri.clone(), file_diagnostics, Some(version))
      .await;
  }
}

#[tower_lsp::async_trait]
impl LanguageServer for Server {
  async fn initialize(
    &self,
    params: InitializeParams,
  ) -> Result<InitializeResult> {
    // Store workspace root
    if let Some(root_uri) = params.root_uri {
      if let Ok(path) = root_uri.to_file_path() {
        self.state.set_root(Some(path)).await;
      }
    }

    Ok(InitializeResult {
      capabilities: ServerCapabilities {
        // Full text sync - client sends entire document on change
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),

        // Document symbols (outline view)
        document_symbol_provider: Some(OneOf::Left(true)),

        // Go to definition
        definition_provider: Some(OneOf::Left(true)),

        // Hover (type info)
        hover_provider: Some(HoverProviderCapability::Simple(true)),

        // TODO: Add more capabilities as we implement features:
        // - references_provider
        // - completion_provider
        ..Default::default()
      },
      server_info: Some(ServerInfo {
        name: "ignis-lsp".to_string(),
        version: Some(env!("CARGO_PKG_VERSION").to_string()),
      }),
    })
  }

  async fn initialized(
    &self,
    _: InitializedParams,
  ) {
    self
      .client
      .log_message(MessageType::INFO, "Ignis LSP initialized")
      .await;
  }

  async fn shutdown(&self) -> Result<()> {
    Ok(())
  }

  async fn did_open(
    &self,
    params: DidOpenTextDocumentParams,
  ) {
    let uri = params.text_document.uri;
    let version = params.text_document.version;
    let text = params.text_document.text;

    self.state.open_document(uri.clone(), version, text).await;
    self.analyze_and_publish(&uri).await;
  }

  async fn did_change(
    &self,
    params: DidChangeTextDocumentParams,
  ) {
    let uri = params.text_document.uri;
    let version = params.text_document.version;

    // With FULL sync, there's exactly one change containing the entire text
    if let Some(change) = params.content_changes.into_iter().next() {
      self.state.update_document(&uri, version, change.text).await;
      self.analyze_and_publish(&uri).await;
    }
  }

  async fn did_close(
    &self,
    params: DidCloseTextDocumentParams,
  ) {
    let uri = params.text_document.uri;

    self.state.close_document(&uri).await;

    // Clear diagnostics for closed file
    self.client.publish_diagnostics(uri, vec![], None).await;
  }

  async fn document_symbol(
    &self,
    params: DocumentSymbolParams,
  ) -> Result<Option<DocumentSymbolResponse>> {
    let uri = &params.text_document.uri;

    // Get document info
    let (path_str, text) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      (doc.path.to_string_lossy().to_string(), doc.text.clone())
    };

    let config = (*self.state.config).clone();

    // Run analysis
    let symbols = {
      let output = ignis_driver::analyze_project_with_text(&config, &path_str, Some(text.clone()));
      let line_index = LineIndex::new(text);

      // Collect symbols from definitions that belong to this file
      // We check by comparing the file path in the span
      let mut symbols: Vec<DocumentSymbol> = Vec::new();

      for (_, def) in output.defs.iter() {
        // Get the file path for this definition
        let def_file_path = output.source_map.get(&def.span.file).path.to_string_lossy();

        // Only include definitions from the current file
        if def_file_path != path_str {
          continue;
        }

        // Skip parameters and internal definitions
        if matches!(
          def.kind,
          DefinitionKind::Parameter(_) | DefinitionKind::TypeParam(_) | DefinitionKind::Placeholder
        ) {
          continue;
        }

        // Skip definitions inside namespaces (they'll be nested)
        if def.owner_namespace.is_some() {
          continue;
        }

        let name = output
          .symbol_names
          .get(&def.name)
          .cloned()
          .unwrap_or_else(|| "?".to_string());

        let kind = definition_kind_to_symbol_kind(&def.kind);
        let range = line_index.span_to_range(&def.span);

        #[allow(deprecated)]
        let symbol = DocumentSymbol {
          name,
          detail: None,
          kind,
          tags: None,
          deprecated: None,
          range,
          selection_range: range,
          children: None,
        };

        symbols.push(symbol);
      }

      symbols
    };

    Ok(Some(DocumentSymbolResponse::Nested(symbols)))
  }

  async fn goto_definition(
    &self,
    params: GotoDefinitionParams,
  ) -> Result<Option<GotoDefinitionResponse>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    // Get document info
    let (path_str, text) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      (doc.path.to_string_lossy().to_string(), doc.text.clone())
    };

    let config = (*self.state.config).clone();

    // Run analysis
    let location = {
      let output = ignis_driver::analyze_project_with_text(&config, &path_str, Some(text.clone()));
      let line_index = LineIndex::new(text);

      // Get the FileId for the current file
      let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
        return Ok(None);
      };

      // Convert LSP position to byte offset
      let byte_offset = line_index.offset(position);

      // Find which node's span contains this position (only from this file)
      let mut found_node = None;
      let mut smallest_span_size = u32::MAX;

      for (node_id, span) in &output.node_spans {
        // Only consider spans from the current file
        if span.file != file_id {
          continue;
        }

        // Check if this span contains the cursor position
        if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
          // Prefer the smallest span that contains the position
          let span_size = span.end.0 - span.start.0;
          if span_size < smallest_span_size {
            smallest_span_size = span_size;
            found_node = Some(node_id.clone());
          }
        }
      }

      // If we found a node, look up its definition
      let Some(node_id) = found_node else {
        return Ok(None);
      };

      // Check node_defs first, then resolved_calls (for overloaded function calls)
      let def_id = output
        .node_defs
        .get(&node_id)
        .or_else(|| output.resolved_calls.get(&node_id));

      let Some(def_id) = def_id else {
        return Ok(None);
      };

      let def = output.defs.get(def_id);

      // Get the file path and text for the definition's file
      let def_file_path = output.source_map.get(&def.span.file).path.clone();
      let def_text = output.source_map.get(&def.span.file).text.clone();

      let def_uri = match Url::from_file_path(&def_file_path) {
        Ok(u) => u,
        Err(_) => return Ok(None),
      };

      let def_line_index = LineIndex::new(def_text);
      let range = def_line_index.span_to_range(&def.span);

      Location { uri: def_uri, range }
    };

    Ok(Some(GotoDefinitionResponse::Scalar(location)))
  }

  async fn hover(
    &self,
    params: HoverParams,
  ) -> Result<Option<Hover>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    // Get document info
    let (path_str, text) = {
      let guard = self.state.open_files.read().await;

      let Some(doc) = guard.get(uri) else {
        return Ok(None);
      };

      (doc.path.to_string_lossy().to_string(), doc.text.clone())
    };

    let config = (*self.state.config).clone();

    // Run analysis
    let hover_info = {
      let output = ignis_driver::analyze_project_with_text(&config, &path_str, Some(text.clone()));
      let line_index = LineIndex::new(text);

      // Get the FileId for the current file
      let Some(file_id) = output.source_map.lookup_by_path(&path_str) else {
        return Ok(None);
      };

      // Convert LSP position to byte offset
      let byte_offset = line_index.offset(position);

      // Find which node's span contains this position (only from this file)
      let mut found_node = None;
      let mut smallest_span_size = u32::MAX;

      for (node_id, span) in &output.node_spans {
        // Only consider spans from the current file
        if span.file != file_id {
          continue;
        }

        if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
          let span_size = span.end.0 - span.start.0;
          if span_size < smallest_span_size {
            smallest_span_size = span_size;
            found_node = Some(node_id.clone());
          }
        }
      }

      // First, check if cursor is on an import item (more specific than the import statement node)
      let import_hover = {
        let mut found_import = None;
        let mut smallest_size = u32::MAX;

        for (span, def_id) in &output.import_item_defs {
          if span.file != file_id {
            continue;
          }

          if span.start.0 <= byte_offset && byte_offset <= span.end.0 {
            let size = span.end.0 - span.start.0;
            if size < smallest_size {
              smallest_size = size;
              found_import = Some((span.clone(), def_id.clone(), size));
            }
          }
        }

        found_import
      };

      // Build hover content based on definition and/or type
      // Import items take priority when cursor is on them (they're more specific than the import statement)
      let (content, hover_span) = if let Some((import_span, import_def_id, import_size)) = import_hover {
        // Check if we also have a node, and if so, whether the import item is more specific
        let use_import = if let Some(ref node_id) = found_node {
          if let Some(node_span) = output.node_spans.get(node_id) {
            let node_size = node_span.end.0 - node_span.start.0;
            import_size <= node_size // Prefer import item if it's same size or smaller
          } else {
            true // No span for node, use import
          }
        } else {
          true // No node found, use import
        };

        if use_import {
          let content = format_definition_hover(
            &output.defs,
            &output.types,
            &output.symbol_names,
            &output.source_map,
            &import_def_id,
          );
          (content, Some(import_span))
        } else {
          // Fall through to node-based lookup
          (String::new(), None)
        }
      } else {
        (String::new(), None)
      };

      // If no import item found (or node was more specific), try node-based lookups
      let (content, hover_span) = if content.is_empty() {
        if let Some(node_id) = found_node {
          // Try node_defs first, then resolved_calls (for overloaded function calls), then types
          let content = if let Some(def_id) = output.node_defs.get(&node_id) {
            format_definition_hover(&output.defs, &output.types, &output.symbol_names, &output.source_map, def_id)
          } else if let Some(def_id) = output.resolved_calls.get(&node_id) {
            // This is a call to an overloaded function - show the resolved overload
            format_definition_hover(&output.defs, &output.types, &output.symbol_names, &output.source_map, def_id)
          } else if let Some(type_id) = output.node_types.get(&node_id) {
            // No definition, just show the type
            format!(
              "```ignis\n{}\n```",
              format_type(&output.types, &output.defs, &output.symbol_names, type_id)
            )
          } else {
            String::new()
          };

          let span = output.node_spans.get(&node_id).cloned();
          (content, span)
        } else {
          (String::new(), None)
        }
      } else {
        (content, hover_span)
      };

      if content.is_empty() {
        return Ok(None);
      }

      // Get the range of the hovered element for highlighting
      let range = hover_span.as_ref().map(|s| line_index.span_to_range(s));

      Some((content, range))
    };

    let Some((content, range)) = hover_info else {
      return Ok(None);
    };

    Ok(Some(Hover {
      contents: HoverContents::Markup(MarkupContent {
        kind: MarkupKind::Markdown,
        value: content,
      }),
      range,
    }))
  }
}

/// Format a type as a human-readable string.
fn format_type(
  types: &ignis_type::types::TypeStore,
  defs: &ignis_type::definition::DefinitionStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
  type_id: &ignis_type::types::TypeId,
) -> String {
  use ignis_type::types::Type;

  let ty = types.get(type_id);
  match ty {
    Type::I8 => "i8".to_string(),
    Type::I16 => "i16".to_string(),
    Type::I32 => "i32".to_string(),
    Type::I64 => "i64".to_string(),
    Type::U8 => "u8".to_string(),
    Type::U16 => "u16".to_string(),
    Type::U32 => "u32".to_string(),
    Type::U64 => "u64".to_string(),
    Type::F32 => "f32".to_string(),
    Type::F64 => "f64".to_string(),
    Type::Boolean => "bool".to_string(),
    Type::Char => "char".to_string(),
    Type::String => "string".to_string(),
    Type::Void => "void".to_string(),
    Type::Never => "never".to_string(),
    Type::Infer => "infer".to_string(),
    Type::NullPtr => "null".to_string(),
    Type::Error => "error".to_string(),
    Type::Pointer { inner, mutable } => {
      if *mutable {
        format!("*mut {}", format_type(types, defs, symbol_names, inner))
      } else {
        format!("*{}", format_type(types, defs, symbol_names, inner))
      }
    },
    Type::Reference { inner, mutable } => {
      if *mutable {
        format!("&mut {}", format_type(types, defs, symbol_names, inner))
      } else {
        format!("&{}", format_type(types, defs, symbol_names, inner))
      }
    },
    Type::Vector { element, size } => {
      if let Some(s) = size {
        format!("[{}; {}]", format_type(types, defs, symbol_names, element), s)
      } else {
        format!("[{}]", format_type(types, defs, symbol_names, element))
      }
    },
    Type::Tuple(elements) => {
      let elem_strs: Vec<_> = elements
        .iter()
        .map(|e| format_type(types, defs, symbol_names, e))
        .collect();
      format!("({})", elem_strs.join(", "))
    },
    Type::Function {
      params,
      ret,
      is_variadic,
    } => {
      let param_strs: Vec<_> = params
        .iter()
        .map(|p| format_type(types, defs, symbol_names, p))
        .collect();
      let variadic = if *is_variadic { ", ..." } else { "" };
      format!(
        "fn({}{}) -> {}",
        param_strs.join(", "),
        variadic,
        format_type(types, defs, symbol_names, ret)
      )
    },
    Type::Record(def_id) => {
      let name = symbol_names
        .get(&defs.get(def_id).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      name
    },
    Type::Enum(def_id) => {
      let name = symbol_names
        .get(&defs.get(def_id).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      name
    },
    Type::Param { index, .. } => {
      format!("T{}", index)
    },
    Type::Instance { generic, args } => {
      let base_name = symbol_names
        .get(&defs.get(generic).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      let arg_strs: Vec<_> = args.iter().map(|a| format_type(types, defs, symbol_names, a)).collect();
      format!("{}<{}>", base_name, arg_strs.join(", "))
    },
  }
}

/// Format a definition for hover display with full signature.
fn format_definition_hover(
  defs: &ignis_type::definition::DefinitionStore,
  types: &ignis_type::types::TypeStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
  source_map: &ignis_type::file::SourceMap,
  def_id: &ignis_type::definition::DefinitionId,
) -> String {
  use ignis_type::definition::DefinitionKind;

  let def = defs.get(def_id);
  let name = symbol_names.get(&def.name).cloned().unwrap_or_else(|| "?".to_string());

  // Get file info for the definition
  let file_info = {
    let file = source_map.get(&def.span.file);
    let path = &file.path;
    // Extract just the filename for display
    path.file_name().map(|n| n.to_string_lossy().to_string())
  };

  let signature = match &def.kind {
    DefinitionKind::Function(func_def) => {
      format_function_signature(&name, func_def, defs, types, symbol_names, func_def.is_extern)
    },

    DefinitionKind::Method(method_def) => format_method_signature(&name, method_def, defs, types, symbol_names),

    DefinitionKind::Variable(var_def) => {
      let ty = format_type(types, defs, symbol_names, &var_def.type_id);
      let mutability = if var_def.mutable { "let mut" } else { "let" };
      format!("```ignis\n{} {}: {}\n```", mutability, name, ty)
    },

    DefinitionKind::Constant(const_def) => {
      let ty = format_type(types, defs, symbol_names, &const_def.type_id);
      format!("```ignis\nconst {}: {}\n```", name, ty)
    },

    DefinitionKind::Parameter(param_def) => {
      let ty = format_type(types, defs, symbol_names, &param_def.type_id);
      let mutability = if param_def.mutable { "mut " } else { "" };
      format!("```ignis\n{}{}: {}\n```\n\n(parameter)", mutability, name, ty)
    },

    DefinitionKind::Record(record_def) => {
      let type_params = format_type_params(&record_def.type_params, defs, symbol_names);
      let fields: Vec<String> = record_def
        .fields
        .iter()
        .map(|f| {
          let field_name = symbol_names.get(&f.name).cloned().unwrap_or_else(|| "?".to_string());
          let field_ty = format_type(types, defs, symbol_names, &f.type_id);
          format!("  {}: {}", field_name, field_ty)
        })
        .collect();
      let fields_str = if fields.is_empty() {
        String::new()
      } else {
        format!("\n{}\n", fields.join(",\n"))
      };
      format!("```ignis\nrecord {}{} {{{}}}\n```", name, type_params, fields_str)
    },

    DefinitionKind::Enum(enum_def) => {
      let type_params = format_type_params(&enum_def.type_params, defs, symbol_names);
      let variants: Vec<String> = enum_def
        .variants
        .iter()
        .map(|v| {
          let variant_name = symbol_names.get(&v.name).cloned().unwrap_or_else(|| "?".to_string());
          if v.payload.is_empty() {
            format!("  {}", variant_name)
          } else {
            let payload: Vec<String> = v
              .payload
              .iter()
              .map(|t| format_type(types, defs, symbol_names, t))
              .collect();
            format!("  {}({})", variant_name, payload.join(", "))
          }
        })
        .collect();
      let variants_str = if variants.is_empty() {
        String::new()
      } else {
        format!("\n{}\n", variants.join(",\n"))
      };
      format!("```ignis\nenum {}{} {{{}}}\n```", name, type_params, variants_str)
    },

    DefinitionKind::TypeAlias(alias_def) => {
      let type_params = format_type_params(&alias_def.type_params, defs, symbol_names);
      let target = format_type(types, defs, symbol_names, &alias_def.target);
      format!("```ignis\ntype {}{} = {}\n```", name, type_params, target)
    },

    DefinitionKind::Namespace(_) => {
      format!("```ignis\nnamespace {}\n```", name)
    },

    DefinitionKind::TypeParam(_) => {
      format!("```ignis\n{}\n```\n\n(type parameter)", name)
    },

    DefinitionKind::Placeholder => {
      format!("`{}`", name)
    },
  };

  // Build the final hover content
  let mut result = signature;

  // Add documentation if present
  if let Some(doc) = &def.doc {
    result.push_str("\n\n---\n\n");
    result.push_str(doc);
  }

  // Add file info if available
  if let Some(filename) = file_info {
    result.push_str(&format!("\n\n*Defined in `{}`*", filename));
  }

  result
}

/// Format a function signature.
fn format_function_signature(
  name: &str,
  func_def: &ignis_type::definition::FunctionDefinition,
  defs: &ignis_type::definition::DefinitionStore,
  types: &ignis_type::types::TypeStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
  is_extern: bool,
) -> String {
  let type_params = format_type_params(&func_def.type_params, defs, symbol_names);

  let params: Vec<String> = func_def
    .params
    .iter()
    .map(|param_id| {
      let param_def = defs.get(param_id);
      let param_name = symbol_names
        .get(&param_def.name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      if let ignis_type::definition::DefinitionKind::Parameter(p) = &param_def.kind {
        let ty = format_type(types, defs, symbol_names, &p.type_id);
        format!("{}: {}", param_name, ty)
      } else {
        param_name
      }
    })
    .collect();

  let variadic = if func_def.is_variadic { ", ..." } else { "" };
  let ret = format_type(types, defs, symbol_names, &func_def.return_type);
  let extern_prefix = if is_extern || func_def.is_extern { "extern " } else { "" };

  format!(
    "```ignis\n{}function {}{}({}{}): {}\n```",
    extern_prefix,
    name,
    type_params,
    params.join(", "),
    variadic,
    ret
  )
}

/// Format a method signature.
fn format_method_signature(
  name: &str,
  method_def: &ignis_type::definition::MethodDefinition,
  defs: &ignis_type::definition::DefinitionStore,
  types: &ignis_type::types::TypeStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
) -> String {
  let type_params = format_type_params(&method_def.type_params, defs, symbol_names);

  // Get owner type name
  let owner_name = symbol_names
    .get(&defs.get(&method_def.owner_type).name)
    .cloned()
    .unwrap_or_else(|| "?".to_string());

  let params: Vec<String> = method_def
    .params
    .iter()
    .map(|param_id| {
      let param_def = defs.get(param_id);
      let param_name = symbol_names
        .get(&param_def.name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      if let ignis_type::definition::DefinitionKind::Parameter(p) = &param_def.kind {
        let ty = format_type(types, defs, symbol_names, &p.type_id);
        format!("{}: {}", param_name, ty)
      } else {
        param_name
      }
    })
    .collect();

  let ret = format_type(types, defs, symbol_names, &method_def.return_type);

  if method_def.is_static {
    format!(
      "```ignis\n{}::{}{}({}): {}\n```\n\n(static method)",
      owner_name,
      name,
      type_params,
      params.join(", "),
      ret
    )
  } else {
    let self_ref = if method_def.self_mutable { "&mut self" } else { "&self" };
    let params_with_self = if params.is_empty() {
      self_ref.to_string()
    } else {
      format!("{}, {}", self_ref, params.join(", "))
    };
    format!(
      "```ignis\n{}::{}{}({}): {}\n```\n\n(instance method)",
      owner_name, name, type_params, params_with_self, ret
    )
  }
}

/// Format type parameters for display.
fn format_type_params(
  type_params: &[ignis_type::definition::DefinitionId],
  defs: &ignis_type::definition::DefinitionStore,
  symbol_names: &std::collections::HashMap<ignis_type::symbol::SymbolId, String>,
) -> String {
  if type_params.is_empty() {
    return String::new();
  }

  let names: Vec<String> = type_params
    .iter()
    .map(|id| {
      symbol_names
        .get(&defs.get(id).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string())
    })
    .collect();

  format!("<{}>", names.join(", "))
}

/// Convert Ignis DefinitionKind to LSP SymbolKind.
fn definition_kind_to_symbol_kind(kind: &DefinitionKind) -> SymbolKind {
  match kind {
    DefinitionKind::Function(_) => SymbolKind::FUNCTION,
    DefinitionKind::Method(_) => SymbolKind::METHOD,
    DefinitionKind::Variable(_) => SymbolKind::VARIABLE,
    DefinitionKind::Constant(_) => SymbolKind::CONSTANT,
    DefinitionKind::Parameter(_) => SymbolKind::VARIABLE,
    DefinitionKind::Namespace(_) => SymbolKind::NAMESPACE,
    DefinitionKind::TypeAlias(_) => SymbolKind::TYPE_PARAMETER,
    DefinitionKind::Record(_) => SymbolKind::STRUCT,
    DefinitionKind::Enum(_) => SymbolKind::ENUM,
    DefinitionKind::TypeParam(_) => SymbolKind::TYPE_PARAMETER,
    DefinitionKind::Placeholder => SymbolKind::NULL,
  }
}
