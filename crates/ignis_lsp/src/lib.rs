#![allow(clippy::too_many_arguments)]
#![allow(clippy::large_enum_variant)]

//! Ignis Language Server Protocol implementation.
//!
//! This crate provides an LSP server for the Ignis programming language,
//! enabling IDE features like real-time diagnostics, hover information,
//! and go-to-definition.
//!
//! # Usage
//!
//! ```no_run
//! use std::sync::Arc;
//! use ignis_config::IgnisConfig;
//!
//! let config = Arc::new(IgnisConfig::default());
//! // ignis_lsp::run(config).await;
//! ```

mod at_items;
mod completion;
mod convert;
mod inlay;
pub mod project;
mod semantic;
mod server;
mod state;
mod type_format;

use std::sync::Arc;

use ignis_config::IgnisConfig;
use tower_lsp::{LspService, Server as TowerServer};

use completion::log;
use server::Server;
use state::LspState;

/// Run the LSP server on stdin/stdout.
///
/// This function blocks until the client disconnects or sends a shutdown request.
/// It should be called from within a Tokio runtime.
///
/// # Arguments
/// * `config` - The compiler configuration to use for analysis.
///
/// # Example
/// ```no_run
/// use std::sync::Arc;
/// use ignis_config::IgnisConfig;
///
/// let config = Arc::new(IgnisConfig::default());
/// let rt = tokio::runtime::Builder::new_current_thread()
///     .enable_all()
///     .build()
///     .unwrap();
/// rt.block_on(ignis_lsp::run(config));
/// ```
pub async fn run(config: Arc<IgnisConfig>) {
  // Install panic hook to log all panics before they kill the server
  std::panic::set_hook(Box::new(|panic_info| {
    let msg = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
      s.to_string()
    } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
      s.clone()
    } else {
      "unknown panic".to_string()
    };

    let location = panic_info
      .location()
      .map(|l| format!("{}:{}:{}", l.file(), l.line(), l.column()))
      .unwrap_or_else(|| "unknown location".to_string());

    let log_msg = format!("[PANIC] {} at {}", msg, location);

    // Log to file
    log(&log_msg);

    // Also log to stderr for immediate visibility
    eprintln!("{}", log_msg);
  }));

  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let state = Arc::new(LspState::new(config));

  let (service, socket) = LspService::new(|client| Server::new(client, state));

  TowerServer::new(stdin, stdout, socket).serve(service).await;
}
