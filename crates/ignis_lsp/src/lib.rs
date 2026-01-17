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

mod convert;
mod server;
mod state;

use std::sync::Arc;

use ignis_config::IgnisConfig;
use tower_lsp::{LspService, Server as TowerServer};

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
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let state = Arc::new(LspState::new(config));

  let (service, socket) = LspService::new(|client| Server::new(client, state));

  TowerServer::new(stdin, stdout, socket).serve(service).await;
}
