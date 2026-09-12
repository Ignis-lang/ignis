use crate::{Id, Store};

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Symbol {
  pub name: String,
}

pub type SymbolId = Id<Symbol>;

pub struct SymbolTable {
  pub symbols: Store<Symbol>,
  pub map: std::collections::HashMap<String, SymbolId>,
}

impl Default for SymbolTable {
  fn default() -> Self {
    Self::new()
  }
}

impl SymbolTable {
  pub fn new() -> Self {
    SymbolTable {
      symbols: Store::new(),
      map: std::collections::HashMap::new(),
    }
  }

  pub fn intern(
    &mut self,
    name: &str,
  ) -> SymbolId {
    if let Some(id) = self.map.get(name) {
      *id
    } else {
      let id = self.symbols.alloc(Symbol { name: name.to_string() });
      self.map.insert(name.to_string(), id);
      id
    }
  }

  pub fn get(
    &self,
    id: &SymbolId,
  ) -> &str {
    &self.symbols.get(id).name
  }

  /// Whether this symbol is the discard name `_`, which never names a binding.
  ///
  /// `let _ = value;` and `import _ from "..."` both spell "evaluate this and keep
  /// nothing", so `_` must not reach a scope as a variable: doing so would make a
  /// discard own its value until the scope ends and would make a second discard in
  /// the same scope a redefinition.
  pub fn is_discard(
    &self,
    id: &SymbolId,
  ) -> bool {
    self.get(id) == "_"
  }

  pub fn get_or_intern(
    &mut self,
    name: &str,
  ) -> SymbolId {
    if let Some(id) = self.map.get(name) {
      *id
    } else {
      let id = self.symbols.alloc(Symbol { name: name.to_string() });
      self.map.insert(name.to_string(), id);
      id
    }
  }
}
