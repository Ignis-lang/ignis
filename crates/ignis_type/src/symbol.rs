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
      id.clone()
    } else {
      let id = self.symbols.alloc(Symbol { name: name.to_string() });
      self.map.insert(name.to_string(), id.clone());
      id.clone()
    }
  }

  pub fn get(
    &self,
    id: &SymbolId,
  ) -> &str {
    &self.symbols.get(id).name
  }

  pub fn get_or_intern(
    &mut self,
    name: &str,
  ) -> SymbolId {
    if let Some(id) = self.map.get(name) {
      id.clone()
    } else {
      let id = self.symbols.alloc(Symbol { name: name.to_string() });
      self.map.insert(name.to_string(), id.clone());
      id.clone()
    }
  }
}
