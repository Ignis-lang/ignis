use std::collections::HashMap;
use ignis_type::{Id, Store};
use ignis_type::symbol::SymbolId;
use ignis_type::definition::DefinitionId;
use ignis_type::namespace::NamespaceId;

pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
  Global,
  Function,
  Block,
  Loop,
  Namespace(NamespaceId),
}

#[derive(Debug, Clone)]
pub struct Scope {
  pub parent: Option<ScopeId>,
  pub kind: ScopeKind,
  pub symbols: HashMap<SymbolId, DefinitionId>,
}

#[derive(Debug, Clone)]
pub struct ScopeTree {
  scopes: Store<Scope>,
  current: ScopeId,
}

impl ScopeTree {
  pub fn new() -> Self {
    let mut scopes = Store::new();
    let global = scopes.alloc(Scope {
      parent: None,
      kind: ScopeKind::Global,
      symbols: HashMap::new(),
    });

    Self {
      scopes,
      current: global,
    }
  }

  pub fn push(
    &mut self,
    kind: ScopeKind,
  ) -> ScopeId {
    let new = self.scopes.alloc(Scope {
      parent: Some(self.current.clone()),
      kind,
      symbols: HashMap::new(),
    });

    self.current = new.clone();

    new
  }

  pub fn pop(&mut self) {
    if let Some(parent) = &self.scopes.get(&self.current).parent {
      self.current = parent.clone();
    }
  }

  pub fn define(
    &mut self,
    name: &SymbolId,
    def: &DefinitionId,
  ) -> Result<(), DefinitionId> {
    let scope = self.scopes.get_mut(&self.current);

    if let Some(existing) = scope.symbols.get(&name) {
      Err(existing.clone())
    } else {
      scope.symbols.insert(name.clone(), def.clone());
      Ok(())
    }
  }

  pub fn lookup(
    &self,
    name: &SymbolId,
  ) -> Option<&DefinitionId> {
    let mut current = &self.current;
    loop {
      let scope = self.scopes.get(&current);
      if let Some(def) = scope.symbols.get(&name) {
        return Some(def);
      }
      match &scope.parent {
        Some(parent) => current = parent,
        None => return None,
      }
    }
  }

  pub fn find_loop_scope(&self) -> Option<ScopeId> {
    let mut current = &self.current;
    loop {
      let scope = self.scopes.get(current);

      if scope.kind == ScopeKind::Loop {
        return Some(current.clone());
      }

      match &scope.parent {
        Some(parent) => current = parent,
        None => return None,
      }
    }
  }

  pub fn current(&self) -> &ScopeId {
    &self.current
  }

  pub fn set_current(
    &mut self,
    scope_id: &ScopeId,
  ) {
    self.current = scope_id.clone();
  }

  pub fn get_scope(
    &self,
    id: &ScopeId,
  ) -> &Scope {
    self.scopes.get(id)
  }
}
