use std::collections::HashMap;
use ignis_type::{Id, Store};
use ignis_type::symbol::SymbolId;
use ignis_type::definition::{DefinitionId, SymbolEntry};
use ignis_type::namespace::NamespaceId;

pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
  Global,
  Function,
  Block,
  Loop,
  Namespace(NamespaceId),
  /// Scope for type parameters in generic functions/records/enums/methods.
  /// Type parameter names (T, U, etc.) are visible within this scope.
  Generic,
}

#[derive(Debug, Clone)]
pub struct Scope {
  pub parent: Option<ScopeId>,
  pub kind: ScopeKind,
  pub symbols: HashMap<SymbolId, SymbolEntry>,
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
    is_overloadable: bool,
  ) -> Result<(), DefinitionId> {
    let scope = self.scopes.get_mut(&self.current);

    match scope.symbols.get_mut(name) {
      None => {
        scope.symbols.insert(name.clone(), SymbolEntry::Single(*def));
        Ok(())
      },
      Some(SymbolEntry::Single(existing)) => Err(existing.clone()),
      Some(SymbolEntry::Overload(group)) => {
        if !is_overloadable {
          Err(group[0].clone())
        } else {
          group.push(*def);
          Ok(())
        }
      },
    }
  }

  /// Promotes an existing Single definition to an Overload group containing both the existing
  /// definition and the new definition.
  /// This should be called only after checking that the existing definition is indeed overloadable (Function/Method).
  pub fn promote_to_overload(
    &mut self,
    name: &SymbolId,
    new_def: &DefinitionId,
  ) {
    let scope = self.scopes.get_mut(&self.current);
    if let Some(entry) = scope.symbols.get_mut(name) {
      if let SymbolEntry::Single(existing) = entry {
        *entry = SymbolEntry::Overload(vec![*existing, *new_def]);
      } else if let SymbolEntry::Overload(group) = entry {
        group.push(*new_def);
      }
    }
  }

  pub fn lookup(
    &self,
    name: &SymbolId,
  ) -> Option<&SymbolEntry> {
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

  pub fn lookup_def(
    &self,
    name: &SymbolId,
  ) -> Option<&DefinitionId> {
    self.lookup(name).and_then(|entry| entry.as_single())
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

  pub fn get_scope_mut(
    &mut self,
    id: &ScopeId,
  ) -> &mut Scope {
    self.scopes.get_mut(id)
  }

  pub fn all_scopes(&self) -> impl Iterator<Item = &Scope> {
    self.scopes.iter().map(|(_, scope)| scope)
  }
}
