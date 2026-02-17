use std::collections::HashMap;

use crate::{
  Id, Store,
  definition::{DefinitionId, SymbolEntry},
  symbol::SymbolId,
};

pub type NamespaceId = Id<Namespace>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Namespace {
  pub name: SymbolId,
  pub parent: Option<NamespaceId>,
  pub is_extern: bool,
  pub children: HashMap<SymbolId, NamespaceId>,
  pub definitions: HashMap<SymbolId, SymbolEntry>,
}

impl Namespace {
  pub fn new(
    name: SymbolId,
    parent: Option<NamespaceId>,
    is_extern: bool,
  ) -> Self {
    Self {
      name,
      parent,
      is_extern,
      children: HashMap::new(),
      definitions: HashMap::new(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamespaceStore {
  namespaces: Store<Namespace>,
  root_children: HashMap<SymbolId, NamespaceId>,
}

impl Default for NamespaceStore {
  fn default() -> Self {
    Self::new()
  }
}

impl NamespaceStore {
  pub fn new() -> Self {
    Self {
      namespaces: Store::new(),
      root_children: HashMap::new(),
    }
  }

  pub fn get_or_create(
    &mut self,
    path: &[SymbolId],
    is_extern: bool,
  ) -> NamespaceId {
    assert!(!path.is_empty(), "namespace path cannot be empty");

    let mut current_parent: Option<NamespaceId> = None;

    for (i, &segment) in path.iter().enumerate() {
      let is_last = i == path.len() - 1;

      let existing = if let Some(parent_id) = current_parent {
        self.namespaces.get(&parent_id).children.get(&segment).copied()
      } else {
        self.root_children.get(&segment).copied()
      };

      current_parent = Some(match existing {
        Some(ns_id) => ns_id,
        None => {
          let ns = Namespace::new(segment, current_parent, is_last && is_extern);
          let ns_id = self.namespaces.alloc(ns);

          if let Some(parent_id) = current_parent {
            self.namespaces.get_mut(&parent_id).children.insert(segment, ns_id);
          } else {
            self.root_children.insert(segment, ns_id);
          }

          ns_id
        },
      });
    }

    current_parent.expect("path is non-empty")
  }

  pub fn lookup(
    &self,
    path: &[SymbolId],
  ) -> Option<NamespaceId> {
    if path.is_empty() {
      return None;
    }

    let mut current = *self.root_children.get(&path[0])?;

    for &segment in &path[1..] {
      current = *self.namespaces.get(&current).children.get(&segment)?;
    }

    Some(current)
  }

  pub fn define(
    &mut self,
    ns: NamespaceId,
    name: SymbolId,
    def: DefinitionId,
    is_overloadable: bool,
  ) {
    let definitions = &mut self.namespaces.get_mut(&ns).definitions;
    match definitions.get_mut(&name) {
      None => {
        definitions.insert(name, SymbolEntry::Single(def));
      },
      Some(entry) => match entry {
        SymbolEntry::Single(existing) => {
          if is_overloadable {
            let existing = *existing;
            *entry = SymbolEntry::Overload(vec![existing, def]);
          }
        },
        SymbolEntry::Overload(group) => {
          if is_overloadable {
            group.push(def);
          }
        },
      },
    }
  }

  pub fn lookup_def(
    &self,
    ns: NamespaceId,
    name: &SymbolId,
  ) -> Option<&SymbolEntry> {
    self.namespaces.get(&ns).definitions.get(name)
  }

  pub fn full_path(
    &self,
    ns: NamespaceId,
  ) -> Vec<SymbolId> {
    let mut path = Vec::new();
    let mut current = Some(ns);

    while let Some(id) = current {
      let namespace = self.namespaces.get(&id);
      path.push(namespace.name);
      current = namespace.parent;
    }

    path.reverse();
    path
  }

  pub fn get(
    &self,
    id: &NamespaceId,
  ) -> &Namespace {
    self.namespaces.get(id)
  }

  pub fn get_mut(
    &mut self,
    id: &NamespaceId,
  ) -> &mut Namespace {
    self.namespaces.get_mut(id)
  }

  pub fn lookup_child(
    &self,
    ns: NamespaceId,
    name: &SymbolId,
  ) -> Option<NamespaceId> {
    self.namespaces.get(&ns).children.get(name).copied()
  }

  pub fn lookup_root(
    &self,
    name: &SymbolId,
  ) -> Option<NamespaceId> {
    self.root_children.get(name).copied()
  }

  /// Return all (name, DefinitionId) pairs in a namespace, flattening overloads.
  pub fn all_defs(
    &self,
    ns: NamespaceId,
  ) -> Vec<(SymbolId, DefinitionId)> {
    let namespace = self.namespaces.get(&ns);
    let mut result = Vec::new();

    for (&name, entry) in &namespace.definitions {
      match entry {
        SymbolEntry::Single(id) => result.push((name, *id)),
        SymbolEntry::Overload(ids) => {
          for id in ids {
            result.push((name, *id));
          }
        },
      }
    }

    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::symbol::SymbolTable;

  fn setup() -> (SymbolTable, NamespaceStore) {
    (SymbolTable::new(), NamespaceStore::new())
  }

  #[test]
  fn test_create_single_namespace() {
    let (mut symbols, mut store) = setup();
    let math = symbols.intern("Math");

    let ns_id = store.get_or_create(&[math], false);
    let ns = store.get(&ns_id);

    assert_eq!(ns.name, math);
    assert!(ns.parent.is_none());
    assert!(!ns.is_extern);
  }

  #[test]
  fn test_create_nested_namespace() {
    let (mut symbols, mut store) = setup();
    let math = symbols.intern("Math");
    let linear = symbols.intern("Linear");

    let ns_id = store.get_or_create(&[math, linear], false);
    let ns = store.get(&ns_id);

    assert_eq!(ns.name, linear);
    assert!(ns.parent.is_some());

    let path = store.full_path(ns_id);
    assert_eq!(path, vec![math, linear]);
  }

  #[test]
  fn test_lookup_existing_namespace() {
    let (mut symbols, mut store) = setup();
    let math = symbols.intern("Math");
    let linear = symbols.intern("Linear");

    let created_id = store.get_or_create(&[math, linear], false);
    let lookup_id = store.lookup(&[math, linear]);

    assert_eq!(Some(created_id), lookup_id);
  }

  #[test]
  fn test_lookup_nonexistent_namespace() {
    let (mut symbols, store) = setup();
    let math = symbols.intern("Math");

    let result = store.lookup(&[math]);
    assert!(result.is_none());
  }

  #[test]
  fn test_get_or_create_idempotent() {
    let (mut symbols, mut store) = setup();
    let math = symbols.intern("Math");

    let id1 = store.get_or_create(&[math], false);
    let id2 = store.get_or_create(&[math], false);

    assert_eq!(id1, id2);
  }

  #[test]
  fn test_extern_namespace() {
    let (mut symbols, mut store) = setup();
    let libc = symbols.intern("libc");

    let ns_id = store.get_or_create(&[libc], true);
    let ns = store.get(&ns_id);

    assert!(ns.is_extern);
  }

  #[test]
  fn test_define_and_lookup_definition() {
    let (mut symbols, mut store) = setup();
    let math = symbols.intern("Math");
    let add = symbols.intern("add");

    let ns_id = store.get_or_create(&[math], false);

    // Create a dummy definition ID
    let def_id = SymbolEntry::Single(DefinitionId::new(42));
    store.define(ns_id, add, *def_id.as_single().unwrap(), false);

    let result = store.lookup_def(ns_id, &add);
    assert_eq!(result, Some(&def_id));
  }

  #[test]
  fn test_lookup_root() {
    let (mut symbols, mut store) = setup();
    let math = symbols.intern("Math");

    let ns_id = store.get_or_create(&[math], false);
    let lookup = store.lookup_root(&math);

    assert_eq!(lookup, Some(ns_id));
  }

  #[test]
  fn test_lookup_child() {
    let (mut symbols, mut store) = setup();
    let math = symbols.intern("Math");
    let linear = symbols.intern("Linear");

    store.get_or_create(&[math, linear], false);

    let math_id = store.lookup_root(&math).unwrap();
    let linear_id = store.lookup_child(math_id, &linear);

    assert!(linear_id.is_some());
  }
}
