use core::hash::Hash;
use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct Scopes<K, V> {
    base: FxHashMap<K, V>,
    scopes: Vec<FxHashMap<K, V>>,
}

impl<K, V> Scopes<K, V> {
    pub fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    pub fn pop_scope(&mut self) -> Option<FxHashMap<K, V>> {
        self.scopes.pop()
    }
}

impl<K: Eq + Hash, V> Scopes<K, V> {
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.scopes
            .last_mut()
            .unwrap_or(&mut self.base)
            .insert(k, v)
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(k))
            .or_else(|| self.base.get(k))
    }

    pub fn contains_key(&self, k: &K) -> bool {
        self.scopes.iter().rev().any(|scope| scope.contains_key(k)) || self.base.contains_key(k)
    }
}

impl<K, V> Default for Scopes<K, V> {
    fn default() -> Self {
        Self {
            base: FxHashMap::default(),
            scopes: Vec::default(),
        }
    }
}

impl<K: Eq + Hash, V> core::ops::Index<&K> for Scopes<K, V> {
    type Output = V;

    fn index(&self, index: &K) -> &Self::Output {
        self.get(index).unwrap()
    }
}
