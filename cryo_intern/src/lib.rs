//! String interner for the `cryo` compiler.
//!
//! This interner uses thread-local string pools, i.e., every thread has it's own string pool.
//! Therefore, even if the same string is interned on two different threads, comparing them would not work.
#![cfg_attr(test, feature(test))]

#[cfg(test)]
extern crate test;

use std::{cell::RefCell, hash::Hash, ops::Deref};

use bumpalo::Bump;
use fxhash::FxHashSet;

const DEFAULT_INTERNER_SIZE: usize = 1024 * 1024; // 1 MiB

thread_local! {
    static POOL: RefCell<FxHashSet<StrPtr>> = RefCell::new(FxHashSet::default());
    static ARENA: Bump = Bump::with_capacity(DEFAULT_INTERNER_SIZE);
}

#[derive(Debug)]
#[repr(transparent)]
struct StrPtr(*const str);

impl PartialEq for StrPtr {
    fn eq(&self, other: &Self) -> bool {
        unsafe { *self.0 == *other.0 }
    }
}

impl Eq for StrPtr {}

impl Hash for StrPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { (*self.0).hash(state) }
    }
}

/// An interned string.
///
/// [`InternStr`] is `Clone`, `Copy`, `PartialEq`, `Eq`, and `Hash`. The implementation uses pointer equality instead of content equality to compare strings.
pub struct InternStr {
    ptr: *const str,
}

impl InternStr {
    /// Create a new interned string from a given one. If the supplied string has already been interned, a [`InternStr`] instance will be returned with the pointer to the interned string.
    /// Otherwise, the new string will be interned.
    pub fn new(s: &str) -> Self {
        let ptr = POOL.with(|container| {
            let mut container = container.borrow_mut();
            let ptr = StrPtr(s);

            if let Some(v) = container.get(&ptr) {
                v.0
            } else {
                let leaked = ARENA.with(|arena| arena.alloc_str(s) as *const str);
                container.insert(StrPtr(leaked));
                leaked
            }
        });

        Self { ptr }
    }
}

impl PartialEq for InternStr {
    fn eq(&self, other: &Self) -> bool {
        ::core::ptr::eq(self.ptr, other.ptr)
    }
}

impl Eq for InternStr {}

impl Hash for InternStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl Clone for InternStr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for InternStr {}

impl AsRef<str> for InternStr {
    fn as_ref(&self) -> &str {
        unsafe { &*self.ptr }
    }
}

impl Deref for InternStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.ptr }
    }
}

impl From<&'_ str> for InternStr {
    fn from(value: &'_ str) -> Self {
        Self::new(value)
    }
}

impl std::fmt::Debug for InternStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InternStr").field("ptr", &self.ptr).finish()
    }
}

impl std::fmt::Display for InternStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

#[cfg(test)]
mod benches {
    use rand::{Rng, distr::Alphanumeric};
    use test::Bencher;

    use crate::InternStr;

    #[bench]
    fn intern_strs(bencher: &mut Bencher) {
        bencher.iter(|| {
            let mut buf = [0u8; 16];
            rand::rng()
                .sample_iter(Alphanumeric)
                .take(16)
                .enumerate()
                .for_each(|(i, c)| buf[i] = c);
            let s = str::from_utf8(&buf).unwrap();

            ::core::hint::black_box(InternStr::new(s))
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::InternStr;

    #[test]
    fn intern_string() {
        let interned_a = InternStr::new("hello, world");
        let interned_b = InternStr::new("hello, world");
        let interned_c = InternStr::new("world, hello");

        assert_eq!(interned_a, interned_b);
        assert_ne!(interned_a, interned_c);
    }
}
