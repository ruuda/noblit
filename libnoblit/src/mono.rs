// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Mononomorphized instances of `noblit::Database` and others.
//!
//! These are used to expose the generic types to the FFI for certain fixed
//! types. This way, we dispatch once per FFI call, rather than using virtual
//! dispatch on every store and heap interaction. This is mostly because boxed
//! trait objects lead to intractable types in many places, that need nontrivial
//! conversion (e.g. from `dyn StoreMut` to `dyn Store`).

use noblit::database;

use noblit::memory_store::{MemoryHeap, MemoryStore};
use noblit::store::PageSize4096;
 
type MemoryStore4096 = MemoryStore<PageSize4096>;

pub enum Database {
    Memory(database::Database<MemoryStore4096, MemoryHeap>),
    // In the future, with file-backed store and heap, there would be more
    // entries here.
}

/// Call the generic function `f` on database `db`.
///
/// If this would be a function, its type would be (Haskell notation):
///
///     :: forall r. &self
///     -> (forall s h. (store::Store s, heap::Heap h) => &Database s h -> r)
///     -> r
///
/// This is a higher-kinded type that we can't express in Rust, so we resort to
/// a macro instead.
macro_rules! with_database {
    ($db: expr, $f: ident) => {
        match *$db {
            Database::Memory(ref db) => $f(db),
        }
    };
}
