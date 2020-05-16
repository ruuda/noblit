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

use std::os::raw::c_void;

use noblit::database;
use noblit::memory_store::{MemoryHeap, MemoryStore};
use noblit::mmap_store::{MmapHeap, MmapStore};
use noblit::store::PageSize4096;

use super::Context;
 
type MemoryStore4096 = MemoryStore<PageSize4096>;

pub enum Database {
    Memory(database::Database<MemoryStore4096, MemoryHeap>),
    Mmap(database::Database<MmapStore, MmapHeap>),
}

/// A value, with context whose database this value was created from.
///
/// The context is stored such that we can dispatch on the type of the database
/// in it to determine the type of the value. The context field is first, such
/// that a pointer to a `Contextual<T>` can be treated as a pointer to a `*mut
/// Context` without knowing what `T` is yet.
#[repr(C)]
pub struct Contextual<T> {
    context: *mut Context,
    pub value: T
}

impl<T> Contextual<T> {
    /// Pack up the given value in a box with context, return a void pointer.
    pub unsafe fn new(context: *mut Context, value: T) -> *mut c_void {
        let boxed_context = Box::new(Contextual {
            context: context,
            value: value,
        });
        Box::into_raw(boxed_context) as *mut c_void
    }
}

/// Call the generic function `f` on the database in context `ctx`.
///
/// If this would be a function, its type would be (Haskell notation):
///
///     :: forall r. *mut Context
///     -> (forall s h. (store::Store s, heap::Heap h) => &Database s h -> r)
///     -> r
///
/// This is a higher-kinded type that we can't express in Rust, so we resort to
/// a macro instead.
macro_rules! with_database {
    ($ctx: ident, $f: expr) => {{
        // Assign the macro arg to a type-annotated local variable, to avoid
        // passing in something of the wrong type but having it compile
        // accidentally.
        let ctx: &mut Context = &mut *($ctx as *mut Context);

        (*ctx).clear_error();
        let result = match ctx.db {
            mono::Database::Memory(ref mut db) => $f(db),
            mono::Database::Mmap(ref mut db) => $f(db),
        };
        match result {
            Ok(()) => 0,
            Err(err) => ctx.observe_error(err),
        }
    }};
}

/// Call the generic function `f` on the database and contextual value.
///
/// If this would be a function, its type would be (Haskell notation):
///
///     :: forall r cv. *mut Context
///     -> (forall s h. (store::Store s, heap::Heap h) => &Database s h -> cv s h -> r)
///     -> r
///
/// The function `f` should accept `&mut Database` and `&mut T`, where `T` is
/// the value in the `Contextual<T>`. `T` must be inferred through type
/// inference. Note that when the `T` itself contains a mutable pointer to the
/// database, then we now alias it, therefore this macro is unsafe. The caller
/// should be careful with the `db` pointer. It is mostly useful to trigger
/// unification in type inference, it should not be dereferenced.
macro_rules! with_context {
    ($contextual: ident, $f: expr) => {
        // The contextual struct stores a pointer to the context as its first
        // field, so we can treat a pointer to it as a pointer to `*const Context`.
        let ctx: *mut Context = *($contextual as *const *mut Context);
        with_database!(ctx, |db: &mut database::Database<_, _>| {
            let contextual: &mut Contextual<_> = &*($contextual as *mut Contextual<_>);
            $f(db, &mut contextual.value)
        });
    };
}

/// Like `with_context`, but receives `T` instead of `&mut T`.
///
/// This drops the `Contextual<T>`.
macro_rules! into_context {
    ($contextual: ident, $f: expr) => {
        // The contextual struct stores a pointer to the context as its first
        // field, so we can treat a pointer to it as a pointer to `*const Context`.
        let ctx: *mut Context = *($contextual as *const *mut Context);
        with_database!(ctx, |db: &mut database::Database<_, _>| {
            let mut contextual: Box<Contextual<_>> = Box::from_raw($contextual as *mut Contextual<_>);
            $f(db, &mut contextual.value)
            // At this point, the box goes out of scope, and the contextual
            // value gets dropped.
        });
    };
}
