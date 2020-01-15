// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines the head, the stateful part of the database.
//!
//! A Noblit database is append-only, but a tiny bit of mutable state is needed
//! to keep track of the root nodes of the indexes, and to track counters for
//! generating new entity ids. This mutable state is called the *head*.

use store::PageId;
use idgen::IdGen;

/// Page ids of root nodes of the index trees.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IndexRoots {
    pub eavt_root: PageId,
    pub aevt_root: PageId,
    pub avet_root: PageId,
}

impl IndexRoots {
    /// Return whether all roots of `self` lie strictly before all roots of `other`.
    ///
    /// Because page ids are allocated sequentially, roots A preceding roots B
    /// implies that the elements reachable from A are a subset of the elements
    /// reachable from B, under the assumption that B was obtained from A by
    /// appending elements.
    pub fn precedes(&self, other: &IndexRoots) -> bool {
        if self.eavt_root >= other.eavt_root { return false }
        if self.aevt_root >= other.aevt_root { return false }
        if self.avet_root >= other.avet_root { return false }
        true
    }
}

/// Volatile state of the database that changes after every transaction.
///
/// Most of the database is append-only, but a few mutable variables are needed:
///
/// * The page ids of the latest roots of the index trees.
/// * The next free ids to use for entities and transactions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Head {
    pub roots: IndexRoots,
    pub id_gen: IdGen,
}
