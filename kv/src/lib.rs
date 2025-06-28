// Noblit -- An immutable append-only database
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::collections::HashMap;
use std::hash::Hash;

/// Trait to allow compile-time parametrization over the page size.
///
/// This is mainly useful in tests, where constructing large trees is
/// undesirable, and for fuzzing, where small examples are faster to explore.
/// In that case we can use smaller pages, while in release mode we use a page
/// size that is a multiple of the OS page size.
pub trait PageSize {
    /// The number of bytes in a page. A page stores exactly one tree node.
    const SIZE: usize;
}

pub struct Error {}

/// Identifies a virtual page.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PageId(u64);

/// Identifies a physical page.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct SlotId(u64);

/// How would we save a new page?
/// 1. Write the data into the slot.
/// 2. Append to the page table.
/// 3. Fsync both.
/// So this needs to be one op, I suppose?
/// Or virtual IO operations with completions, but how to link them properly?
/// DECISION: For now, let's start with synchronous IO and let's not worry about
/// efficient batching. That's not the goal of this project.

type Result<T> = std::result::Result<T, Error>;

pub trait PageTableRead {
    /// Load the entire table from backing storage.
    fn read(&mut self) -> Result<HashMap<PageId, SlotId>>;
}

pub trait PageTableWrite: PageTableRead {
    /// Append a new mapping entry.
    ///
    /// Returns the number of entries (page_id_slot_id pairs) in the backing file.
    fn append(&mut self, v_id: PageId, p_id: SlotId) -> Result<usize>;

    /// Replace the entire table with a new version.
    fn rewrite(&mut self, ids: &HashMap<PageId, SlotId>) -> Result<()>;
}

pub struct PageTable<T> {
    /// The in-memory mapping of page id to physical slot.
    mapping: HashMap<PageId, SlotId>,

    /// Backing IO system.
    io: T,
}

impl<T: PageTableRead> PageTable<T> {
    pub fn new(mut io: T) -> Result<Self> {
        let result = Self {
            mapping: io.read()?,
            io,
        };
        Ok(result)
    }

    pub fn get(&self, v_id: PageId) -> Option<SlotId> {
        self.mapping.get(&v_id).cloned()
    }
}

impl<T: PageTableWrite> PageTable<T> {
    /// Insert a new mapping into the table.
    ///
    /// This consumes self, because in the case of an IO error, we are not sure
    /// whether the on-disk table is in sync with what we keep in memory, and we
    /// can't allow execution to continue.
    pub fn append(mut self, v_id: PageId, p_id: SlotId) -> Result<Self> {
        // Save the new entry to the backing file, and if the backing file is
        // growing too large, then we replace it to ensure the loading time and
        // disk usage remain bounded.
        let n_entries = self.io.append(v_id, p_id)?;
        if n_entries > self.mapping.len() * 10 {
            self.io.rewrite(&self.mapping)?;
        }

        self.mapping.insert(v_id, p_id);

        Ok(self)
    }
}
