// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An `Store` and `Heap` backed by a memory-mapped file.

use std::io;

use binary;
use heap::{CidBytes, CidInt, Heap};
use store::{PageId, PageSize4096, Store};

use mmap::Mmap;

/// A page store backed by a memory-mapped file.
pub struct MmapStore {
    /// The part of the file that contains the store.
    buffer: Mmap,
}

impl MmapStore {
    pub fn new(buffer: Mmap) -> MmapStore {
        MmapStore { buffer }
    }
}

impl Store for MmapStore {
    type Size = PageSize4096;

    fn get(&self, page: PageId) -> &[u8] {
        assert!(page != PageId::max(), "Should never get() PageId::max().");
        let begin = page.0 as usize * 4096;
        let end = (1 + page.0 as usize) * 4096;

        &self.buffer[begin..end]
    }

    fn dump(&self, out: &mut dyn io::Write) -> io::Result<()> {
        out.write_all(&self.buffer[..])
    }
}

/// A constant heap backed by a memory-mapped file.
pub struct MmapHeap {
    /// The part of the file that contains the heap.
    buffer: Mmap,
}

impl MmapHeap {
    pub fn new(buffer: Mmap) -> MmapHeap {
        MmapHeap { buffer }
    }
}

// TODO: Could this be unified with MemoryHeap with something like
// MemoryHeap<T: AsRef<[u8]>> which could then be both Vec and Mmap?
impl Heap for MmapHeap {
    fn get_u64(&self, offset: CidInt) -> u64 {
        debug_assert_eq!(offset.0 % 8, 0, "Constant ids must be 8-byte aligned.");
        assert!(offset.0 + 8 <= self.buffer.len() as u64, "Constant id out of bounds.");

        let bytes = binary::slice_8(&self.buffer[offset.0 as usize..]);
        binary::u64_from_le_bytes(bytes)
    }

    /// Retrieve a byte string constant.
    fn get_bytes(&self, offset: CidBytes) -> &[u8] {
        let len = self.get_u64(CidInt(offset.0));

        // We must fit an 8-byte length, and at least 8 bytes of data (otherwise
        // the data could be stored inline in a value; it would not need to be
        // on the heap).
        debug_assert!(len >= 8, "Encountered small value on the heap.");

        assert!(offset.0 + 8 + len <= self.buffer.len() as u64, "Constant bytestring out of bounds.");
        &self.buffer[offset.0 as usize + 8..offset.0 as usize + 8 + len as usize]
    }
}
