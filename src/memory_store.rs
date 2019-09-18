// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An in-memory `Store` implementation.

use std::io;

use pool::{ConstId, Pool, PoolMut};
use store::{PageId, PageSize, Store, StoreMut};

/// An in-memory page store, not backed by a file.
pub struct MemoryStore<Size: PageSize> {
    /// The backing buffer.
    buffer: Vec<u8>,

    /// The next unused page id.
    fresh: u64,

    /// Instance of page size traits.
    _size_sentinel: Size,
}

impl<Size: PageSize> MemoryStore<Size> {
    pub fn new() -> MemoryStore<Size> {
        MemoryStore {
            buffer: Vec::new(),
            fresh: 0,
            _size_sentinel: PageSize::new(),
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.buffer[..]
    }
}

impl<Size: PageSize> Store for MemoryStore<Size> {
    type Size = Size;

    fn get(&self, page: PageId) -> &[u8] {
        assert!(page != PageId::max(), "Should never get() PageId::max().");
        let begin = page.0 as usize * Size::SIZE;
        let end = (1 + page.0 as usize) * Size::SIZE;

        &self.buffer[begin..end]
    }
}

impl<Size: PageSize> StoreMut for MemoryStore<Size> {
    fn write_page(&mut self, page: &[u8]) -> io::Result<PageId> {
        assert_eq!(page.len(), Size::SIZE);

        self.buffer.extend_from_slice(page);

        let pid = self.fresh;
        self.fresh += 1;

        Ok(PageId(pid))
    }
}

/// An in-memory constant pool, not backed by a file.
pub struct MemoryPool {
    /// The backing buffer.
    buffer: Vec<u8>,
}

impl Pool for MemoryPool {
    fn get_u64(&self, offset: ConstId) -> u64 {
        debug_assert_eq!(offset.0 % 8, 0, "Constant ids must be 8-byte aligned.");
        assert!(offset.0 + 8 < self.buffer.len() as u64, "Constant id out of bounds.");

        let bytes = &self.buffer[offset.0 as usize..offset.0 as usize + 8];

        0
            | (bytes[0] as u64) << 56
            | (bytes[1] as u64) << 48
            | (bytes[2] as u64) << 40
            | (bytes[3] as u64) << 32
            | (bytes[4] as u64) << 24
            | (bytes[5] as u64) << 16
            | (bytes[6] as u64) << 8
            | (bytes[7] as u64)
    }

    /// Retrieve a byte string constant.
    fn get_bytes(&self, offset: ConstId) -> &[u8] {
        let len = self.get_u64(offset);

        // We must fit an 8-byte length, and at least 8 bytes of data (otherwise
        // the data could be stored inline in a value; it would not need to be
        // in the pool).
        debug_assert!(len >= 8, "Store 7-byte values inline, not in the pool.");

        assert!(offset.0 + 8 + len < self.buffer.len() as u64, "Constant bytestring out of bounds.");
        &self.buffer[offset.0 as usize + 8..offset.0 as usize + 8 + len as usize]
    }
}
