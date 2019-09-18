// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An in-memory `Store` implementation.

use std::io;

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
