// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Thin abstractions for storing data in files on disk.

use std::io;

/// A page id.
///
/// A page id is the offset of the page (counted in pages) from the start of the
/// file that contains it. For 4096-byte pages, page 0 starts at byte 0, page
/// 2 starts at byte 8192, etc.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
// TODO: Field should not be pub, but it's useful in tests.
pub struct PageId(pub u64);

/// The number of bytes in a page. A page stores exactly one tree node.
// TODO: Make this an associated constant of `Store`, to facilitate testing with
// different page sizes.
pub const PAGE_SIZE: usize = 4096;

/// A page store.
pub trait Store {
    type Writer: io::Write;

    /// Return the underlying writer.
    fn writer(&mut self) -> &mut Self::Writer;

    /// Reserve a new page.
    ///
    /// Writing the data to the underlying writer is the responsibility
    /// of the caller.
    // TODO: Can this be safer?
    fn allocate_page(&mut self) -> PageId;
}

/// An in-memory page store, not backed by a file.
pub struct MemoryStore {
    /// The backing buffer.
    buffer: Vec<u8>,

    /// The next unused page id.
    fresh: u64,
}

impl MemoryStore {
    pub fn new() -> MemoryStore {
        MemoryStore {
            buffer: Vec::new(),
            fresh: 0,
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.buffer[..]
    }
}

impl Store for MemoryStore {
    type Writer = Vec<u8>;

    fn writer(&mut self) -> &mut Vec<u8> {
        &mut self.buffer
    }

    fn allocate_page(&mut self) -> PageId {
        let pid = self.fresh;
        self.fresh += 1;
        PageId(pid)
    }
}
