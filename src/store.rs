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

impl PageId {
    pub fn max() -> PageId {
        use std::u64;
        PageId(u64::MAX)
    }
}

/// Traits to allow compile-time parametrization over the page size.
///
/// This is mainly useful in tests, where constructing large trees is
/// undesirable, and for fuzzing, where small examples are faster to explore.
/// In that case we can use smaller pages, while in release mode we use a page
/// size that is a multiple of the OS page size.
pub trait PageSize {
    /// The number of bytes in a page. A page stores exactly one tree node.
    const SIZE: usize;

    /// The number of datoms that fit in a page.
    const CAPACITY: usize;

    /// Construct a sentinel value.
    fn new() -> Self;

    /// The size of the datoms array in bytes.
    #[inline(always)]
    fn datoms_bytes_len() -> usize {
        // A datom is 32 bytes.
        32 * Self::CAPACITY
    }

    /// The size of the children page id array in bytes.
    fn children_bytes_len() -> usize {
        // A page id is 8 bytes, and there is one more page id than datoms.
        8 * (Self::CAPACITY + 1)
    }

    /// Byte offset at which the children page id array starts.
    #[inline(always)]
    fn children_offset() -> usize {
        // The page layout is first the datom array (32 bytes per datom),
        // then the child page id array (8 bytes per element), the the header.
        Self::datoms_bytes_len()
    }

    /// Byte offset at which the page header starts.
    #[inline(always)]
    fn header_offset() -> usize {
        // The page layout is first the datom array (32 bytes per datom),
        // then the child page id array (8 bytes per element), the the header.
        Self::datoms_bytes_len() + Self::children_bytes_len()
    }
}

pub struct PageSize256;
pub struct PageSize563;
pub struct PageSize4096;

impl PageSize for PageSize256 {
    const SIZE: usize = 256;
    const CAPACITY: usize = 6;
    fn new() -> PageSize256 { PageSize256 }
}

// A 563-byte page can hold 13 datoms, the 2-byte header, and then it has 33
// bytes to spare. The size is deliberately prime (and not a power of two) to
// increase the chances of catching edge cases in tests.
impl PageSize for PageSize563 {
    const SIZE: usize = 563;
    const CAPACITY: usize = 13;
    fn new() -> PageSize563 { PageSize563 }
}

impl PageSize for PageSize4096 {
    const SIZE: usize = 4096;
    const CAPACITY: usize = 102;
    fn new() -> PageSize4096 { PageSize4096 }
}

/// Interface for retrieving and writing pages.
pub trait Store {
    type Writer: io::Write;
    type Size: PageSize;

    /// Write a page and return its page id.
    ///
    /// The page must be exactly `Size::SIZE` bytes long.
    fn write_page(&mut self, page: &[u8]) -> io::Result<PageId>;

    /// Retrieve a page.
    fn get(&self, page: PageId) -> &[u8];
}

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
    type Writer = Vec<u8>;
    type Size = Size;

    fn write_page(&mut self, page: &[u8]) -> io::Result<PageId> {
        assert_eq!(page.len(), Size::SIZE);

        self.buffer.extend_from_slice(page);

        let pid = self.fresh;
        self.fresh += 1;

        Ok(PageId(pid))
    }

    fn get(&self, page: PageId) -> &[u8] {
        assert!(page != PageId::max(), "Should never get() PageId::max().");
        let begin = page.0 as usize * Size::SIZE;
        let end = (1 + page.0 as usize) * Size::SIZE;

        &self.buffer[begin..end]
    }
}
