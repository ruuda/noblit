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
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
// TODO: Field should not be pub, but it's useful in tests.
pub struct PageId(pub u64);

impl PageId {
    pub fn max() -> PageId {
        use std::u64;
        PageId(u64::MAX)
    }
}

/// Trait to allow compile-time parametrization over the page size.
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

/// 256 bytes per page.
pub struct PageSize256;

/// 568 bytes per page.
///
/// A 568-byte page can hold 13 datoms, the 2-byte header, and then it has 38
/// bytes to spare. The size is deliberately not a power of two, to increase the
/// chances of catching edge cases in tests. It is a multiple of 8 for alignment
/// requirements, but it is a prime multiple of 8: 568 = 71 * 8.
pub struct PageSize568;

/// 4096 bytes per page.
pub struct PageSize4096;

impl PageSize for PageSize256 {
    const SIZE: usize = 256;
    const CAPACITY: usize = 6;
    fn new() -> PageSize256 { PageSize256 }
}

impl PageSize for PageSize568 {
    const SIZE: usize = 568;
    const CAPACITY: usize = 13;
    fn new() -> PageSize568 { PageSize568 }
}

impl PageSize for PageSize4096 {
    const SIZE: usize = 4096;
    const CAPACITY: usize = 102;
    fn new() -> PageSize4096 { PageSize4096 }
}

/// A page store that can be read from.
pub trait Store {
    type Size: PageSize;

    /// Retrieve a page.
    fn get(&self, page: PageId) -> &[u8];

    /// Serialize the entire store to a sink.
    fn dump(&self, out: &mut dyn io::Write) -> io::Result<()>;
}

/// A page store that can be appended to, in addition to reading from it.
pub trait StoreMut: Store {
    /// Write a page and return its page id.
    ///
    /// The page must be exactly `Size::SIZE` bytes long.
    fn write_page(&mut self, page: &[u8]) -> io::Result<PageId>;
}

impl<'a, T: Store> Store for &'a T {
    type Size = T::Size;
    fn get(&self, page: PageId) -> &[u8] { (**self).get(page) }
    fn dump(&self, out: &mut dyn io::Write) -> io::Result<()> { (**self).dump(out) }
}

impl<'a, T: Store> Store for &'a mut T {
    type Size = T::Size;
    fn get(&self, page: PageId) -> &[u8] { (**self).get(page) }
    fn dump(&self, out: &mut dyn io::Write) -> io::Result<()> { (**self).dump(out) }
}

impl<'a, T: StoreMut> StoreMut for &'a mut T {
    fn write_page(&mut self, page: &[u8]) -> io::Result<PageId> {
        (**self).write_page(page)
    }
}
