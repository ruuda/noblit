// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Thin abstractions for a bump-pointer heap of constants.
//!
//! *Heap* refers to an area of storage, it is unrelated to e.g. the
//! *binary heap* data structure.

use std::io;

/// An id for a 64-bit unsigned integer constant.
///
/// A constant id is the offset of a constant from the start of the heap.
/// Offsets of persistent constants should be aligned to 8 bytes. For 64-bit
/// unsigned integer constants, the heap stores the 8-byte integer at that
/// offset.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CidInt(pub u64);

/// An id for a byte string constant.
///
/// A constant id is the offset of a constant from the start of the heap.
/// Offsets of persistent constants should be aligned to 8 bytes. For byte
/// strings, the heap stores an unsigned 64-bit length at the offset, followed
/// by the data itself.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CidBytes(pub u64);

/// A heap of constants that can be read from.
pub trait Heap {
    /// Retrieve a 64-bit unsigned integer constant.
    fn get_u64(&self, offset: CidInt) -> u64;

    /// Retrieve a byte string constant.
    fn get_bytes(&self, offset: CidBytes) -> &[u8];
}

/// A heap of constants to which new constants can be appended.
pub trait HeapMut: Heap {
    /// Store a 64-bit unsigned integer constant.
    fn append_u64(&mut self, value: u64) -> io::Result<CidInt>;

    /// Store a byte string constant.
    fn append_bytes(&mut self, value: &[u8]) -> io::Result<CidBytes>;
}

/// A heap that exposes the number of bytes it stores.
pub trait SizedHeap: Heap {
    /// The size of the data in the heap, in bytes.
    fn len(&self) -> u64;
}

impl<'a, T: Heap> Heap for &'a T {
    fn get_u64(&self, offset: CidInt) -> u64 { (**self).get_u64(offset) }
    fn get_bytes(&self, offset: CidBytes) -> &[u8] { (**self).get_bytes(offset) }
}

impl<'a, T: Heap> Heap for &'a mut T {
    fn get_u64(&self, offset: CidInt) -> u64 { (**self).get_u64(offset) }
    fn get_bytes(&self, offset: CidBytes) -> &[u8] { (**self).get_bytes(offset) }
}

impl<'a, T: SizedHeap> SizedHeap for &'a T {
    fn len(&self) -> u64 { (**self).len() }
}

impl<'a, T: SizedHeap> SizedHeap for &'a mut T {
    fn len(&self) -> u64 { (**self).len() }
}

/// Assert that the heap is well-formed, that all invariants hold.
///
/// This is used in tests and during fuzzing.
pub fn check_invariants<H: SizedHeap>(heap: H) {
    // The maximum value that can still be stored inline. Integers on the heap
    // should be larger than this, otherwise they should have been stored
    // inline. Consequently, integers smaller than this will be length prefixes.
    let max_inline_u64 = 0x4000_0000_0000_0000 - 1;

    let mut off = 0;
    loop {
        if off >= heap.len() {
            assert_eq!(off, heap.len(), "Heap size must be a multiple of 8.");
            break
        }

        let value = heap.get_u64(CidInt(off));
        if value > max_inline_u64 {
            // The value must have been an 8-byte unsigned int, move on.
            off += 8;
        } else {
            // The value was a length prefix. Skip over the data (aligned to 8
            // bytes).
            let data_len = value;
            let data_len_aligned = (data_len + 7) / 8 * 8;
            assert!(
                off + 8 + data_len_aligned <= heap.len(),
                "Byte string of length {} at offset {} does not fit heap of size {}.",
                off + 8, data_len, heap.len()
            );
            off += data_len_aligned + 8;
        }
    }
}
