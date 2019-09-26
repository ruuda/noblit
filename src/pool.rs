// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Thin abstractions for a bump-pointer pool of constants.

use std::io;

/// An id for a 64-bit unsigned integer constant.
///
/// A constant id is the offset of a constant from the start of the pool.
/// Offsets of persistent constants should be aligned to 8 bytes. For 64-bit
/// unsigned integer constants, the pool stores the 8-byte integer at that
/// offset.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CidInt(pub u64);

/// An id for a byte string constant.
///
/// A constant id is the offset of a constant from the start of the pool.
/// Offsets of persistent constants should be aligned to 8 bytes. For byte
/// strings, the pool stores an unsigned 64-bit length at the offset, followed
/// by the data itself.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CidBytes(pub u64);

/// A constant pool that can be read from.
pub trait Pool {
    /// Retrieve a 64-bit unsigned integer constant.
    fn get_u64(&self, offset: CidInt) -> u64;

    /// Retrieve a byte string constant.
    fn get_bytes(&self, offset: CidBytes) -> &[u8];
}

/// A constant pool to which new constants can be appended.
pub trait PoolMut: Pool {
    /// Store a 64-bit unsigned integer constant.
    fn append_u64(&mut self, value: u64) -> io::Result<CidInt>;

    /// Store a byte string constant.
    fn append_bytes(&mut self, value: &[u8]) -> io::Result<CidBytes>;
}

/// A pool that exposes the number of bytes it stores.
pub trait SizedPool: Pool {
    /// The size of the data in the pool, in bytes.
    fn len(&self) -> u64;
}

impl<'a, T: Pool> Pool for &'a T {
    fn get_u64(&self, offset: CidInt) -> u64 { (**self).get_u64(offset) }
    fn get_bytes(&self, offset: CidBytes) -> &[u8] { (**self).get_bytes(offset) }
}

impl<'a, T: Pool> Pool for &'a mut T {
    fn get_u64(&self, offset: CidInt) -> u64 { (**self).get_u64(offset) }
    fn get_bytes(&self, offset: CidBytes) -> &[u8] { (**self).get_bytes(offset) }
}

impl<'a, T: SizedPool> SizedPool for &'a T {
    fn len(&self) -> u64 { (**self).len() }
}

impl<'a, T: SizedPool> SizedPool for &'a mut T {
    fn len(&self) -> u64 { (**self).len() }
}

/// Assert that the pool is well-formed, that all invariants hold.
///
/// This is used in tests and during fuzzing.
pub fn check_invariants<P: SizedPool>(pool: P) {
    // The maximum value that can still be stored inline. Integers on the pool
    // should be larger than this, otherwise they should have been stored
    // inline. Consequently, integers smaller than this will be length prefixes.
    let max_inline_u64 = 0x4000_0000_0000_0000 - 1;

    let mut off = 0;
    loop {
        if off >= pool.len() {
            assert_eq!(off, pool.len(), "Pool size must be a multiple of 8.");
            break
        }

        let value = pool.get_u64(CidInt(off));
        if value > max_inline_u64 {
            // The value must have been an 8-byte unsigned int, move on.
            off += 8;
        } else {
            // The value was a length prefix. Skip over the data (aligned to 8
            // bytes).
            let data_len = value;
            let data_len_aligned = (data_len + 7) / 8 * 8;
            assert!(
                off + 8 + data_len_aligned <= pool.len(),
                "Byte string of length {} at offset {} does not fit pool of size {}.",
                off + 8, data_len, pool.len()
            );
            off += data_len_aligned + 8;
        }
    }
}
