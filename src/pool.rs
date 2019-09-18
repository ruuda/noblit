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
/// Offsets are aligned to 8 bytes. For 64-bit unsigned integer constants, the
/// pool stores the 8-byte integer at that offset.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CidInt(pub u64);

/// An id for a byte string constant.
///
/// A constant id is the offset of a constant from the start of the pool.
/// Offsets are aligned to 8 bytes. For byte strings, the pool stores an
/// unsigned 64-bit length at the offset, followed by the data itself.
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
