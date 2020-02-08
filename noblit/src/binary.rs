// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Support functionality for dealing with binary data.
//!
//! This is used for the query parser, but also for the fuzzers (TODO).

use std::io;

#[inline]
pub fn u16_from_le_bytes(buffer: [u8; 2]) -> u16 {
    // On Rust 1.32, could use u16::from_le_bytes instead.
    0
        | (buffer[1] as u16) << 8
        | (buffer[0] as u16) << 0
}

#[inline]
pub fn u64_from_le_bytes(buffer: [u8; 8]) -> u64 {
    // On Rust 1.32, could use u64::from_le_bytes instead.
    0
        | (buffer[7] as u64) << 56
        | (buffer[6] as u64) << 48
        | (buffer[5] as u64) << 40
        | (buffer[4] as u64) << 32
        | (buffer[3] as u64) << 24
        | (buffer[2] as u64) << 16
        | (buffer[1] as u64) << 8
        | (buffer[0] as u64) << 0
}

#[inline]
pub fn u16_to_le_bytes(x: u16) -> [u8; 2] {
    // On Rust 1.32, could use u16::to_le_bytes instead.
    [
        (x >>  0) as u8,
        (x >>  8) as u8,
    ]
}

#[inline]
pub fn u64_to_le_bytes(x: u64) -> [u8; 8] {
    // On Rust 1.32, could use u64::to_le_bytes instead.
    [
        (x >>  0) as u8,
        (x >>  8) as u8,
        (x >> 16) as u8,
        (x >> 24) as u8,
        (x >> 32) as u8,
        (x >> 40) as u8,
        (x >> 48) as u8,
        (x >> 56) as u8,
    ]
}

/// Take the first 2 bytes out of a slice.
///
/// See also `slice_8`.
#[inline(always)]
pub fn slice_2(buffer: &[u8]) -> [u8; 2] {
    [
        buffer[0],
        buffer[1],
    ]
}

/// Take the first 8 bytes out of a slice.
///
/// This function is a bit silly, but it is needed sometimes, because indexing
/// into a fixed-size slice produces a dynamically sized slice, even if the
/// indexes are compile-time constants. So we sometimes need this to e.g. slice
/// a `[u8; 8]` out of a `[u8; 40]`. I hope that LLVM will see through it and
/// optimize it away, but I am not aware of a way to guarantee that.
#[inline(always)]
pub fn slice_8(buffer: &[u8]) -> [u8; 8] {
    [
        buffer[0],
        buffer[1],
        buffer[2],
        buffer[3],
        buffer[4],
        buffer[5],
        buffer[6],
        buffer[7],
    ]
}

/// Newtype wrapper for `dyn io::Read`.
pub struct Cursor {
    reader: Box<dyn io::Read>,
}

impl Cursor {
    pub fn new<R: 'static + io::Read>(reader: R) -> Cursor {
        Cursor {
            reader: Box::new(reader),
        }
    }

    pub fn take_u8(&mut self) -> io::Result<u8> {
        let mut buffer = [0];
        self.reader.read_exact(&mut buffer[..])?;
        Ok(buffer[0])
    }

    pub fn take_u16_le(&mut self) -> io::Result<u16> {
        let mut buffer = [0, 0];
        self.reader.read_exact(&mut buffer[..])?;
        Ok(u16_from_le_bytes(buffer))
    }

    pub fn take_u64_le(&mut self) -> io::Result<u64> {
        let mut buffer = [0, 0, 0, 0, 0, 0, 0, 0];
        self.reader.read_exact(&mut buffer[..])?;
        Ok(u64_from_le_bytes(buffer))
    }

    pub fn take_bytes(&mut self, len: usize) -> io::Result<Vec<u8>> {
        use std::iter;
        let mut buffer: Vec<u8> = iter::repeat(0).take(len).collect();
        self.reader.read_exact(&mut buffer[..])?;
        Ok(buffer)
    }

    pub fn take_utf8(&mut self, len: usize) -> io::Result<String> {
        // Take the bytes, parse as UTF-8, replace errors with CursorError.
        let err = io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF-8.");
        let bytes = self.take_bytes(len as usize)?;
        String::from_utf8(bytes).map_err(|_| err)
    }
}

#[cfg(test)]
mod test {
    use binary::{u16_from_le_bytes, u16_to_le_bytes, u64_from_le_bytes, u64_to_le_bytes};

    #[test]
    fn u16_to_from_le_bytes_roundtrips() {
        // Test 5 powers of 9 that span almost the full u16 range.
        let mut v = 1;
        for _ in 0..5 {
            v = v * 9;
            assert_eq!(v, u16_from_le_bytes(u16_to_le_bytes(v)));
        }
    }

    #[test]
    fn u64_to_from_le_bytes_roundtrips() {
        // Test 18 powers of 11 that span almost the full u64 range.
        let mut v = 1;
        for _ in 0..18 {
            v = v * 11;
            assert_eq!(v, u64_from_le_bytes(u64_to_le_bytes(v)));
        }
    }
}
