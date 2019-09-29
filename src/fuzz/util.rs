// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

/// Support functionality for fuzz tests.

/// Print, except when fuzzing.
///
/// This is useful for printf-style debugging of fuzz artifacts. During fuzzing,
/// this macro is a no-op, to keep the output clean and the fuzzer fast. But in
/// the `inspect_fuzz_artifact` binary, these prints leave a trace of how the
/// test sample was interpreted.
#[macro_export]
macro_rules! dprintln {
    () => (#[cfg(not(fuzzing))] println!());
    ($($arg:tt)*) => (#[cfg(not(fuzzing))] println!($($arg)*));
}

/// Evaluate a closure on byte slices of various lengths, up to 2^16.
pub fn for_slices_u16<'a, F>(data: &'a [u8], mut f: F) where F: FnMut(&'a [u8]) -> bool {
    let mut left = data;

    while left.len() > 2 {
        // Read a 16-bit length prefix.
        let len = (left[0] as usize) << 8 | (left[1] as usize);

        // Stop on invalid lengths, rather than capping the slice. This improves
        // the chances of byte strings combining in interesting ways.
        if len > left.len() - 2 { break }

        if !f(&left[2..2 + len]) { break }
        left = &left[2 + len..];
    }
}

// TODO: Use binary::Cursor instead.
pub struct Cursor<'a> {
    data: &'a [u8],
    offset: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(data: &'a [u8]) -> Cursor<'a> {
        Cursor {
            data: data,
            offset: 0,
        }
    }

    pub fn take_u8(&mut self) -> Option<u8> {
        if self.offset + 1 <= self.data.len() {
            let v = self.data[self.offset];
            self.offset += 1;
            Some(v)
        } else {
            None
        }
    }

    pub fn take_u16(&mut self) -> Option<u16> {
        if self.offset + 2 <= self.data.len() {
            let v = 0
                | (self.data[self.offset + 0] as u16) << 8
                | (self.data[self.offset + 1] as u16) << 0;
            self.offset += 2;
            Some(v)
        } else {
            None
        }
    }

    pub fn take_u32(&mut self) -> Option<u32> {
        if self.offset + 4 <= self.data.len() {
            let v = 0
                | (self.data[self.offset + 0] as u32) << 24
                | (self.data[self.offset + 1] as u32) << 16
                | (self.data[self.offset + 2] as u32) << 8
                | (self.data[self.offset + 3] as u32) << 0;
            self.offset += 4;
            Some(v)
        } else {
            None
        }
    }

    pub fn take_slice(&mut self, len: usize) -> Option<&'a [u8]> {
        if self.offset + len <= self.data.len() {
            let v = &self.data[self.offset..self.offset + len];
            self.offset += len;
            Some(v)
        } else {
            None
        }
    }
}
