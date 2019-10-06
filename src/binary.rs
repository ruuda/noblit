// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

/// Support functionality for dealing with binary data.
///
/// This is used for the query parser, but also for the fuzzers (TODO).

/// A slice of bytes, with an offset into it.
pub struct Cursor<'a> {
    data: &'a [u8],
    offset: usize,
}

#[derive(Debug)]
pub struct CursorError {
    pub offset: usize,
    pub message: &'static str,
}

impl<'a> Cursor<'a> {
    pub fn new(data: &'a [u8]) -> Cursor<'a> {
        Cursor {
            data: data,
            offset: 0,
        }
    }

    /// Generate an error at the current offset.
    pub fn error(&self, message: &'static str) -> CursorError {
        CursorError {
            offset: self.offset,
            message: message,
        }
    }

    pub fn peek_u8(&mut self) -> Option<u8> {
        if self.offset + 1 <= self.data.len() {
            let v = self.data[self.offset];
            Some(v)
        } else {
            None
        }
    }

    pub fn take_u8(&mut self) -> Result<u8, CursorError> {
        if self.offset + 1 <= self.data.len() {
            let v = self.data[self.offset];
            self.offset += 1;
            Ok(v)
        } else {
            Err(self.error("Expected byte."))
        }
    }

    pub fn take_u16_be(&mut self) -> Result<u16, CursorError> {
        if self.offset + 2 <= self.data.len() {
            let v = 0
                | (self.data[self.offset + 0] as u16) << 8
                | (self.data[self.offset + 1] as u16) << 0;
            self.offset += 2;
            Ok(v)
        } else {
            Err(self.error("Expected 16-bit big endian integer."))
        }
    }

    pub fn take_u16_le(&mut self) -> Result<u16, CursorError> {
        if self.offset + 2 <= self.data.len() {
            let v = 0
                | (self.data[self.offset + 0] as u16) << 0
                | (self.data[self.offset + 1] as u16) << 8;
            self.offset += 2;
            Ok(v)
        } else {
            Err(self.error("Expected 16-bit little endian integer."))
        }
    }

    pub fn take_u32_be(&mut self) -> Result<u32, CursorError> {
        if self.offset + 4 <= self.data.len() {
            let v = 0
                | (self.data[self.offset + 0] as u32) << 24
                | (self.data[self.offset + 1] as u32) << 16
                | (self.data[self.offset + 2] as u32) << 8
                | (self.data[self.offset + 3] as u32) << 0;
            self.offset += 4;
            Ok(v)
        } else {
            Err(self.error("Expected 32-bit big endian integer."))
        }
    }

    pub fn take_u64_le(&mut self) -> Result<u64, CursorError> {
        if self.offset + 8 <= self.data.len() {
            let v = 0
                | (self.data[self.offset + 0] as u64) << 0
                | (self.data[self.offset + 1] as u64) << 8
                | (self.data[self.offset + 2] as u64) << 16
                | (self.data[self.offset + 3] as u64) << 24
                | (self.data[self.offset + 4] as u64) << 32
                | (self.data[self.offset + 5] as u64) << 40
                | (self.data[self.offset + 6] as u64) << 48
                | (self.data[self.offset + 7] as u64) << 56;
            self.offset += 8;
            Ok(v)
        } else {
            Err(self.error("Expected 64-bit little endian integer."))
        }
    }

    pub fn take_slice(&mut self, len: usize) -> Result<&'a [u8], CursorError> {
        if self.offset + len <= self.data.len() {
            let v = &self.data[self.offset..self.offset + len];
            self.offset += len;
            Ok(v)
        } else {
            Err(self.error("Requested slice out of bounds."))
        }
    }

    pub fn take_utf8(&mut self, len: usize) -> Result<&'a str, CursorError> {
        // Take the bytes, parse as UTF-8, replace errors with CursorError.
        use std::str;
        let err = self.error("Invalid UTF-8 string.");
        let bytes = self.take_slice(len as usize)?;
        str::from_utf8(bytes).map_err(|_| err)
    }
}

