// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

/// Support functionality for dealing with binary data.
///
/// This is used for the query parser, but also for the fuzzers (TODO).

use std::io;

/// A reader that remembers its offset.
pub struct Cursor {
    reader: Box<dyn io::Read>,
    offset: usize,
}

impl Cursor {
    pub fn new<R: 'static + io::Read>(reader: R) -> Cursor {
        Cursor {
            reader: Box::new(reader),
            offset: 0,
        }
    }

    pub fn take_u8(&mut self) -> io::Result<u8> {
        let mut buffer = [0];
        self.reader.read_exact(&mut buffer[..])?;
        self.offset += 1;
        Ok(buffer[0])
    }

    pub fn take_u16_be(&mut self) -> io::Result<u16> {
        let mut buffer = [0, 0];
        self.reader.read_exact(&mut buffer[..])?;
        self.offset += 2;
        // On Rust 1.32, could use from_be_bytes instead.
        let v = 0
            | (buffer[0] as u16) << 8
            | (buffer[1] as u16) << 0
            ;
        Ok(v)
    }

    pub fn take_u16_le(&mut self) -> io::Result<u16> {
        let mut buffer = [0, 0];
        self.reader.read_exact(&mut buffer[..])?;
        self.offset += 2;
        // On Rust 1.32, could use from_le_bytes instead.
        let v = 0
            | (buffer[0] as u16) << 0
            | (buffer[1] as u16) << 8
            ;
        Ok(v)
    }

    pub fn take_u32_be(&mut self) -> io::Result<u32> {
        let mut buffer = [0, 0, 0, 0];
        self.reader.read_exact(&mut buffer[..])?;
        self.offset += 4;
        // On Rust 1.32, could use from_be_bytes instead.
        let v = 0
            | (buffer[0] as u32) << 24
            | (buffer[1] as u32) << 16
            | (buffer[2] as u32) << 8
            | (buffer[3] as u32) << 0
            ;
        Ok(v)
    }

    pub fn take_u64_le(&mut self) -> io::Result<u64> {
        let mut buffer = [0, 0, 0, 0, 0, 0, 0, 0];
        self.reader.read_exact(&mut buffer[..])?;
        self.offset += 4;
        // On Rust 1.32, could use from_be_bytes instead.
        let v = 0
            | (buffer[0] as u64) << 0
            | (buffer[1] as u64) << 8
            | (buffer[2] as u64) << 16
            | (buffer[3] as u64) << 24
            | (buffer[4] as u64) << 32
            | (buffer[5] as u64) << 40
            | (buffer[6] as u64) << 48
            | (buffer[7] as u64) << 56
            ;
        Ok(v)
    }

    pub fn take_bytes(&mut self, len: usize) -> io::Result<Vec<u8>> {
        use std::iter;
        let mut buffer: Vec<u8> = iter::repeat(0).take(len).collect();
        self.reader.read_exact(&mut buffer[..])?;
        self.offset += len;
        Ok(buffer)
    }

    pub fn take_utf8(&mut self, len: usize) -> io::Result<String> {
        // Take the bytes, parse as UTF-8, replace errors with CursorError.
        let err = io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF-8.");
        let bytes = self.take_bytes(len as usize)?;
        String::from_utf8(bytes).map_err(|_| err)
    }
}

