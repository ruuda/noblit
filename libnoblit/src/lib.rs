// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

use std::ffi::OsStr;
use std::fs;
use std::io;
use std::os::unix::ffi::OsStrExt;
use std::slice;
use std::string::ToString;

use noblit::database;
use noblit::disk;
use noblit::error::Error;
use noblit::memory_store::{MemoryHeap, MemoryStore};
use noblit::store::PageSize4096;

type MemoryStore4096 = MemoryStore<PageSize4096>;

/// Fixes the type parameters of `noblit::Database` to a few predetermined cases.
enum FixedDatabase {
    /// An in-memory database (mutable).
    InMemory(database::Database<MemoryStore4096, MemoryHeap>),
    // TODO: In the future we can add variants for immutable on-disk
    // and mutable on-disk here. FFI functions then have to dispatch on it,
    // and report an error for unsupported cases.
}

/// Wraps a `noblit::Database` for use through the C API.
///
/// This struct is referred to as `noblit_t` in the C API reference.
pub struct Context {
    /// The database to manage.
    db: FixedDatabase,

    /// If the last call returned an error, this contains the formatted message.
    last_error: Option<String>,
}

impl Context {
    /// Set `self.error` to the error message, return the associated error code.
    fn observe_error(&mut self, err: Error) -> u32 {
        self.last_error = Some(err.to_string());
        match err {
            Error::IoError(..) => 1,
        }
    }
}

#[repr(C)]
pub struct noblit_slice_t {
    data: *const u8,
    len: usize,
}

/// Unsafely cast a byte slice to a struct exposable through the FFI.
unsafe fn as_slice(data: &[u8]) -> noblit_slice_t {
    noblit_slice_t {
        data: data.as_ptr(),
        len: data.len(),
    }
}

#[no_mangle]
pub unsafe extern fn noblit_get_last_error(db: *const Context) -> noblit_slice_t {
    match (*db).last_error {
        None => as_slice(b""),
        Some(ref message) => as_slice(message.as_bytes()),
    }
}

fn noblit_db_read_packed_impl(fname: &OsStr) -> Box<Context> {
    // TODO: Proper error handling.
    let f = fs::File::open(fname).expect("Failed to open database file.");
    let db = disk::read_packed(&mut io::BufReader::new(f)).expect("Failed to read database.");
    let context = Context {
        db: FixedDatabase::InMemory(db),
        last_error: None,
    };
    Box::new(context)
}

#[no_mangle]
pub unsafe extern fn noblit_db_free(db: *mut Context) {
    // Take back ownership of the box, and drop that box when it goes out of scope.
    let _ = Box::from_raw(db);
}

#[no_mangle]
pub unsafe extern fn noblit_db_read_packed(fname: *const u8, fname_len: usize) -> *mut Context {
    let fname_bytes = slice::from_raw_parts(fname, fname_len);
    let fname_osstr = OsStr::from_bytes(fname_bytes);
    let db_wrapper = noblit_db_read_packed_impl(fname_osstr);
    Box::into_raw(db_wrapper)
}
