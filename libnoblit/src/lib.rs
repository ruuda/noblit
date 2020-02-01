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

use noblit::disk;
use noblit::database;
use noblit::memory_store::{MemoryHeap, MemoryStore};
use noblit::store::PageSize4096;

type MemoryStore4096 = MemoryStore<PageSize4096>;

/// Wraps a `noblit::Database` for use through the C API.
pub enum Database {
    /// An in-memory database (mutable).
    InMemory(database::Database<MemoryStore4096, MemoryHeap>),
    // TODO: In the future we can add variants for immutable on-disk
    // and mutable on-disk here. FFI functions then have to dispatch on it,
    // and report an error for unsupported cases.
}

fn noblit_db_read_packed_impl(fname: &OsStr) -> Box<Database> {
    // TODO: Proper error handling.
    let f = fs::File::open(fname).expect("Failed to open database file.");
    let db = disk::read_packed(&mut io::BufReader::new(f)).expect("Failed to read database.");
    let wrapper = Database::InMemory(db);
    Box::new(wrapper)
}

#[no_mangle]
pub unsafe extern fn noblit_db_free(db: *mut Database) {
    // Take back ownership of the box, and drop that box when it goes out of scope.
    let _ = Box::from_raw(db);
}

#[no_mangle]
pub unsafe extern fn noblit_db_read_packed(fname: *const u8, fname_len: usize) -> *mut Database {
    let fname_bytes = slice::from_raw_parts(fname, fname_len);
    let fname_osstr = OsStr::from_bytes(fname_bytes);
    let db_wrapper = noblit_db_read_packed_impl(fname_osstr);
    Box::into_raw(db_wrapper)
}
