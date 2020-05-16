// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Rust wrapper for `mmap(3)`.

use std::fs;
use std::io;
use std::ops::Deref;
use std::os::raw::{c_int, c_void};
use std::os::unix::io::AsRawFd;
use std::ptr;
use std::slice;

#[link(name="c")]
extern {
    fn mmap(addr: *mut c_void, len: usize, prot: c_int, flags: c_int, fildes: c_int, off: i64) -> *mut c_void;
    fn munmap(addr: *mut c_void, len: usize) -> c_int;
}

const PROT_READ: c_int = 1;
const MAP_PRIVATE: c_int = 2;
const MAP_FAILED: *mut c_void = !0 as *mut c_void; // !0 = -1.

pub struct Mmap {
    buffer: *const u8,
    length: usize,
}

impl Mmap {
    pub fn new(file: &fs::File, offset: i64, len: usize) -> io::Result<Mmap> {
        assert!(len > 0, "Cannot mmap an empty range.");
        let fd = file.as_raw_fd();

        let prot = PROT_READ;
        let flags = MAP_PRIVATE;

        let result = unsafe {
            mmap(ptr::null_mut(), len, prot, flags, fd, offset)
        };

        if result == MAP_FAILED {
            Err(io::Error::last_os_error())
        } else {
            let map = Mmap {
                buffer: result as *const u8,
                length: len,
            };
            Ok(map)
        }
    }
}

impl Drop for Mmap {
    fn drop(&mut self) {
        unsafe {
            // We ignore the return value; `munmap` should only fail due to
            // programmer error, not because of runtime errors. And even then,
            // if we fail to unmap the memory, it leaks, but Rust still prevents
            // us accessing it.
            munmap(self.buffer as *mut c_void, self.length);
        }
    }
}

impl Deref for Mmap {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.buffer, self.length) }
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use super::Mmap;

    #[test]
    fn mmap_maps_readme() {
        let readme = fs::File::open("../README.md").expect("Expected repo README.md to exist.");
        match Mmap::new(&readme, 0, 10) {
            Err(e) => panic!("{:?}", e),
            Ok(map) => assert_eq!(&map[..10], b"# Noblit\n\n"),
        }
    }
}
