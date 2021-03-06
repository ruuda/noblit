// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An in-memory `Store` implementation.

use std::io;

use binary;
use heap::{CidBytes, CidInt, Heap, HeapMut, SizedHeap};
use store::{PageId, PageSize, Store, StoreMut};

/// An in-memory page store, not backed by a file.
pub struct MemoryStore<Size: PageSize> {
    /// The backing buffer.
    ///
    /// TODO: Add a mechanism to ensure 8-byte alignment of the buffer. In
    /// practice it already is aligned correctly, but in theory it is not.
    buffer: Vec<u8>,

    /// The next unused page id.
    fresh: u64,

    /// Instance of page size traits.
    _size_sentinel: Size,
}

impl<Size: PageSize> MemoryStore<Size> {
    pub fn new() -> MemoryStore<Size> {
        MemoryStore {
            buffer: Vec::new(),
            fresh: 0,
            _size_sentinel: PageSize::new(),
        }
    }

    /// Use a pre-populated vector as in-memory store.
    ///
    /// The length of the vector must be a multiple of the page size.
    pub fn from_vec(buffer: Vec<u8>) -> MemoryStore<Size> {
        let n_pages = (buffer.len() / Size::SIZE) as u64;
        assert_eq!(
            n_pages as usize * Size::SIZE, buffer.len(),
            "Buffer length must be a multiple of the page size.",
        );
        MemoryStore {
            buffer: buffer,
            // n_pages-1 is the greatest used page id,
            // so n_pages is the next fresh one.
            fresh: n_pages,
            _size_sentinel: PageSize::new(),
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.buffer[..]
    }
}

impl<Size: PageSize> Store for MemoryStore<Size> {
    type Size = Size;

    fn get(&self, page: PageId) -> &[u8] {
        assert!(page != PageId::max(), "Should never get() PageId::max().");
        let begin = page.0 as usize * Size::SIZE;
        let end = (1 + page.0 as usize) * Size::SIZE;

        &self.buffer[begin..end]
    }

    fn dump(&self, out: &mut dyn io::Write) -> io::Result<()> {
        out.write_all(&self.buffer[..])
    }
}

impl<Size: PageSize> StoreMut for MemoryStore<Size> {
    fn write_page(&mut self, page: &[u8]) -> io::Result<PageId> {
        assert_eq!(page.len(), Size::SIZE);

        self.buffer.extend_from_slice(page);

        let pid = self.fresh;
        self.fresh += 1;

        Ok(PageId(pid))
    }
}

/// An in-memory constant heap, not backed by a file.
pub struct MemoryHeap {
    /// The backing buffer.
    buffer: Vec<u8>,
}

impl MemoryHeap {
    pub fn new() -> MemoryHeap {
        MemoryHeap {
            buffer: Vec::new(),
        }
    }

    /// Use a pre-populated vector as in-memory heap.
    pub fn from_vec(buffer: Vec<u8>) -> MemoryHeap {
        MemoryHeap {
            buffer: buffer
        }
    }
}

impl Heap for MemoryHeap {
    fn get_u64(&self, offset: CidInt) -> u64 {
        debug_assert_eq!(offset.0 % 8, 0, "Constant ids must be 8-byte aligned.");
        assert!(offset.0 + 8 <= self.buffer.len() as u64, "Constant id out of bounds.");

        let bytes = binary::slice_8(&self.buffer[offset.0 as usize..]);
        binary::u64_from_le_bytes(bytes)
    }

    /// Retrieve a byte string constant.
    fn get_bytes(&self, offset: CidBytes) -> &[u8] {
        let len = self.get_u64(CidInt(offset.0));

        // We must fit an 8-byte length, and at least 8 bytes of data (otherwise
        // the data could be stored inline in a value; it would not need to be
        // on the heap).
        debug_assert!(len >= 8, "Encountered small value on the heap.");

        assert!(offset.0 + 8 + len <= self.buffer.len() as u64, "Constant bytestring out of bounds.");
        &self.buffer[offset.0 as usize + 8..offset.0 as usize + 8 + len as usize]
    }
}

impl HeapMut for MemoryHeap {
    fn append_u64(&mut self, value: u64) -> io::Result<CidInt> {
        debug_assert_eq!(self.buffer.len() % 8, 0, "Buffer should remain 8-byte aligned.");
        let offset = self.buffer.len();

        let bytes = binary::u64_to_le_bytes(value);
        self.buffer.extend_from_slice(&bytes[..]);

        Ok(CidInt(offset as u64))
    }

    /// Retrieve a byte string constant.
    fn append_bytes(&mut self, value: &[u8]) -> io::Result<CidBytes> {
        debug_assert!(value.len() >= 8, "Store small values inline, not on the heap.");

        // Reserve enough space for the length prefix and value.
        // Mostly to not fool the fuzzer by making some insert sizes special.
        self.buffer.reserve(8 + value.len());

        let id = self.append_u64(value.len() as u64)?;
        self.buffer.extend_from_slice(value);

        // Zero-pad to align the buffer to 8 bytes again.
        while self.buffer.len() % 8 > 0 {
            self.buffer.push(0);
        }

        Ok(CidBytes(id.0))
    }
}

impl SizedHeap for MemoryHeap {
    fn len(&self) -> u64 {
        self.buffer.len() as u64
    }

    fn dump(&self, out: &mut dyn io::Write) -> io::Result<()> {
        out.write_all(&self.buffer[..])
    }
}

#[cfg(test)]
mod test {
    use heap::{Heap, HeapMut};
    use super::{MemoryHeap};

    #[test]
    fn heap_append_get_u64_roundtrips() {
        use std::u64;
        let mut heap = MemoryHeap::new();
        let mut ids = Vec::new();

        // First store some integers.
        for i in 0..523 {
            let p = i << 32;
            let q = u64::MAX - p * 17;
            ids.push(heap.append_u64(p).unwrap());
            ids.push(heap.append_u64(q).unwrap());
        }

        assert_eq!(heap.buffer.len(), 523 * 2  * 8);

        // Then read them back.
        for i in (0..523).rev() {
            let p = i << 32;
            let q = u64::MAX - p * 17;
            let q_id = ids.pop().unwrap();
            let p_id = ids.pop().unwrap();
            assert_eq!(heap.get_u64(p_id), p);
            assert_eq!(heap.get_u64(q_id), q);
        }
    }

    #[test]
    fn heap_append_get_bytes_roundtrips() {
        let messages = [
            &b"Roy Batty"[..],
            &b"Alden Tyrell"[..],
            &b"Rick Deckard"[..],
        ];
        let mut heap = MemoryHeap::new();
        let mut ids = Vec::new();

        // First store the messages.
        for m in messages.iter() {
            ids.push(heap.append_bytes(m).unwrap());
        }

        // Then read them back.
        for (&m_id, &m) in ids.iter().zip(messages.iter()) {
            assert_eq!(heap.get_bytes(m_id), m);
        }
    }

    #[test]
    fn heap_append_get_interleaved_roundtrips() {
        use std::iter;
        let mut heap = MemoryHeap::new();
        let mut ids_u64 = Vec::new();
        let mut ids_bytes = Vec::new();

        // Interleave some integers and variable-length strings.
        for i in 8..256 {
            let message: Vec<u8> = iter::repeat(1).take(i as usize).collect();
            ids_u64.push(heap.append_u64(i).unwrap());
            ids_bytes.push(heap.append_bytes(&message[..]).unwrap());
        }

        // Then read them back.
        while ids_u64.len() > 0 {
            let id_bytes = ids_bytes.pop().unwrap();
            let id_u64 = ids_u64.pop().unwrap();
            let len = heap.get_u64(id_u64);
            let message = heap.get_bytes(id_bytes);
            assert_eq!(message.len() as u64, len);
            assert_eq!(message[0], 1);
        }
    }
}
