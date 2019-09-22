// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An in-memory `Store` implementation.

use std::io;

use pool::{CidBytes, CidInt, Pool, PoolMut};
use store::{PageId, PageSize, Store, StoreMut};

/// An in-memory page store, not backed by a file.
pub struct MemoryStore<Size: PageSize> {
    /// The backing buffer.
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

/// An in-memory constant pool, not backed by a file.
pub struct MemoryPool {
    /// The backing buffer.
    buffer: Vec<u8>,
}

impl MemoryPool {
    pub fn new() -> MemoryPool {
        MemoryPool {
            buffer: Vec::new(),
        }
    }
}

impl Pool for MemoryPool {
    fn get_u64(&self, offset: CidInt) -> u64 {
        debug_assert_eq!(offset.0 % 8, 0, "Constant ids must be 8-byte aligned.");
        assert!(offset.0 + 8 <= self.buffer.len() as u64, "Constant id out of bounds.");

        let bytes = &self.buffer[offset.0 as usize..offset.0 as usize + 8];

        0
            | (bytes[0] as u64) << 56
            | (bytes[1] as u64) << 48
            | (bytes[2] as u64) << 40
            | (bytes[3] as u64) << 32
            | (bytes[4] as u64) << 24
            | (bytes[5] as u64) << 16
            | (bytes[6] as u64) << 8
            | (bytes[7] as u64)
    }

    /// Retrieve a byte string constant.
    fn get_bytes(&self, offset: CidBytes) -> &[u8] {
        let len = self.get_u64(CidInt(offset.0));

        // We must fit an 8-byte length, and at least 8 bytes of data (otherwise
        // the data could be stored inline in a value; it would not need to be
        // in the pool).
        debug_assert!(len >= 8, "Encountered small value in the pool.");

        assert!(offset.0 + 8 + len < self.buffer.len() as u64, "Constant bytestring out of bounds.");
        &self.buffer[offset.0 as usize + 8..offset.0 as usize + 8 + len as usize]
    }
}

impl PoolMut for MemoryPool {
    fn append_u64(&mut self, value: u64) -> io::Result<CidInt> {
        debug_assert_eq!(self.buffer.len() % 8, 0, "Buffer should remain 8-byte aligned.");
        let offset = self.buffer.len();

        let bytes = [
            (value >> 56) as u8,
            (value >> 48) as u8,
            (value >> 40) as u8,
            (value >> 32) as u8,
            (value >> 24) as u8,
            (value >> 16) as u8,
            (value >> 8) as u8,
            (value >> 0) as u8,
        ];
        self.buffer.extend_from_slice(&bytes[..]);

        Ok(CidInt(offset as u64))
    }

    /// Retrieve a byte string constant.
    fn append_bytes(&mut self, value: &[u8]) -> io::Result<CidBytes> {
        debug_assert!(value.len() >= 8, "Store small values inline, not in the pool.");

        let id = self.append_u64(value.len() as u64)?;
        self.buffer.extend_from_slice(value);

        // Zero-pad to align the buffer to 8 bytes again.
        while self.buffer.len() % 8 > 0 {
            self.buffer.push(0);
        }

        Ok(CidBytes(id.0))
    }
}

#[cfg(test)]
mod test {
    use pool::{Pool, PoolMut};
    use super::{MemoryPool};

    #[test]
    fn pool_append_get_u64_roundtrips() {
        use std::u64;
        let mut pool = MemoryPool::new();
        let mut ids = Vec::new();

        // First store some integers.
        for i in 0..523 {
            let p = i << 32;
            let q = u64::MAX - p * 17;
            ids.push(pool.append_u64(p).unwrap());
            ids.push(pool.append_u64(q).unwrap());
        }

        assert_eq!(pool.buffer.len(), 523 * 2  * 8);

        // Then read them back.
        for i in (0..523).rev() {
            let p = i << 32;
            let q = u64::MAX - p * 17;
            let q_id = ids.pop().unwrap();
            let p_id = ids.pop().unwrap();
            assert_eq!(pool.get_u64(p_id), p);
            assert_eq!(pool.get_u64(q_id), q);
        }
    }

    #[test]
    fn pool_append_get_bytes_roundtrips() {
        let messages = [
            &b"Roy Batty"[..],
            &b"Alden Tyrell"[..],
            &b"Rick Deckard"[..],
        ];
        let mut pool = MemoryPool::new();
        let mut ids = Vec::new();

        // First store the messages.
        for m in messages.iter() {
            ids.push(pool.append_bytes(m).unwrap());
        }

        // Then read them back.
        for (&m_id, &m) in ids.iter().zip(messages.iter()) {
            assert_eq!(pool.get_bytes(m_id), m);
        }
    }

    #[test]
    fn pool_append_get_interleaved_roundtrips() {
        use std::iter;
        let mut pool = MemoryPool::new();
        let mut ids_u64 = Vec::new();
        let mut ids_bytes = Vec::new();

        // Interleave some integers and variable-length strings.
        for i in 8..256 {
            let message: Vec<u8> = iter::repeat(1).take(i as usize).collect();
            ids_u64.push(pool.append_u64(i).unwrap());
            ids_bytes.push(pool.append_bytes(&message[..]).unwrap());
        }

        // Then read them back.
        while ids_u64.len() > 0 {
            let id_bytes = ids_bytes.pop().unwrap();
            let id_u64 = ids_u64.pop().unwrap();
            let len = pool.get_u64(id_u64);
            let message = pool.get_bytes(id_bytes);
            assert_eq!(message.len() as u64, len);
            assert_eq!(message[0], 1);
        }
    }
}
