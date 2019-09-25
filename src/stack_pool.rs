// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Contains a pool for transient constants.

use pool::{CidBytes, CidInt, Pool};

/// A stack of transient constants, on top of a (persistent) append-only pool.
///
/// A query may contain constants. In order to compare those against database
/// values, the values that are too large to be inlined, need to be backed by
/// external storage. We don't want to store every value in the database's pool,
/// because there may be zero datoms that reference it. Instead, we can create a
/// temporary constant on this stack, use it in the query, and then pop it.
///
/// Because ids of persistent constants are aligned to 8 bytes, we are free to
/// use non-aligned ids for temporary constants, and we can tell to which class
/// a constant belongs from its id.
pub struct StackPool<P: Pool> {
    /// The underlying append-only pool.
    pool: P,

    /// The stack of temporary 64-bit unsigned integer constants.
    stack_u64: Vec<u64>,

    /// The stack of temporary byte string constants.
    stack_bytes: Vec<Box<[u8]>>,
}

impl<P: Pool> StackPool<P> {
    pub fn new(inner: P) -> StackPool<P> {
        StackPool {
            pool: inner,
            stack_u64: Vec::new(),
            stack_bytes: Vec::new(),
        }
    }

    pub fn push_u64(&mut self, value: u64) -> CidInt {
        let index = self.stack_u64.len();
        self.stack_u64.push(value);
        // Constant ids of the underlying pool ale always aligned to 8 bytes,
        // so make sure that ours are not, so we can always keep the two apart.
        // TODO: We could include a generation number that increments when the
        // stack height falls to zero. That makes it harder to accidentally
        // re-use values across generations.
        CidInt(index as u64 * 8 + 1)
    }

    pub fn push_bytes(&mut self, value: Box<[u8]>) -> CidBytes {
        let index = self.stack_bytes.len();
        self.stack_bytes.push(value);
        // Constant ids of the underlying pool are always aligned to 8 bytes,
        // so make sure that ours are not, so we can always keep the two apart.
        CidBytes(index as u64 * 8 + 1)
    }

    pub fn pop_u64(&mut self, cid: CidInt) {
        let index = (cid.0 - 1) / 8;
        self.stack_u64.pop();
        debug_assert_eq!(
            index, self.stack_u64.len() as u64,
            "Value to be popped must be the value on top of the u64 stack.",
        );
    }

    pub fn pop_bytes(&mut self, cid: CidBytes) {
        let index = (cid.0 - 1) / 8;
        self.stack_bytes.pop();
        debug_assert_eq!(
            index, self.stack_bytes.len() as u64,
            "Value to be popped must be the value on top of the byte string stack.",
        );
    }
}

impl<P: Pool> Pool for StackPool<P> {
    fn get_u64(&self, offset: CidInt) -> u64 {
        // If the id is aligned, it comes from the underlying pool,
        // otherwise it was ours.
        let index = match offset.0 & 7 {
            0 => return self.pool.get_u64(offset),
            1 => (offset.0 - 1) / 8,
            _ => unreachable!("Invalid integer constant id."),
        };
        self.stack_u64[index as usize]
    }

    fn get_bytes(&self, offset: CidBytes) -> &[u8] {
        // If the id is aligned, it comes from the underlying pool,
        // otherwise it was ours.
        let index = match offset.0 & 7 {
            0 => return self.pool.get_bytes(offset),
            1 => (offset.0 - 1) / 8,
            _ => unreachable!("Invalid byte string constant id: 0x{:x}", offset.0),
        };
        &self.stack_bytes[index as usize]
    }
}
