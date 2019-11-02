// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Contains a heap for transient constants.

use heap::{CidBytes, CidInt, Heap};

/// A stack of transient constants, on top of a (persistent) append-only heap.
///
/// A query may contain constants. In order to compare those against database
/// values, the values that are too large to be inlined, need to be backed by
/// external storage. We don't want to store every value in the database's heap,
/// because there may be zero datoms that reference it. Instead, we can create a
/// temporary constant on this stack, use it in the query, and then pop it.
///
/// Because ids of persistent constants are aligned to 8 bytes, we are free to
/// use non-aligned ids for temporary constants, and we can tell to which class
/// a constant belongs from its id.
pub struct TempHeap<H: Heap> {
    /// The underlying append-only heap.
    heap: H,

    /// The stack of temporary 64-bit unsigned integer constants.
    stack_u64: Vec<u64>,

    /// The stack of temporary byte string constants.
    stack_bytes: Vec<Box<[u8]>>,
}

/// A stack of transient constants that used to live on the temp heap.
///
/// The temp heap is used to track temporaries during queries. At the end of a
/// mutation, when it is known which datoms to insert, the temporaries need to
/// be persisted to the heap as persistent values. This struct eases that
/// transition: it allows the values to escape, while releasing the read-only
/// borrow of the underlying heap, so it can be mutated again in order to
/// persist the values.
pub struct Temporaries {
    /// The stack of temporary 64-bit unsigned integer constants.
    stack_u64: Vec<u64>,

    /// The stack of temporary byte string constants.
    stack_bytes: Vec<Box<[u8]>>,
}

impl<H: Heap> TempHeap<H> {
    pub fn new(inner: H) -> TempHeap<H> {
        TempHeap {
            heap: inner,
            stack_u64: Vec::new(),
            stack_bytes: Vec::new(),
        }
    }

    pub fn push_u64(&mut self, value: u64) -> CidInt {
        let index = self.stack_u64.len();
        self.stack_u64.push(value);
        // Constant ids of the underlying heap ale always aligned to 8 bytes,
        // so make sure that ours are not, so we can always keep the two apart.
        // TODO: We could include a generation number that increments when the
        // stack height falls to zero. That makes it harder to accidentally
        // re-use values across generations.
        CidInt(index as u64 * 8 + 1)
    }

    pub fn push_bytes(&mut self, value: Box<[u8]>) -> CidBytes {
        let index = self.stack_bytes.len();
        self.stack_bytes.push(value);
        // Constant ids of the underlying heap are always aligned to 8 bytes,
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

    pub fn into_temporaries(self) -> Temporaries {
        Temporaries {
            stack_u64: self.stack_u64,
            stack_bytes: self.stack_bytes,
        }
    }
}

impl<H: Heap> Heap for TempHeap<H> {
    fn get_u64(&self, offset: CidInt) -> u64 {
        // If the id is aligned, it comes from the underlying heap,
        // otherwise it was ours.
        let index = match offset.0 & 7 {
            0 => return self.heap.get_u64(offset),
            1 => (offset.0 - 1) / 8,
            _ => unreachable!("Invalid integer constant id."),
        };
        self.stack_u64[index as usize]
    }

    fn get_bytes(&self, offset: CidBytes) -> &[u8] {
        // If the id is aligned, it comes from the underlying heap,
        // otherwise it was ours.
        let index = match offset.0 & 7 {
            0 => return self.heap.get_bytes(offset),
            1 => (offset.0 - 1) / 8,
            _ => unreachable!("Invalid byte string constant id: 0x{:x}", offset.0),
        };
        &self.stack_bytes[index as usize]
    }
}

impl Temporaries {
    /// Resolve a temporary constant id to the `u64` value.
    pub fn get_u64(&self, cid: CidInt) -> u64 {
        debug_assert_eq!(cid.0 & 7, 1, "Expected temporary id to be 1 mod 8.");
        let index = (cid.0 - 1) / 8;
        self.stack_u64[index as usize]
    }

    /// Resolve a temporary constant id to the byte string value.
    pub fn get_bytes(&self, cid: CidBytes) -> &[u8] {
        debug_assert_eq!(cid.0 & 7, 1, "Expected temporary id to be 1 mod 8.");
        let index = (cid.0 - 1) / 8;
        &self.stack_bytes[index as usize]
    }
}
