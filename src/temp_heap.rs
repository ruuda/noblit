// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Contains a heap for transient constants.

use heap::{CidBytes, CidInt, Heap};

/// Transient constants used in queries, not yet in databases.
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
pub struct Temporaries {
    /// The temporary 64-bit unsigned integer constants.
    stack_u64: Vec<u64>,

    /// The temporary byte string constants.
    stack_bytes: Vec<Box<[u8]>>,
}

impl Temporaries {
    pub fn new() -> Temporaries {
        // TODO: We could give every temporaries instance a generation number,
        // that also gets mixed in to the constant ids it generates. Every time
        // we construct a temporaries container, we would use a new generation
        // number. That will make it harder to mix up ids form different
        // generations. It would make things fail fast, rather than silently
        // corrupting data, in the face of bugs.
        Temporaries {
            stack_u64: Vec::new(),
            stack_bytes: Vec::new(),
        }
    }

    pub fn push_u64(&mut self, value: u64) -> CidInt {
        let index = self.stack_u64.len();
        self.stack_u64.push(value);
        // Constant ids of the underlying heap ale always aligned to 8 bytes,
        // so make sure that ours are not, so we can always keep the two apart.
        CidInt(index as u64 * 8 + 1)
    }

    pub fn push_bytes(&mut self, value: Box<[u8]>) -> CidBytes {
        let index = self.stack_bytes.len();
        self.stack_bytes.push(value);
        // Constant ids of the underlying heap are always aligned to 8 bytes,
        // so make sure that ours are not, so we can always keep the two apart.
        CidBytes(index as u64 * 8 + 1)
    }

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

/// A heap that can look up temporaries, and constants form an underlying heap.
///
/// Contains `Temporaries`, and an inner heap. For a lookup, we check if the id
/// is aligned to 8 bytes. Persistent constants are, so in that case we look up
/// the constant in the underlying heap. If the id is not 0 modulo 8, then it
/// must be a temporary, so we look it up in the temporary heap.
pub struct TempHeap<H: Heap> {
    /// The underlying append-only heap, for persistent constants.
    inner: H,

    /// Temporary constants, used in a query, but not yet in the database.
    temporaries: Temporaries,
}

impl<H: Heap> TempHeap<H> {
    pub fn new(inner: H, temporaries: Temporaries) -> TempHeap<H> {
        TempHeap {
            inner: inner,
            temporaries: temporaries,
        }
    }

    pub fn into_inner(self) -> (H, Temporaries) {
        (self.inner, self.temporaries)
    }
}

impl<H: Heap> Heap for TempHeap<H> {
    fn get_u64(&self, offset: CidInt) -> u64 {
        // If the id is aligned, it comes from the underlying heap,
        // otherwise it was a temporary.
        match offset.0 & 7 {
            0 => self.inner.get_u64(offset),
            1 => self.temporaries.get_u64(offset),
            _ => unreachable!("Invalid integer constant id."),
        }
    }

    fn get_bytes(&self, offset: CidBytes) -> &[u8] {
        // If the id is aligned, it comes from the underlying heap,
        // otherwise it was a temporary.
        match offset.0 & 7 {
            0 => self.inner.get_bytes(offset),
            1 => self.temporaries.get_bytes(offset),
            _ => unreachable!("Invalid byte string constant id: 0x{:x}", offset.0),
        }
    }
}
