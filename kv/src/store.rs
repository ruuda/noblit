// Noblit -- An immutable append-only database
// Copyright 2026 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::collections::HashMap;
use std::hash::Hash;
use std::num::NonZeroU64;

struct Error;
type Result<T> = std::result::Result<T, Error>;

trait Object {
    /// Identifies an object in the store.
    ///
    /// Object ids are stable: they don't change when the objects move to later
    /// generations in the backing storage. Object ids are allocated sequentially,
    /// and are never recycled.
    type Id: From<NonZeroU64> + Hash + Copy;

    /// Restore an object in memory from its disk representation.
    fn deserialize(data: &[u8]) -> Self;

    /// Serialize an object for writing to disk.
    fn serialize(&self, sink: &mut Vec<u8>);
}

/// An entry of the index of a level of a store.
///
/// See also [`Index`].
#[repr(C)]
#[derive(Clone)]
pub struct IndexEntry {
    /// Id of the object pointed at.
    id: NonZeroU64,

    /// Byte offset of the first byte of the *next* object.
    ///
    /// If this index entry is entry `i` in the index, then the object spans
    /// `index[i - 1].offset..index[i].offset` in the backing file. For `i = 0`,
    /// the object spans `0..index[i].offset`.
    offset: u64,
}

/// File pointer.
type Fp = u64;

pub struct Generation {
    index: Vec<IndexEntry>,

    /// Least upper bound on the id range stored in this generation, exclusive.
    ///
    /// In other words, for generations `g > 0`, this is the first id that is
    /// stored in generation `g - 1` rather than `g`. For generation 0, this is
    /// the next available object id.
    id_upper: NonZeroU64,

    /// File pointer for the backing file.
    fp: Fp,
}

impl Generation {
    /// Return the total length of all objects concatenated.
    pub fn total_len(&self) -> u64 {
        self.index.last().map(|ent| ent.offset).unwrap_or(0)
    }
}

pub struct Store<Obj: Object> {
    generations: Vec<Generation>,
    cache: HashMap<Obj::Id, Obj>,
    prefix: String,
}

impl<Obj: Object> Store<Obj> {
    pub fn new(prefix: String) -> Self {
        Store {
            generations: Vec::new(),
            cache: HashMap::new(),
            prefix,
        }
    }

    /// Store an object, return its assigned id.
    pub fn append<I: Io>(&mut self, io: &mut I, object: Obj) -> (Obj::Id, I::WriteId) {
        // TODO: Let object pick the size instead? But we'll need to replace the
        // buffer type anyway.
        let mut buffer = Vec::with_capacity(4096);
        object.serialize(&mut buffer);

        let g0 = &mut self.generations[0];
        let id = g0.id_upper;
        let entry = IndexEntry {
            id,
            offset: g0.total_len() + buffer.len() as u64,
        };
        g0.id_upper = g0.id_upper.checked_add(1).expect("Ran out of object ids.");
        g0.index.push(entry);

        let wid = io.append(g0.fp, buffer);

        (id.into(), wid)
    }
}

trait Io {
    /// File handle (file pointer);
    type WriteId: Copy;

    /// Open a file at the given path.
    fn open(&mut self, path: &str) -> Result<Fp>;

    /// Append bytes at the end of the file.
    fn append(&mut self, fp: Fp, data: Vec<u8>) -> Self::WriteId;

    /// Wait for an IO operation to complete, return its result.
    fn wait(&mut self, wid: Self::WriteId) -> Result<()>;
}
