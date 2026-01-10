// Noblit -- An immutable append-only database
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

mod store;

use std::collections::HashMap;
use std::hash::Hash;
use std::net::Shutdown::Write;
use std::num::NonZeroU64;
use std::sync::atomic::AtomicU64;

use parking_lot::{Mutex, MutexGuard};

pub struct Error {}

#[cfg(feature = "node-size-4096")]
const ENTRIES_PER_NODE: usize = 170;

#[cfg(feature = "node-size-256")]
const ENTRIES_PER_NODE: usize = 10;

#[cfg(feature = "node-size-88")]
const ENTRIES_PER_NODE: usize = 3;

/// Identifies hitchhiker tree node in the node store.
///
/// Node ids are stable: they don't change when the nodes are moved around in
/// backing storage. Node ids are allocated sequentially, and are never
/// recycled.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct NodeId(NonZeroU64);

/// Identifies a blob in the blob store.
///
/// Blob ids are stable: they don't change when blobs are moved around in
/// backing storage. Blob ids are allocated sequentially, and are never
/// recycled.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct BlobId(NonZeroU64);

/// An entry of the index of a level of a store.
///
/// See also [`Index`].
#[repr(C)]
#[derive(Clone)]
pub struct IndexEntry {
    /// Id of the object (node or blob) pointed at.
    id: NonZeroU64,

    /// Byte offset of the first byte of the *next* object.
    ///
    /// If this index entry is entry `i` in the index, then the object spans
    /// `index[i - 1].offset..index[i].offset` in the backing file. For `i = 0`,
    /// the object spans `0..index[i].offset`.
    offset: u64,
}

/// The index of a level of the store.
///
/// The index maps object ids (node or blob ids) to byte ranges in the backing
/// file.
pub struct Index {
    /// TODO: Support loading parts of the index from disk when needed.
    /// For now we keep the entire index in memory.
    entries: Vec<IndexEntry>,
}

/// Raw representation of an entry in a tree node in the hitchhiker tree.
///
/// All entries that are in use include a key, and when they are not a
/// tombstone, also a value. Entries can optionally point to a child node,
/// in which case the entry is called a *midpoint*. Entries are stored in the
/// node by ascending key. (Fully dereferenced key, not by ascending key id!)
///
/// The tree maintains the following invariant:
///
/// Suppose entry `i` and `j` are midpoints, with `i` ordering before `j`. Then
/// it holds that:
///
/// * For every entry `z` in `i.child.entries`, `z.key < i.key`.
/// * For every entry `z` in `j.child.entries`, `i.key < z.key`.
///
/// So midpoints order the keys: lesser keys go before it, greater keys go
/// after it. However, there may be entries between two midpoints which are not
/// themselves midpoints, and these are unordered with respect to the subtree
/// of the midpoint which is the upper bound of this range of entries.
///
/// TODO: In a future version, we could inline small keys and small values. For
/// v1, we keep things simple and store everything as blobs.
#[repr(C)]
#[derive(Clone)]
pub struct Entry {
    /// The key of the KV pair or tombstone.
    pub key_id: BlobId,

    /// If nonzero, the value of the KV pair; if zero, this entry is a tombstone.
    pub value_id: Option<BlobId>,

    /// If nonzero, the node id of the child node.
    pub child_id: Option<NodeId>,
}

#[repr(align(16))]
#[repr(C)]
pub struct NodeHeader {
    /// The node's own node id.
    ///
    /// This value is redundant: we also store node ids externally in the
    /// store's index. We embed it here as well so we can perform sanity
    /// checks, and also to aid data recovery and debugging.
    pub id: NodeId,

    /// The number of entries in this node.
    pub len: u16,

    /// Layer in the tree: leaves have level 0, their parents level 1, etc.
    pub level: u8,
    // TODO: We could store some kind of checksum here?
}

/// Contains structs whose representation matches the disk format.
///
/// TODO: Add a statement about endianness.
mod raw {
    use super::{ENTRIES_PER_NODE, KeyId, NodeId, ValueId};
    use std::num::NonZeroU64;

    #[repr(align(32))]
    #[repr(C)]
    pub struct Node {
        pub raw_header: NodeHeader,
        pub raw_entries: [Entry; ENTRIES_PER_NODE],
    }

    impl Node {
        pub fn id(&self) -> NodeId {
            NodeId(NonZeroU64::new(self.raw_header.raw_id).expect("Node ids are nonzero."))
        }

        pub fn entries(&self) -> &[Entry] {
            &self.raw_entries[..self.raw_header.raw_len as _]
        }
    }

    #[cfg(test)]
    mod test {
        use super::{Entry, Node, NodeHeader};

        #[test]
        fn size_and_alignment() {
            assert_eq!(size_of::<Entry>(), 32, "Raw entry is 32 bytes.");
            assert_eq!(align_of::<Entry>(), 32, "Raw entry aligns to 32 bytes.");
            assert_eq!(size_of::<NodeHeader>(), 32, "Raw header is 32 bytes.");
            assert_eq!(
                align_of::<NodeHeader>(),
                32,
                "Raw header aligns to 32 bytes."
            );

            #[cfg(feature = "node-size-4096")]
            assert_eq!(
                size_of::<Node>(),
                4096,
                "A 4k node fits a header + 127 entries."
            );

            #[cfg(feature = "node-size-256")]
            assert_eq!(
                size_of::<Node>(),
                256,
                "A 256-byte node fits a header + 7 entries."
            );

            #[cfg(feature = "node-size-160")]
            assert_eq!(
                size_of::<Node>(),
                160,
                "A 160-byte node fits a header + 4 entries."
            );
        }
    }
}

use raw::{Entry, Node};

/// A slice into a generation's key data.
#[derive(Copy, Clone, Debug)]
pub struct KeyRef {
    offset: u64,
    len: u32,
}

/// A slice into a generation's value data.
#[derive(Copy, Clone, Debug)]
pub struct ValueRef {
    offset: u64,
    len: u32,
}

/// Stores tree nodes for a single epoch of a single generation.
pub struct Arena {
    /// The generation that we represent.
    generation: u8,

    /// The number of times this generation was restarted.
    epoch: u64,

    /// Tree nodes stored in this generation.
    nodes: Vec<Node>,

    /// Element i is the id of `self.nodes[i]`. Nodes are stored by ascending id.
    node_ids: Vec<NodeId>,

    /// The nodes' key offset and key length slice keys out of this buffer.
    key_data: Vec<u8>,

    /// The nodes' value offset and value length slice values out of this buffer.
    value_data: Vec<u8>,
}

impl Arena {
    pub fn assert_invariants(&self) {
        let mut prev_id = 0;
        for node_id in self.node_ids.iter().map(|n| n.0.get()) {
            assert!(node_id > prev_id, "Node ids are ascending.");
            prev_id = node_id;
        }

        assert_eq!(
            self.nodes.len(),
            self.node_ids.len(),
            "We have one id per node."
        );

        for node in &self.nodes {
            for entry in node.entries() {
                // Assert that the key and value slices are in bounds.
                assert!(entry.raw_key_offset <= self.key_data.len() as u64);
                assert!(
                    entry.raw_key_offset + entry.raw_key_len as u64 <= self.key_data.len() as u64
                );
                assert!(entry.raw_value_offset <= self.value_data.len() as u64);
                assert!(
                    entry.raw_value_offset + entry.raw_value_len as u64
                        <= self.value_data.len() as u64
                );
            }
        }
    }

    /// Append the key and value to their heaps.
    ///
    /// Returns an entry suitable for inserting into a new tree node.
    pub fn push_key_value(&mut self, key: &[u8], value: Option<&[u8]>) -> Entry {
        let mut entry = Entry::default();

        // TODO: As an optimization, we can keep a hash table of string hash to
        // (offset, len) in the heap, so that in case of duplicates (which can
        // happen especially with keys, when we keep updating the same key), we
        // reuse the existing value instead of storing it twice.

        entry.raw_key_offset = self.key_data.len() as u64;
        entry.raw_key_len = key.len().try_into().expect("Key fits in 4 GiB.");
        self.key_data.extend_from_slice(key);

        if let Some(v) = value {
            entry.raw_value_offset = self.value_data.len() as u64;
            entry.raw_value_len = v.len().try_into().expect("Value fits in 4 GiB.");
            self.value_data.extend_from_slice(v);
        }

        entry
    }

    pub fn update_node(&mut self, node: &Node, new_entries: Vec<Entry>) {
        // TODO: I need something like a dedup_merge that merges two sorted slices
        // of entries, skipping duplicate keys.

        // TODO: If the node that we are updating lived in an older generation,
        // then with the current format, we need to copy the keys and values
        // back into this generation. That would be a shame, can we include a
        // generation id in the references to fix that?
    }

    pub fn push_node(&mut self, node: Node) {
        self.node_ids.push(node.id());
        self.nodes.push(node);

        #[cfg(debug_assertions)]
        self.assert_invariants();
    }
}

struct DatabaseState {
    /// The id of the current tree root.
    root: NodeId,

    /// For current and past tree roots, the count of how many transactions are reading from it.
    ///
    /// These tree nodes are used as the GC roots when we perform a collection.
    refcounts: HashMap<NodeId, u32>,
}

impl DatabaseState {
    /// Bump the refcount for the current root node by 1, and return the node id.
    pub fn get_root_increment_refcount(&mut self) -> NodeId {
        *self.refcounts.entry(self.root).or_insert(0) += 1;
        self.root
    }

    /// Decrement the refcount for the given node id.
    pub fn decrement_refcount(&mut self, node_id: NodeId) {
        let mut entry = match self.refcounts.entry(node_id) {
            std::collections::hash_map::Entry::Occupied(entry) => entry,
            _ => panic!("Trying to decrement refcount that is not present in the table."),
        };
        *entry.get_mut() -= 1;
        if *entry.get() == 0 {
            entry.remove();
        }
    }
}

pub struct Database {
    /// For every GC generation, an arena that stores the nodes in that generation.
    generations: Vec<Arena>,

    /// Database state that needs to be synchronized.
    state: Mutex<DatabaseState>,
}

pub struct ReadTransaction<'a> {
    db: &'a Database,
    root: NodeId,
}

pub struct WriteTransaction<'a> {
    /// The inner read transaction.
    ///
    /// Write transactions are a superset of read transactions. We still need to
    /// track the root and database reference, and we can do reads through a
    /// write transaction.
    /// TODO: Implement that, and ensure that reads see its own writes.
    inner: ReadTransaction<'a>,

    /// Keys and values to insert into the database.
    ///
    /// When the value is `None`, that's a tombstone, which marks that the key
    /// should be deleted.
    operations: HashMap<Vec<u8>, Option<Vec<u8>>>,
}

impl Database {
    /// Begin a new transaction against the latest version of the database.
    ///
    /// All reads from the transaction will observe the current version of the
    /// database, even if new writes are later committed to it.
    ///
    /// A read transaction can be upgraded to a write transaction.
    pub fn begin(&self) -> ReadTransaction {
        let mut state = self.state.lock();
        let root = state.get_root_increment_refcount();
        ReadTransaction { db: self, root }
    }
}

impl<'a> ReadTransaction<'a> {
    /// Upgrade the transaction to a write transaction.
    ///
    /// Write transactions are opportunistically concurrent: write transactions
    /// do not block reads or other write transactions, until commit. If we
    /// try to commit, but the database has seen writes since this transaction
    /// was created, then the commit fails, and we have to restart. This works
    /// well when write contention is low.
    pub fn write(self) -> WriteTransaction<'a> {
        WriteTransaction {
            inner: self,
            operations: HashMap::new(),
        }
    }
}

impl<'a> Drop for ReadTransaction<'a> {
    fn drop(&mut self) {
        self.db.state.lock().decrement_refcount(self.root);
    }
}

impl<'a> WriteTransaction<'a> {
    /// Insert a key and value into the database. Replaces the value if the key exists.
    pub fn insert(&mut self, key: Vec<u8>, value: Vec<u8>) {
        self.operations.insert(key, Some(value));
    }

    /// Delete a key from the database.
    pub fn remove(&mut self, key: Vec<u8>) {
        self.operations.insert(key, None);
    }

    /// Shared implementation of the two types of commit.
    fn commit_impl(
        inner: ReadTransaction<'a>,
        state: &mut DatabaseState,
        operations: HashMap<Vec<u8>, Option<Vec<u8>>>,
    ) {
        // The destructor of the read transaction takes state lock and
        // decrements the refcount, but since we already hold the lock,
        // we can just decrement it now.
        state.decrement_refcount(inner.root);
        std::mem::forget(inner);
    }

    /// Commit, if no mutations were committed since the start of this transaction.
    ///
    /// If a different transaction committed before this one, then the mutations
    /// that this transaction makes are potentially invalid, and the application
    /// should restart the transaction. In case of such a conflict, the `Err`
    /// returns this transaction.
    ///
    /// This commit acts as a compare-and-swap on the root node of the database:
    /// if the root did not change since we started the transaction, then we
    /// replace it with the new root.
    pub fn commit(self) -> Result<(), WriteTransaction<'a>> {
        let mut state = self.inner.db.state.lock();
        if state.root != self.inner.root {
            return Err(self);
        }
        WriteTransaction::commit_impl(self.inner, &mut state, self.operations);
        Ok(())
    }

    /// Commit, regardless of mutations that were committed since the start of this transaction.
    ///
    /// Commit in this case means that we apply all `insert` and `remove`
    /// operations against the _current_ version of the database, which may be
    /// different from the version of the database when this transaction started.
    /// Whether this is safe to do depends on application logic. In general, it
    /// is safe when the mutations were not based on any reads, and unsafe when
    /// they were.
    pub fn commit_unconditional(self) {
        let mut state = self.inner.db.state.lock();
        WriteTransaction::commit_impl(self.inner, &mut state, self.operations);
    }

    pub fn abort(self) {
        // Abort is a no-op, we just forget about all operations!
        // The Drop for the inner read transaction will decrement the refcount.
    }
}
