// Noblit -- An immutable append-only database
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::hash::Hash;
use std::num::NonZeroU64;

pub struct Error {}

#[cfg(feature = "node-size-4096")]
const ENTRIES_PER_NODE: usize = 127;

#[cfg(feature = "node-size-256")]
const ENTRIES_PER_NODE: usize = 7;

#[cfg(feature = "node-size-160")]
const ENTRIES_PER_NODE: usize = 4;

/// Contains structs whose representation matches the disk format.
///
/// TODO: Add a statement about endianness.
mod raw {
    use super::{ENTRIES_PER_NODE, KeyRef, NodeId, ValueRef};
    use std::num::NonZeroU64;

    /// Raw representation of an entry in a tree node in the hitchhiker tree.
    ///
    /// All entries, except for the last one in a non-leaf node, reference a key
    /// and a value.
    ///
    /// We use raw integers here, to ensure that the node can be packed into
    /// memory efficiently in the way we like.
    /// TODO: Document hitchhiker tree invariant.
    #[repr(align(32))]
    #[repr(C)]
    pub struct Entry {
        /// Byte offset into this generation's keys file.
        pub raw_key_offset: u64,

        /// Byte offset into this generation's values file.
        pub raw_value_offset: u64,

        /// Value length in bytes.
        pub raw_value_len: u32,

        /// Key length in bytes.
        pub raw_key_len: u32,

        /// If nonzero, the node id of the child node.
        pub raw_child_id: u64,
    }

    impl Entry {
        #[inline]
        pub fn key_ref(&self) -> KeyRef {
            debug_assert!(
                ((self.raw_key_offset == 0) && (self.raw_key_len == 0)) || (self.raw_key_len > 0),
                "The canonical encoding of the empty key has offset 0.",
            );
            KeyRef {
                offset: self.raw_key_offset,
                len: self.raw_key_len,
            }
        }

        #[inline]
        pub fn value_ref(&self) -> Option<ValueRef> {
            // We use a nonzero offset together with zero length to signal
            // a tombstone: an entry that marks that a key has been deleted.
            let result = match (self.raw_value_offset, self.raw_value_len) {
                (0, 0) => ValueRef { offset: 0, len: 0 },
                (_, 0) => return None,
                (i, n) => ValueRef { offset: i, len: n },
            };
            Some(result)
        }

        #[inline]
        pub fn child_id(&self) -> Option<NodeId> {
            NonZeroU64::new(self.raw_child_id).map(NodeId)
        }
    }

    #[repr(align(32))]
    #[repr(C)]
    pub struct NodeHeader {
        /// The node's own node id.
        ///
        /// This value is redundant: we also store node ids externally in the
        /// list of node ids. We embed it here as well so we can perform sanity
        /// checks, and also to aid data recovery and debugging.
        pub raw_id: u64,

        /// The epoch of the generation that this node is stored in.
        ///
        /// This value is redundant: it is implied by the epoch counter of the
        /// generation that stores this node. We embed it here as well for the
        /// same reasons that we embed the node id.
        pub raw_epoch: u64,

        pub _reserved_1: u64,
        pub _reserved_2: u32,

        /// The number of entries in this node.
        pub raw_len: u16,

        /// The generation that this node is stored in.
        ///
        /// This value is redundant: it is implied by the generation that stores
        /// this node. We embed it here as well for the same reason that we
        /// embed the node id.
        pub generation: u8,

        /// Layer in the tree: leaves have level 0, their parents level 1, etc.
        pub level: u8,
    }

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

/// Identifies a node in the hitchhiker tree.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct NodeId(std::num::NonZeroU64);

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
    nodes: Vec<raw::Node>,

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

    pub fn push_node(&mut self, node: raw::Node) {
        self.node_ids.push(node.id());
        self.nodes.push(node);

        #[cfg(debug_assertions)]
        self.assert_invariants();
    }
}

pub struct Database {
    generations: Vec<Arena>,
}
