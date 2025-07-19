// Noblit -- An immutable append-only database
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::hash::Hash;

/// Trait to allow compile-time parametrization over the page size.
///
/// This is mainly useful in tests, where constructing large trees is
/// undesirable, and for fuzzing, where small examples are faster to explore.
/// In that case we can use smaller pages, while in release mode we use a page
/// size that is a multiple of the OS page size.
pub trait PageSize {
    /// The number of bytes in a page. A page stores exactly one tree node.
    const SIZE: usize;
}

pub struct Error {}

/// Identifies a node in the hitchhiker tree.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct NodeId(std::num::NonZeroU64);

/// Raw representation of an entry in a tree node in the hitchhiker tree.
///
/// All entries, except for the last one in a non-leaf node, reference a key and
/// a value.
///
/// We use raw integers here, to ensure that the node can be packed into memory
/// efficiently in the way we like. In a tree node, we store entries
/// sequentially, such that for entry i, all keys in entries[i].child order
/// before entry i, and all keys in entries[i + 1].child order after entry i.
#[repr(align(32))]
struct RawEntry {
    /// Byte offset into this generation's keys file.
    key_offset: u64,
    /// Byte offset into this generation's values file.
    value_offset: u64,
    /// Value length in bytes.
    value_len: u32,
    /// Key length in bytes.
    key_len: u16,
    /// 0 for a regular node, 1 for a tombstone.
    /// TODO: Or we could use u64::MAX in value offset as a sentinel?
    /// Then we could lift the key length limit restriction.
    flags: u16,
    /// If nonzero, the node id of the child node.
    child: u64,
}

struct RawNode {
    /// Leaf nodes are at level 0, their parents at level 1, etc.
    level: u8,

    /// The generation that this node lives in. We need to know this to resolve keys and values.
    generation: u8,

    /// Keys and values, and pointers to child nodes if the level is greater than 0.
    entries: Vec<RawEntry>,
}

pub struct Generation {
    /// The number of times this generation was restarted.
    epoch: u64,

    /// Tree nodes themselves. Their ids are stored separately in `node_ids`.
    nodes: Vec<RawNode>,

    /// Element i is the id of `self.nodes[i]`. Nodes are stored by ascending id.
    node_ids: Vec<NodeId>,

    /// The nodes' key offset and key length slice keys out of this arena.
    key_data: Vec<u8>,

    /// The nodes' value offset and value length slice values out of this arena.
    value_data: Vec<u8>,
}

impl Generation {
    pub fn assert_invariants(&self) {
        let mut prev_id = 0;
        for node_id in self.node_ids.iter().map(|n| n.0.get()) {
            assert!(node_id > prev_id, "Node ids are ascending.");
            prev_id = node_id;
        }

        assert_eq!(self.nodes.len(), self.node_ids.len(), "We have one id per node.");

        for node in &self.nodes {
            for entry in &node.entries {
                // Assert that the key and value slices are in bounds.
                assert!(entry.key_offset <= self.key_data.len() as u64);
                assert!(entry.key_offset + entry.key_len as u64 <= self.key_data.len() as u64);
                assert!(entry.value_offset <= self.value_data.len() as u64);
                assert!(entry.value_offset + entry.value_len as u64 <= self.value_data.len() as u64);
            }
        }
    }

    pub fn push_node(&mut self, node_id: NodeId, node: RawNode) {
        self.node_ids.push(node_id);
        self.nodes.push(node);

        #[cfg(debug_assertions)]
        self.assert_invariants();
    }
}

pub struct Database {
    generations: Vec<Generation>,
}
