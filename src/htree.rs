// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines on-disk hitchhiker trees.

use std::io;
use store::{PAGE_SIZE, PageId, Store};
use datom::Datom;

/// A tree node.
pub struct Node<'a> {
    /// Depth: 0 for leaves, 1 + depth of children for interior nodes.
    pub depth: u8,

    /// Datoms stored in this node.
    ///
    /// * If `children[i]` is not `u64::MAX`, `datoms[i]` is a midpoint.
    /// * All datoms in `children[i]` are less than `datoms[i]`.
    /// * All datoms in `children[i + 1]` are greater than `datoms[i]`.
    pub datoms: &'a [Datom],

    /// Child node page indices.
    ///
    /// The length is `datoms.len() + 1`.
    pub children: &'a [PageId],
}

unsafe fn transmute_slice<T, U>(ts: &[T]) -> &[U] {
    use std::mem;
    use std::slice;

    let ptr = ts.as_ptr();
    let byte_len = ts.len() * mem::size_of::<T>();
    let len = byte_len / mem::size_of::<U>();
    debug_assert_eq!(len * mem::size_of::<U>(), byte_len);
    // TODO: Is it possible to assert alignment?

    slice::from_raw_parts(mem::transmute(ptr), len)
}

impl<'a> Node<'a> {
    /// Interpret a page-sized byte slice as node.
    pub fn from_bytes(bytes: &'a [u8]) -> Node<'a> {
        assert_eq!(bytes.len(), PAGE_SIZE);
        // TODO: Assert alignment of byte array.

        // TODO: Check that these are within range. Should return Result then.
        let depth = bytes[4088];
        let num_datoms = bytes[4089] as usize;

        // The datom array is stored at the start of the page.
        let num_datom_bytes = 32 * num_datoms;
        let datoms: &[Datom] = unsafe {
            transmute_slice(&bytes[0..num_datom_bytes])
        };

        // The array with child page ids starts at 3264.
        let num_children_bytes = 8 * (num_datoms + 1);
        let children: &[PageId] = unsafe {
            transmute_slice(&bytes[3264..3264 + num_children_bytes])
        };

        Node {
            depth: depth,
            datoms: datoms,
            children: children,
        }
    }

    /// Write the node to backing storage.
    pub fn write<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        let datom_bytes: &[u8] = unsafe { transmute_slice(self.datoms) };
        let children_bytes: &[u8] = unsafe { transmute_slice(self.children) };

        // First up is the datom array.
        writer.write_all(&datom_bytes)?;

        // If necessary, pad with zeros between the datom array, and the child
        // page id array.
        let num_zeros = 3264 - datom_bytes.len();
        let zeros = [0u8; 32];
        let mut num_zeros_written = 0;
        while num_zeros_written < num_zeros {
            let num_zeros_left = num_zeros - num_zeros_written;
            let num_zeros_write = num_zeros_left.min(zeros.len());
            num_zeros_written += writer.write(&zeros[..num_zeros_write])?;
        }

        // Next up is the child array, also optionally padded.
        writer.write_all(&children_bytes)?;

        let num_zeros = 824 - children_bytes.len();
        let mut num_zeros_written = 0;
        while num_zeros_written < num_zeros {
            let num_zeros_left = num_zeros - num_zeros_written;
            let num_zeros_write = num_zeros_left.min(zeros.len());
            num_zeros_written += writer.write(&zeros[..num_zeros_write])?;
        }

        // Finally, the header.
        let mut header = [0u8; 8];
        header[0] = self.depth;
        header[1] = self.datoms.len() as u8;
        writer.write_all(&header[..])?;

        Ok(())
    }
}

/// Write a sorted slice of datoms as a tree.
///
/// Returns the page id of the root node.
pub fn write_tree<S: Store>(store: &mut S, datoms: &[Datom]) -> io::Result<PageId> {
    // TODO: redo this thing.
    unimplemented!()
}

/// A hittchhiker tree.
struct HTree<'a> {
    root: Node<'a>,
    data: &'a [u8],
}

impl<'a> HTree<'a> {
    pub fn new(data: &'a [u8]) -> HTree<'a> {
        unimplemented!()
    }

    pub fn get(node: PageId) -> Node<'a> {
        unimplemented!()
    }
}

/*
/// Indicates how to reach a datom in the tree.
struct DatomPointer {
    /// The pages to traverse.
    ///
    /// The first element is the root node, the last element is the node that
    /// contains the datom.
    route: Vec<PageId>,
}

struct Iter<'a, Cmp: DatomOrd> {
    comparator: &'a Cmp,
    tree: &'a HTree<'a>,
    next_pending_index: usize,
    next_midpoint_index: usize,
}

impl<'a, Cmp: DatomOrd> Iter<'a, Cmp> {
    fn new(tree: &'a Htree<'a>, comparator: &'a Cmp) -> Iter<'a, Cmp> {
        Iter {
            comparator: comparator,
            tree: tree,
            next_pending_index: 0,
            next_midpoint_index: 0,
        }
    }
}

impl std::Iterator for Iter<'a> {
    type Item = Datom;

    fn next(&mut self) -> Option<Datom> {

    }
}
*/

#[cfg(test)]
mod test {
    use store::PageId;
    use super::Node;

    #[test]
    fn node_write_after_read_is_identity() {
        use std::iter;
        use datom::{Aid, Datom, Eid, Tid, Value};

        // TODO: Generate some test data.
        let datom = Datom::assert(Eid::min(), Aid::max(), Value::min(), Tid::max());
        let datoms: Vec<_> = iter::repeat(datom).take(17).collect();
        let child_ids: Vec<_> = (0..17).map(|i| PageId(i)).collect();

        let node = Node {
            depth: 0,
            datoms: &datoms[..],
            children: &child_ids[..],
        };

        let mut buffer_a: Vec<u8> = Vec::new();
        node.write(&mut buffer_a).unwrap();

        let node_a = Node::from_bytes(&buffer_a[..]);

        let mut buffer_b: Vec<u8> = Vec::new();
        node_a.write(&mut buffer_b).unwrap();

        assert_eq!(buffer_a, buffer_b);
    }
}
