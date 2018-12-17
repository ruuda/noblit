// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines on-disk hitchhiker trees.

use datom::Datom;
use std::cmp::Ordering;
use std::io;
use store::{PAGE_SIZE, PageId, Store};

/// An ordering on datoms.
trait DatomOrd {
    fn cmp(&self, lhs: &Datom, rhs: &Datom) -> Ordering;
}

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

/// Indicates how to reach a datom in the tree.
#[derive(Eq, PartialEq)]
struct Route {
    /// The pages to traverse from the root.
    ///
    /// The first element is the root node, the last element is the node that
    /// contains the datom.
    // TODO: Could use a small slice: depth will not be more than 9 anyway for a
    // branching factor of 102: more datoms are not addressable with 64 bits.
    pages: Vec<PageId>,

    /// Index of the datom in the datoms array that we followed.
    ///
    /// For the last element, this is the index into the `datoms` array. For the
    /// preceding elements, this is the index into the `children` array.
    indices: Vec<usize>,
}

/// A hittchhiker tree.
struct HTree<'a, Cmp: 'a + DatomOrd> {
    root: Node<'a>,
    data: &'a [u8],
    comparator: &'a Cmp,
}

impl<'a, Cmp: DatomOrd> HTree<'a, Cmp> {
    pub fn new(data: &'a [u8], comparator: &'a Cmp) -> HTree<'a, Cmp> {
        unimplemented!()
    }

    pub fn get(&self, node: PageId) -> Node<'a> {
        unimplemented!()
    }

    /// Locate the first datom that is greater than or equal to the queried one.
    pub fn find(&self, datom: &Datom) -> Route {
        unimplemented!()
    }
}

struct Iter<'a, Cmp: 'a + DatomOrd> {
    /// The tree to iterate.
    tree: &'a HTree<'a, Cmp>,

    /// The node into which `begin` points.
    node: Node<'a>,

    /// The pointer to the next datom to yield.
    begin: Route,

    /// The pointer to the first datom not to yield.
    end: Route,
}

impl<'a, Cmp: DatomOrd> Iter<'a, Cmp> {
    fn new(tree: &'a HTree<'a, Cmp>, begin: Route, end: Route) -> Iter<'a, Cmp> {
        Iter {
            tree: tree,
            node: tree.get(*begin.pages.last().unwrap()),
            begin: begin,
            end: end,
        }
    }
}

impl<'a, Cmp: DatomOrd> Iterator for Iter<'a, Cmp> {
    type Item = &'a Datom;

    fn next(&mut self) -> Option<&'a Datom> {
        let mut index = *self.begin.indices.last().unwrap();

        // First of all, try to move into a child node, if there is any.
        let child_page = self.node.children[index];
        if child_page != PageId::max() {
            self.begin.pages.push(child_page);
            self.begin.indices.push(0);
            self.node = self.tree.get(child_page);

            // Tail call: yield from the child page.
            return self.next();
        }

        // TODO: Check if we reached the end, and stop if so.

        loop {
            // There are no children left at the curent index, so we can yield
            // the midpoint datom, if there is any. If we exhausted all datoms
            // in this node, then we need to move up one node in the tree.
            if index < self.node.datoms.len() {
                // There is a midpoint datom, yield it.
                let result = &self.node.datoms[index];
                *self.begin.indices.last_mut().unwrap() += 1;
                return Some(result);
            } else {
                // We exhausted this node, move up one node, then loop. If we
                // loop, we will yield the datom that is there, otherwise we
                // will move up all the way if the tree is exhausted.
                self.begin.pages.pop();
                self.begin.indices.pop();
                if let Some(parent_page) = self.begin.pages.last() {
                    // There is still a parent. Move up.
                    index = *self.begin.indices.last().unwrap();
                    self.node = self.tree.get(*parent_page);
                } else {
                    // The tree is exhausted.
                    return None
                }
            }
        }
    }
}

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
