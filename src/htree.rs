// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines on-disk hitchhiker trees.

use datom::Datom;
use std::cmp::Ordering;
use std::io;
use store::{PageId, PageSize, Store};

/// An ordering on datoms.
trait DatomOrd {
    fn cmp(&self, lhs: &Datom, rhs: &Datom) -> Ordering;
}

impl DatomOrd for () {
    fn cmp(&self, lhs: &Datom, rhs: &Datom) -> Ordering {
        Ordering::Less
    }
}

/// A tree node.
#[derive(Clone)]
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
    pub fn from_bytes<Size: PageSize>(bytes: &'a [u8]) -> Node<'a> {
        assert_eq!(bytes.len(), Size::SIZE);
        // TODO: Assert alignment of byte array.

        // TODO: Check that these are within range. Should return Result then.
        let header = &bytes[Size::header_offset()..];
        let depth = header[0];
        let num_datoms = header[1] as usize;

        // The datom array is stored at the start of the page.
        let num_datom_bytes = 32 * num_datoms;
        let datoms: &[Datom] = unsafe {
            transmute_slice(&bytes[0..num_datom_bytes])
        };

        // The array with child page ids starts at 3264.
        let num_children_bytes = 8 * num_datoms;
        let child_off = Size::children_offset();
        let children: &[PageId] = unsafe {
            transmute_slice(&bytes[child_off..child_off + num_children_bytes])
        };

        Node {
            depth: depth,
            datoms: datoms,
            children: children,
        }
    }

    /// Write the node to backing storage.
    pub fn write<Size: PageSize, W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        assert_eq!(
            self.datoms.len(),
            self.children.len(),
            "Node must have as many children as datoms."
        );
        assert!(
            self.datoms.len() <= Size::CAPACITY,
            "Node has more datoms than a page can hold.",
        );

        let datoms_bytes: &[u8] = unsafe { transmute_slice(self.datoms) };
        let children_bytes: &[u8] = unsafe { transmute_slice(self.children) };

        // First up is the datom array.
        writer.write_all(&datoms_bytes)?;

        // If necessary, pad with zeros between the datom array, and the child
        // page id array.
        let num_zeros = Size::datoms_bytes_len() - datoms_bytes.len();
        let zeros = [0u8; 32];
        let mut num_zeros_written = 0;
        while num_zeros_written < num_zeros {
            let num_zeros_left = num_zeros - num_zeros_written;
            let num_zeros_write = num_zeros_left.min(zeros.len());
            num_zeros_written += writer.write(&zeros[..num_zeros_write])?;
        }

        // Next up is the child array, also optionally padded.
        writer.write_all(&children_bytes)?;

        let num_zeros = Size::children_bytes_len() - children_bytes.len();
        let mut num_zeros_written = 0;
        while num_zeros_written < num_zeros {
            let num_zeros_left = num_zeros - num_zeros_written;
            let num_zeros_write = num_zeros_left.min(zeros.len());
            num_zeros_written += writer.write(&zeros[..num_zeros_write])?;
        }

        // Finally, the header. For now it consists of two bytes, and we pad
        // with zeros to fill up the page.
        let mut header = [0u8; 42];
        let header_len = Size::SIZE - Size::header_offset();
        debug_assert!(header_len < 42, "Header too large, could fit extra datom.");
        debug_assert!(header_len >= 2, "Header too small, cannot fit all fields.");
        debug_assert!(self.datoms.len() < 256, "Datoms length must fit in a byte.");
        header[0] = self.depth;
        header[1] = self.datoms.len() as u8;
        writer.write_all(&header[..header_len])?;

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
struct Route {
    /// Index into the datom array in the node, of the next pending datom to yield.
    ///
    /// When the index points past the datom array, or when the datom at the
    /// given index is a midpoint, then the pending datoms have been exhausted.
    pending_index: usize,
}

/// A hittchhiker tree.
struct HTree<'a, Cmp: 'a + DatomOrd, S: 'a + Store> {
    /// The page that contains the root node.
    root_page: PageId,

    /// Ordering on datoms.
    comparator: &'a Cmp,

    /// The backing store to read pages from.
    store: S,
}

impl<'a, Cmp: DatomOrd, S: Store> HTree<'a, Cmp, S> {
    pub fn new(store: S, comparator: &'a Cmp) -> HTree<'a, Cmp, S> {
        unimplemented!()
    }

    pub fn get(&self, page: PageId) -> Node {
        Node::from_bytes::<S::Size>(self.store.get(page))
    }

    /// Locate the first datom that is greater than or equal to the queried one.
    pub fn find(&self, datom: &Datom) -> Route {
        let node = self.get(self.root_page);

        for (i, datom_i) in node.datoms.iter().enumerate() {
            match self.comparator.cmp(datom_i, datom) {
                Ordering::Less => continue,
                Ordering::Equal | Ordering::Greater => return Route {
                    pending_index: i,
                },
            }
        }

        // Everything is less than the given datom, return a route past the end.
        Route {
            pending_index: node.datoms.len(),
        }
    }
}

struct Iter<'a, Cmp: 'a + DatomOrd, S: 'a + Store> {
    /// The tree to iterate.
    tree: &'a HTree<'a, Cmp, S>,

    /// The node into which `begin` points.
    node: Node<'a>,

    /// The pointer to the next datom to yield.
    begin: Route,

    /// The pointer to the first datom not to yield.
    end: Route,
}

impl<'a, Cmp: DatomOrd, S: Store> Iter<'a, Cmp, S> {
    fn new(tree: &'a HTree<'a, Cmp, S>, begin: Route, end: Route) -> Iter<'a, Cmp, S> {
        Iter {
            tree: tree,
            node: tree.get(tree.root_page),
            begin: begin,
            end: end,
        }
    }
}

impl<'a, Cmp: DatomOrd, S: Store> Iterator for Iter<'a, Cmp, S> {
    type Item = &'a Datom;

    fn next(&mut self) -> Option<&'a Datom> {
        // Check if we exhausted the range.
        if self.begin.pending_index == self.end.pending_index {
            return None
        }

        // We did not exhaust the range, locate the next datom to yield.
        let datom = &self.node.datoms[self.begin.pending_index];

        self.begin.pending_index += 1;

        Some(datom)
    }
}

#[cfg(test)]
mod test {
    use std::iter;

    use datom::{Aid, Datom, Eid, Tid, Value};
    use store::{MemoryStore, PageId, PageSize563, PageSize4096, Store};
    use super::{HTree, Iter, Node, Route};

    #[test]
    fn node_write_after_read_is_identity() {
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
        node.write::<PageSize4096, _>(&mut buffer_a).unwrap();

        let node_a = Node::from_bytes::<PageSize4096>(&buffer_a[..]);

        let mut buffer_b: Vec<u8> = Vec::new();
        node_a.write::<PageSize4096, _>(&mut buffer_b).unwrap();

        assert_eq!(buffer_a, buffer_b);
    }

    #[test]
    fn iter_iterates_single_page() {
        let datoms: Vec<_> = (0..13)
            .map(|i| Datom::assert(Eid(i), Aid::max(), Value::min(), Tid::max()))
            .collect();
        let child_ids: Vec<_> = iter::repeat(PageId::max())
            .take(datoms.len())
            .collect();

        let node = Node {
            depth: 0,
            datoms: &datoms[..],
            children: &child_ids[..],
        };

        type Size = PageSize563;
        let mut store = MemoryStore::<Size>::new();
        let page = store.allocate_page();
        node.write::<Size, _>(store.writer()).unwrap();

        let tree = HTree {
            root_page: PageId(0),
            comparator: &(),
            store: store,
        };

        let iter = Iter {
            tree: &tree,
            node: tree.get(tree.root_page).clone(),
            begin: Route {
                pending_index: 0,
            },
            end: Route {
                pending_index: datoms.len(),
            },
        };

        for (&x, &y) in iter.zip(datoms.iter()) {
            assert_eq!(x, y);
        }
    }
}
