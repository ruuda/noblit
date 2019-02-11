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
        // TODO: Implement ordering properly, with heap lookup.
        lhs.eavt().cmp(&rhs.eavt())
    }
}

/// A tree node.
#[derive(Clone)]
pub struct Node<'a> {
    /// Level: 0 for leaves, 1 + level of children for interior nodes.
    pub level: u8,

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
        let level = header[0];
        let num_datoms = header[1] as usize;

        // The datom array is stored at the start of the page.
        let num_datom_bytes = 32 * num_datoms;
        let datoms: &[Datom] = unsafe {
            transmute_slice(&bytes[0..num_datom_bytes])
        };

        // The array with child page ids starts at an offset dependent on the
        // size of the node.
        let num_children_bytes = 8 * (num_datoms + 1);
        let child_off = Size::children_offset();
        let children: &[PageId] = unsafe {
            transmute_slice(&bytes[child_off..child_off + num_children_bytes])
        };

        assert_eq!(
            datoms.len() + 1,
            children.len(),
            "Node must have one more child than datoms.",
        );

        Node {
            level: level,
            datoms: datoms,
            children: children,
        }
    }

    /// Write the node to backing storage.
    pub fn write<Size: PageSize>(&self) -> Vec<u8> {
        assert_eq!(
            self.datoms.len() + 1,
            self.children.len(),
            "Node must have one more child than datoms.",
        );
        assert!(
            self.datoms.len() <= Size::CAPACITY,
            "Node has more datoms than a page can hold.",
        );

        let datoms_bytes: &[u8] = unsafe { transmute_slice(self.datoms) };
        let children_bytes: &[u8] = unsafe { transmute_slice(self.children) };

        let mut buffer = Vec::with_capacity(Size::SIZE);

        // First up is the datom array.
        buffer.extend_from_slice(&datoms_bytes);

        // If necessary, pad with zeros between the datom array, and the child
        // page id array.
        let num_zeros = Size::datoms_bytes_len() - datoms_bytes.len();
        buffer.extend((0..num_zeros).map(|_| 0x00));

        // Next up is the child array, also optionally padded.
        buffer.extend_from_slice(&children_bytes);

        let num_zeros = Size::children_bytes_len() - children_bytes.len();
        buffer.extend((0..num_zeros).map(|_| 0x00));

        // Finally, the header. For now it consists of two bytes, and we pad
        // with zeros to fill up the page.
        let header_len = Size::SIZE - Size::header_offset();
        debug_assert!(header_len < 42, "Header too large, could fit extra datom.");
        debug_assert!(header_len >= 2, "Header too small, cannot fit all fields.");
        debug_assert!(self.datoms.len() < 256, "Datoms length must fit in a byte.");
        buffer.push(self.level);              // Header byte 0.
        buffer.push(self.datoms.len() as u8); // Header byte 1.
        let num_zeros = header_len - 2;
        buffer.extend((0..num_zeros).map(|_| 0x00));

        debug_assert_eq!(buffer.len(), Size::SIZE, "Write must fill exactly one page.");

        buffer
    }

    /// Return whether the datom at the given index is a midpoint datom.
    ///
    /// A midpoint datom is one for which a child node exists. The midpoint
    /// datom is greater than all datoms in its child nodes. A datom which is
    /// not a _midpoint_ datom is said to be a _pending_ datom.
    pub fn is_midpoint_at(&self, index: usize) -> bool {
        self.children[index] != PageId::max()
    }

    /// Return the page id of the child for the midpoint at or after `index`.
    pub fn next_midpoint(&self, index: usize) -> Option<PageId> {
        for &page_id in self.children[index..].iter() {
            if page_id != PageId::max() {
                return Some(page_id);
            }
        }
        None
    }

    /// Return the page id of the child for the midpoint at or before `index`.
    pub fn previous_midpoint(&self, index: usize) -> Option<PageId> {
        for &page_id in self.children[..index + 1].iter().rev() {
            if page_id != PageId::max() {
                return Some(page_id);
            }
        }
        None
    }

    /// Return the index into the `datoms` array of the middle midpoint.
    fn median_midpoint(&self) -> usize {
        let mut num_midpoints = 0;
        for i in 0..self.children.len() {
            num_midpoints += if self.is_midpoint_at(i) { 1 } else { 0 };
        }

        assert!(
            num_midpoints == 0 || num_midpoints > 1,
            "Node must have either no children at all, or at least two otherwise.",
        );

        let mut num_visited = 0;
        for i in 0..self.children.len() {
            if self.is_midpoint_at(i) {
                num_visited += 1;
                if num_visited * 2 >= num_midpoints {
                    return i
                }
            }
        }

        unreachable!("Would have returned past median midpoint.")
    }

    /// Split this node at the given index (internal helper method).
    ///
    /// Returns (n0, midpoint, n1) where midpoint is the datom at the split index.
    fn split_impl(&self, split_index: usize) -> (Node, Datom, Node) {
        debug_assert!(
            self.datoms.len() >= 2,
            "Need at least three datoms to split: left, midpoint, and right."
        );
        let midpoint = self.datoms[split_index];
        let n0 = Node {
            level: 0,
            datoms: &self.datoms[..split_index],
            children: &self.children[..split_index],
        };
        let n1 = Node {
            level: 0,
            datoms: &self.datoms[split_index + 1..],
            children: &self.children[split_index + 1..],
        };
        (n0, midpoint, n1)
    }

    /// Split a leaf (level 0) node evenly.
    ///
    /// Returns (n0, midpoint, n1) such that all datoms in n0 are less than
    /// the midpoint, and all datoms in n1 are greater than the midpoint.
    fn split_leaf(&self) -> (Node, Datom, Node) {
        debug_assert_eq!(self.level, 0, "Leaves should have level 0.");

        let midpoint_index = self.datoms.len() / 2;
        debug_assert!(!self.is_midpoint_at(midpoint_index));

        self.split_impl(midpoint_index)
    }

    /// Split an internal node at the middle midpoint datom.
    ///
    /// Returns (n0, midpoint, pm, n1) such that all datoms in n0 are less than
    /// the midpoint, and all datoms in n1 are greater than the midpoint. The
    /// page id of the child that the midpoint pointed to is pm.
    fn split_internal(&self) -> (Node, Datom, PageId, Node) {
        debug_assert!(self.level > 0, "Internal nodes should have level > 0.");

        let midpoint_index = self.median_midpoint();
        let child_page = self.children[midpoint_index];
        debug_assert!(self.is_midpoint_at(midpoint_index));

        let (n0, midpoint, n1) = self.split_impl(midpoint_index);

        (n0, midpoint, child_page, n1)
    }
}

/// Write a sorted slice of datoms as a tree.
///
/// Returns the page id of the root node.
pub fn write_tree<S: Store>(store: &mut S, datoms: &[Datom]) -> io::Result<PageId> {
    // TODO: redo this thing.
    unimplemented!()
}

/// Points to a datom in the tree.
///
/// Datoms in the tree are ordered. A cursor identifies how to reach a given
/// datom, and also how to reach the next ones.
///
/// A cursor is a stack of indices into nodes. There is one index for each level
/// of the tree.
///
/// * `nodes[i]` contains the node pointed to, with `nodes[i].level == i`.
/// * `indices[i]` is an index into the datoms array of `nodes[i]`.
/// * `nodes[i]` is the node pointed at by `nodes[i + 1].children[j]`, where
///   `j >= indices[i + 1]` is the smallest `j` at which the children array
///   contains a page id (as opposed to being paired with a pending datom).
///
/// Note that the nodes are not stored in the cursor. Rather, they are stored
/// in the iterator when iterating.
struct Cursor {
    /// Stack of indices into the datom array, of the next datom to yield.
    ///
    /// The element at index `i` indexes into a node of level `i`.
    // TODO: Could be a fixed-size array, max depth is not very deep. The index
    // could also be u32 or even u16, as there aren't that much datoms per node.
    indices: Vec<usize>,
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
    pub fn find(&self, datom: &Datom) -> Cursor {
        panic!("TODO: Fix for multi-node tree.");

        let node = self.get(self.root_page);

        for (i, datom_i) in node.datoms.iter().enumerate() {
            match self.comparator.cmp(datom_i, datom) {
                Ordering::Less => continue,
                Ordering::Equal | Ordering::Greater => return Cursor {
                    indices: vec![i],
                },
            }
        }

        // Everything is less than the given datom, return a route past the end.
        Cursor {
            indices: vec![node.datoms.len()],
        }
    }

    /// Take the greatest datom out of a given node.
    ///
    /// This makes a copy of the node at the given page, with its greatest datom
    /// removed. If that node was not a leaf node, then that would leave a hole:
    /// the greatest datom in an internal node is always a midpoint, which has a
    /// child node. So we recurse, and extract the maximum from the child, which
    /// will become the maximum of its parent, filling the hole just created.
    ///
    /// TODO: As we write new nodes anyway, we should also flush any pending
    /// datoms that belong in the node we are about to write, while we have the
    /// opportunity.
    pub fn extract_max(&mut self, page: PageId) -> (PageId, Datom) {
        unimplemented!();
    }

    /// Split a given node into two.
    ///
    /// Returns `(n0, m0, n1)` such that:
    ///
    /// * `m0` is greater than any datom in node `n0`.
    /// * `m0` is smaller than any datom in node `n1`.
    pub fn split(&mut self, page: PageId) -> io::Result<(PageId, Datom, PageId)> {
        // Split the node and serialize the new nodes. This is done in a scope,
        // so we can later mutate the store again.
        let (n0_bytes, midpoint, n1_bytes) = {
            let node = self.get(page);

            assert!(node.datoms.len() >= 3, "Can only split node with at least three datoms.");

            let (n0, midpoint, n1) = if node.level == 0 {
                node.split_leaf()
            } else {
                let (mut n0, midpoint, _pm, n1) = node.split_internal();

                // TODO: Rewrite node n0, to have an extra datom to fit pm in
                // there. Or, perhaps easier, admid defeat on storing upper
                // bounds, and extend nodes to contain an extra child past the
                // last midpoint datom. Then we can extract the midpoint without
                // having to find a substitute, but iteration would need some
                // modifications.
                (n0, midpoint, n1)
            };

            (n0.write::<S::Size>(), midpoint, n1.write::<S::Size>())
        };

        let p0 = self.store.write_page(&n0_bytes)?;
        let p1 = self.store.write_page(&n1_bytes)?;

        Ok((p0, midpoint, p1))
    }
}

struct Iter<'a, Cmp: 'a + DatomOrd, S: 'a + Store> {
    /// The tree to iterate.
    tree: &'a HTree<'a, Cmp, S>,

    /// The nodes into which `begin.indices` point.
    nodes: Vec<Node<'a>>,

    /// The pointer to the next datom to yield.
    begin: Cursor,

    /// The pointer to the first datom not to yield.
    end: Cursor,
}

impl<'a, Cmp: DatomOrd, S: Store> Iter<'a, Cmp, S> {
    fn new(tree: &'a HTree<'a, Cmp, S>, begin: Cursor, end: Cursor) -> Iter<'a, Cmp, S> {
        Iter {
            tree: tree,
            // TODO: Find the correct level.
            nodes: vec![tree.get(tree.root_page)],
            begin: begin,
            end: end,
        }
    }

    /// Advance the begin pointer after yielding a datom.
    ///
    /// `level` is the index into the `nodes` and `indices` stacks
    /// from which we just yielded a datom.
    fn advance(&mut self, level: usize) {
        let old_index = self.begin.indices[level];
        self.begin.indices[level] = old_index + 1;

        assert!(
            old_index < self.nodes[level].datoms.len(),
            "Cannot have yielded from past the datoms array."
        );
        assert!(
            self.begin.indices[level] < self.nodes[level].children.len(),
            "Cannot increment out of bounds of children array."
        );

        // If we stepped over a midpoint datom, then we exhausted all of the
        // child nodes below it, so we need to replenish the levels below.
        if self.nodes[level].is_midpoint_at(old_index) {
            for k in (0..level).rev() {
                let index = self.begin.indices[k + 1];
                let child_pid = match self.nodes[k + 1].next_midpoint(index) {
                    Some(pid) => pid,
                    None => panic!("Node at level {} should have one more child.", k + 1),
                };
                let node = self.tree.get(child_pid);
                assert_eq!(node.level as usize, k);
                self.nodes[k] = node;
                self.begin.indices[k] = 0;
            }
        }
    }
}

impl<'a, Cmp: DatomOrd, S: Store> Iterator for Iter<'a, Cmp, S> {
    type Item = &'a Datom;

    fn next(&mut self) -> Option<&'a Datom> {
        // If we exhausted the range, then we are done.
        if self.begin.indices == self.end.indices {
            return None
        }

        // Inspect the datom that the begin pointer points at, and all datoms
        // along the way there from the root: these could be pending datoms, and
        // we need to merge them in now. Note that because each parent stores a
        // midpoint datom that bounds the datoms in its children from above,
        // indexing is within bounds at all levels: we will yield the parent
        // datoms last.
        let mut candidate = None;
        for (k, (&i, node)) in self
            .begin.indices.iter()
            .zip(&self.nodes)
            .enumerate()
        {
            // If we exhausted the datoms in this node, skip over it. There
            // might still be the last child node to iterate.
            if i >= node.datoms.len() { continue }

            let datom = &node.datoms[i];

            match candidate {
                None => candidate = Some((k, datom)),
                Some((level, least_datom)) => {
                    match self.tree.comparator.cmp(least_datom, datom) {
                        Ordering::Less => continue,
                        Ordering::Equal => panic!("Encountered duplicate datom in htree."),
                        Ordering::Greater => candidate = Some((k, datom)),
                    }
                }
            }
        }

        match candidate {
            None => panic!("Iter::next should find a candidate when not exhausted."),
            Some((level, least_datom)) => {
                self.advance(level);
                Some(least_datom)
            }
        }
    }
}

/// A mutable tree node backed by vectors on the heap.
///
/// `VecNode` is to `Node` as `Vec` is to `slice`, and `String` to `str`:
/// it owns its contents.
///
/// The mutable tree node can be used to accumulate novelty in memory. Once it
/// is large enough, a frozen copy of it can be written to disk, to be a new
/// node in the immutable tree.
#[derive(Clone)]
pub struct VecNode {
    /// See `Node::level`.
    level: u8,

    /// See `Node::datoms`.
    datoms: Vec<Datom>,

    /// See `Node::children`.
    children: Vec<PageId>,
}

impl VecNode {
    /// Return an empty node.
    pub fn new(level: u8) -> VecNode {
        VecNode {
            level: level,
            datoms: Vec::new(),
            children: Vec::new(),
        }
    }

    /// View this vec node as a regular immutable node.
    pub fn as_node(&self) -> Node {
        Node {
            level: self.level,
            datoms: &self.datoms[..],
            children: &self.children[..],
        }
    }
}

#[cfg(test)]
mod test {
    use std::iter;

    use datom::{Aid, Datom, Eid, Tid, Value};
    use store::{MemoryStore, PageId, PageSize563, PageSize4096, Store};
    use super::{HTree, Iter, Node, Cursor};

    #[test]
    fn node_write_after_read_is_identity() {
        // TODO: Generate some test data.
        let datom = Datom::assert(Eid::min(), Aid::max(), Value::min(), Tid::max());
        let datoms: Vec<_> = iter::repeat(datom).take(17).collect();
        let child_ids: Vec<_> = (0..18).map(|i| PageId(i)).collect();

        let node = Node {
            level: 0,
            datoms: &datoms[..],
            children: &child_ids[..],
        };

        let buffer_a = node.write::<PageSize4096>();

        let node_a = Node::from_bytes::<PageSize4096>(&buffer_a[..]);

        let buffer_b = node_a.write::<PageSize4096>();

        assert_eq!(buffer_a, buffer_b);
    }

    #[test]
    fn iter_iterates_single_page() {
        // Build a tree that consists of a single node. All datoms in it are
        // pending datoms, there are no midpoints because there are no children.

        let datoms: Vec<_> = (0..13)
            .map(|i| Datom::assert(Eid(i), Aid::max(), Value::min(), Tid::max()))
            .collect();
        let child_ids: Vec<_> = iter::repeat(PageId::max())
            .take(datoms.len() + 1)
            .collect();

        let node = Node {
            level: 0,
            datoms: &datoms[..],
            children: &child_ids[..],
        };

        type Size = PageSize563;
        let mut store = MemoryStore::<Size>::new();
        let page = store.write_page(&node.write::<Size>()).unwrap();

        let tree = HTree {
            root_page: PageId(0),
            comparator: &(),
            store: store,
        };

        let iter = Iter {
            tree: &tree,
            nodes: vec![tree.get(tree.root_page)],
            begin: Cursor {
                indices: vec![0],
            },
            end: Cursor {
                indices: vec![node.datoms.len()],
            },
        };

        for (&x, &y) in iter.zip(datoms.iter()) {
            assert_eq!(x, y);
        }
    }

    #[test]
    fn iter_iterates_depth_2_tree_no_inner_pending() {
        // Build a tree that consists of three nodes. The only pending datoms
        // are in the leaves; the root contains only midpoint datoms.

        let make_datom = |&i| Datom::assert(Eid(i), Aid::max(), Value::min(), Tid::max());
        let make_child_ids = |n| iter::repeat(PageId::max()).take(n).collect();

        let datoms0: Vec<_> = [0, 1, 2, 3].iter().map(make_datom).collect();
        let datoms2: Vec<_> = [4].iter().map(make_datom).collect();
        let datoms1: Vec<_> = [5, 6, 7, 8, 9].iter().map(make_datom).collect();

        let children0: Vec<_> = make_child_ids(datoms0.len() + 1);
        let children2 = vec![PageId(0), PageId(1)];
        let children1: Vec<_> = make_child_ids(datoms1.len() + 1);

        let node0 = Node {
            level: 0,
            datoms: &datoms0[..],
            children: &children0[..],
        };
        let node1 = Node {
            level: 0,
            datoms: &datoms1[..],
            children: &children1[..],
        };
        let node2 = Node {
            level: 1,
            datoms: &datoms2[..],
            children: &children2[..],
        };

        type Size = PageSize563;
        let mut store = MemoryStore::<Size>::new();
        let p0 = store.write_page(&node0.write::<Size>()).unwrap();
        let p1 = store.write_page(&node1.write::<Size>()).unwrap();
        let p2 = store.write_page(&node2.write::<Size>()).unwrap();

        assert_eq!(p0, PageId(0));
        assert_eq!(p1, PageId(1));
        assert_eq!(p2, PageId(2));

        let tree = HTree {
            root_page: PageId(2),
            comparator: &(),
            store: store,
        };

        let iter = Iter {
            tree: &tree,
            nodes: vec![
                tree.get(PageId(0)),
                tree.get(PageId(2)),
            ],
            begin: Cursor {
                indices: vec![0, 0],
            },
            end: Cursor {
                indices: vec![node1.datoms.len(), node2.datoms.len()],
            },
        };

        for (&datom, y) in iter.zip(0..10) {
            assert_eq!(datom.entity.0, y);
        }
    }

    // TODO: Dry up these tests.
    #[test]
    fn iter_iterates_depth_2_tree_with_inner_pending() {
        // Build a tree that consists of three nodes. The root node contains
        // pending datoms in addition to midpoints.

        let make_datom = |&i| Datom::assert(Eid(i), Aid::max(), Value::min(), Tid::max());
        let make_child_ids = |n| iter::repeat(PageId::max()).take(n).collect();

        let datoms0: Vec<_> = [0, 2, 3].iter().map(make_datom).collect();
        let datoms2: Vec<_> = [1, 4, 6, 7].iter().map(make_datom).collect();
        let datoms1: Vec<_> = [5, 8, 9].iter().map(make_datom).collect();

        let children0: Vec<_> = make_child_ids(datoms0.len() + 1);
        let children2 = vec![PageId::max(), PageId(0), PageId::max(), PageId::max(), PageId(1)];
        let children1: Vec<_> = make_child_ids(datoms1.len() + 1);

        let node0 = Node {
            level: 0,
            datoms: &datoms0[..],
            children: &children0[..],
        };
        let node1 = Node {
            level: 0,
            datoms: &datoms1[..],
            children: &children1[..],
        };
        let node2 = Node {
            level: 1,
            datoms: &datoms2[..],
            children: &children2[..],
        };

        type Size = PageSize563;
        let mut store = MemoryStore::<Size>::new();
        let p0 = store.write_page(&node0.write::<Size>()).unwrap();
        let p1 = store.write_page(&node1.write::<Size>()).unwrap();
        let p2 = store.write_page(&node2.write::<Size>()).unwrap();

        assert_eq!(p0, PageId(0));
        assert_eq!(p1, PageId(1));
        assert_eq!(p2, PageId(2));

        let tree = HTree {
            root_page: PageId(2),
            comparator: &(),
            store: store,
        };

        let iter = Iter {
            tree: &tree,
            nodes: vec![
                tree.get(PageId(0)),
                tree.get(PageId(2)),
            ],
            begin: Cursor {
                indices: vec![0, 0],
            },
            end: Cursor {
                indices: vec![node1.datoms.len(), node2.datoms.len()],
            },
        };

        for (&datom, y) in iter.zip(0..10) {
            assert_eq!(datom.entity.0, y);
        }
    }
}
