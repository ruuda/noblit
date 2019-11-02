// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines on-disk hitchhiker trees.

use std::cmp::Ordering;
use std::io;
use std::iter;
use std::ops::Range;

use datom::Datom;
use index::DatomOrd;
use heap;
use store::{PageId, PageSize, self};

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
    /// Return an empty node of the given level.
    pub fn empty_of_level(level: u8) -> Node<'a> {
        use std::u64;
        Node {
            level: level,
            datoms: &[],
            // NOTE: We can't use PageId::max() here because const fn is not yet
            // stable.
            children: &[PageId(u64::MAX)],
        }
    }

    /// Create a copy of this node that owns its contents.
    pub fn to_vec_node(&self) -> VecNode {
        VecNode {
            level: self.level,
            datoms: self.datoms.to_vec(),
            children: self.children.to_vec(),
        }
    }

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

    /// Serialize the node to bytes, for writing to a store.
    pub fn write<Size: PageSize>(&self) -> Vec<u8> {
        assert_eq!(
            self.datoms.len() + 1,
            self.children.len(),
            "Node must have one more child than datoms.",
        );
        assert!(
            self.datoms.len() <= Size::CAPACITY,
            "Node has {} datoms, more than a page can hold.", self.datoms.len(),
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

    /// Return the number of children that this node has.
    pub fn num_children(&self) -> usize {
        let mut n = 0;

        for i in 0..self.children.len() {
            n += if self.is_midpoint_at(i) { 1 } else { 0 };
        }

        debug_assert!(
            n == 0 || n > 1,
            "Node must have either no children at all, or at least two otherwise.",
        );

        n
    }

    /// Return whether this node has any pending datoms.
    pub fn has_pending_datoms(&self) -> bool {
        self.children.iter().any(|&pid| pid == PageId::max())
    }

    /// Return the longest span of pending datoms.
    ///
    /// The returned range can be safely used to index into `datoms`. The child
    /// node into which the datoms need to be flushed is `children[range.end]`.
    pub fn longest_pending_span(&self) -> Range<usize> {
        let mut start = 0;
        let mut range = 0..0;

        // Find the longest span of pending datoms.
        for i in 0..self.children.len() {
            if self.is_midpoint_at(i) {
                let len = i - start;
                if len > range.len() {
                    range = start..i;
                }
                start = i + 1;
            }
        }

        range
    }

    /// Assert that the slice of datoms is monotonically increasing.
    ///
    /// That is, every datom should be strictly greater than the preceding
    /// datom. This rules out duplicates too.
    #[cfg(debug_assertions)]
    fn check_datoms_increasing<
        Cmp: DatomOrd,
        Heap: heap::Heap,
    > (
        &self,
        comparator: &Cmp,
        heap: &Heap,
        datoms: &[Datom]
    ) {
        let mut minimum = None;
        for (i, datom) in datoms.iter().enumerate() {
            if let Some(inf) = minimum {
                assert_eq!(
                    comparator.cmp(inf, datom, heap), Ordering::Less,
                    "Monotonicity violation at index {}: datom ({:?}) >= ({:?}).", i, inf, datom
                );
            }
            minimum = Some(datom);
        }
    }

    /// Insert datoms into a node.
    ///
    /// Construct a new node, which incudes all of the datoms in the current
    /// node, and additionally all of `datoms` as pending datoms.
    fn insert<
        Cmp: DatomOrd,
        Heap: heap::Heap,
    > (
        &self,
        comparator: &Cmp,
        heap: &Heap,
        datoms: &[Datom]
    ) -> VecNode
    {
        #[cfg(debug_assertions)]
        self.check_datoms_increasing(comparator, heap, datoms);

        let other_datoms = datoms;
        let new_len = self.datoms.len() + other_datoms.len();

        let mut new_datoms = Vec::with_capacity(new_len);
        let mut new_children = Vec::with_capacity(new_len + 1);

        // Do a merge sort of the two datom slices.
        let mut i = 0;
        let mut j = 0;

        loop {
            // Stop if both slices are exhaused, copy the remainder if only one
            // slice is exhausted.
            let (i_end, j_end) = (i == self.datoms.len(), j == other_datoms.len());
            match () {
                _ if i_end && j_end => {
                    assert_eq!(new_datoms.len(), i + j);
                    break
                }
                _ if i_end => {
                    new_datoms.push(other_datoms[j]);
                    new_children.push(PageId::max());
                    j += 1;
                    continue
                }
                _ if j_end => {
                    new_datoms.push(self.datoms[i]);
                    new_children.push(self.children[i]);
                    i += 1;
                    continue
                }
                _ => {}
            }

            // Neither slice is exhausted, compare the two candidates.
            let datom_i = self.datoms[i];
            let datom_j = other_datoms[j];
            match comparator.cmp(&datom_i, &datom_j, heap) {
                Ordering::Equal => panic!("Encountered duplicate datom in htree."),
                Ordering::Less => {
                    new_datoms.push(datom_i);
                    new_children.push(self.children[i]);
                    i += 1;
                },
                Ordering::Greater => {
                    new_datoms.push(datom_j);
                    new_children.push(PageId::max());
                    j += 1;
                }
            }
        }

        // Copy over the final child pointer.
        new_children.push(self.children[i]);

        VecNode {
            level: self.level,
            datoms: new_datoms,
            children: new_children,
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

    /// Copy datoms into a vec node of level 0.
    pub fn from_slice(datoms: &[Datom]) -> VecNode {
        VecNode {
            level: 0,
            datoms: datoms.to_vec(),
            children: iter::repeat(PageId::max()).take(datoms.len() + 1).collect(),
        }
    }
}

/// A hitchhiker tree.
///
/// A hitchhiker tree is similar to a persistent (as in _immutable_, not as in
/// _durable_) B-tree, with some modifications that reduce write amplification
/// on inserts. See doc/htree.md for more details.
///
/// The `HTree` struct is a thin wrapper around the `Store`, which provides
/// storage for its tree nodes. A tree node is stored in exactly one page in
/// the store.
pub struct HTree<Cmp, Store, Heap> {
    /// The page that contains the root node.
    // TODO: Remove this member and pass it in externally?
    pub root_page: PageId,

    /// Ordering on datoms.
    pub comparator: Cmp,

    /// The backing store to read pages from and append to.
    pub store: Store,

    /// The backing large value heap to read large values from.
    pub heap: Heap,
}

/// Read the node with the given page id from the store.
fn get_node<Store: store::Store>(store: &Store, page: PageId) -> Node {
    Node::from_bytes::<Store::Size>(store.get(page))
}

impl<Cmp: DatomOrd, Store: store::Store, Heap: heap::Heap> HTree<Cmp, Store, Heap> {
    /// Create a new view into an existing index in the store.
    pub fn new(root_page: PageId, comparator: Cmp, store: Store, heap: Heap) -> HTree<Cmp, Store, Heap> {
        HTree {
            root_page: root_page,
            comparator: comparator,
            store: store,
            heap: heap,
        }
    }

    /// Write a new index to the store that contains the given datoms.
    ///
    /// The datoms need not be sorted, so the same slice can be used to
    /// construct multiple trees with different orderings. Large values
    /// do need to be stored on the heap already though.
    pub fn initialize(
        comparator: Cmp,
        store: Store,
        heap: Heap,
        datoms: &[Datom]
    ) -> io::Result<HTree<Cmp, Store, Heap>>
    where Store: store::StoreMut
    {
        let mut node = VecNode::from_slice(datoms);

        // TODO: Debug-assert that large values are on the heap.

        // Inserting datoms into the tree requires that they are sorted. Now
        // that we made a copy of the datoms slice anyway, we can sort it in
        // place before we insert it.
        node.datoms.sort_by(|x, y| comparator.cmp(x, y, &heap));

        let mut tree = HTree {
            root_page: PageId::max(), // Will be overwritten immediately.
            comparator: comparator,
            store: store,
            heap: heap,
        };

        tree.root_page = tree.write_root(node)?;

        Ok(tree)
    }

    /// Return 1 + the level of the root node.
    ///
    /// Leaf nodes have level 0, so a single-node tree has height 1.
    pub fn height(&self) -> u8 {
        let root_node = get_node(&self.store, self.root_page);
        1 + root_node.level
    }

    /// Split a given node into pieces, and write them.
    ///
    /// Returns a `VecNode` with the split midpoints, and the children pointing
    /// to the nodes that were split off. The returned node satisfies:
    ///
    /// * `datoms[i]` is greater than any datom in node `children[i]`.
    /// * `datoms[i]` is less than any datom in node `children[i + 1]`.
    ///
    /// While the pieces that the node is split into all fit in a page, the
    /// returned `VecNode` of midpoints may need to be split further to fit.
    fn split(&mut self, node: &Node) -> io::Result<VecNode> where Store: store::StoreMut {
        assert!(node.datoms.len() >= 2, "Can only split node with at least two datoms.");

        // Compute the number of pages we need, rounding up. For every split we
        // make, there is one less datom to account for (because the datom
        // becomes a midpoint), but we don't take that into account, spliting a
        // node into nodes that are all full is a bad idea anyway, because then
        // they cannot hold pending datoms.
        let num_pages = (node.datoms.len() + Store::Size::CAPACITY - 1) / Store::Size::CAPACITY;

        debug_assert!(
            // Every page can fit CAPACITY datoms, and num_pages - 1 midpoint
            // datoms are retured as part of the result.
            node.datoms.len() <= num_pages * (Store::Size::CAPACITY + 1) - 1,
            "In split, the computed number of pages must be able to fit all datoms.",
        );

        let mut datoms = Vec::with_capacity(num_pages - 1);
        let mut children = Vec::with_capacity(num_pages);
        let children_per_page = node.children.len() / num_pages;

        for p in 0..num_pages {
            // Compute the lower (inclusive) and upper (exclusive) bound of the
            // slice.
            let begin = children_per_page * p;
            let end = (begin + children_per_page).min(node.children.len());

            // Extract one slice and write it. There is one less datom than
            // children: that datom will become the midpoint in the parent.
            let part = Node {
                level: node.level,
                datoms: &node.datoms[begin..end - 1],
                children: &node.children[begin..end],
            };
            let part_page_id = self.store.write_page(&part.write::<Store::Size>())?;

            // Track the page id as a new child of the upper node, and use the
            // final datom in the slice as the midpoint datom. Only the last
            // slice does not contain a final datom, but that is fine, because
            // the vec node we return should not either.
            if p + 1 < num_pages {
                datoms.push(node.datoms[end - 1]);
            }
            children.push(part_page_id);
        }

        let result = VecNode {
            level: node.level + 1,
            datoms: datoms,
            children: children,
        };

        Ok(result)
    }

    /// Flush pending datoms of the node into one child node.
    ///
    /// Flushes the longest span of pending datoms. There must be at least one
    /// pending datom in order to flush. This method panics if there are no
    /// pending datoms.
    ///
    /// This method updates the node to be flushed in place, such that when the
    /// method returns, the child id has been updated for the child into which
    /// datoms were flushed, and the flushed datoms have been removed from the
    /// input node.
    ///
    /// To flush all pending datoms, call this method repeatedly.
    fn flush(&mut self, node: &mut VecNode) -> io::Result<()> where Store: store::StoreMut {
        assert!(node.level > 0, "Cannot flush leaf nodes: no children to flush into.");

        let span = node.as_node().longest_pending_span();
        assert!(span.len() > 0, "Trying to flush node with no pending datoms.");

        // Make a new version of the child node, with the pending datoms flushed
        // into it. We get at least one new child page id, and possibly
        // additional midpoint datoms and child pages to add in this node.
        let child = node.children[span.end];
        let mut to_merge = self.insert_into(child, &node.datoms[span.start..span.end])?;

        debug_assert_eq!(
            to_merge.level, node.level,
            "Result of insert_into must have same level as node to flush.",
        );
        debug_assert_eq!(
            to_merge.datoms.len() + 1, to_merge.children.len(),
            "Result of insert_into must have one more child than midpoint datoms.",
        );
        debug_assert!(
            to_merge.datoms.len() <= span.len(),
            "Flushing must not make a node bigger.",
        );

        // Update the current node to remove the flushed datoms, and also the
        // old child pointer. Then insert the new midpoints (if there are any),
        // and the new child pointer.
        node.datoms.splice(span.start..span.end, to_merge.datoms.drain(..));
        node.children.splice(span.start..span.end + 1, to_merge.children.drain(..));

        Ok(())
    }

    /// Insert datoms into a given node.
    ///
    /// Inserting into a node may cause the subtree to overflow, requiring node
    /// splits. The midpoints for these splits need to be pushed up one level,
    /// so they are returned from the insert.
    ///
    /// The last child of the returned node contains the page id of the new
    /// node, where the datoms have been inserted. If the insert required
    /// splits, then the midpoint datoms and associated page ids precede the
    /// final child, respecting the ordering.
    ///
    /// Note that the returned `VecNode` might contain more datoms than would
    /// fit in a page, it may need to be split further to become writeable.
    fn insert_into(&mut self, page: PageId, datoms: &[Datom]) -> io::Result<VecNode>
    where Store: store::StoreMut
    {
        // Make a heap-allocated copy of the node to insert into, and merge-sort
        // insert the datoms into it.
        let mut new_node = get_node(&self.store, page).insert(&self.comparator, &self.heap, datoms);

        // If the new node does not fit in a page, try flushing pending datoms
        // until it fits.
        while new_node.level > 0
            && new_node.datoms.len() > Store::Size::CAPACITY
            && new_node.as_node().has_pending_datoms()
        {
            self.flush(&mut new_node)?;
        }

        // If flushing did not help (enough), then we need to split the node.
        if new_node.datoms.len() > Store::Size::CAPACITY {
            assert!(
                !new_node.as_node().has_pending_datoms() || new_node.level == 0,
                "Only at level 0, an oversized node can have pending datoms.",
            );
            return self.split(&new_node.as_node());
        }

        // If the node is small enough to fit in one page, then we just write
        // the new node, and return its id as the sole child of a `VecNode`.
        // (A `VecNode` with more children is only returned if we do splits.)
        let new_page_id = self.store.write_page(&new_node.as_node().write::<Store::Size>())?;

        let result = VecNode {
            level: new_node.level + 1,
            datoms: vec![],
            children: vec![new_page_id],
        };

        Ok(result)
    }

    /// Write a new tree root, return the root page id.
    ///
    /// This is used to implement insertion as well as initialization of a
    /// non-empty tree. The node should contain all datoms that need to be
    /// inserted into a higher level than the current root. In the case of
    /// initialization, there is no current root, and the node will become
    /// the root (it might be split, if required). If the node has no datoms,
    /// then it should point to a single child node which is the new root. In
    /// that case nothing is written.
    fn write_root(&mut self, mut node: VecNode) -> io::Result<PageId> where Store: store::StoreMut {
        // The node contains all datoms that need to be inserted into a new
        // higher level in the tree. There might be so many of those, that one
        // page is not enough. Then we need to split, and create yet a higher
        // level, until there are few enough midpoints to fit in one page.
        while node.datoms.len() > Store::Size::CAPACITY {
            node = self.split(&node.as_node())?;
        }

        // If we get only one new page id, then that is the new root. If there
        // is more, then the tree grows in height, and we write the entire new
        // node as the new root.
        let root_page = match node.children.len() {
            0 => panic!("Result of insert_into must contain at least one child page."),
            1 => node.children[0],
            _ => self.store.write_page(&node.as_node().write::<Store::Size>())?,
        };

        Ok(root_page)
    }

    /// Insert datoms into the tree, return the new root page.
    pub fn insert(&mut self, datoms: &[Datom]) -> io::Result<PageId>
    where Store: store::StoreMut {
        let old_root_page = self.root_page;
        let new_root_node = self.insert_into(old_root_page, datoms)?;
        let new_root_page = self.write_root(new_root_node)?;
        // TODO: Maybe not return the new page id? The function is &mut self
        // anyway, so one might reasonably expect it to mutate the root pointer.
        self.root_page = new_root_page;
        Ok(new_root_page)
    }

    /// Assert that the node at the given page, and its children, are well-formed.
    ///
    /// This is used in tests and during fuzzing. Checks furthermore that all
    /// datoms in the node are greater than the infimum, if one is provided, and
    /// smaller than the supremum, if one is provided.
    fn check_invariants_at(
        &self,
        page: PageId,
        expected_level: Option<u8>,
        infimum: Option<&Datom>,
        supremum: Option<&Datom>,
    ) -> io::Result<()> {
        let node = get_node(&self.store, page);

        if let Some(level) = expected_level {
            assert_eq!(node.level, level);
        }

        // We track two infima: one for the datoms array, and one for the
        // children. For the datoms array, we need every datom to be larger than
        // the previous one. For the children, we need every child to be larger
        // than the previous midpoint datom.
        let mut all_infimum = infimum;
        let mut mid_infimum = infimum;

        for (i, datom) in node.datoms.iter().enumerate() {
            if let Some(inf) = all_infimum {
                assert_eq!(
                    self.comparator.cmp(inf, datom, &self.heap), Ordering::Less,
                    "Violation at page {:?}: infimum ({:?}) >= datom {} ({:?}).", page, inf, i, datom
                );
            }
            if node.is_midpoint_at(i) {
                assert!(
                    node.level > 0,
                    "Violation at page {:?}: node {} at level 0 has child.", page, i,
                );
                self.check_invariants_at(
                    node.children[i],
                    // The level of the child should be one less than the parent.
                    Some(node.level - 1),
                    mid_infimum,
                    Some(datom)
                )?;
                mid_infimum = Some(datom);
            }
            all_infimum = Some(datom);
        }

        if let (Some(inf), Some(sup)) = (all_infimum, supremum) {
            assert_eq!(
                self.comparator.cmp(inf, sup, &self.heap), Ordering::Less,
                "Violation at page {:?}, final infimum >= supremum.", page,
            );
        }

        if node.is_midpoint_at(node.datoms.len()) {
            self.check_invariants_at(
                node.children[node.datoms.len()],
                Some(node.level - 1),
                mid_infimum,
                supremum,
            )?;
        } else {
            assert_eq!(
                node.level, 0,
                "Violation at page {:?}: only level 0 nodes can lack a final child.", page,
            );
        }

        Ok(())
    }

    /// Assert that the tree is well-formed, that all invariants hold.
    ///
    /// This is used in tests and during fuzzing. The heap is not checked; it
    /// can be checked separately with `heap::check_invariants`.
    pub fn check_invariants(&self) -> io::Result<()> {
        self.check_invariants_at(self.root_page, None, None, None)
    }
}

impl<'a, Cmp: DatomOrd, Store: store::Store, Heap: heap::Heap> HTree<Cmp, &'a Store, &'a Heap> {
    /// Locate the first datom that is greater than or equal to the queried one.
    ///
    /// Returns a cursor that points to the datom, which can be used as the
    /// lower bound for an iterator. Also returns a vector of nodes that the
    /// indices point into.
    fn find(&self, datom: &Datom) -> (Cursor, Vec<Node<'a>>) {
        let mut node = get_node(self.store, self.root_page);
        let mut indices = Vec::with_capacity(node.level as usize + 1);
        let mut nodes = Vec::with_capacity(node.level as usize + 1);

        loop {
            nodes.push(node.clone());

            // Find the index of the first datom >= the search datom. The
            // default value is one past the last datom: if every datom in the
            // node is smaller than the search datom, the cursor should point
            // past the end.
            let mut index = node.datoms.len();

            for (i, datom_i) in node.datoms.iter().enumerate() {
                match self.comparator.cmp(datom_i, datom, &self.heap) {
                    Ordering::Less => continue,
                    Ordering::Equal | Ordering::Greater => {
                        index = i;
                        break;
                    }
                }
            }

            indices.push(index);

            if node.level == 0 {
                break
            } else {
                let pid = node
                    .next_midpoint(index)
                    .expect("Node at level > 0 must have a final child.");
                node = get_node(self.store, pid);
            }
        }

        // We added the nodes top to bottom, but the cursor expects them in the
        // opposite order, lowest level first, so reverse them.
        indices.reverse();
        nodes.reverse();

        let result = Cursor {
            indices: indices,
        };

        (result, nodes)
    }

    /// Return an iterator over the range `[begin..end)`.
    pub fn into_iter(self, begin: &Datom, end: &Datom) -> Iter<'a, Cmp, Store, Heap> {
        let (cursor_begin, nodes) = self.find(begin);
        let (cursor_end, _) = self.find(end);
        Iter {
            store: self.store,
            heap: self.heap,
            comparator: self.comparator,
            nodes: nodes,
            begin: cursor_begin,
            end: cursor_end,
        }
    }
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
pub struct Cursor {
    /// Stack of indices into the datom array, of the next datom to yield.
    ///
    /// The element at index `i` indexes into a node of level `i`.
    // TODO: Could be a fixed-size array, max depth is not very deep. The index
    // could also be u32 or even u16, as there aren't that much datoms per node.
    indices: Vec<usize>,
}

/// An iterator over a range of a tree.
///
/// The iterator yields datoms in ascending order.
pub struct Iter<'a, Cmp: 'a, Store: 'a, Heap: 'a> {
    /// The store that contains the tree to iterate.
    store: &'a Store,

    /// The backing heap for large values.
    heap: &'a Heap,

    /// Datom ordering used by the tree.
    comparator: Cmp,

    /// The nodes into which `begin.indices` point.
    nodes: Vec<Node<'a>>,

    /// The pointer to the next datom to yield.
    begin: Cursor,

    /// The pointer to the first datom not to yield.
    end: Cursor,
}

impl<'a, Cmp: DatomOrd, Store: store::Store, Heap> Iter<'a, Cmp, Store, Heap> {
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
                let node = get_node(self.store, child_pid);
                assert_eq!(node.level as usize, k);
                self.nodes[k] = node;
                self.begin.indices[k] = 0;
            }
        }
    }
}

impl<'a, Cmp: DatomOrd, Store: store::Store, Heap: heap::Heap> Iterator for Iter<'a, Cmp, Store, Heap> {
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
                Some((_level, least_datom)) => {
                    match self.comparator.cmp(least_datom, datom, self.heap) {
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

#[cfg(test)]
mod test {
    use std::iter;

    use datom::{Aid, Datom, Eid, Tid, Value};
    use store::{PageId, PageSize, PageSize256, PageSize563, PageSize4096, StoreMut};
    use memory_store::{MemoryStore, MemoryHeap};
    use super::{HTree, Iter, Node, Cursor};
    use index::Eavt;

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
    fn initialize_is_equal_to_manual_initialization() {
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
        let mut store1 = MemoryStore::<Size>::new();
        store1.write_page(&node.write::<Size>()).unwrap();

        let heap = MemoryHeap::new();
        let mut store2 = MemoryStore::<Size>::new();
        HTree::initialize(Eavt, &mut store2, &heap, &node.datoms).unwrap();

        assert_eq!(store1.as_bytes(), store2.as_bytes());
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
        let heap = MemoryHeap::new();
        let mut store = MemoryStore::<Size>::new();
        store.write_page(&node.write::<Size>()).unwrap();

        let iter = Iter {
            store: &store,
            heap: &heap,
            comparator: Eavt,
            nodes: vec![node],
            begin: Cursor {
                indices: vec![0],
            },
            end: Cursor {
                indices: vec![datoms.len()],
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
        let heap = MemoryHeap::new();
        let mut store = MemoryStore::<Size>::new();
        let p0 = store.write_page(&node0.write::<Size>()).unwrap();
        let p1 = store.write_page(&node1.write::<Size>()).unwrap();
        let p2 = store.write_page(&node2.write::<Size>()).unwrap();

        assert_eq!(p0, PageId(0));
        assert_eq!(p1, PageId(1));
        assert_eq!(p2, PageId(2));

        let tree = HTree {
            root_page: PageId(2),
            comparator: Eavt,
            store: &store,
            heap: &heap,
        };

        let iter = tree.into_iter(&make_datom(&0), &make_datom(&10));
        println!("begin: {:?}, end: {:?}", iter.begin.indices, iter.end.indices);

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
        let heap = MemoryHeap::new();
        let mut store = MemoryStore::<Size>::new();
        let p0 = store.write_page(&node0.write::<Size>()).unwrap();
        let p1 = store.write_page(&node1.write::<Size>()).unwrap();
        let p2 = store.write_page(&node2.write::<Size>()).unwrap();

        assert_eq!(p0, PageId(0));
        assert_eq!(p1, PageId(1));
        assert_eq!(p2, PageId(2));

        let tree = HTree {
            root_page: PageId(2),
            comparator: Eavt,
            store: &store,
            heap: &heap,
        };

        let iter = tree.into_iter(&make_datom(&0), &make_datom(&10));

        for (&datom, y) in iter.zip(0..10) {
            assert_eq!(datom.entity.0, y);
        }
    }

    #[test]
    fn tree_insert_accepts_new_datoms() {
        let make_datom = |i| Datom::assert(Eid(i), Aid::max(), Value::min(), Tid::max());

        type Size = PageSize563;
        let heap = MemoryHeap::new();
        let mut store = MemoryStore::<Size>::new();
        let node = Node::empty_of_level(0);
        let root = store.write_page(&node.write::<Size>()).unwrap();
        let mut tree = HTree::new(root, Eavt, store, heap);

        for i in 0..500 {
            let datoms = &[
                make_datom(i * 1000 + 0),
                make_datom(i * 1000 + 1),
                make_datom(i * 1000 + 2),
                make_datom(i * 1000 + 3),
            ];
            tree.insert(&datoms[..]).unwrap();
            tree.check_invariants().unwrap();
        }
    }

    #[test]
    fn tree_insert_accepts_base_square_datoms() {
        // This test is a regression test that catches an edge case in insert,
        // where there are so many datoms to insert, that splitting to create a
        // new upper layer is not sufficient; we need to create more than one
        // upper layer.
        let make_datom = |i| Datom::assert(Eid(i), Aid::max(), Value::min(), Tid::max());

        type Size = PageSize256;
        let capacity = <Size as PageSize>::CAPACITY as u64;
        let heap = MemoryHeap::new();
        let mut store = MemoryStore::<Size>::new();
        let node = Node::empty_of_level(0);
        let root = store.write_page(&node.write::<Size>()).unwrap();
        let mut tree = HTree::new(root, Eavt, store, heap);

        // One node in the tree can hold `capacity` datoms. A two-layer tree can
        // hold `capacity^2` datoms, plus `capacity` for the midpoints. If we
        // add one more, that does not fit in a 2-layer tree, we need one more.
        let n = 1 + capacity * (capacity + 1);
        let datoms: Vec<Datom> = (0..n as u64).map(make_datom).collect();

        tree.insert(&datoms[..]).unwrap();
        tree.check_invariants().unwrap();
    }
}
