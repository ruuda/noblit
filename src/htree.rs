// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines on-disk hitchhiker trees.

use std;
use std::io;
use datom::Datom;

/// A page id.
///
/// A page id is the offset of the page (counted in pages) from the start of the
/// file that contains it. For 4096-byte pages, page 0 starts at byte 0, page
/// 2 starts at byte 8192, etc.
pub struct Pid(u64);

/// The number of bytes in a page. A page stores exactly one tree node.
pub const PAGE_SIZE: usize = 4096;

/// A tree node.
pub struct Node<'a> {
    /// Depth: 0 for leaves, 1 + depth of children for interior nodes.
    pub depth: u8,

    /// Datoms stored in this node.
    ///
    /// * All datoms in `children[i]` are less than `midpoints[i]`.
    /// * All datoms in `children[i + 1]` are greater than `midpoints[i]`.
    pub midpoints: &'a [Datom],

    /// Child node page indices.
    ///
    /// The length is `midpoints.len() + 1`.
    pub children: &'a [Pid],

    /// Datoms that need to be flushed into child nodes.
    pub pending: &'a [Datom],
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
        let num_children = bytes[1] as usize;
        let num_pending = bytes[2] as usize;

        // The datom array stores the midpoints and pending datoms after each
        // other, and it starts at byte 32.
        let num_datom_bytes = 32 * (num_children + num_pending - 1);
        let datoms: &[Datom] = unsafe {
            transmute_slice(&bytes[32..32 + num_datom_bytes])
        };

        // The array with child page ids is at the end of the page.
        let num_children_bytes = 8 * num_children;
        let children: &[Pid] = unsafe {
            transmute_slice(&bytes[PAGE_SIZE - num_children_bytes..])
        };

        Node {
            depth: bytes[0],
            midpoints: &datoms[..num_children - 1],
            pending: &datoms[num_children - 1..],
            children: children,
        }
    }

    /// Write the node to backing storage.
    pub fn write<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        // The first 32 bytes (the size of a datom) do not contain a datom,
        // but the node header. See also doc/htree.md for more information.
        let mut header = [0u8; 32];
        header[0] = self.depth;
        header[1] = self.children.len() as u8;
        header[2] = self.pending.len() as u8;
        writer.write_all(&header[..])?;

        let midpoint_bytes: &[u8] = unsafe { transmute_slice(self.midpoints) };
        let pending_bytes: &[u8] = unsafe { transmute_slice(self.pending) };
        let children_bytes: &[u8] = unsafe { transmute_slice(self.children) };

        // After the header is the datom array, first all midpoints, then all
        // pending datoms.
        writer.write_all(&midpoint_bytes)?;
        writer.write_all(&pending_bytes)?;
        let num_bytes_written = 32 + midpoint_bytes.len() + pending_bytes.len();

        // If necessary, pad with zeros between the datom array, and the child
        // page id array.
        let num_zeros = PAGE_SIZE - num_bytes_written - children_bytes.len();
        let zeros = [0u8; 32];
        let mut num_zeros_written = 0;
        while num_zeros_written < num_zeros {
            let num_zeros_left = num_zeros - num_zeros_written;
            let num_zeros_write = num_zeros_left.min(zeros.len());
            num_zeros_written += writer.write(&zeros[..num_zeros_write])?;
        }

        // At the end of the page is the child page id array.
        writer.write_all(&children_bytes)?;

        debug_assert_eq!(
            num_bytes_written + num_zeros_written + children_bytes.len(),
            PAGE_SIZE
        );

        Ok(())
    }
}

/// An ordering on datoms.
trait DatomOrd {
    fn cmp(lhs: &Datom, rhs: &Datom) -> std::cmp::Ordering;
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

    pub fn get(node: Pid) -> Node<'a> {
        unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use super::{Node, Pid};

    #[test]
    fn node_write_after_read_is_identity() {
        use std::iter;
        use datom::{Aid, Datom, Eid, Operation, Tid, Value};

        // TODO: Generate some test data.
        let datom = Datom::assert(Eid::min(), Aid::max(), Value::min(), Tid::max());
        let datoms: Vec<_> = iter::repeat(datom).take(17).collect();
        let child_ids = [Pid(2), Pid(3), Pid(5), Pid(7), Pid(11)];

        // TODO: Test various combinations of child nodes and pending datoms.
        let node = Node {
            depth: 0,
            midpoints: &datoms[..4],
            pending: &datoms[4..],
            children: &child_ids[..],
        };

        let mut buffer_a: Vec<u8> = Vec::new();
        node.write(&mut buffer_a);

        let node_a = Node::from_bytes(&buffer_a[..]);

        let mut buffer_b: Vec<u8> = Vec::new();
        node_a.write(&mut buffer_b);

        assert_eq!(buffer_a, buffer_b);
    }
}
