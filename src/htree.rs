// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines on-disk hitchhiker trees.

use std;
use datom::Datom;

/// A page id.
///
/// A page id is the offset of the page (counted in pages) from the start of the
/// file that contains it. For 4096-byte pages, page 0 starts at byte 0, page
/// 2 starts at byte 8106, etc.
pub struct Pid(u64);

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

type DatomOrd = Fn(&Datom, &Datom) -> std::cmp::Ordering;

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
