// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::cmp::Ordering;

use datom::{Datom, Aid, Eid, Value, Tid};
use htree::{DatomOrd, HTree, Node};
use store::{MemoryStore, PageSize, Store};

/// Print, except when fuzzing.
///
/// This is useful for printf-style debugging of fuzz artifacts. During fuzzing,
/// this macro is a no-op, to keep the output clean and the fuzzer fast. But in
/// the `inspect_fuzz_artifact` binary, these prints leave a trace of how the
/// test sample was interpreted.
macro_rules! dprintln {
    () => (#[cfg(not(fuzzing))] println!());
    ($($arg:tt)*) => (#[cfg(not(fuzzing))] println!($($arg)*));
}

/// Evaluate a closure on byte slices of various lengths.
fn for_slices<F>(data: &[u8], mut f: F) where F: FnMut(&[u8]) -> bool {
    let mut left = data;

    while left.len() > 2 {
        // Read a 16-bit length prefix.
        let len = (left[0] as usize) << 8 | (left[1] as usize);

        // Stop on invalid lengths, rather than capping the slice. This improves
        // the chances of byte strings combining in interesting ways.
        if len > left.len() - 2 { break }

        if !f(&left[2..2 + len]) { break }
        left = &left[2 + len..];
    }
}

fn run<Size: PageSize>(full_data: &[u8]) {
    let mut store = MemoryStore::<Size>::new();
    let node = Node::empty_of_level(0);
    let root = store.write_page(&node.write::<Size>()).unwrap();
    let comparator = ();
    let mut tree = HTree::new(root, &comparator, store);

    let mut tid = 0;

    // Insert a batch of datoms at a time, with increasing transaction id in
    // order not to create duplicates.
    for_slices(full_data, |xs| {
        // Transaction id must be even.
        tid += 2;

        let mut datoms: Vec<Datom> = xs.iter().map(|&x|
            Datom::assert(Eid(x as u64), Aid::max(), Value::min(), Tid(tid))
        ).collect();

        // The precondition for tree insertion is that datoms be sorted, and
        // that datoms are unique. Sort to enforce this, then remove consecutive
        // duplicates. Previously we simply exited if the list was not sorted,
        // but it is difficult for the fuzzer to discover sorted lists by
        // chance. Sorting here is slower and adds more distracting branches as
        // interesting cases, but it also helps to discover interesting inputs
        // faster.
        datoms.sort_by(|x, y| (&comparator as &DatomOrd).cmp(x, y));
        datoms.dedup_by(|x, y| (&comparator as &DatomOrd).cmp(x, y) == Ordering::Equal);

        dprintln!("Inserting {} datoms:", datoms.len());
        for &datom in &datoms {
            dprintln!("  {:?}", datom);
        }

        tree.insert(&datoms[..]).unwrap();

        true
    });
}

pub fn main(data: &[u8]) {
    use store::{PageSize256, PageSize563, PageSize4096};

    if data.len() == 0 { return }

    // Fuzz at different page sizes, in order to test all page sizes thoroughly.
    // The first byte determines the page size.
    match data[0] {
        0 => {
            dprintln!("Page size: 256.");
            run::<PageSize256>(&data[1..]);
        }
        1 => {
            dprintln!("Page size: 563.");
            run::<PageSize563>(&data[1..]);
        }
        2 => {
            dprintln!("Page size: 4096.");
            run::<PageSize4096>(&data[1..]);
        }
        _ => return,
    }
}
