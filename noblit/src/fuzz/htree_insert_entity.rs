// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Fuzz test that asserts tree invariants after insertions.

use std::collections::HashSet;

use datom::{Datom, Aid, Eid, Value, Tid};
use fuzz::util::for_slices_u16;
use htree::{HTree, Node};
use index::{DatomOrd, Eavt};
use memory_store::{MemoryStore, MemoryHeap};
use store::{PageSize, StoreMut};

fn run<Size: PageSize>(full_data: &[u8]) {
    let mut store = MemoryStore::<Size>::new();
    let heap = MemoryHeap::new();
    let node = Node::empty_of_level(0);
    let root = store.write_page(&node.write::<Size>()).unwrap();
    let mut tree = HTree::new(root, Eavt, store, &heap);
    let mut unique_values = HashSet::new();
    let mut datoms = Vec::new();

    let mut tid = 0;

    // Insert a batch of datoms at a time, with increasing transaction id in
    // order not to create duplicates.
    for_slices_u16(full_data, |xs| {
        // Transaction id must be even.
        tid += 2;

        datoms.clear();
        unique_values.clear();

        for &x in xs.iter() {
            // Track unique values, and bail out early if there is a duplicate.
            // We can't insert duplicate datoms anyway, so this does not shrink
            // the space of operations. What it does do, is speed up the fuzzer
            // by not making it focus on pointless duplicates.
            if unique_values.contains(&x) { break }
            unique_values.insert(x);

            let datom = Datom::assert(Eid(x as u64), Aid::max(), Value::min(), Tid(tid));
            datoms.push(datom);
        }

        // The precondition for tree insertion is that datoms be sorted, and
        // that datoms are unique. Uniqueness is guaranteed by the hash set, but
        // we still need to sort. Previously we simply exited if the list was not
        // sorted, but it is difficult for the fuzzer to discover sorted lists by
        // chance. Sorting here is slower and adds more distracting branches as
        // interesting cases, but it also helps to discover interesting inputs
        // faster.
        datoms.sort_by(|x, y| (&tree.comparator as &dyn DatomOrd).cmp(x, y, &heap));

        #[cfg(not(fuzzing))]
        {
            println!("Inserting {} datoms:", datoms.len());
            for &datom in &datoms {
                println!("  {:?}", datom);
            }
        }

        tree.insert(&datoms[..]).unwrap();
        tree.check_invariants().unwrap();

        // Only allow zero datoms once in a while, otherwise the fuzzer will
        // focus too much on zero-datom inserts with very few interesting
        // operations in between.
        datoms.len() > 0 || (tid % 32 == 0)
    });
}

pub fn main(data: &[u8]) {
    use store::{PageSize256, PageSize568, PageSize4096};

    if data.len() == 0 { return }

    // Fuzz at different page sizes, in order to test all page sizes thoroughly.
    // The first byte determines the page size.
    match data[0] {
        0 => {
            dprintln!("Page size: 256.");
            run::<PageSize256>(&data[1..]);
        }
        1 => {
            dprintln!("Page size: 568.");
            run::<PageSize568>(&data[1..]);
        }
        2 => {
            dprintln!("Page size: 4096.");
            run::<PageSize4096>(&data[1..]);
        }
        _ => return,
    }
}
