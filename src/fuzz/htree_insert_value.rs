// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Fuzz test that asserts tree invariants after insertions.

use std::collections::HashSet;
use std::io;

use datom::{Datom, Aid, Eid, Value, Tid};
use fuzz::util::for_slices;
use htree::{HTree, Node};
use index::{DatomOrd, Eavt};
use memory_store::{MemoryStore, MemoryPool};
use pool::{PoolMut, self};
use store::{PageSize, StoreMut};

fn run<Size: PageSize>(full_data: &[u8]) {
    let mut store = MemoryStore::<Size>::new();
    let mut pool = MemoryPool::new();
    let node = Node::empty_of_level(0);
    let root = store.write_page(&node.write::<Size>()).unwrap();
    let mut tree = HTree::new(root, Eavt, store, &mut pool);
    let mut unique_values = HashSet::new();
    let mut datoms = Vec::new();

    let mut tid = 0;

    // Insert a batch of datoms at a time, with increasing transaction id in
    // order not to create duplicates.
    for_slices(full_data, |tx_slice| {
        // Transaction id must be even.
        tid += 2;

        datoms.clear();
        unique_values.clear();

        dprintln!("Appending values:");
        for_slices(tx_slice, |datom_slice| {
            // Track unique values, and bail out early if there is a duplicate.
            // We can't insert duplicate datoms anyway, so apart from the
            // duplicate values that it would put in the pool, this does not
            // shrink the space of operations. What it does do, is speed up the
            // fuzzer by not making it focus on pointless duplicates.
            if unique_values.contains(&datom_slice) { return false }
            unique_values.insert(datom_slice);

            let value = match datom_slice.len() {
                0...7 => {
                    dprintln!("  inline: (len {}) {:?}", datom_slice.len(), datom_slice);
                    Value::from_bytes(datom_slice)
                }
                _ => {
                    let cid = tree.pool.append_bytes(datom_slice).unwrap();
                    dprintln!("  {}: (len {}) {:?}", cid.0, datom_slice.len(), datom_slice);
                    Value::from_const_bytes(cid)
                }
            };
            let datom = Datom::assert(Eid(0), Aid::max(), value, Tid(tid));
            datoms.push(datom);

            true
        });

        pool::check_invariants(&tree.pool);

        // The precondition for tree insertion is that datoms be sorted, and
        // that datoms are unique. Uniqueness is guaranteed by the hash set, but
        // we still need to sort. Previously we simply exited if the list was not
        // sorted, but it is difficult for the fuzzer to discover sorted lists by
        // chance. Sorting here is slower and adds more distracting branches as
        // interesting cases, but it also helps to discover interesting inputs
        // faster.
        {
            let pool = &tree.pool;
            datoms.sort_by(|x, y| (&tree.comparator as &DatomOrd).cmp(x, y, pool));
        }

        dprintln!("Inserting {} datoms:", datoms.len());
        for &datom in &datoms {
            dprintln!("  {:?}", datom);
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

pub fn generate_seed() -> io::Result<()> {
    use std::fs;
    use std::io::Write;

    let mut file = fs::File::create("fuzz/corpus/htree_insert_value/0")?;

    // Page size 256 indicator.
    file.write_all(&[0])?;

    // A tree of 258 elements fits in 3 layers of 6-element nodes (page size
    // 256). With 259, we need a fourth layer. So we write 258, and we let the
    // fuzzer discover an input of depth 3.
    let num_elements = 258;

    // Split the elements over two transactions, to make it slightly easier for
    // the fuzzer to find variations without creating duplicate values.
    for _tx in 0..2 {
        let n = num_elements / 2;

        // Write 16-bit transaction slice length prefix.
        let len = n * 4;
        file.write_all(&[(len >> 8) as u8, (len & 0xff) as u8])?;

        for i in 0..n {
            // A slice of length 2, with value 7i, both 16 bit.
            file.write_all(&[0, 2, ((i * 7) >> 8) as u8, ((i * 7) & 0xff) as u8])?;
        }
    }

    Ok(())
}
