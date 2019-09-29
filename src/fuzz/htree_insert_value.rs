// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Fuzz test that asserts tree invariants after insertions.

use std::cmp::Ordering;
use std::io;

use datom::{Datom, Aid, Eid, Value, Tid};
use fuzz::util::Cursor;
use htree::{HTree, Node};
use index::{DatomOrd, Eavt};
use memory_store::{MemoryStore, MemoryPool};
use pool::{PoolMut, self};
use store::{PageSize, StoreMut};

fn run<'a, Size: PageSize>(mut cursor: Cursor<'a>) -> Option<()> {
    let mut store = MemoryStore::<Size>::new();
    let mut pool = MemoryPool::new();

    let node = Node::empty_of_level(0);
    let root = store.write_page(&node.write::<Size>()).unwrap();
    let mut tree = HTree::new(root, Eavt, store, &mut pool);

    // Reserve capacity for a large-ish number of datoms, to prevent most
    // reallocations during fuzzing, thereby reducing distracting control flow.
    let mut datoms = Vec::with_capacity(255);
    let mut tid = 0;

    loop {
        match cursor.take_u8()? {
            0 => {
                dprintln!("VALUE");
                let len = cursor.take_u8()?;
                let value_slice = cursor.take_slice(len as usize)?;

                let value = match len {
                    0..=7 => {
                        dprintln!("  inline, len {}: {:?}", len, value_slice);
                        Value::from_bytes(value_slice)
                    }
                    _ => {
                        let cid = tree.pool.append_bytes(value_slice).unwrap();
                        dprintln!("  at pool offset {}, len {}: {:?}", cid.0, len, value_slice);
                        pool::check_invariants(&tree.pool);
                        Value::from_const_bytes(cid)
                    }
                };

                let datom = Datom::assert(Eid(0), Aid::max(), value, Tid(tid));

                // We perform an insertion sort of the datoms to insert the new
                // datom, exiting on duplicates. This serves two purposes:
                // * Tree insertion requires the datoms to insert to be sorted.
                //   We can achieve this by sorting on the fly rather than
                //   sorting afterwards. Because insertion sort is so simple,
                //   there is less control flow that the fuzzer considers
                //   interesting than for e.g. `slice.sort_by`, which puts more
                //   focus the tree insertion that we are actually interested in.
                // * Tree insertion also requires unique datoms. By exiting on
                //   duplicates, rather than skipping the insertion, we force
                //   the fuzzer to focus on interesting inputs, rather than
                //   inserting useless zeros everywhere.
                let mut insert_index = datoms.len();
                for i in 0..datoms.len() {
                    match (&tree.comparator as &DatomOrd).cmp(&datom, &datoms[i], &tree.pool) {
                        Ordering::Less => { insert_index = i; break }
                        Ordering::Equal => return None,
                        Ordering::Greater => continue,
                    }
                }
                datoms.insert(insert_index, datom);
            }
            1 => {
                dprintln!("COMMIT");
                dprintln!("  Inserting {} datoms at transaction {}.", datoms.len(), tid);
                for &datom in &datoms {
                    dprintln!("  {:?}", datom);
                }

                tree.insert(&datoms[..]).unwrap();
                tree.check_invariants().unwrap();
                datoms.clear();

                // Transaction ids must be even.
                tid += 2;
            }
            _ => return None,
        }
    }
}

pub fn main(data: &[u8]) {
    use store::{PageSize256, PageSize563, PageSize4096};

    let mut cursor = Cursor::new(data);

    // Fuzz at different page sizes, in order to test all page sizes thoroughly.
    // The first byte determines the page size.
    match cursor.take_u8() {
        Some(0) => {
            dprintln!("Page size: 256.");
            let _ = run::<PageSize256>(cursor);
        }
        Some(1) => {
            dprintln!("Page size: 563.");
            let _ = run::<PageSize563>(cursor);
        }
        Some(2) => {
            dprintln!("Page size: 4096.");
            let _ =run::<PageSize4096>(cursor);
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

        for i in 0..n {
            // Action 0 (value), followed by an 8-bit length (2), followed by
            // the value (7i in 16 bits).
            file.write_all(&[0, 2, ((i * 7) >> 8) as u8, ((i * 7) & 0xff) as u8])?;
        }

        // Action 1 (commit).
        file.write_all(&[1])?;
    }

    Ok(())
}
