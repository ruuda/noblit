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
use heap::{HeapMut, self};
use htree::{HTree, Node};
use index::{DatomOrd, Eavt};
use memory_store::{MemoryStore, MemoryHeap};
use store::{PageSize, StoreMut};

fn run<'a, Size: PageSize>(mut cursor: Cursor<'a>) -> Option<()> {
    let mut store = MemoryStore::<Size>::new();
    let mut heap = MemoryHeap::new();

    let node = Node::empty_of_level(0);
    let root = store.write_page(&node.write::<Size>()).unwrap();
    let mut tree = HTree::new(root, Eavt, store, &mut heap);

    // Reserve capacity for a large-ish number of datoms, to prevent most
    // reallocations during fuzzing, thereby reducing distracting control flow.
    let mut datoms = Vec::with_capacity(255);
    let mut tid = 0;

    loop {
        match cursor.take_u8()? {
            // Commands [0, ..., 64) enqueue a value of the given length.
            n if n < 64 => {
                dprintln!("VALUE");
                let len = n;
                let value_slice = cursor.take_slice(len as usize)?;

                let value = match Value::from_bytes(value_slice) {
                    Some(v) => {
                        dprintln!("  inline, len {}: {:?}", len, value_slice);
                        v
                    }
                    None => {
                        let cid = tree.heap.append_bytes(value_slice).unwrap();
                        dprintln!("  at heap offset {}, len {}: {:?}", cid.0, len, value_slice);
                        heap::check_invariants(&tree.heap);
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
                    match (&tree.comparator as &DatomOrd).cmp(&datom, &datoms[i], &tree.heap) {
                        Ordering::Less => { insert_index = i; break }
                        Ordering::Equal => return None,
                        Ordering::Greater => continue,
                    }
                }
                datoms.insert(insert_index, datom);
            }
            // Command 0xff commits a transaction with the datoms so far.
            0xff => {
                dprintln!("COMMIT");
                dprintln!("  Inserting {} datoms at transaction {}.", datoms.len(), tid);
                for &datom in &datoms {
                    dprintln!("  {:?}", datom);
                }

                tree.insert(&datoms[..]).unwrap();
                tree.check_invariants().unwrap();
                datoms.clear();

                dprintln!("  Tree has height {}.", tree.height());

                // Transaction ids must be even.
                tid += 2;
            }
            // Other commands are currently unused.
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

    let write_val_u0 =  |w: &mut fs::File, _v: ()|  w.write_all(&[0]);
    let write_val_u8 =  |w: &mut fs::File,  v: u8|  w.write_all(&[1, v]);
    let write_val_u16 = |w: &mut fs::File,  v: u16| w.write_all(&[2, (v >> 8) as u8, (v & 0xff) as u8]);

    // We can fit one value of length 0.
    write_val_u0(&mut file, ())?;

    // Next up we can get 256 values of length 1.
    for i in 0..256 {
        write_val_u8(&mut file, i as u8)?;
    }

    // Next, we need to do one more value of length 2.
    write_val_u16(&mut file, 0);

    // Finally, commit the datoms with those value. (Action 0xff.)
    file.write_all(&[0xff])?;

    // At this point, we have 258 values. Page size 256 can fit 6 datoms per
    // node (so it can have 7 children), so a tree of height 3 can fit at most
    // 6 + 7*6 + 7*7*6 = 342 datoms. One more datom would require creating a
    // tree of theight 4. Although in practice, we already need a tree of height
    // 4 at 264 values.

    for k in 0..8 {
        write_val_u16(&mut file, k * 257);
    }
    file.write_all(&[0xff])?;

    Ok(())
}
