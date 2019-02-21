#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate noblit;

use std::cmp::Ordering;

use noblit::datom::{Datom, Aid, Eid, Value, Tid};
use noblit::htree::{DatomOrd, HTree, Node};
use noblit::store::{MemoryStore, PageSize, Store};

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

    // Insert one datom at a time, with increasing transaction id in order not
    // to create duplicates.
    for_slices(full_data, |xs| {
        // Transaction id must be even.
        tid += 2;

        let mut datoms: Vec<Datom> = xs.iter().map(|&x|
            Datom::assert(Eid(x as u64), Aid::max(), Value::min(), Tid(tid))
        ).collect();

        // The precondition for tree insertion is that datoms be sorted, and
        // that datoms are unique. Sort to enforce this, and exit if there are
        // duplicates. Previously we simply exited if the list was not sorted,
        // but it is difficult for the fuzzer to discover sorted lists by
        // chance. Sorting here is slower and adds more distracting branches as
        // interesting cases, but it also helps to discover interesting inputs
        // faster. Do exit if the input
        datoms.sort_by(|x, y| (&comparator as &DatomOrd).cmp(x, y));
        datoms.dedup_by(|x, y| (&comparator as &DatomOrd).cmp(x, y) == Ordering::Equal);

        tree.insert(&datoms[..]).unwrap();

        true
    });
}

fuzz_target!(|data: &[u8]| {
    use noblit::store::{PageSize256, PageSize563, PageSize4096};
    if data.len() == 0 { return }

    // Fuzz at different page sizes, in order to test all page sizes thoroughly.
    // The first byte determines the page size.
    match data[0] {
        0 => run::<PageSize256>(&data[1..]),
        1 => run::<PageSize563>(&data[1..]),
        2 => run::<PageSize4096>(&data[1..]),
        _ => return,
    }
});
