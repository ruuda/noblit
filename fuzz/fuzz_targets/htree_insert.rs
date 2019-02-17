#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate noblit;

use noblit::datom::{Datom, Aid, Eid, Value, Tid};
use noblit::htree::{HTree, Node};
use noblit::store::{MemoryStore, PageId, PageSize563, Store};

fuzz_target!(|data: &[u8]| {
    let make_datom = |i, t| Datom::assert(Eid(i), Aid::max(), Value::min(), Tid(t));

    type Size = PageSize563;
    let mut store = MemoryStore::<Size>::new();
    let node = Node::empty_of_level(0);
    let root = store.write_page(&node.write::<Size>()).unwrap();
    let comparator = ();
    let mut tree = HTree::new(root, &comparator, store);

    // Insert one datom at a time, with increasing transaction id in order not
    // to create duplicates.
    for (t, &i) in data.iter().enumerate() {
        let tid = 2 * t as u64; // Transaction id must be even.
        let datoms = vec![make_datom(i as u64, tid)];
        tree.insert(&datoms[..]);
    }
});
