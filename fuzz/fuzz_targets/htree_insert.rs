#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate noblit;

use noblit::datom::{Datom, Aid, Eid, Value, Tid};
use noblit::htree::{HTree, Node};
use noblit::store::{MemoryStore, PageSize, Store};

fn run<Size: PageSize>(data: &[u8]) {
    let make_datom = |i, t| Datom::assert(Eid(i), Aid::max(), Value::min(), Tid(t));

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
        tree.insert(&datoms[..]).unwrap();
    }
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
