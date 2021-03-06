// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Fuzz test that asserts tree invariants after insertions.

#![no_main]

extern crate arbitrary;
extern crate libfuzzer_sys;
extern crate noblit;

use std::cmp::Ordering;

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;

use noblit::datom::{Datom, Aid, Eid, Value, Tid};
use noblit::heap::{HeapMut, self};
use noblit::htree::{HTree, Node};
use noblit::index::{DatomOrd, Aevt, Avet, Eavt};
use noblit::memory_store::{MemoryStore, MemoryHeap};
use noblit::store::{StoreMut, PageSize, PageSize256, PageSize568, PageSize4096};

#[derive(Arbitrary, Debug)]
enum FuzzValue {
    U64(u64),
    Bytes(Vec<u8>),
}

#[derive(Arbitrary, Debug)]
enum FuzzAction {
    Append { entity: u64, attribute: u64, value: FuzzValue },
    Insert,
}

#[derive(Arbitrary, Debug)]
enum FuzzPageSize {
    PageSize256,
    PageSize568,
    PageSize4096,
}

#[derive(Arbitrary, Debug)]
enum FuzzOrd {
    Aevt,
    Avet,
    Eavt,
}

#[derive(Arbitrary, Debug)]
struct FuzzInput {
    page_size: FuzzPageSize,
    cmp: FuzzOrd,
    actions: Vec<FuzzAction>,
}

fuzz_target!(|input: FuzzInput| {
    // We test all combinations of page size and datom ordering (so we test all
    // indexes). Because these are type parameters, we do a two-stage dispatch
    // on the value, to move them to the type level.
    run2(input.page_size, input.cmp, &input.actions);
});

// Outer stage: page size and comparator are values, not type parameters.
fn run2(page_size: FuzzPageSize, cmp: FuzzOrd, actions: &[FuzzAction]) -> Option<()> {
    match page_size {
        FuzzPageSize::PageSize256 => run1::<PageSize256>(cmp, actions),
        FuzzPageSize::PageSize568 => run1::<PageSize568>(cmp, actions),
        FuzzPageSize::PageSize4096 => run1::<PageSize4096>(cmp, actions),
    }
}

// Middle stage: page size is a type parameter, ordering is a value.
fn run1<Size: PageSize>(cmp: FuzzOrd, actions: &[FuzzAction]) -> Option<()> {
    match cmp {
        FuzzOrd::Aevt => run0::<Size, _>(Aevt, actions),
        FuzzOrd::Avet => run0::<Size, _>(Avet, actions),
        FuzzOrd::Eavt => run0::<Size, _>(Eavt, actions),
    }
}

// Inner stage: page size and ordering are type parameters.
fn run0<Size: PageSize, Cmp: DatomOrd>(cmp: Cmp, actions: &[FuzzAction]) -> Option<()> {
    let mut store = MemoryStore::<Size>::new();
    let mut heap = MemoryHeap::new();

    let node = Node::empty_of_level(0);
    let root = store.write_page(&node.write::<Size>()).unwrap();
    let mut tree = HTree::new(root, cmp, store, &mut heap);

    // Reserve capacity for a large-ish number of datoms, to prevent most
    // reallocations during fuzzing, thereby reducing distracting control flow.
    let mut datoms = Vec::with_capacity(255);
    let mut tid = 0;

    for action in actions {
        match action {
            FuzzAction::Append { entity, attribute, value: fuzz_value } => {
                let value = match fuzz_value {
                    FuzzValue::U64(x) => {
                        match Value::try_from_u64_inline(*x) {
                            Some(v) => v,
                            None => {
                                let cid = tree.heap.append_u64(*x).unwrap();
                                heap::check_invariants(&tree.heap);
                                Value::from_const_u64(cid)
                            }
                        }
                    }
                    FuzzValue::Bytes(bytes) => {
                        match Value::try_from_bytes_inline(bytes) {
                            Some(v) => v,
                            None => {
                                let cid = tree.heap.append_bytes(bytes).unwrap();
                                heap::check_invariants(&tree.heap);
                                Value::from_const_bytes(cid)
                            }
                        }
                    }
                };

                let datom = Datom::assert(Eid(*entity), Aid(*attribute), value, Tid(tid));

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
                    match (&tree.comparator as &dyn DatomOrd).cmp(&datom, &datoms[i], &tree.heap) {
                        Ordering::Less => { insert_index = i; break }
                        Ordering::Equal => return None,
                        Ordering::Greater => continue,
                    }
                }
                datoms.insert(insert_index, datom);
            }
            FuzzAction::Insert => {
                tree.insert(&datoms[..]).unwrap();
                tree.check_invariants().unwrap();

                // Stop fuzzing if we are not inserting datoms. This prevents
                // the fuzzer from focussing too much on doing insert all the
                // time, without inserting datoms. This line speeds up the
                // fuzzer by roughly 10x.
                if datoms.len() == 0 { return None }

                // Transaction ids must be even.
                tid += 2;
                datoms.clear();
            }
        }
    }

    Some(())
}
