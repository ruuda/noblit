// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An evaluator for query plans.

use std::iter;

use datom::{Datom, Eid, Operation, Value, Tid};
use heap;
use store;
use database::View;
use plan::{EntityValueFlow, Index, Plan, Scan, SlotDefinition};

type DatomIter<'a> = Box<dyn Iterator<Item = &'a Datom> + 'a>;

/// Iterator that yields results from a given query plan.
pub struct Evaluator<'a, Store: 'a + store::Store, Heap: 'a + heap::Heap> {
    /// The database to query.
    view: &'a View<'a, Store, Heap>,

    /// The query plan.
    plan: &'a Plan,

    /// One iterator for every scan in the plan.
    iters: Vec<DatomIter<'a>>,

    /// The slots that store the current values produced by the scans.
    slots: Vec<Value>,
}

impl<'a, Store: store::Store, Heap: heap::Heap> Evaluator<'a, Store, Heap> {
    pub fn new(plan: &'a Plan, view: &'a View<'a, Store, Heap>) -> Evaluator<'a, Store, Heap> {

        // At the start, fill all iterators with empty ones. We will later
        // overwrite iterator 0, and that gets the evaluation going: once it
        // produces a new value, we rebuild all iterators after it.
        let mut iters = Vec::with_capacity(plan.scans.len());
        for _ in 0..plan.scans.len() {
            let empty: DatomIter = Box::new(iter::empty());
            iters.push(empty);
        }

        // Prepare all slots. For the constants, we put them there, for the
        // slots that will be filled by the scans, we put min there for now.
        let slots = plan.slots.iter().map(|slot_def| match *slot_def {
            SlotDefinition::Variable { .. } => Value::min(),
            SlotDefinition::Constant { value } => value,
        }).collect();

        let mut evaluator = Evaluator {
            view: view,
            plan: plan,
            iters: iters,
            slots: slots,
        };

        // Initialize the first iterator: this is the only one that we do not
        // replenish when exhausted, so it needs to be there already.
        if plan.scans.len() > 0 {
            evaluator.iters[0] = evaluator.get_iter(&plan.scans[0]);
        }

        evaluator
    }

    /// Return the [min, max) datoms that determine the range to scan over.
    fn get_scan_bounds(&self, scan: &Scan) -> (Datom, Datom) {
        let attribute = scan.attribute;
        let entity = self.slots[scan.entity.0 as usize].as_eid(self.view.heap());
        let value = self.slots[scan.value.0 as usize];

        // For outputs, we need to scan the full range; inputs are fixed.
        match scan.entity_value_flow() {
            EntityValueFlow::OutOut => (
                Datom::new(Eid::min(), attribute, Value::min(), Tid::min(), Operation::Retract),
                Datom::new(Eid::max(), attribute, Value::max(), Tid::max(), Operation::Assert),
            ),
            EntityValueFlow::InOut => (
                Datom::new(entity, attribute, Value::min(), Tid::min(), Operation::Retract),
                Datom::new(entity, attribute, Value::max(), Tid::max(), Operation::Assert),
            ),
            EntityValueFlow::OutIn => (
                Datom::new(Eid::min(), attribute, value, Tid::min(), Operation::Retract),
                Datom::new(Eid::max(), attribute, value, Tid::max(), Operation::Assert),
            ),
            EntityValueFlow::InIn => (
                Datom::new(entity, attribute, value, Tid::min(), Operation::Retract),
                Datom::new(entity, attribute, value, Tid::max(), Operation::Assert),
            ),
        }
    }

    /// Given the current slot values, produce the iterator for the given scan.
    fn get_iter(&self, scan: &Scan) -> DatomIter<'a> {
        let (min, max) = self.get_scan_bounds(scan);
        match scan.index {
            Index::Avet => Box::new(self.view.avet().into_iter(&min, &max)),
            Index::Aevt => Box::new(self.view.aevt().into_iter(&min, &max)),
            Index::Eavt => Box::new(self.view.eavt().into_iter(&min, &max)),
        }
    }

    /// Increment the i-th iterator.
    ///
    /// If it is exhausted, increment the (i-1)-th iterator and reset the i-th
    /// one, etc., until the 0-th iterator is exhausted.
    ///
    /// Returns whether a new value was successfully stored.
    fn increment(&mut self, i: usize) -> bool {
        loop {
            match self.iters[i].next() {
                None if i == 0 => {
                    return false
                }
                Some(datom) => {
                    let scan = &self.plan.scans[i];
                    match scan.entity_value_flow() {
                        EntityValueFlow::OutOut => {
                            self.slots[scan.entity.0 as usize] = Value::from_eid(datom.entity);
                            self.slots[scan.value.0 as usize] = datom.value;
                        }
                        EntityValueFlow::OutIn => {
                            self.slots[scan.entity.0 as usize] = Value::from_eid(datom.entity);
                        }
                        EntityValueFlow::InOut => {
                            self.slots[scan.value.0 as usize] = datom.value;
                        }
                        EntityValueFlow::InIn => {
                            // Only the iteration matters, the values are known.
                        }
                    }
                    return true
                }
                None => {
                    if !self.increment(i - 1) {
                        return false
                    }
                    self.iters[i] = self.get_iter(&self.plan.scans[i]);
                }
            }
        }
    }
}
