// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines query plans.

use datom::{Aid, Value};
use types::Type;

/// The index of a slot.
///
/// A slot stores the current value of a variable or constant during evaluation.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Slot(pub u16);

/// The index to scan.
pub enum Index {
    Aevt,
    Avet,
    Eavt,
}

/// Which slots to fill, and which to read.
///
/// An index stores datoms ordered on various permutations of (entity,
/// attribute, value). (Transaction is always last.) In queries, the attribute
/// is always known ahead of time. This leaves two slots, *first* and
/// *second*.
///
/// | Index | First  | Second |
/// |-------|--------|--------|
/// | Aevt  | Entity | Value  |
/// | Eavt  | Entity | Value  |
/// | Avet  | Value  | Entity |
///
/// With the index, we can do three things:
///
/// * Scan the full index. During the scan, we can fill both *first* and
///   *second* at once.
///
/// * Scan a part of the index, constrained by *first*. We *read first* and
///   *write second*.
///
/// * Test the existence of known datoms. We read both *first* and *second*.
pub enum Flow {
    OutOut,
    InOut,
    InIn,
}

/// A scan over an index.
///
/// A scan may fill zero, one, or two variables depending on its kind. Depending
/// on the index used, `slots[0]` and `slots[1]` can refer to the entity or
/// value, as described in `Flow`.
pub struct Scan {
    index: Index,
    flow: Flow,
    attribute: Aid,
    slots: [Slot; 2],
}

/// Determines how to fill a slot.
pub enum SlotContents {
    /// A query variable that fills a slot in the query plan.
    ///
    /// In the query plan, slots are indentified by index, not by name. But for
    /// debug printing, it is still useful to have the humand-meaningful name of
    /// the query variable that this slot originated from.
    Variable { name: String },

    /// A constant that fills a slot in the query plan.
    Constant { value: Value },
}

/// Defines how a slot is filled during evaluation.
///
/// A slot can contain either a constant, or the current value for a variable.
/// Constants are filled once at the start of evaluation; variable slots can be
/// assigned new values at every iteration.
pub struct SlotDefinition {
    /// How to fill this slot.
    pub contents: SlotContents,

    /// The type of the value in this slot.
    pub datatype: Type
}

/// A query plan.
///
/// Where a query specifies *what* to find, the query plan specifies *how* to
/// find it. A query plan defines a number of slots, that get filled with
/// values. A slot definition determines how the slot is filled: either during
/// evalutation, by a scan, or at the start of evaluation, with a constant.
/// A query is executed as a number of nested loops, one for each scan. Scans
/// can read from slots that were written to by an earlier scan (one with a
/// lower index). The innermost loop yields the result of the query.
pub struct Plan {
    /// The scans that fill the variable slots.
    ///
    /// Scans are evaluated as nested loops. The scan with the lowest index
    /// becomes the outermost loop, the scan with the highest index becomes the
    /// innermost loop.
    pub scans: Vec<Scan>,

    /// The slots that are needed during query evaluation.
    pub slots: Vec<SlotDefinition>,

    /// The slots to yield the values from in every iteration.
    ///
    /// The selects are in the select order of the query; the tuple indices match.
    pub selects: Vec<Slot>,
}

impl Plan {
    /// Assert invariants about slot initialization.
    ///
    /// * Check that the slots referenced by scans are all in bounds.
    /// * Check that scans only read from slots that were written to by earlier scans.
    /// * Check that every slot is written to by at least one scan or constant.
    /// * Check that every slot is written to by at most one scan or constant.
    ///
    /// Panics if any of the invariants is violated.
    pub fn check_slot_initialization(&self) {
        // For every slot, we store how it is initialized.
        #[derive(Eq, Debug, PartialEq)]
        enum Initialization {
            /// Not initialized yet, safe to write, unsafe to read.
            Uninitialized,
            /// Initialized to a constant value.
            Constant,
            /// Initialized by the scan with the given scan number.
            Scan(usize),
        };
        let mut initialization = Vec::with_capacity(self.slots.len());

        for slot_def in &self.slots {
            match slot_def.contents {
                SlotContents::Variable { .. } => initialization.push(Initialization::Uninitialized),
                SlotContents::Constant { .. } => initialization.push(Initialization::Constant),
            }
        }

        for (i_scan, scan) in self.scans.iter().enumerate() {
            let is_writes = match scan.flow {
                Flow::OutOut => [true, true],
                Flow::InOut => [false, true],
                Flow::InIn => [false, false],
            };

            for (slot, &is_write) in scan.slots.as_ref().iter().zip(is_writes.as_ref()) {
                assert!(
                    (slot.0 as usize) < self.scans.len(),
                    "Scan {} references non-existing slot {}.",
                    i_scan, slot.0,
                );
                let init = &mut initialization[slot.0 as usize];

                if is_write {
                    assert_eq!(
                        *init, Initialization::Uninitialized,
                        "Scan {} writes to slot {}, which is already initialized by {:?}.",
                        i_scan, slot.0, *init,
                    );
                    *init = Initialization::Scan(i_scan);
                } else {
                    assert!(
                        *init != Initialization::Uninitialized,
                        "Scan {} reads uninitialized slot {}.",
                        i_scan, slot.0
                    );
                }
            }
        }

        for (i_slot, init) in initialization.iter().enumerate() {
            assert!(
                *init != Initialization::Uninitialized,
                "The plan does not initialize slot {}.",
                i_slot,
            );
        }
    }
}
