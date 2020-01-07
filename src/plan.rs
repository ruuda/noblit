// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines query plans.

use datom::Aid;

/// A placeholder variable in a query.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Var(pub u16);

/// A slot stores the current value of a variable during evaluation.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Slot(pub u16);

/// The index to scan.
pub enum Index {
    Aevt,
    Avet,
    Eavt,
}

/// Which variables to fill, and which to read.
///
/// An index stores datoms ordered on various permutations of (entity,
/// attribute, value). (Transaction is always last.) In queries, the attribute
/// is always known ahead of time. This leaves two variables, *first* and
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
/// * Test the existence of datoms. We read both *first* and *second*.
pub enum Kind {
    OutOut,
    InOut,
    InIn,
}

/// A scan over an index.
///
/// A scan may fill zero, one, or two variables depending on its kind.
pub struct Scan {
    index: Index,
    kind: Kind,
    entity: Var,
    attribute: Aid,
    value: Var,
}
