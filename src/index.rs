// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines indexes.

use std::cmp::{PartialOrd, Ord, Ordering};

use datom::Datom;

/// An ordering on datoms.
pub trait DatomOrd {
    fn cmp(&self, lhs: &Datom, rhs: &Datom) -> Ordering;
}

/// An (attribute, entity, value, transaction) ordering.
pub struct AevtOrd;

/// An (attribute, value, entity, transaction) ordering.
pub struct AvetOrd;

/// An (entity, attribute, value, transaction) ordering.
pub struct EavtOrd;

/// Helper macro to compare on a specific property.
///
/// Returns immediately if the elements are not equal.
macro_rules! compare_by {
    ($lhs:expr, $rhs:expr) => {
        match $lhs.cmp(&$rhs) {
            Ordering::Less => return Ordering::Less,
            Ordering::Greater => return Ordering::Greater,
            Ordering::Equal => {}
        }
    };
}

impl DatomOrd for AevtOrd {
    fn cmp(&self, lhs: &Datom, rhs: &Datom) -> Ordering {
        compare_by!(lhs.attribute, rhs.attribute);
        compare_by!(lhs.entity, rhs.entity);
        compare_by!(lhs.value, rhs.value);
        compare_by!(lhs.transaction_operation, rhs.transaction_operation);
        Ordering::Equal
    }
}

impl DatomOrd for AvetOrd {
    fn cmp(&self, lhs: &Datom, rhs: &Datom) -> Ordering {
        compare_by!(lhs.attribute, rhs.attribute);
        compare_by!(lhs.value, rhs.value);
        compare_by!(lhs.entity, rhs.entity);
        compare_by!(lhs.transaction_operation, rhs.transaction_operation);
        Ordering::Equal
    }
}

impl DatomOrd for EavtOrd {
    fn cmp(&self, lhs: &Datom, rhs: &Datom) -> Ordering {
        compare_by!(lhs.entity, rhs.entity);
        compare_by!(lhs.attribute, rhs.attribute);
        compare_by!(lhs.value, rhs.value);
        compare_by!(lhs.transaction_operation, rhs.transaction_operation);
        Ordering::Equal
    }
}
