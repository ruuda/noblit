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

/// An (attribute, entity, value, transaction) ordered datom.
pub struct Aevt(pub Datom);

/// An (attribute, value, entity, transaction) ordered datom.
pub struct Avet(pub Datom);

/// An (entity, attribute, value, transaction) ordered datom.
pub struct Eavt(pub Datom);

/// A (value, attribute, entity, transaction) ordered datom.
pub struct Vaet(pub Datom);

impl Eq for Aevt {}
impl Eq for Avet {}
impl Eq for Eavt {}
impl Eq for Vaet {}

impl Ord for Aevt {
    fn cmp(&self, other: &Aevt) -> Ordering {
        self.0.aevt().cmp(&other.0.aevt())
    }
}

impl Ord for Avet {
    fn cmp(&self, other: &Avet) -> Ordering {
        self.0.avet().cmp(&other.0.avet())
    }
}

impl Ord for Eavt {
    fn cmp(&self, other: &Eavt) -> Ordering {
        self.0.eavt().cmp(&other.0.eavt())
    }
}

impl Ord for Vaet {
    fn cmp(&self, other: &Vaet) -> Ordering {
        self.0.vaet().cmp(&other.0.vaet())
    }
}

impl PartialOrd for Aevt {
    fn partial_cmp(&self, other: &Aevt) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd for Avet {
    fn partial_cmp(&self, other: &Avet) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd for Eavt {
    fn partial_cmp(&self, other: &Eavt) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd for Vaet {
    fn partial_cmp(&self, other: &Vaet) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Aevt {
    fn eq(&self, other: &Aevt) -> bool {
        self.0.aevt().eq(&other.0.aevt())
    }
}

impl PartialEq for Avet {
    fn eq(&self, other: &Avet) -> bool {
        self.0.avet().eq(&other.0.avet())
    }
}

impl PartialEq for Eavt {
    fn eq(&self, other: &Eavt) -> bool {
        self.0.eavt().eq(&other.0.eavt())
    }
}

impl PartialEq for Vaet {
    fn eq(&self, other: &Vaet) -> bool {
        self.0.vaet().eq(&other.0.vaet())
    }
}

impl DatomOrd for () {
    fn cmp(&self, lhs: &Datom, rhs: &Datom) -> Ordering {
        // TODO: Implement ordering properly, with heap lookup.
        lhs.eavt().cmp(&rhs.eavt())
    }
}
