// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! This module deals with indexes.

use std::cmp::{PartialOrd, Ord, Ordering};

use datom::Datom;

pub struct Aevt(pub Datom);
pub struct Avet(pub Datom);
pub struct Eavt(pub Datom);
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
