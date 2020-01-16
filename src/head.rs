// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines the head, the stateful part of the database.
//!
//! A Noblit database is append-only, but a tiny bit of mutable state is needed
//! to keep track of the root nodes of the indexes, and to track counters for
//! generating new entity ids. This mutable state is called the *head*.

use binary::{u64_from_le_bytes, u64_to_le_bytes, slice_8};
use datom::{Eid, Tid};
use store::PageId;

/// Page ids of root nodes of the index trees.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IndexRoots {
    pub eavt_root: PageId,
    pub aevt_root: PageId,
    pub avet_root: PageId,
}

impl IndexRoots {
    /// Return whether all roots of `self` lie strictly before all roots of `other`.
    ///
    /// Because page ids are allocated sequentially, roots A preceding roots B
    /// implies that the elements reachable from A are a subset of the elements
    /// reachable from B, under the assumption that B was obtained from A by
    /// appending elements.
    pub fn precedes(&self, other: &IndexRoots) -> bool {
        if self.eavt_root >= other.eavt_root { return false }
        if self.aevt_root >= other.aevt_root { return false }
        if self.avet_root >= other.avet_root { return false }
        true
    }
}

/// Counters to generate fresh ids.
///
/// The generator can generate transaction ids and entity ids. Transaction ids
/// have the restriction that they must be even, entity ids have no such
/// restriction. This generator tries fill the available u64 space evenly. That
/// is, when generating entity ids, the generator acts as a normal counter, but
/// when a transaction id is needed, we may skip an integer to satisfy the 0 mod
/// 2 requirement.
///
/// By filling the id space, rather than devoting half of the id space to
/// transaction ids and half to non-transaction entity ids, datoms that relate
/// to the transaction (such as a transaction timestamp) are more likely to end
/// up adjacent to other datoms from the same transaction during insertions,
/// which enables more efficient updates of the htree indexes.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdGen {
    /// The next unused id which is 0 mod 2.
    next_even: u64,
    /// The next unused id which is 1 mod 2.
    next_odd: u64,
}

/// Increment `x` by two, return the value of `x` before the increment.
fn post_increment_two(x: &mut u64) -> u64 {
    let result = *x;
    *x += 2;
    result
}

impl IdGen {
    /// Return an id generator that generatest `start` as the first fresh id.
    ///
    /// The start id must be even.
    pub fn new(start: u64) -> IdGen {
        assert_eq!(start % 2, 0, "IdGen start must be even.");
        IdGen {
            next_even: start,
            next_odd: start + 1,
        }
    }

    /// Consume the next unused id, even or odd.
    fn take_any(&mut self) -> u64 {
        if self.next_even > self.next_odd {
            post_increment_two(&mut self.next_odd)
        } else {
            post_increment_two(&mut self.next_even)
        }
    }

    /// Consume the next even id.
    fn take_even(&mut self) -> u64 {
        post_increment_two(&mut self.next_even)
    }

    /// Generate a fresh entity id.
    pub fn take_entity_id(&mut self) -> Eid {
        Eid(self.take_any())
    }

    /// Generate a fresh transaction id.
    pub fn take_transaction_id(&mut self) -> Tid {
        Tid(self.take_even())
    }
}

/// Volatile state of the database that changes after every transaction.
///
/// Most of the database is append-only, but a few mutable variables are needed:
///
/// * The page ids of the latest roots of the index trees.
/// * The next free ids to use for entities and transactions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Head {
    pub roots: IndexRoots,
    pub id_gen: IdGen,
}

impl Head {
    /// Deserialize a head from bytes.
    pub fn from_bytes(buffer: &[u8; 40]) -> Head {
        Head {
            roots: IndexRoots {
                eavt_root: PageId(u64_from_le_bytes(slice_8(&buffer[ 0.. 8]))),
                aevt_root: PageId(u64_from_le_bytes(slice_8(&buffer[ 8..16]))),
                avet_root: PageId(u64_from_le_bytes(slice_8(&buffer[16..24]))),
            },
            id_gen: IdGen {
                next_even: u64_from_le_bytes(slice_8(&buffer[24..32])),
                next_odd: u64_from_le_bytes(slice_8(&buffer[32..40])),
            },
        }
    }

    /// Serialize a head as bytes.
    pub fn to_bytes(&self) -> [u8; 40] {
        let mut result = [0_u8; 40];
        result[ 0.. 8].copy_from_slice(&u64_to_le_bytes(self.roots.eavt_root.0));
        result[ 8..16].copy_from_slice(&u64_to_le_bytes(self.roots.aevt_root.0));
        result[16..24].copy_from_slice(&u64_to_le_bytes(self.roots.avet_root.0));
        result[24..32].copy_from_slice(&u64_to_le_bytes(self.id_gen.next_even));
        result[32..40].copy_from_slice(&u64_to_le_bytes(self.id_gen.next_odd));
        result
    }

    /// Return the max of the root page ids.
    ///
    /// As pages are written sequentially, the max page id is an upper bound on
    /// the reachable pages.
    pub fn max_page(&self) -> PageId {
        self.roots.eavt_root
            .max(self.roots.avet_root)
            .max(self.roots.aevt_root)
    }
}

#[cfg(test)]
mod test {
    use datom::{Eid, Tid};
    use head::{Head, IdGen, IndexRoots};
    use store::PageId;

    enum IdGenResult {
        Tid(Tid),
        Eid(Eid),
    }

    /// Iterator that generates ids so we can inspect them.
    struct IdGenTest {
        /// Generator.
        gen: IdGen,

        /// Iteration.
        i: u32,

        /// Modulus (every m-th iteration generates a transaction id).
        m: u32,
    }

    impl IdGenTest {
        pub fn new() -> IdGenTest {
            IdGenTest {
                gen: IdGen::new(0),
                i: 0,
                m: 1,
            }
        }

        /// Advance to the next state, return whether done.
        fn increment(&mut self) -> bool {
            if self.i < 500 {
                self.i += 1;
                return false;
            }

            if self.m < 8 {
                self.m += 1;
                self.i = 0;
                return false;
            }

            true
        }
    }

    impl Iterator for IdGenTest {
        type Item = (IdGenResult, IdGen);

        fn next(&mut self) -> Option<(IdGenResult, IdGen)> {
            let result = match self.i % self.m {
                0 => IdGenResult::Tid(self.gen.take_transaction_id()),
                _ => IdGenResult::Eid(self.gen.take_entity_id()),
            };
            let is_done = self.increment();

            match is_done {
                true => None,
                false => Some((result, self.gen.clone())),
            }
        }
    }

    #[test]
    fn id_gen_take_transaction_id_is_even() {
        for (result, _gen) in IdGenTest::new() {
            match result {
                IdGenResult::Tid(tid) => assert_eq!(tid.0 % 2, 0, "Transaction ids should be even."),
                _ => (),
            };
        }
    }

    #[test]
    fn id_gen_ids_are_unique() {
        use std::collections::HashSet;
        let mut ids = HashSet::new();
        for (n, (result, _gen)) in IdGenTest::new().enumerate() {
            match result {
                IdGenResult::Tid(tid) => ids.insert(tid.0),
                IdGenResult::Eid(eid) => ids.insert(eid.0),
            };
            assert_eq!(ids.len(), n + 1);
        }
    }

    #[test]
    fn id_gen_ids_are_close() {
        let mut allowed_gap: i64 = 1;
        for (result, gen) in IdGenTest::new() {
            match result {
                IdGenResult::Tid(..) => allowed_gap += 2,
                IdGenResult::Eid(..) => allowed_gap = (allowed_gap - 2).max(1),
            };
            let actual_gap = (gen.next_even as i64 - gen.next_odd as i64).abs();
            assert!(
                actual_gap <= allowed_gap,
                "Actual gap {} exceeds allowed gap of {}",
                actual_gap,
                allowed_gap,
            );
        }
    }

    #[test]
    fn head_to_from_bytes_roundtrips() {
        let vs = [0, 0x1111, 0x2222_3333, 0x4444_5555_6666_7777];
        for &a in &vs {
            for &b in &vs {
                for &c in &vs {
                    for &d in &vs {
                        for &e in &vs {
                            let head = Head {
                                roots: IndexRoots {
                                    eavt_root: PageId(a),
                                    aevt_root: PageId(b),
                                    avet_root: PageId(c),
                                },
                                id_gen: IdGen {
                                    next_even: d,
                                    next_odd: e,
                                },
                            };
                            let bytes = head.to_bytes();
                            let head2 = Head::from_bytes(&bytes);
                            assert_eq!(head, head2);
                        }
                    }
                }
            }
        }
    }
}
