// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Counters to generate fresh ids.

use datom::{Eid, Tid};

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
struct IdGen {
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
    fn take_entity_id(&mut self) -> Eid {
        Eid(self.take_any())
    }

    /// Generate a fresh transaction id.
    fn take_transaction_id(&mut self) -> Tid {
        Tid(self.take_even())
    }
}

#[cfg(test)]
mod test {
    use idgen::IdGen;
    use datom::{Eid, Tid};

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
            let gen_copy = IdGen { ..self.gen };

            match is_done {
                true => None,
                false => Some((result, gen_copy)),
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
}
