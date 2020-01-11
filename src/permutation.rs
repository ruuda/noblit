// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Support functionality to generate permutations.

/// An struct to generate all permutations of the integers [0, n).
///
/// The generator implements the non-recursive version of Heap's algorithm.
pub struct Permutations {
    indices: Vec<usize>,
    ks: Vec<usize>,
    i: usize,
    first: bool,
}

impl Permutations {
    /// Return an iterator over the integers 0 up to n, excluding n itself.
    pub fn new(n: usize) -> Permutations {
        let mut indices = Vec::with_capacity(n);
        let mut ks = Vec::with_capacity(n);

        for i in 0..n {
            indices.push(i);
            ks.push(0);
        }

        Permutations {
            indices: indices,
            ks: ks,
            i: 0,
            first: true,
        }
    }

    /// Return the next permutation, until all of them have been visited.
    pub fn next(&mut self) -> Option<&[usize]> {
        // The fisrt permutation is just the unpermuted indices.
        if self.first {
            self.first = false;
            return Some(&self.indices);
        }

        while self.i < self.indices.len() {
            if self.ks[self.i] < self.i {
                if self.i % 2 == 0 {
                    self.indices.swap(0, self.i);
                } else {
                    self.indices.swap(self.ks[self.i], self.i);
                }
                self.ks[self.i] += 1;
                self.i = 0;
                return Some(&self.indices);
            } else {
                self.ks[self.i] = 0;
                self.i += 1;
            }
        }

        None
    }
}

#[cfg(test)]
mod test {
    use permutation::Permutations;

    #[test]
    pub fn permutations_is_correct_for_n_zero() {
        let mut ps = Permutations::new(0);
        assert_eq!(ps.next(), Some(&[][..]));
        assert_eq!(ps.next(), None);
    }

    #[test]
    pub fn permutations_is_correct_for_n_one() {
        let mut ps = Permutations::new(1);
        assert_eq!(ps.next(), Some(&[0][..]));
        assert_eq!(ps.next(), None);
    }

    #[test]
    pub fn permutations_is_correct_for_n_two() {
        let mut ps = Permutations::new(2);
        assert_eq!(ps.next(), Some(&[0, 1][..]));
        assert_eq!(ps.next(), Some(&[1, 0][..]));
        assert_eq!(ps.next(), None);
    }

    #[test]
    pub fn permutations_is_correct_for_n_three() {
        let mut ps = Permutations::new(3);
        assert_eq!(ps.next(), Some(&[0, 1, 2][..]));
        assert_eq!(ps.next(), Some(&[1, 0, 2][..]));
        assert_eq!(ps.next(), Some(&[2, 0, 1][..]));
        assert_eq!(ps.next(), Some(&[0, 2, 1][..]));
        assert_eq!(ps.next(), Some(&[1, 2, 0][..]));
        assert_eq!(ps.next(), Some(&[2, 1, 0][..]));
        assert_eq!(ps.next(), None);
    }

    #[test]
    pub fn permutations_is_correct_for_n_four() {
        let mut ps = Permutations::new(4);
        assert_eq!(ps.next(), Some(&[0, 1, 2, 3][..]));
        assert_eq!(ps.next(), Some(&[1, 0, 2, 3][..]));
        assert_eq!(ps.next(), Some(&[2, 0, 1, 3][..]));
        assert_eq!(ps.next(), Some(&[0, 2, 1, 3][..]));
        assert_eq!(ps.next(), Some(&[1, 2, 0, 3][..]));
        assert_eq!(ps.next(), Some(&[2, 1, 0, 3][..]));
        assert_eq!(ps.next(), Some(&[3, 1, 0, 2][..]));
        assert_eq!(ps.next(), Some(&[1, 3, 0, 2][..]));
        assert_eq!(ps.next(), Some(&[0, 3, 1, 2][..]));
        assert_eq!(ps.next(), Some(&[3, 0, 1, 2][..]));
        assert_eq!(ps.next(), Some(&[1, 0, 3, 2][..]));
        assert_eq!(ps.next(), Some(&[0, 1, 3, 2][..]));
        assert_eq!(ps.next(), Some(&[0, 2, 3, 1][..]));
        assert_eq!(ps.next(), Some(&[2, 0, 3, 1][..]));
        assert_eq!(ps.next(), Some(&[3, 0, 2, 1][..]));
        assert_eq!(ps.next(), Some(&[0, 3, 2, 1][..]));
        assert_eq!(ps.next(), Some(&[2, 3, 0, 1][..]));
        assert_eq!(ps.next(), Some(&[3, 2, 0, 1][..]));
        assert_eq!(ps.next(), Some(&[3, 2, 1, 0][..]));
        assert_eq!(ps.next(), Some(&[2, 3, 1, 0][..]));
        assert_eq!(ps.next(), Some(&[1, 3, 2, 0][..]));
        assert_eq!(ps.next(), Some(&[3, 1, 2, 0][..]));
        assert_eq!(ps.next(), Some(&[2, 1, 3, 0][..]));
        assert_eq!(ps.next(), Some(&[1, 2, 3, 0][..]));
        assert_eq!(ps.next(), None);
    }

    #[test]
    pub fn permutations_yields_unique_results() {
        use std::collections::HashSet;

        // Up to n=8 (40k permutations), this test runs quickly. For n=9 (363k
        // permutations), it takes roughly a second to run. So we go up to 8 to
        // keep the test suite fast.
        for n in 0..=8 {
            let mut xs = HashSet::new();
            let mut ps = Permutations::new(n);

            // Check that no duplicate permutations are generated.
            while let Some(p) = ps.next() {
                let p_clone = p.to_vec();
                assert!(!xs.contains(&p_clone));
                xs.insert(p_clone);
            }

            // There are n! permutations of n elements,
            // check that we have all of them.
            let expected_len = (1..=n).product();
            assert_eq!(xs.len(), expected_len);
        }
    }
}
