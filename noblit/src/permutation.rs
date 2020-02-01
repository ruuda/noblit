// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Support functionality to generate permutations.

/// A struct to generate all permutations on n elements.
///
/// This is an iterator that yields the indices of the elements to swap. As it
/// yields the swaps that generate the permutations, it yields `n! - 1` swaps.
/// The first permutation is that with no swaps applied.
///
/// This iterator implements the non-recursive version of Heap's algorithm.
pub struct Permutations {
    ks: Vec<usize>,
    i: usize,
}

impl Permutations {
    /// Return an iterator over swaps to generate all permutations on n elements.
    pub fn new(n: usize) -> Permutations {
        Permutations {
            ks: vec![0; n],
            i: 0,
        }
    }
}

impl Iterator for Permutations {
    type Item = (usize, usize);

    /// Return the next permutation, until all of them have been visited.
    fn next(&mut self) -> Option<(usize, usize)> {
        while self.i < self.ks.len() {
            if self.ks[self.i] < self.i {
                let result = if self.i % 2 == 0 {
                    (0, self.i)
                } else {
                    (self.ks[self.i], self.i)
                };
                self.ks[self.i] += 1;
                self.i = 0;
                return Some(result);
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

    // To test the permutation iterator, we build a small struct that generates
    // all permutations of 0..n, and test those.
    struct IndexPermutations {
        elements: Vec<usize>,
        permutations: Permutations,
        is_first: bool,
    }

    impl IndexPermutations {
        pub fn new(n: usize) -> IndexPermutations {
            IndexPermutations {
                elements: (0..n).collect(),
                permutations: Permutations::new(n),
                is_first: true,
            }
        }

        pub fn next(&mut self) -> Option<&[usize]> {
            if self.is_first {
                self.is_first = false;
                Some(&self.elements)
            } else {
                match self.permutations.next() {
                    Some((i, j)) => {
                        self.elements.swap(i, j);
                        Some(&self.elements)
                    }
                    None => None,
                }
            }
        }
    }


    #[test]
    pub fn permutations_is_correct_for_n_zero() {
        let mut ps = IndexPermutations::new(0);
        assert_eq!(ps.next(), Some(&[][..]));
        assert_eq!(ps.next(), None);
    }

    #[test]
    pub fn permutations_is_correct_for_n_one() {
        let mut ps = IndexPermutations::new(1);
        assert_eq!(ps.next(), Some(&[0][..]));
        assert_eq!(ps.next(), None);
    }

    #[test]
    pub fn permutations_is_correct_for_n_two() {
        let mut ps = IndexPermutations::new(2);
        assert_eq!(ps.next(), Some(&[0, 1][..]));
        assert_eq!(ps.next(), Some(&[1, 0][..]));
        assert_eq!(ps.next(), None);
    }

    #[test]
    pub fn permutations_is_correct_for_n_three() {
        let mut ps = IndexPermutations::new(3);
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
        let mut ps = IndexPermutations::new(4);
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
            let mut ps = IndexPermutations::new(n);

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
