// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

/// Support functionality for fuzz tests.

/// Print, except when fuzzing.
///
/// This is useful for printf-style debugging of fuzz artifacts. During fuzzing,
/// this macro is a no-op, to keep the output clean and the fuzzer fast. But in
/// the `inspect_fuzz_artifact` binary, these prints leave a trace of how the
/// test sample was interpreted.
#[macro_export]
macro_rules! dprintln {
    () => (#[cfg(not(fuzzing))] println!());
    ($($arg:tt)*) => (#[cfg(not(fuzzing))] println!($($arg)*));
}

/// Evaluate a closure on byte slices of various lengths.
pub fn for_slices<'a, F>(data: &'a [u8], mut f: F) where F: FnMut(&'a [u8]) -> bool {
    let mut left = data;

    while left.len() > 2 {
        // Read a 16-bit length prefix.
        let len = (left[0] as usize) << 8 | (left[1] as usize);

        // Stop on invalid lengths, rather than capping the slice. This improves
        // the chances of byte strings combining in interesting ways.
        if len > left.len() - 2 { break }

        if !f(&left[2..2 + len]) { break }
        left = &left[2 + len..];
    }
}
