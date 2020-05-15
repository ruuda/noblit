// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

pub mod binary;
pub mod database;
pub mod datom;
pub mod disk;
pub mod error;
pub mod eval;
pub mod head;
pub mod heap;
pub mod htree;
pub mod index;
pub mod memory_store;
pub mod mmap;
pub mod mmap_store;
pub mod mutation;
pub mod parse;
pub mod permutation;
pub mod plan;
pub mod planner;
pub mod query;
pub mod store;
pub mod temp_heap;
pub mod types;

// TODO: I can guard this with #[cfg(fuzzing)] and #[cfg(test)] to keep it
// compiling. But then how to build the inspect_fuzz_artifact binary? Guarding
// by a feature is probably the answer.
pub mod fuzz;
