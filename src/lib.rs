// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

// TODO: Remove once the API is fleshed out more. Or when this crate is turned
// into a library.

pub mod binary;
pub mod database;
pub mod datom;
pub mod htree;
pub mod index;
pub mod memory_store;
pub mod pool;
pub mod query;
pub mod query_plan;
pub mod stack_pool;
pub mod store;
pub mod types;

// TODO: I can guard this with #[cfg(fuzzing)] and #[cfg(test)] to keep it
// compiling. But then how to build the inspect_fuzz_artifact binary? Guarding
// by a feature is probably the answer.
pub mod fuzz;
