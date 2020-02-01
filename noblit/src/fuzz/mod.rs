// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Internal fuzz tests exposed as functions.
//!
//! The contents of this module is intended for internal use only. It is exposed
//! to allow the fuzzing binaries, and the `inspect_fuzz_artifact` program to
//! share the same implementation, where the fuzzing binaries have debug prints
//! disabled, but the the inspection program is istrumented with extra print
//! statements to trace program flow and pretty-print a fuzz input.

#[macro_use]
mod util;

pub mod htree_insert_entity;
pub mod htree_insert_value;
