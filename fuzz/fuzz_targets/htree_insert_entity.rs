// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate noblit;

fuzz_target!(|data| {
    noblit::fuzz::htree_insert_entity::main(data)
});
