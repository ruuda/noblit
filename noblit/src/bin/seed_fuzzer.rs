// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::env;
use std::io;

extern crate noblit;

use noblit::fuzz;

fn main() {
    let mut args = env::args();

    // Skip the program name.
    args.next();

    let handler: fn() -> io::Result<()> = match args.next().as_ref().map(|s| &s[..]) {
        // Some("htree_insert_entity") => fuzz::htree_insert_entity::generate_seed,
        Some("htree_insert_value") => fuzz::htree_insert_value::generate_seed,
        Some(target) => return println!("Unknown fuzz target '{}'.", target),
        None => return println!("Expected fuzz target."),
    };

    handler().unwrap();
}
