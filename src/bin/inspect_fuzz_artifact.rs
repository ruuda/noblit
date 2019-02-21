// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::env;
use std::fs;

extern crate noblit;

use noblit::fuzz;

fn main() {
    let mut args = env::args();

    // Skip the program name.
    args.next();

    let handler: fn(&[u8]) = match args.next().as_ref().map(|s| &s[..]) {
        Some("htree_insert") => fuzz::htree_insert::main,
        Some(target) => return println!("Unknown fuzz target '{}'.", target),
        None => return println!("Expected fuzz target and filename."),
    };

    for fname in args {
        println!("Running {}.", fname);
        let data = fs::read(&fname).expect("Failed to read file.");
        handler(&data);
        println!("Finished {}.", fname);
    }
}
