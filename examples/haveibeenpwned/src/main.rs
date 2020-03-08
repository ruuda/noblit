// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Build a Noblit database from the "Have I Been Pwned?" file.
//!
//! This example provides a binary that converts the text-based dumps from
//! [Have I Been Pwned][hibp] into a Noblit database, in order to query quickly
//! whether a password is present in the dump.
//!
//! This example is inspired by [this post][stryku] on stryku.pl.
//!
//! [hibp]:   https://haveibeenpwned.com/
//! [stryku]: http://stryku.pl/poetry/okon.php

extern crate noblit;

use std::env;
use std::process;

use noblit::database;
use noblit::datom::Aid;
use noblit::datom::Value;
use noblit::heap::SizedHeap;
use noblit::memory_store::{MemoryStore, MemoryHeap};
use noblit::store::{PageSize4096};
use noblit::temp_heap::Temporaries;

type MemoryStore4096 = MemoryStore<PageSize4096>;
type Database = database::Database<MemoryStore4096, MemoryHeap>;

fn init_database() -> (Database, Schema) {
    let store: MemoryStore4096 = MemoryStore::new();
    let heap = MemoryHeap::new();
    let mut db = Database::new(store, heap).unwrap();
    let schema = assert_schema(&mut db);
    (db, schema)
}

struct Schema {
    pw_sha1: Aid,
    pw_count: Aid,
}

fn assert_schema(db: &mut Database) -> Schema {
    let db_attr_many = db.builtins.attribute_db_attribute_many;
    let db_attr_name = db.builtins.attribute_db_attribute_name;
    let db_attr_type = db.builtins.attribute_db_attribute_type;
    let db_attr_unique = db.builtins.attribute_db_attribute_unique;
    let db_type_bytes = db.builtins.entity_db_type_bytes;
    let db_type_uint64 = db.builtins.entity_db_type_uint64;

    // TODO: Put this somewhere in Noblit.
    fn value_from_str(tmps: &mut Temporaries, val: &str) -> Value {
        match Value::from_str(val) {
            Some(v) => v,
            None => {
                let cid = tmps.push_string(val.to_string());
                Value::from_const_bytes(cid)
            }
        }
    }

    // Build a transaction to set up the schema.
    let mut tx = db.begin();
    let mut tmps = Temporaries::new();

    // Define two attributes: pw.sha1: bytes, and pw.count: uint64.
    let eid_pw_sha1 = tx.create_entity();
    tx.assert(eid_pw_sha1, db_attr_name, value_from_str(&mut tmps, "pw.sha1"));
    tx.assert(eid_pw_sha1, db_attr_type, Value::from_eid(db_type_bytes));
    tx.assert(eid_pw_sha1, db_attr_unique, Value::from_bool(true));
    tx.assert(eid_pw_sha1, db_attr_many, Value::from_bool(false));
    let pw_sha1 = Aid(eid_pw_sha1.0);

    let eid_pw_count = tx.create_entity();
    tx.assert(eid_pw_count, db_attr_name, value_from_str(&mut tmps, "pw.count"));
    tx.assert(eid_pw_count, db_attr_type, Value::from_eid(db_type_uint64));
    tx.assert(eid_pw_count, db_attr_unique, Value::from_bool(false));
    tx.assert(eid_pw_count, db_attr_many, Value::from_bool(false));
    let pw_count = Aid(eid_pw_count.0);

    db.commit(&tmps, tx).expect("TODO: Good Result types.");

    Schema {
        pw_sha1: pw_sha1,
        pw_count: pw_count,
    }
}

fn print_usage() {
    println!("Usage:");
    println!("  haveibeenpwned build out.ndb pwned-passwords-sha1.txt");
    println!("  haveibeenpwned check out.ndb <pw-sha1-hex>");
}

fn main() {
    if env::args().len() < 4 {
        print_usage();
        process::exit(1);
    }

    let cmd = env::args().nth(1).unwrap();
    let db_path = env::args().nth(2).unwrap();
    let arg = env::args().nth(3).unwrap();

    match &cmd[..] {
        "build" => {
            let (_db, _schema) = init_database();
            unimplemented!("TODO: Fill database.");
        }
        "check" => {

        }
        _ => {
            print_usage();
            process::exit(1);
        }
    }
}
