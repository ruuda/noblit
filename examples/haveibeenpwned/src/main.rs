// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Build a Noblit database from a "Have I Been Pwned?" file.
//!
//! This example provides a binary that converts the text-based dumps from
//! [Have I Been Pwned][hibp] into a Noblit database, in order to query quickly
//! whether a password is present in the dump.
//!
//! To run this example, first [download][hibp] and extract the text file with
//! pwned SHA-1 hashes from Have I Been Pwned. To convert it into a Noblit
//! database, run this example in “build” mode:
//!
//! ```text
//! $ haveibeenpwned build pwned.ndb pwned-passwords-sha1-ordered-by-count-v5.txt
//! ```
//!
//! Once the database has been built, we can check for the presence of a given
//! password with “check” mode:
//!
//! ```text
//! $ echo -n 'hello' | sha1sum
//! aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d  -
//!
//! $ haveibeenpwned check pwned.ndb aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d
//! SHA1 is present with count 253581.
//! ```
//!
//! This example is inspired by [this post][stryku] on stryku.pl.
//!
//! [hibp]:   https://haveibeenpwned.com/
//! [stryku]: http://stryku.pl/poetry/okon.php

extern crate noblit;

use std::env;
use std::fs;
use std::io::{BufRead, Write};
use std::io;
use std::process;
use std::str::FromStr;

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

struct Password {
    sha1: [u8; 20],
    count: u64,
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

/// Insert all passwords into the database. Leaves `pws` empty.
fn insert_batch(db: &mut Database, schema: &Schema, pws: &mut Vec<Password>) {
    let mut tx = db.begin();
    let mut tmps = Temporaries::new();

    for pw in pws.drain(..) {
        let eid = tx.create_entity();
        let cid = tmps.push_bytes(Box::new(pw.sha1));
        tx.assert(eid, schema.pw_sha1, Value::from_const_bytes(cid));
        tx.assert(eid, schema.pw_count, Value::from_u64_inline(pw.count));
    }

    db.commit(&tmps, tx).expect("TODO: Good Result types.");
}

/// Run a query to check whether the SHA1 is present, print the results.
fn check_password(db: &Database, sha1: &[u8; 20]) {
    use noblit::query::{Query, Statement, Var};

    let mut temporaries = Temporaries::new();
    let cid_pw_sha1 = temporaries.push_string("pw.sha1".to_string());
    let cid_pw_count = temporaries.push_string("pw.count".to_string());
    let cid_sha1 = temporaries.push_bytes(Box::new(sha1.clone()));

    let mut view = db.view(temporaries);

    // Encode this query:
    // TODO: Add a more user-friendly way of doing this.
    // where
    //   pw pw.sha1 <sha1>
    //   pw pw.count c
    // select
    //   c
    let mut query = Query {
        variable_names: vec![
            "pw".to_string(), // 0
            "c".to_string(),  // 1
        ],
        where_statements: vec![
            Statement::named_const(Var(0), cid_pw_sha1, Value::from_const_bytes(cid_sha1)),
            Statement::named_var(Var(0), cid_pw_count, Var(1)),
        ],
        select: vec![Var(1)],
    };
    query.fix_attributes(&mut view);
    query.infer_types(&view).expect("Query contains a type error.");

    let plan = noblit::planner::Planner::plan(&query);
    let eval = noblit::eval::Evaluator::new(&plan, &view);
    let rows: Vec<_> = eval.collect();

    match rows.len() {
        0 => println!("SHA1 is not present in the database."),
        1 => println!("SHA1 is present with count {}.", rows[0][0].as_u64(view.heap())),
        n => panic!("SHA1 was found more than once."),
    }

    // TODO: Print timing information.
}

/// Parse a hexadecimal sha1 hash, or crash.
fn parse_sha1(sha1_hex: &str) -> [u8; 20] {
    assert_eq!(sha1_hex.len(), 40);

    let mut sha1 = [0; 20];
    for i in 0..20 {
        let byte_str = &sha1_hex[i * 2..i * 2 + 2];
        let byte = u8::from_str_radix(byte_str, 16).expect("Expected hexadecimal SHA1 hash.");
        sha1[i] = byte;
    }

    sha1
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
            let (mut db, schema) = init_database();
            let f = fs::File::open(arg).expect("Failed to open input file.");
            let mut reader = io::BufReader::new(f);
            let mut batch = Vec::new();
            let mut i_batch = 0;

            for (i, opt_line) in reader.lines().enumerate() {
                let line = opt_line.expect("Failed to read input line.");

                // The lines have the format "<sha1>:<count>", with the sha1 in
                // hexadecimal (40 characters), a colon, and the count in ascii
                // decimal digits.
                let sha1_hex = &line[..40];
                let count_str = &line[41..];
                assert_eq!(&line[40..41], ":");

                let pw = Password {
                    sha1: parse_sha1(sha1_hex),
                    count: u64::from_str(count_str).expect("Failed to parse count."),
                };
                batch.push(pw);

                if batch.len() >= 100_000 {
                    insert_batch(&mut db, &schema, &mut batch);
                    i_batch += 1;
                    print!("\rInserted {} batches, {} passwords.", i_batch, i + 1);
                    io::stdout().flush().unwrap();
                }

                if db.get_store().as_bytes().len() >= 1_000_000_000 {
                    println!("");
                    println!("Stopping after {} passwords, store grew larger than 1G.", i + 1);
                    break
                }
            }

            let f = fs::File::create(db_path).expect("Failed to open output file.");
            let mut writer = io::BufWriter::new(f);
            noblit::disk::write_packed(&db, &mut writer).expect("Failed to write databse.");
        }
        "check" => {
            let needle = parse_sha1(&arg[..]);
            let f = fs::File::open(db_path).expect("Failed to open input database.");
            let mut reader = io::BufReader::new(f);
            let db = noblit::disk::read_packed(&mut reader).expect("Failed to read databse.");
            check_password(&db, &needle);
        }
        _ => {
            print_usage();
            process::exit(1);
        }
    }
}
