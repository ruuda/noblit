// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

use std::io::{Read, self};

use noblit::binary::Cursor;
use noblit::database::Database;
use noblit::memory_store::{MemoryStore, MemoryPool};
use noblit::query;
use noblit::query_plan::{Evaluator, QueryPlan};
use noblit::store::{PageSize4096};
use noblit::types;
use query::Query;

fn main() {
    // Read stdin until EOF. Then parse that binary data into the query.
    let mut query_bytes = Vec::new();
    io::stdin().read_to_end(&mut query_bytes).unwrap();

    let mut cursor = Cursor::new(&query_bytes[..]);
    let mut query = Query::parse(&mut cursor).expect("Failed to parse query.");

    // Construct an empty in-ememory database, and query engine on top.
    let store: MemoryStore<PageSize4096> = MemoryStore::new();
    let pool = MemoryPool::new();

    let db = Database::new(store, pool).unwrap();
    let mut engine = db.query();

    // Resolve named attributes to id-based attributes, and plan the query.
    query.fix_attributes(&mut engine);
    let plan = QueryPlan::new(query, &engine);

    // Evaluate the query, and pretty-print the results in a table.
    let eval = Evaluator::new(&plan, &engine);
    let rows: Vec<_> = eval.collect();
    let stdout = io::stdout();
    types::draw_table(
        &mut stdout.lock(),
        engine.pool(),
        plan.select.iter().map(|&v| &plan.variable_names[v.0 as usize][..]),
        rows.iter().map(|ref row| &row[..]),
        &plan.select_types[..],
    ).unwrap();
}
