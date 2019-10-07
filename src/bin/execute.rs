// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

use std::io::{Read, self};

use noblit::binary::Cursor;
use noblit::datom::{Datom, Eid, Tid, Value};
use noblit::database::{Database, self};
use noblit::memory_store::{MemoryStore, MemoryPool};
use noblit::query::{Query, QueryMut, QueryAttribute, QueryValue};
use noblit::query_plan::{Evaluator, QueryPlan};
use noblit::store::{PageSize4096};
use noblit::types;

type MemoryStore4096 = MemoryStore<PageSize4096>;
type QueryEngine<'a> = database::QueryEngine<'a, MemoryStore4096, MemoryPool>;

fn run_query(cursor: &mut Cursor, engine: &mut QueryEngine) {
    let mut query = Query::parse(cursor, engine.pool_mut()).expect("Failed to parse query.");

    // Resolve named attributes to id-based attributes, and plan the query.
    query.fix_attributes(engine);
    let plan = QueryPlan::new(query, engine);

    // Evaluate the query, and pretty-print the results in a table.
    let eval = Evaluator::new(&plan, engine);
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

fn run_query_mut(cursor: &mut Cursor, engine: &mut QueryEngine) {
    let mut query_mut = QueryMut::parse(cursor, engine.pool_mut()).expect("Failed to parse query.");

    // Resolve named attributes to id-based attributes, and plan the read-only
    // part of the query.
    query_mut.fix_attributes(engine);
    let plan = QueryPlan::new(query_mut.read_only_part(), engine);

    // TODO: Allocate real entity ids and transaction ids from the database.
    let mut eid = 200;
    let transaction = Tid(50);

    // For each assignment of values to the bound variables, run the assertions.
    for result in Evaluator::new(&plan, engine) {
        // To generate the asserted datoms, we need a value for every variable.
        // For the bound variables, we get them from the query results. For the
        // free variables, we need to generate new entities.
        let mut bound_values = Vec::with_capacity(query_mut.variable_names.len());
        bound_values.extend_from_slice(&result[..]);

        for _ in query_mut.free_variables.iter() {
            // TODO: Allocate real entity ids from the database.
            eid += 2;
            bound_values.push(Value::from_eid(Eid(eid)));
        }

        for assertion in &query_mut.assertions[..] {
            let entity = bound_values[assertion.entity.0 as usize].as_eid(engine.pool());
            let attribute = match assertion.attribute {
                QueryAttribute::Fixed(aid) => aid,
                QueryAttribute::Named(..) => {
                    panic!("Should have resolved attribute name to id already.");
                }
            };
            let value = match assertion.value {
                QueryValue::Const(v) => v,
                QueryValue::Var(var) => bound_values[var.0 as usize],
            };
            let datom = Datom::assert(entity, attribute, value, transaction);
            println!("  {:?}", datom);
        }
    }
}

fn main() {
    // Construct an empty in-ememory database, and query engine on top.
    let store: MemoryStore4096 = MemoryStore::new();
    let pool = MemoryPool::new();

    let db = Database::new(store, pool).unwrap();
    let mut engine = db.query();

    // Read stdin until EOF so we can parse it later.
    let mut query_bytes = Vec::new();
    io::stdin().read_to_end(&mut query_bytes).unwrap();

    let mut cursor = Cursor::new(&query_bytes[..]);

    loop {
        match cursor.peek_u8() {
            Some(0) => run_query(&mut cursor, &mut engine),
            Some(1) => run_query_mut(&mut cursor, &mut engine),
            Some(n) => panic!("Unsupported operation: {}", n),
            None => break,
        }
    }
}
