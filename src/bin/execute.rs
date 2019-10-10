// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

use std::io;

use noblit::binary::Cursor;
use noblit::datom::{Datom, Eid, Tid, Value};
use noblit::database::{Database, self};
use noblit::memory_store::{MemoryStore, MemoryPool};
use noblit::query::{Query, QueryMut, QueryAttribute, QueryValue};
use noblit::query_plan::{Evaluator, QueryPlan};
use noblit::store::{PageSize4096};
use noblit::types::{Type, self};

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

fn run_query_mut(
    cursor: &mut Cursor,
    engine: &mut QueryEngine,
    transaction: Tid,
    next_id: &mut u64,
) -> Vec<Datom> {
    let mut query_mut = QueryMut::parse(cursor, engine.pool_mut()).expect("Failed to parse query.");

    // Resolve named attributes to id-based attributes, and plan the read-only
    // part of the query.
    query_mut.fix_attributes(engine);
    let plan = QueryPlan::new(query_mut.read_only_part(), engine);

    let mut datoms_to_insert = Vec::new();
    let mut rows_to_return = Vec::new();

    // For each assignment of values to the bound variables, run the assertions.
    for result in Evaluator::new(&plan, engine) {
        // To generate the asserted datoms, we need a value for every variable.
        // For the bound variables, we get them from the query results. For the
        // free variables, we need to generate new entities. The read-only part
        // of the query selects all bound variables in their order of
        // occurrence, so we can map the result onto the bound values directly.
        let mut bound_values = Vec::with_capacity(query_mut.variable_names.len());
        bound_values.extend_from_slice(&result[..]);

        for _ in query_mut.free_variables.iter() {
            // Generate a fresh entity id for every new entity. Increment by 2
            // because transaction ids claim the even ones (for now).
            // TODO: Encapsulate id generation better.
            bound_values.push(Value::from_eid(Eid(*next_id)));
            *next_id += 2;
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
            datoms_to_insert.push(datom);
        }

        // Collect the values to return from the query; those listed in the
        // select clause.
        let mut row = Vec::with_capacity(query_mut.select.len());
        for &v in query_mut.select.iter() {
            row.push(bound_values[v.0 as usize]);
        }
        rows_to_return.push(row.into_boxed_slice());
    }

    // Before we can print the results, we need to know the types of the
    // selected variables. For bound variables, we can get them from the plan.
    // For free variables, the type is always ref, because we create entities.
    let mut select_types = Vec::with_capacity(query_mut.select.len());
    for &v in query_mut.select.iter() {
        let is_bound = (v.0 as usize) < query_mut.bound_variables.len();
        if is_bound {
            let index = plan.mapping[v.0 as usize];
            select_types.push(plan.variable_types[index.0 as usize]);
        } else {
            // Free variables are entities.
            select_types.push(Type::Ref);
        }
    }

    let stdout = io::stdout();
    types::draw_table(
        &mut stdout.lock(),
        engine.pool(),
        query_mut.select.iter().map(|&v| &query_mut.variable_names[v.0 as usize][..]),
        rows_to_return.iter().map(|ref row| &row[..]),
        &select_types[..],
    ).unwrap();

    datoms_to_insert
}

fn main() {
    // Construct an empty in-ememory database, and query engine on top.
    let store: MemoryStore4096 = MemoryStore::new();
    let pool = MemoryPool::new();

    let mut db = Database::new(store, pool).unwrap();
    let mut cursor = Cursor::new(io::stdin());

    loop {
        match cursor.take_u8() {
            Ok(0) => {
                let mut engine = db.query();
                run_query(&mut cursor, &mut engine);
            }
            Ok(1) => {
                let transaction = Tid(db.next_transaction_id);
                db.next_transaction_id += 2;

                let mut next_id = db.next_id;

                let (mut datoms, temporaries) = {
                    let mut engine = db.query();
                    let datoms = run_query_mut(&mut cursor, &mut engine, transaction, &mut next_id);
                    (datoms, engine.into_temporaries())
                };
                db.persist_temporaries(&temporaries, &mut datoms[..]).unwrap();
                db.insert(datoms).expect("Failed to insert datoms.");
                db.next_id = next_id;
            }
            Ok(n) => panic!("Unsupported operation: {}", n),
            // EOF is actually expected. At EOF, we are done.
            Err(ref e) if e.kind() == io::ErrorKind::UnexpectedEof => break,
            Err(e) => panic!("{}", e),
        }

        // This "execute" program is used in the golden tests, so perform a
        // consistency check after every mutation, so we can use the goldens to
        // detect/reproduce consistency issues in the indexes, if any come up.
        db.eavt().check_invariants().unwrap();
        db.aevt().check_invariants().unwrap();
        db.avet().check_invariants().unwrap();
    }
}
