// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

use std::io;

use noblit::binary::Cursor;
use noblit::database;
use noblit::datom::Value;
use noblit::memory_store::{MemoryStore, MemoryHeap};
use noblit::parse;
use noblit::query::{QueryAttribute, QueryValue};
use noblit::query_plan::{Evaluator, QueryPlan};
use noblit::store::PageSize4096;
use noblit::temp_heap::Temporaries;
use noblit::types::{Type, self};

type MemoryStore4096 = MemoryStore<PageSize4096>;
type Database = database::Database<MemoryStore4096, MemoryHeap>;

fn run_query(cursor: &mut Cursor, database: &Database) {
    let mut temporaries = Temporaries::new();
    let mut query = parse::parse_query(cursor, &mut temporaries).expect("Failed to parse query.");

    let view = database.view(temporaries);

    // Resolve named attributes to id-based attributes, and plan the query.
    query.fix_attributes(&view);
    let plan = QueryPlan::new(query, &view);

    // Evaluate the query, and pretty-print the results in a table.
    let eval = Evaluator::new(&plan, &view);
    let rows: Vec<_> = eval.collect();
    let stdout = io::stdout();
    types::draw_table(
        &mut stdout.lock(),
        view.heap(),
        plan.select.iter().map(|&v| &plan.variable_names[v.0 as usize][..]),
        rows.iter().map(|ref row| &row[..]),
        &plan.select_types[..],
    ).unwrap();
}

/// Evaluate the mutation, return the datoms produced.
fn run_mutation(cursor: &mut Cursor, database: &mut Database) {
    let mut transaction = database.begin();
    let mut temporaries = Temporaries::new();
    let mut mutation = parse::parse_mutation(cursor, &mut temporaries).expect("Failed to parse query.");

    let temporaries = {
        let view = database.view(temporaries);

        // Resolve named attributes to id-based attributes, and plan the read-only
        // part of the query.
        mutation.fix_attributes(&view);
        let plan = QueryPlan::new(mutation.read_only_part(), &view);

        let mut rows_to_return = Vec::new();

        // For each assignment of values to the bound variables, run the assertions.
        for result in Evaluator::new(&plan, &view) {
            // To generate the asserted datoms, we need a value for every variable.
            // For the bound variables, we get them from the query results. For the
            // free variables, we need to generate new entities. The read-only part
            // of the query selects all bound variables in their order of
            // occurrence, so we can map the result onto the bound values directly.
            let mut bound_values = Vec::with_capacity(mutation.variable_names.len());
            bound_values.extend_from_slice(&result[..]);

            for _ in mutation.free_variables.iter() {
                // Generate a fresh entity id for every new entity.
                let entity_id = transaction.create_entity();
                bound_values.push(Value::from_eid(entity_id));
            }

            for assertion in &mutation.assertions[..] {
                let entity = bound_values[assertion.entity.0 as usize].as_eid(view.heap());
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
                transaction.assert(entity, attribute, value);
            }

            // Collect the values to return from the query; those listed in the
            // select clause.
            let mut row = Vec::with_capacity(mutation.select.len());
            for &v in mutation.select.iter() {
                row.push(bound_values[v.0 as usize]);
            }
            rows_to_return.push(row.into_boxed_slice());
        }

        // Before we can print the results, we need to know the types of the
        // selected variables. For bound variables, we can get them from the plan.
        // For free variables, the type is always ref, because we create entities.
        let mut select_types = Vec::with_capacity(mutation.select.len());
        for &v in mutation.select.iter() {
            let is_bound = (v.0 as usize) < mutation.bound_variables.len();
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
            view.heap(),
            mutation.select.iter().map(|&v| &mutation.variable_names[v.0 as usize][..]),
            rows_to_return.iter().map(|ref row| &row[..]),
            &select_types[..],
        ).unwrap();

        view.into_temporaries()
    };
    database.commit(&temporaries, transaction).expect("Failed to commit transaction.");
}

fn main() {
    // Construct an empty in-ememory database, and query engine on top.
    let store: MemoryStore4096 = MemoryStore::new();
    let heap = MemoryHeap::new();

    let mut db = Database::new(store, heap).unwrap();
    let mut cursor = Cursor::new(io::stdin());

    loop {
        match cursor.take_u8() {
            Ok(0) => run_query(&mut cursor, &db),
            Ok(1) => run_mutation(&mut cursor, &mut db),
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
