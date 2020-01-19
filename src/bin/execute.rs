// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

use std::io;
use std::time::{Duration, Instant};

use noblit::binary::Cursor;
use noblit::database;
use noblit::datom::Value;
use noblit::disk;
use noblit::eval::Evaluator;
use noblit::memory_store::{MemoryStore, MemoryHeap};
use noblit::parse;
use noblit::permutation::Permutations;
use noblit::planner::Planner;
use noblit::query::{QueryAttribute, QueryValue};
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

    // TODO: Add getter to get the slice of selected types.
    let types = query.infer_types(&view).expect("Type error.");
    let select_types: Vec<_> = query.select.iter().map(|s| types[s.0 as usize]).collect();

    // TODO: Add one-step wrapper that does not involve the manual
    // initialization step.
    let mut planner = Planner::new(&query);
    planner.initialize_scans();
    let plan = planner.get_plan();

    // Evaluate the query, and pretty-print the results in a table.
    let eval = Evaluator::new(&plan, &view);
    let rows: Vec<_> = eval.collect();
    let stdout = io::stdout();
    types::draw_table(
        &mut stdout.lock(),
        view.heap(),
        plan.select.iter().enumerate().map(|(i, _)| plan.get_select_name(i)),
        rows.iter().map(|ref row| &row[..]),
        &select_types[..],
    ).unwrap();
}

fn explain_query(cursor: &mut Cursor, database: &Database) {
    let mut temporaries = Temporaries::new();
    let mut query = parse::parse_query(cursor, &mut temporaries).expect("Failed to parse query.");
    let view = database.view(temporaries);
    query.fix_attributes(&view);

    // Print the initial plan.
    let mut planner = Planner::new(&query);
    planner.initialize_scans();
    println!("{:?}", planner.get_plan());
}

fn optimize_query(cursor: &mut Cursor, database: &Database) {
    let mut temporaries = Temporaries::new();
    let mut query = parse::parse_query(cursor, &mut temporaries).expect("Failed to parse query.");
    let view = database.view(temporaries);
    query.fix_attributes(&view);

    let mut planner = Planner::new(&query);

    // Print the initial plan.
    planner.initialize_scans();

    let mut best_plan = None;
    let mut min_duration = Duration::from_secs(600);
    let mut all_durations = Vec::new();

    planner.initialize_scans();

    for _ in 0..50 {
        loop {
            {
                let plan = planner.get_plan();
                let mut durations = Vec::with_capacity(5);

                for _ in 0..30 {
                    let start = Instant::now();
                    let eval = Evaluator::new(&plan, &view);
                    let _count = eval.count();
                    let end = Instant::now();
                    let duration = end.duration_since(start);
                    durations.push(duration);
                }

                // Take the p33 duration.
                durations.sort();
                let duration = durations[10];

                if duration < min_duration {
                    min_duration = duration;
                    best_plan = Some(plan.clone());
                    println!("IMPROVE {:?}", min_duration);
                }

                all_durations.push(duration);
            }

            if !planner.next() { break }
        }
    }

    if let Some(best) = best_plan {
        println!("Minimum time: {:?}", min_duration);
        println!("{:?}\n", best);
        all_durations.sort();
        let p10 = all_durations[all_durations.len() * 1 / 10];
        let p50 = all_durations[all_durations.len() * 5 / 10];
        let p90 = all_durations[all_durations.len() * 9 / 10];
        println!("p10 time: {:?}", p10);
        println!("p50 time: {:?}", p50);
        println!("p90 time: {:?}", p90);
    }
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

        let query = mutation.read_only_part();
        let types = query.infer_types(&view).expect("Type error.");

        let mut planner = Planner::new(&query);
        planner.initialize_scans();
        let plan = planner.get_plan();

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
        // selected variables. For bound variables, we can get them from type
        // inference of the query. For free variables, the type is always ref,
        // because we create entities.
        let mut select_types = Vec::with_capacity(mutation.select.len());
        for &v in mutation.select.iter() {
            let is_bound = (v.0 as usize) < mutation.bound_variables.len();
            match is_bound {
                true => select_types.push(types[v.0 as usize]),
                false => select_types.push(Type::Ref),
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
    use std::env;
    use std::fs;

    // If a filename was provided on the command line, load that database into
    // memory and use it as a starting point. If not, we start with an empty in-
    // memory database.
    let mut db = match env::args().nth(1) {
        Some(fname) => {
            let f = fs::File::open(fname).expect("Failed to open database file.");
            disk::read_packed(&mut io::BufReader::new(f)).expect("Failed to read database.")
        }
        None => {
            let store: MemoryStore4096 = MemoryStore::new();
            let heap = MemoryHeap::new();
            Database::new(store, heap).unwrap()
        }
    };

    let mut cursor = Cursor::new(io::stdin());

    loop {
        match cursor.take_u8() {
            Ok(0) => run_query(&mut cursor, &db),
            Ok(1) => run_mutation(&mut cursor, &mut db),
            Ok(2) => explain_query(&mut cursor, &db),
            Ok(3) => optimize_query(&mut cursor, &db),
            Ok(n) => panic!("Unsupported operation: {}", n),
            // EOF is actually expected. At EOF, we are done.
            Err(ref e) if e.kind() == io::ErrorKind::UnexpectedEof => break,
            Err(e) => panic!("{}", e),
        }

        // This "execute" program is used in the golden tests, so perform a
        // consistency check after every mutation, so we can use the goldens to
        // detect/reproduce consistency issues in the indexes, if any come up.
        db.check_invariants().unwrap();

        // After every response, print ascii 0x04 "end of transmission" on its
        // own line.
        println!("\u{04}");
    }
}
