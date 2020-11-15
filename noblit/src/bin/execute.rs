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
use noblit::plan::Plan;
use noblit::planner::Planner;
use noblit::query::{QueryAttribute, QueryValue, Statement, Query};
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

    let plan = Planner::plan(&query);

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

    let plan = Planner::plan(&query);
    println!("{:?}", plan);
}

/// The minimum of a number of duration measurements.
#[derive(Clone)]
struct MinDuration {
    min_duration: Duration,
    num_samples: u32,
}

impl MinDuration {
    fn new() -> MinDuration {
        MinDuration {
            min_duration: Duration::from_secs(0),
            num_samples: 0,
        }
    }

    /// Observe a new duration measurement.
    fn observe(&mut self, duration: Duration) {
        if self.num_samples == 0 {
            self.min_duration = duration;
            self.num_samples = 1;
        } else {
            self.min_duration = self.min_duration.min(duration);
            self.num_samples += 1;
        }
    }

    /// Return a probablistic lower bound on the minimum duration.
    ///
    /// Returns the current minimum scaled by `(1 - 1 / sqrt(n))`. As the number
    /// of samples goes to infinity, the returned value goes to the minimum.
    fn estimate_lower_bound_ns(&self) -> f64 {
        let duration_ns
            = self.min_duration.subsec_nanos() as f64
            + (self.min_duration.as_secs() as f64) * 1e-9;

        match self.num_samples {
            0 => 0.0,
            n => duration_ns * (1.0 - (n as f64).sqrt().recip())
        }
    }
}

struct TimedPlan {
    duration: MinDuration,
    plan: Plan,
}

struct PartialQuery {
    /// Duration of running the query with `include` statements included.
    duration: MinDuration,
    include: Vec<Statement>,
    exclude: Vec<Statement>,

    /// The possible plans for the query with `include` statements included.
    ///
    /// Also the times to run them. For the partial query, we are interested in
    /// the most efficient micro-plan. In other words, for a given statement
    /// order, we want to use the choose the indexes for each scan such that the
    /// total query time is minimized.
    plans: Vec<TimedPlan>,
}

impl PartialQuery {
    fn new(query: &Query) -> PartialQuery {
        PartialQuery {
            duration: MinDuration::new(),
            include: Vec::new(),
            exclude: query.where_statements.clone(),
            plans: Vec::new(),
        }
    }

    fn as_query(&self, query: &Query) -> Query {
        Query {
            variable_names: query.variable_names.clone(),
            where_statements: self.include.clone(),
            select: Vec::new(),
        }
    }

    fn is_done(&self) -> bool {
        self.exclude.len() == 0
    }

    fn expand(&self, into: &mut Vec<PartialQuery>, full_query: &Query) {
        assert!(self.exclude.len() > 0);

        for i in 0..self.exclude.len() {
            // Build a new partial query that includes one more statement.
            let mut exclude = self.exclude.clone();
            let mut include = self.include.clone();
            let stmt = exclude.remove(i);
            include.push(stmt);

            let mut partial = PartialQuery {
                include: include,
                exclude: exclude,
                duration: MinDuration::new(),
                plans: Vec::new(),
            };

            // Generate all possible query plans for this given statement order.
            let query = partial.as_query(&full_query);
            let mut planner = Planner::new(&query);
            planner.initialize_scans();

            loop {
                let tp = TimedPlan {
                    duration: MinDuration::new(),
                    plan: planner.get_plan().clone(),
                };
                partial.plans.push(tp);
                if !planner.next() { break }
            }

            into.push(partial);
        }
    }
}

fn optimize_query(cursor: &mut Cursor, database: &Database) {
    let mut temporaries = Temporaries::new();
    let mut query = parse::parse_query(cursor, &mut temporaries).expect("Failed to parse query.");
    let view = database.view(temporaries);
    query.fix_attributes(&view);

    let mut open = Vec::new();
    open.push(PartialQuery::new(&query));

    // The minimum number of timing measurements we need to have done for a
    // particular plan, before we are confident enough about its timing to
    // accept the minimum observed duration over all iterations as the "true
    // duration". Higher numbers give us more accurate results, but we hit
    // diminishing results quickly. The goal is to rank plans, so we only need
    // enough accuracy to tell which plan is faster with some confidence.
    let mut min_iters_expand: u32 = 2_000;
    let min_iters_final: u32 = 5_000;

    while let Some(mut candidate) = open.pop() {
        if candidate.is_done() && candidate.plans[0].duration.num_samples >= min_iters_final {
            println!(
                "\nOptimization complete.",
            );
            for timed_plan in &candidate.plans {
                println!(
                    "  Micro {:.1} µs .. {:?} (n={})",
                    timed_plan.duration.estimate_lower_bound_ns() * 1e-3,
                    timed_plan.duration.min_duration,
                    timed_plan.duration.num_samples,
                );
            }
            println!("\nPlan:\n{:?}", candidate.plans[0].plan);
            break
        }

        let can_expand = candidate.exclude.len() > 0;
        let must_expand = candidate.include.len() == 0;

        if must_expand || (can_expand && candidate.duration.num_samples >= min_iters_expand) {
            println!(
                "[{}/{}] [{} open] Candidate {:?} (n={})",
                candidate.include.len(),
                candidate.include.len() + candidate.exclude.len(),
                open.len(),
                candidate.duration.min_duration,
                candidate.duration.num_samples,
            );

            // Adjust the number of iterations before expanding for a target
            // time of 0.5 seconds per candidate, capped at a maximum of 2000
            // iterations and a minimum of 5. Without this, optimization can
            // take hours as soon as the partial query gets in the millisecond
            // range.
            min_iters_expand = if candidate.duration.min_duration.as_secs() == 0 {
                5.max(2000.min(500_000 / (1 + candidate.duration.min_duration.subsec_micros())))
            } else {
                5
            };

            for timed_plan in &candidate.plans {
                println!(
                    "  Micro {:.1} µs .. {:?} (n={})",
                    timed_plan.duration.estimate_lower_bound_ns() * 1e-3,
                    timed_plan.duration.min_duration,
                    timed_plan.duration.num_samples,
                );
            }

            // Print the best 3 alternatives too, so we can see where the
            // ranking is focussing.
            for c in open.iter().rev().take(3) {
                println!(
                    "  {:.1} µs .. {:?} (n={})",
                    c.duration.estimate_lower_bound_ns() * 1e-3,
                    c.duration.min_duration,
                    c.duration.num_samples,
                );
            }

            // If we've taken enough samples to know that the candidate we are
            // looking at is really the best one, expand it. We add all the
            // child nodes in the tree to the open set, and this candidate one
            // is now considered closed.
            candidate.expand(&mut open, &query);
        } else {
            // If this base ranks as the current most promising candidate, but
            // we don't have sufficient samples to prove it, do more one more
            // measurement to increase our confidence.

            let duration = {
                let start = Instant::now();
                let eval = Evaluator::new(&candidate.plans[0].plan, &view);
                let _count = eval.count();
                let end = Instant::now();
                end.duration_since(start)
            };

            // Record durations for the micro-plan and macro-plan. For a given
            // statement order, the minimum duration is the minimium duration of
            // the fastest micro-plan.
            candidate.plans[0].duration.observe(duration);
            candidate.duration.observe(duration);

            // Sort plans in the candidate by ascending duration, so the best
            // plan is at index 0.
            candidate.plans.sort_by(|a, b| {
                let a_ns = a.duration.estimate_lower_bound_ns();
                let b_ns = b.duration.estimate_lower_bound_ns();
                a_ns.partial_cmp(&b_ns).unwrap()
            });

            open.push(candidate);
        }

        // Sort candidates by descending duration; we pop from the end. We don't
        // really need to sort, we only need the best candidate, so a binary
        // heap would suffice. But sorting is fast enough anyway for now.
        open.sort_by(|a, b| {
            let a_ns = a.duration.estimate_lower_bound_ns();
            let b_ns = b.duration.estimate_lower_bound_ns();
            b_ns.partial_cmp(&a_ns).unwrap()
        });
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

        let plan = Planner::plan(&query);

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
