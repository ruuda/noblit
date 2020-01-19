// Noblit -- An immutable append-only database
// Copyright 2019 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

use std::cmp::Ordering;
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

struct PlanBench {
    plan: Plan,
    durations_ns: Vec<f64>,
    duration_lo_ns: f64,
    duration_hi_ns: f64,
}

impl PlanBench {
    pub fn new(plan: Plan) -> PlanBench {
        PlanBench {
            plan: plan,
            durations_ns: Vec::new(),
            duration_lo_ns: 0.0,
            duration_hi_ns: 1000.0,
        }
    }

    pub fn observe(&mut self, duration: Duration) {
        // Insert the new duration, keeping the vector sorted.
        let duration_ns = duration.subsec_nanos() as f64 + 1e9 * (duration.as_secs() as f64);
        match self.durations_ns.binary_search_by(|x| x.partial_cmp(&duration_ns).unwrap()) {
            Ok(i) => self.durations_ns.insert(i, duration_ns),
            Err(i) => self.durations_ns.insert(i, duration_ns),
        };

        // Compute the lower bound of a 99.7% confidence interval for the mean,
        // and the mean itself. We cache it to ease sorting.
        let (duration_lo_ns, duration_hi_ns) = match self.durations_ns.len() {
            0 => (0.0, 1000.0),
            n => {
                self.durations_ns.sort_by(|a, b| a.partial_cmp(b).unwrap());
                let p05 = self.durations_ns[self.durations_ns.len() / 20];
                let f = 1.0 / (n as f64).sqrt();
                (p05 * (1.0 - f), p05 * (1.0 + f))
            }
        };
        self.duration_lo_ns = duration_lo_ns;
        self.duration_hi_ns = duration_hi_ns;
    }

    pub fn cmp(&self, other: &PlanBench) -> Ordering {
        match self.duration_lo_ns.partial_cmp(&other.duration_lo_ns).unwrap() {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => self.duration_hi_ns.partial_cmp(&other.duration_hi_ns).unwrap(),
        }
    }
}

fn _micro_optimize_query(cursor: &mut Cursor, database: &Database) {
    let mut temporaries = Temporaries::new();
    let mut query = parse::parse_query(cursor, &mut temporaries).expect("Failed to parse query.");
    let view = database.view(temporaries);
    query.fix_attributes(&view);

    let mut planner = Planner::new(&query);
    planner.initialize_scans();

    let mut benches = Vec::new();

    loop {
        benches.push(PlanBench::new(planner.get_plan().clone()));
        if !planner.next() { break }
    }

    while benches[0].duration_hi_ns - benches[0].duration_lo_ns > 100.0 {
        for _ in 0..1000 {
            benches.sort_by(|a, b| a.cmp(b));
            for &i in &[5, 1, 0] {
                let pb = &mut benches[i];
                let duration = {
                    let start = Instant::now();
                    let eval = Evaluator::new(&pb.plan, &view);
                    let _count = eval.count();
                    let end = Instant::now();
                    end.duration_since(start)
                };
                pb.observe(duration);
            }
        }

        benches.sort_by(|a, b| a.cmp(b));
        println!();
        for &i in &[0, 1, 2, benches.len() / 5, benches.len() - 1] {
            let pb = &benches[i];
            println!(
                "#{:2}: {:.3} us  ({:.3} us .. {:.3} us) (n = {})",
                i + 1,
                (pb.duration_lo_ns + pb.duration_hi_ns) * 0.5e-3,
                pb.duration_lo_ns * 1e-3,
                pb.duration_hi_ns * 1e-3,
                pb.durations_ns.len(),
            );
        }
    }

    println!("\n#1:\n{:?}", benches[0].plan);
    println!("\n#2:\n{:?}", benches[1].plan);
}

#[derive(Clone)]
struct PartialQuery {
    /// Duration of running the query with `include` statements included.
    duration: Duration,
    include: Vec<Statement>,
    exclude: Vec<Statement>,
}

impl PartialQuery {
    fn new(query: &Query) -> PartialQuery {
        PartialQuery {
            duration: Duration::from_secs(0),
            include: Vec::new(),
            exclude: query.where_statements.clone(),
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

    fn expand(&self, into: &mut Vec<PartialQuery>) {
        assert!(self.exclude.len() > 0);

        for i in 0..self.exclude.len() {
            let mut partial = self.clone();
            let stmt = partial.exclude.remove(i);
            partial.include.push(stmt);
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
    let mut candidates = Vec::new();
    let mut fastest_at_depth = Vec::new();
    open.push(PartialQuery::new(&query));
    fastest_at_depth.push(Duration::from_secs(0));

    while let Some(base) = open.pop() {
        if base.is_done() {
            let final_query = base.as_query(&query);
            let mut planner = Planner::new(&final_query);
            planner.initialize_scans();
            let plan = planner.get_plan();
            println!("Duration: {:?}\nPlan:\n{:?}", base.duration, plan);
            break
        }

        // For every number of included statements, keep track of the fastest
        // way of doing that many statements. We use this later to bail out
        // early when we try something at that depth that is obviously worse.
        if base.include.len() >= fastest_at_depth.len() {
            fastest_at_depth.push(base.duration);
        } else {
            let fastest = &mut fastest_at_depth[base.include.len()];
            *fastest = base.duration.min(*fastest);
        }

        println!(
            "[{}/{}] [{} open] Candidate {:?}",
            base.include.len(),
            base.include.len() + base.exclude.len(),
            open.len(),
            base.duration,
        );

        let record = if fastest_at_depth.len() > base.include.len() + 1 {
            fastest_at_depth[base.include.len() + 1]
        } else {
            Duration::from_secs(600)
        };

        base.expand(&mut candidates);
        'expand: for mut candidate in candidates.drain(..) {
            let partial_query = candidate.as_query(&query);
            let mut planner = Planner::new(&partial_query);
            planner.initialize_scans();
            let plan = planner.get_plan();

            let mut duration = Duration::from_secs(600);

            for i in 0..500 {
                let start = Instant::now();
                let eval = Evaluator::new(&plan, &view);
                let _count = eval.count();
                let end = Instant::now();
                duration = duration.min(end.duration_since(start));

                // If it is clear that this candidate is a lot slower than the
                // fastest known candidate at that depth, there is no point in
                // exploring further; cut off and go to the next candidate then.
                if duration > record * 100 {
                    continue 'expand;
                }
                if duration > record * 5 && i > 10 {
                    continue 'expand;
                }
            }

            candidate.duration = duration;
            open.push(candidate);
        }

        // Sort candidates by descending duration; we pop from the end.
        open.sort_by(|a, b| b.duration.cmp(&a.duration));
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
