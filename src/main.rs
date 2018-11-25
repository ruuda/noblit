// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

mod database;
mod datom;
mod index;
mod query_plan;
mod types;

use database::Database;
use query_plan::{Evaluator, QueryPlan};

fn main() {
    let db = Database::new();
    db.debug_print();

    {
        println!("\nAll attributes:");
        let plan = QueryPlan::example_all_attributes(&db.builtins);
        let eval = Evaluator::new(&plan, &db);
        let rows: Vec<_> = eval.collect();
        let stdout = std::io::stdout();
        types::draw_table(
            &mut stdout.lock(),
            ["id", "name", "type", "type_name", "unique", "many"].iter().map(|s| &s[..]),
            rows.iter().map(|ref row| &row[..]),
            &plan.types[..],
        ).unwrap();
    }

    {
        println!("\nAll types:");
        let plan = QueryPlan::example_all_types(&db.builtins);
        let eval = Evaluator::new(&plan, &db);
        let rows: Vec<_> = eval.collect();
        let stdout = std::io::stdout();
        types::draw_table(
            &mut stdout.lock(),
            ["id", "name"].iter().map(|s| &s[..]),
            rows.iter().map(|ref row| &row[..]),
            &plan.types[..],
        ).unwrap();
    }
}
