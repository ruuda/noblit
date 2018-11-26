// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

mod database;
mod datom;
mod index;
mod query;
mod query_plan;
mod types;

use database::Database;
use query_plan::{Evaluator, QueryPlan};

fn main() {
    let db = Database::new();
    db.debug_print();

    {
        use query::{Query, Statement, Var};
        let query = Query {
            variable_names: vec![
                "id".to_string(),
                "name".to_string(),
                "type".to_string(),
                "type_name".to_string(),
                "unique".to_string(),
                "many".to_string(),
            ],
            where_statements: vec![
                Statement::named_var(Var(0), "a.name".to_string(), Var(1)),
                Statement::named_var(Var(0), "a.type".to_string(), Var(2)),
                Statement::named_var(Var(2), "t.name".to_string(), Var(3)),
                Statement::named_var(Var(0), "a.uniq".to_string(), Var(4)),
                Statement::named_var(Var(0), "a.many".to_string(), Var(5)),
            ],
        };

        // TODO: Build the plan from the above query.

        println!("\nAll attributes:");
        let plan = QueryPlan::example_all_attributes(&db.builtins);
        let eval = Evaluator::new(&plan, &db);
        let rows: Vec<_> = eval.collect();
        let stdout = std::io::stdout();
        types::draw_table(
            &mut stdout.lock(),
            plan.variable_names.iter().map(|s| &s[..]),
            rows.iter().map(|ref row| &row[..]),
            &plan.variable_types[..],
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
            plan.variable_names.iter().map(|s| &s[..]),
            rows.iter().map(|ref row| &row[..]),
            &plan.variable_types[..],
        ).unwrap();
    }
}
