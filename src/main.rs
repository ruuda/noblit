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
        // where
        //   a db.attribute.name name
        //   a db.attribute.type t
        //   a db.attribute.unique unique
        //   a db.attribute.many many
        //   t db.type.name type
        // select
        //   a, name, t, type, unique, many

        use query::{Query, Statement, Var};
        let mut query = Query {
            variable_names: vec![
                "a".to_string(),        // 0
                "t".to_string(),        // 1
                "name".to_string(),     // 2
                "unique".to_string(),   // 3
                "many".to_string(),     // 4
                "type".to_string(),     // 5
            ],
            where_statements: vec![
                Statement::named_var(Var(0), "a.name", Var(2)),
                Statement::named_var(Var(0), "a.type", Var(1)),
                Statement::named_var(Var(0), "a.uniq", Var(3)),
                Statement::named_var(Var(0), "a.many", Var(4)),
                Statement::named_var(Var(1), "t.name", Var(5)),
            ],
        };
        query.fix_attributes(&db);
        let plan = QueryPlan::new(query, &db);

        println!("\nAll attributes:");
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
