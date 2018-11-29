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
    let mut db = Database::new();

    {
        // Insert a bit of test data: a new attribute "level" in transaction 0,
        // and a few entities with different levels in transaction 1.
        use datom::{Aid, Operation, Value};
        let db_attr_name = db.builtins.attribute_db_attribute_name;
        let db_attr_type = db.builtins.attribute_db_attribute_type;
        let db_attr_unique = db.builtins.attribute_db_attribute_unique;
        let db_attr_many = db.builtins.attribute_db_attribute_many;
        let db_type_uint64 = db.builtins.entity_db_type_uint64;

        let t0 = db.create_transaction();
        let eid = db.create_entity(db_attr_name, Value::from_str("level"), t0);
        db.assert(eid, db_attr_type, Value::from_eid(db_type_uint64), t0);
        db.assert(eid, db_attr_unique, Value::from_bool(false), t0);
        db.assert(eid, db_attr_many, Value::from_bool(false), t0);

        let attr_level = Aid(eid.0);
        let t1 = db.create_transaction();
        let l5 = db.create_entity(attr_level, Value::from_u64(5), t1);
        let l10 = db.create_entity(attr_level, Value::from_u64(10), t1);
        let l11 = db.create_entity(attr_level, Value::from_u64(11), t1);
    }

    {
        // where
        //   a db.attribute.name name
        //   a db.attribute.type t
        //   a db.attribute.unique unique
        //   a db.attribute.many many
        //   t db.type.name type
        // select
        //   a, name, type, unique, many

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
            select: vec![Var(0), Var(2), Var(5), Var(3), Var(4)],
        };
        query.fix_attributes(&db);
        let plan = QueryPlan::new(query, &db);

        println!("\nAll attributes:");
        let eval = Evaluator::new(&plan, &db);
        let rows: Vec<_> = eval.collect();
        let stdout = std::io::stdout();
        types::draw_table(
            &mut stdout.lock(),
            plan.select.iter().map(|&v| &plan.variable_names[v.0 as usize][..]),
            rows.iter().map(|ref row| &row[..]),
            &plan.select_types[..],
        ).unwrap();
    }

    {
        // where
        //   t db.type.name name
        // select
        //   t, name

        use query::{Query, Statement, Var};
        let mut query = Query {
            variable_names: vec![
                "t".to_string(),        // 0
                "name".to_string(),     // 1
            ],
            where_statements: vec![
                Statement::named_var(Var(0), "t.name", Var(1)),
            ],
            select: vec![Var(0), Var(1)],
        };
        query.fix_attributes(&db);
        let plan = QueryPlan::new(query, &db);

        println!("\nAll types:");
        let eval = Evaluator::new(&plan, &db);
        let rows: Vec<_> = eval.collect();
        let stdout = std::io::stdout();
        types::draw_table(
            &mut stdout.lock(),
            plan.select.iter().map(|&v| &plan.variable_names[v.0 as usize][..]),
            rows.iter().map(|ref row| &row[..]),
            &plan.select_types[..],
        ).unwrap();
    }
}
