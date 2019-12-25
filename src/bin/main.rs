// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

use noblit::database::Database;
use noblit::datom::{Aid, Datom, Value};
use noblit::memory_store::{MemoryStore, MemoryHeap};
use noblit::query;
use noblit::query_plan::{Evaluator, QueryPlan};
use noblit::store::{PageSize4096};
use noblit::temp_heap::Temporaries;
use noblit::types;

fn main() {
    let store: MemoryStore<PageSize4096> = MemoryStore::new();
    let heap = MemoryHeap::new();

    let mut db = Database::new(store, heap).unwrap();

    {
        // Insert a bit of test data: a new attribute "level" in transaction 0,
        // and a few entities with different levels in transaction 1.
        let db_attr_name = db.builtins.attribute_db_attribute_name;
        let db_attr_type = db.builtins.attribute_db_attribute_type;
        let db_attr_unique = db.builtins.attribute_db_attribute_unique;
        let db_attr_many = db.builtins.attribute_db_attribute_many;
        let db_type_uint64 = db.builtins.entity_db_type_uint64;
        let db_type_string = db.builtins.entity_db_type_string;

        let mut datoms = Vec::new();
        let t0 = db.create_transaction();
        let eid_level = db.create_entity(&mut datoms, db_attr_name, Value::from_str_inline("level"), t0);
        datoms.push(Datom::assert(eid_level, db_attr_type, Value::from_eid(db_type_uint64), t0));
        datoms.push(Datom::assert(eid_level, db_attr_unique, Value::from_bool(false), t0));
        datoms.push(Datom::assert(eid_level, db_attr_many, Value::from_bool(false), t0));
        let eid_name = db.create_entity(&mut datoms, db_attr_name, Value::from_str_inline("name"), t0);
        datoms.push(Datom::assert(eid_name, db_attr_type, Value::from_eid(db_type_string), t0));
        datoms.push(Datom::assert(eid_name, db_attr_unique, Value::from_bool(true), t0));
        datoms.push(Datom::assert(eid_name, db_attr_many, Value::from_bool(false), t0));
        db.insert(datoms).expect("Failed to commit transaction.");

        let mut datoms = Vec::new();
        let attr_level = Aid(eid_level.0);
        let attr_name = Aid(eid_name.0);
        let t1 = db.create_transaction();
        // Note: the insertion order is deliberately not sorted in advance to
        // expose ordering bugs.
        // TODO: Turn this into a test case.
        let e1 = db.create_entity(&mut datoms, attr_level, Value::from_u64_inline(11), t1);
        let e2 = db.create_entity(&mut datoms, attr_level, Value::from_u64_inline(13), t1);
        let e3 = db.create_entity(&mut datoms, attr_level, Value::from_u64_inline(5), t1);
        let e4 = db.create_entity(&mut datoms, attr_level, Value::from_u64_inline(97), t1);
        let v1 = db.persist_value_bytes("Henk de Steen".as_bytes()).unwrap();
        let v2 = db.persist_value_bytes("Klaas de Rots".as_bytes()).unwrap();
        let v3 = db.persist_value_bytes("Sjaak de Kei".as_bytes()).unwrap();
        let v4 = db.persist_value_bytes("Aart".as_bytes()).unwrap();
        datoms.push(Datom::assert(e1, attr_name, v1, t1));
        datoms.push(Datom::assert(e2, attr_name, v2, t1));
        datoms.push(Datom::assert(e3, attr_name, v3, t1));
        datoms.push(Datom::assert(e4, attr_name, v4, t1));
        db.insert(datoms).expect("Failed to commit transaction.");
    }

    db.eavt().check_invariants().unwrap();
    db.aevt().check_invariants().unwrap();
    db.avet().check_invariants().unwrap();

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

        let mut temporaries = Temporaries::new();
        let cid_db_attribute_name = temporaries.push_string("db.attribute.name".to_string());
        let cid_db_attribute_type = temporaries.push_string("db.attribute.type".to_string());
        let cid_db_attribute_unique = temporaries.push_string("db.attribute.unique".to_string());
        let cid_db_attribute_many = temporaries.push_string("db.attribute.many".to_string());
        let cid_db_type_name = temporaries.push_string("db.type.name".to_string());

        let view = db.view(temporaries);

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
                Statement::named_var(Var(0), cid_db_attribute_name, Var(2)),
                Statement::named_var(Var(0), cid_db_attribute_type, Var(1)),
                Statement::named_var(Var(0), cid_db_attribute_unique, Var(3)),
                Statement::named_var(Var(0), cid_db_attribute_many, Var(4)),
                Statement::named_var(Var(1), cid_db_type_name, Var(5)),
            ],
            select: vec![Var(0), Var(2), Var(5), Var(3), Var(4)],
        };
        query.fix_attributes(&view);
        let plan = QueryPlan::new(query, &view);

        println!("\nAll attributes:");
        let eval = Evaluator::new(&plan, &view);
        let rows: Vec<_> = eval.collect();
        let stdout = std::io::stdout();
        types::draw_table(
            &mut stdout.lock(),
            view.heap(),
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

        let mut temporaries = Temporaries::new();
        let cid_db_type_name = temporaries.push_string("db.type.name".to_string());

        let mut view = db.view(temporaries);

        let mut query = Query {
            variable_names: vec![
                "t".to_string(),        // 0
                "name".to_string(),     // 1
            ],
            where_statements: vec![
                Statement::named_var(Var(0), cid_db_type_name, Var(1)),
            ],
            select: vec![Var(0), Var(1)],
        };
        query.fix_attributes(&mut view);
        let plan = QueryPlan::new(query, &view);

        println!("\nAll types:");
        let eval = Evaluator::new(&plan, &view);
        let rows: Vec<_> = eval.collect();
        let stdout = std::io::stdout();
        types::draw_table(
            &mut stdout.lock(),
            view.heap(),
            plan.select.iter().map(|&v| &plan.variable_names[v.0 as usize][..]),
            rows.iter().map(|ref row| &row[..]),
            &plan.select_types[..],
        ).unwrap();
    }

    {
        // where
        //   e name n
        //   e level l
        // select
        //   l, n

        use query::{Query, Statement, Var};

        let mut temporaries = Temporaries::new();
        let cid_name = temporaries.push_string("name".to_string());
        let cid_level = temporaries.push_string("level".to_string());

        let mut view = db.view(temporaries);

        let mut query = Query {
            variable_names: vec![
                "entity".to_string(), // 0
                "name".to_string(),   // 1
                "level".to_string(),  // 2
            ],
            where_statements: vec![
                Statement::named_var(Var(0), cid_name, Var(1)),
                Statement::named_var(Var(0), cid_level, Var(2)),
            ],
            select: vec![Var(1), Var(2)],
        };
        query.fix_attributes(&mut view);
        let plan = QueryPlan::new(query, &view);

        println!("\nNamed entities with level:");
        let eval = Evaluator::new(&plan, &view);
        let rows: Vec<_> = eval.collect();
        let stdout = std::io::stdout();
        types::draw_table(
            &mut stdout.lock(),
            view.heap(),
            plan.select.iter().map(|&v| &plan.variable_names[v.0 as usize][..]),
            rows.iter().map(|ref row| &row[..]),
            &plan.select_types[..],
        ).unwrap();
    }
}
