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

    println!("\nAll attributes:\n");
    let attributes = db.select_where_has_attribute(db.builtins.attribute_db_attribute_name);
    db.debug_print_table(&attributes[..]);

    println!("\nAll types:\n");
    let types = db.select_where_has_attribute(db.builtins.attribute_db_type_name);
    db.debug_print_table(&types[..]);

    println!("\nAll attributes, via query plan:\n");
    let plan = QueryPlan::example_all_attributes(&db.builtins);
    let eval = Evaluator::new(plan, &db);
    for tuple in eval {
        for field_value in &tuple[..] {
            // TODO: Type-based formatter.
            print!("{}\t", field_value.0);
        }
        println!("");
    }
}
