// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The planner translates queries into plans.

use plan::{Plan, SlotDefinition};
use query::{Query, QueryValue};

pub fn generate_plans<F>(query: &Query, inspect_plan: F) where F: FnMut(&Plan) {
    // First we set up a slot for every variable, and then a slot for every
    // constant. We do the variables first, so the indices of variables in the
    // query match those of the slots.
    let mut slots = Vec::new();

    for variable_name in &query.variable_names {
        let def = SlotDefinition::Variable {
            name: variable_name.clone(),
        };
        slots.push(def);
    }

    for statement in &query.where_statements {
        match statement.value {
            QueryValue::Const(v) => {
                let def = SlotDefinition::Constant {
                    value: v
                };
                slots.push(def);
            }
            QueryValue::Var(..) => continue,
        }
    }
}
