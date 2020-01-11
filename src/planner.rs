// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The planner translates queries into plans.

use plan::{Plan, Slot, SlotDefinition};
use query::{Query, QueryValue};

pub struct Planner<'a> {
    /// The query that we are planning.
    query: &'a Query,

    /// The slots in the plan.
    slots: Vec<SlotDefinition>,

    /// For every where-statement in the query, the slot of the entity and value.
    statement_slots: Vec<(Slot, Slot)>,
}

impl<'a> Planner<'a> {
    /// Initialize a planner for the given query.
    pub fn new(query: &'a Query) -> Planner<'a> {
        let mut slots = Vec::new();
        let mut statement_slots = Vec::with_capacity(query.where_statements.len());

        assert!(
            query.variable_names.len() <= 0xffff,
            "Can have at most 2^16 slots, slot index is u16."
        );

        for variable_name in &query.variable_names {
            let def = SlotDefinition::Variable {
                name: variable_name.clone(),
            };
            slots.push(def);
        }

        for statement in &query.where_statements {
            let slot_entity = Slot(statement.entity.0);
            let slot_value = match statement.value {
                QueryValue::Var(var) => Slot(var.0),
                QueryValue::Const(v) => {
                    let def = SlotDefinition::Constant { value: v };
                    slots.push(def);
                    Slot((slots.len() - 1) as u16)
                }
            };
            statement_slots.push((slot_entity, slot_value));
            assert!(
                slots.len() <= 0xffff,
                "Can have at most 2^16 slots, slot index is u16."
            );
        }


        Planner {
            query: query,
            slots: slots,
            statement_slots: statement_slots,
        }
    }
}

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
