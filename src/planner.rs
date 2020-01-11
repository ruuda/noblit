// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The planner translates queries into plans.

use plan::{Flow, Index, Plan, Slot, SlotDefinition};
use query::{Query, QueryValue, Var};

pub struct Planner {
    /// The where statements in the query, with values mapped into slots.
    ///
    /// The triples are (entity, attribute, value) triples. For the entity and
    /// value, we store the slot where the value will reside during evaluation.
    statements: Vec<(Slot, Aid, Slot)>,

    /// The plan that we are building.
    plan: Plan,
}

impl Planner {
    /// Initialize a planner for the given query.
    pub fn new(query: &Query) -> Planner {
        let mut slots = Vec::new();
        let mut statements = Vec::with_capacity(query.where_statements.len());

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
            let attribute = match statement.attribute {
                QueryAttribute::Fixed(aid) => aid,
                QueryAttribute::Named(..) => {
                    panic!("Attributes should have been fixed before planning.");
                }
            };
            let slot_value = match statement.value {
                QueryValue::Var(var) => Slot(var.0),
                QueryValue::Const(v) => {
                    let def = SlotDefinition::Constant { value: v };
                    slots.push(def);
                    Slot((slots.len() - 1) as u16)
                }
            };
            statement.push((slot_entity, attribute, slot_value));
            assert!(
                slots.len() <= 0xffff,
                "Can have at most 2^16 slots, slot index is u16."
            );
        }

        let plan = Plan {
            // Every where statement will be translated in exactly one scan.
            scans: Vec::with_capacity(query.where_statements.len()),
            slots: slots,
            // As we mapped 1:1 to the first n slots, we can pass along the
            // selects without modification.
            select: query.select.iter().map(|&Var(i)| Slot(i)).collect(),
        };

        Planner {
            statements: statements,
            plan: plan,
        }
    }

    /// Generate the initial scans to implement the query.
    ///
    /// For some statements, multiple index scans would be a valid way to
    /// implement the statement. In that case, we always choose the first one.
    /// Later calls to `next` can cycle through them to explore all possible
    /// plans.
    fn initialize_scans(&mut self) {
        // Track which slots are already defined (those we can read from), and
        // which slots still need to be initialized (those we need to plan).
        let mut slot_initialized = Vec::with_capacity(self.plan.slots.len());

        // Constants are initialized at the start, before any scans.
        for slot_def in &self.plan.slots {
            match slot_def {
                SlotDefinition::Variable { .. } => slot_initialized.push(false),
                SlotDefinition::Constant { .. } => slot_initialized.push(true),
            }
        }

        // Then we can plan a scan for every statement.
        for &(slot_entity, attribute, slot_value) in &self.statements {
            let is_entity_init = slot_initialized[slot_entity.0 as usize];
            let is_value_init = slot_initialized[slot_value.0 as usize];

            // Depending on what slots are initialized already, various index-
            // flow combinations can be used to fill the remaining slots. When
            // there are multiple indexes that could fill the slot, we pick the
            // first one, and later on, iterating plans will explore alternatives.
            let (index, flow, first, second) = match (is_entity_init, is_value_init) {
                (true, true) => (Index::Aevt, Flow::InIn, slot_entity, slot_value),
                (false, true) => (Index::Avet, Flow::InOut, slot_value, slot_entity),
                (true, false) => (Index::Aevt, Flow::InOut, slot_entity, slot_value),
                (false, false) => (Index::Aevt, Flow::OutOut, slot_entity, slot_value),
            };

            let scan = Scan {
                index: index,
                flow: Flow,
                attribute: attribute,
                slots: [first, second],
            };

            self.plan.scans.push(scan);

            // The scan will initialize any uninitialized slots it references.
            slot_initialized[slot_entity.0 as usize] = true;
            slot_initialized[slot_value.0 as usize] = true;
        }
    }
}
