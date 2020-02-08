// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The planner translates queries into plans.

use datom::Aid;
use plan::{Flow, Index, Plan, Scan, Slot, SlotDefinition};
use query::{Query, QueryAttribute, QueryValue, Var};

/// Update a scan in-place to a different scan that fills the same slots.
///
/// Returns true when incremented to a new scan in the cycle, and false when we
/// cycled back to the initial scan.
fn next_scan(scan: &mut Scan) -> bool {
    let (index, is_new) = match (&scan.flow, &scan.index) {
        // For out-out, we can use the attribute-leading indexes.
        (Flow::OutOut, Index::Aevt) => (Index::Avet, true),
        (Flow::OutOut, Index::Avet) => (Index::Aevt, false),
        (Flow::OutOut, Index::Eavt) => panic!("Using eavt for out-out is invalid."),

        // For in-in, we can use all three indexes.
        (Flow::InIn, Index::Aevt) => (Index::Avet, true),
        (Flow::InIn, Index::Avet) => (Index::Eavt, true),
        (Flow::InIn, Index::Eavt) => (Index::Aevt, false),

        // For in-out, we need to keep the slot that is the input (either the
        // entity or the value) the input, and the other one the output. There
        // are two cycles here, one of length 2, and one of length 1.
        (Flow::InOut, Index::Aevt) => (Index::Eavt, true),
        (Flow::InOut, Index::Eavt) => (Index::Aevt, false),
        (Flow::InOut, Index::Avet) => (Index::Avet, false),
    };

    scan.index = index;
    is_new
}

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
    /// Plan the query.
    ///
    /// This is a convenience wrapper that performs all steps needed to plan the
    /// query. Individual steps are also exposed through `Planner::new()` and
    /// other methods.
    pub fn plan(query: &Query) -> Plan {
        let mut planner = Planner::new(query);
        planner.initialize_scans();
        planner.plan
    }

    /// Initialize a planner for the given query.
    ///
    /// Before the plan is available, the scans need to be initialized.
    /// Separating this step from the constuctor allows the planner to be reused
    /// for different permutations of the statements, at the cost of introducing
    /// invalid states of the planner.
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
            statements.push((slot_entity, attribute, slot_value));
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
    pub fn initialize_scans(&mut self) {
        // If there was a previous plan, clear its scans, because we are going
        // to initialize them with new scans.
        self.plan.scans.clear();

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
            let (index, flow) = match (is_entity_init, is_value_init) {
                (true, true) => (Index::Aevt, Flow::InIn),
                (false, true) => (Index::Avet, Flow::InOut),
                (true, false) => (Index::Aevt, Flow::InOut),
                (false, false) => (Index::Aevt, Flow::OutOut),
            };

            let scan = Scan {
                index: index,
                flow: flow,
                entity: slot_entity,
                attribute: attribute,
                value: slot_value,
            };

            self.plan.scans.push(scan);

            // The scan will initialize any uninitialized slots it references.
            slot_initialized[slot_entity.0 as usize] = true;
            slot_initialized[slot_value.0 as usize] = true;
        }
    }

    /// Get the current plan.
    pub fn get_plan(&self) -> &Plan {
        debug_assert_eq!(
            self.plan.scans.len(), self.statements.len(),
            "Planner can only expose the plan after initializing scans."
        );
        &self.plan
    }

    /// Advance to the next alternative of the plan.
    ///
    /// Returns whether a new plan was generated. If it was, it can be inspected
    /// with `get_plan()`. Returns false when all plans have been exhausted.
    pub fn next(&mut self) -> bool {
        for scan in self.plan.scans.iter_mut() {
            let is_new = next_scan(scan);
            if is_new {
                // The altered scan is one that we had not seen before.
                // Therefore, the plan is also a new one. Return that.
                return true;
            }

            // If the scan is not new, then we cycled back to the initial scan.
            // But we can still try to cycle a different scan, in the next
            // iteration. And for that different value, we can then try all
            // options for the earlier scans too.
        }

        // If we ended up here, we tried incrementing every scan, but all of
        // them cycled back to the initial one, which means we have exhausted
        // all options.
        false
    }
}
