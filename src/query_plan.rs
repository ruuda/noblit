// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! This module defines query plans and their evaluation.

use std::collections::BTreeSet;
use std::collections::HashSet;

use datom::{Eid, Aid, Value, Tid, Operation, TidOp, Datom};
use database::Database;
use index::{Avet};
use types::Type;

/// A placeholder variable in a query.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Var(pub u32);

pub enum Retrieval {
    /// Return entities that have the given attribute.
    ScanAvetAny { attribute: Aid },

    /// Return entities e for which (e, attribute, value) exists.
    ScanAvetConst { attribute: Aid, value: Value },

    /// Return entities e for which (e, attribute, value) exists.
    ScanAvetVar { attribute: Aid, value: Var },

    /// Look up a value in the eavt index.
    LookupEavt { entity: Var, attribute: Aid },
}

pub enum Filter {
    /// Require (entity, attribute, value) to exists.
    ExistsAvet { entity: Var, attribute: Aid, value: Var },
}

/// A definition defines the allowed values of a variable.
///
/// A variable is defined in two parts
///
///  * A *retrieval* that returns the set of all values that this variable might
///    possibly assume. The more restricted this set is, the more efficient the
///    query.
///
///  * A number of *filters* that further restrict which values are valid.
///
/// The retrieval and filters can refer to variables defined before the current
/// variable. During evaluation, we have a value for all of these variables.
pub struct Definition {
    retrieval: Retrieval,
    filters: Vec<Filter>,
}

/// A query plan.
///
/// Like a query, a query plan specifies which values to find for a number of
/// variables. But where a query only specifes *what* to find, the query plan
/// specifies specifically *how* to find it.
///
/// A query plan defines a number of variables, for which to find values. A
/// definition defines how to find the values (and the query planner should
/// ensure that this is the operation that returns the desired results).
/// Definitions are ordered, where a definition may refer to variables defined
/// earlier.
///
/// A query is always executed as a number of nested loops, one for each
/// variable. The loop is over all possible values of that variable. The
/// innermost loop yields the results.
pub struct QueryPlan {
    /// The definition of every numbered variable.
    definitions: Vec<Definition>,

    /// The type of every numbered variable.
    types: Vec<Type>,
}

impl QueryPlan {
    /// Return the type of a variable in this query plan.
    fn get_type(&self, var: Var) -> Type {
        self.types[var.0 as usize]
    }

    /// Assert that all invariants are respected.
    pub fn assert_valid(&self, db: &Database) {
        for (i, ref def) in self.definitions.iter().enumerate() {
            let v = Var(i as u32);

            match def.retrieval {
                Retrieval::ScanAvetAny { .. } => { }
                Retrieval::ScanAvetConst { .. } => { }
                Retrieval::ScanAvetVar { attribute, value } => {
                    let attr_value_type = db.lookup_attribute_type(attribute);
                    let var_type = self.get_type(value);
                    assert_eq!(var_type, attr_value_type,
                               "Type mismatche in avet scan for variable {:?}. \
                                Attribute has type {:?} but value variable has type {:?}.",
                               v, attr_value_type, var_type);
                    assert!(value < v, "Variable {:?} refers to later variable {:?}.", v, value);
                },
                Retrieval::LookupEavt { entity, attribute } => {
                    let attr_value_type = db.lookup_attribute_type(attribute);
                    let var_type = self.get_type(v);
                    assert_eq!(var_type, attr_value_type,
                               "Type mismatche in eavt lookup for entity {:?}. \
                                Attribute has type {:?} but value variable has type {:?}.",
                               entity, attr_value_type, var_type);
                    assert!(entity < v, "Variable {:?} refers to later variable {:?}.", v, entity);
                }
            }
        }
    }

    pub fn execute(&self) {

    }
}
