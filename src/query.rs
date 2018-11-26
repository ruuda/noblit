// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines queries.
//!
//! Queries need to be translated into a query plan before they can be
//! evaluated.

use database::Database;
use datom::{Eid, Aid, Value};

/// A placeholder variable in a query.
///
/// Variables can be named, but the names are specified separately; internally
/// variables are referenced by index.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Var(pub u32);

/// An attribute in a query can be named, or a fixed attribute id.
///
/// Named attributes are converted into fixed attribute ids at the start
/// of evaluation. Names provide a level of indirection: attribute names
/// may change, but their ids will not.
pub enum QueryAttribute {
    Named(String),
    Fixed(Aid),
}

/// The value in a query can be a constant or a variable.
pub enum QueryValue {
    Const(Value),
    Variable(Var),
}

/// A statement that relates an entity, attribute, and a value.
pub struct Statement {
    pub entity: Var,
    pub attribute: QueryAttribute,
    pub value: QueryValue,
}

impl Statement {
    pub fn named_var(entity: Var, attribute: String, value: Var) -> Statement {
        Statement {
            entity: entity,
            attribute: QueryAttribute::Named(attribute),
            value: QueryValue::Variable(value),
        }
    }

    pub fn named_const(entity: Var, attribute: String, value: Value) -> Statement {
        Statement {
            entity: entity,
            attribute: QueryAttribute::Named(attribute),
            value: QueryValue::Const(value),
        }
    }
}

/// A query.
pub struct Query {
    /// A human-meaningful name for every variable.
    pub variable_names: Vec<String>,

    /// Relations that must be true about the results.
    pub where_statements: Vec<Statement>,
}

impl Query {
    pub fn fix_attributes(&mut self, database: &Database) {
        for stmt in self.where_statements.iter_mut() {
            let aid = match stmt.attribute {
                QueryAttribute::Named(ref name) => database
                    .lookup_attribute_id(&name[..])
                    .expect("TODO: Deal with missing attributes."),
                QueryAttribute::Fixed(aid) => aid,
            };

            stmt.attribute = QueryAttribute::Fixed(aid);
        }
    }
}
