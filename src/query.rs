// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines queries (read-only).
//!
//! Queries need to be translated into a query plan before they can be
//! evaluated.

use database::View;
use datom::{Aid, Value};
use heap::{CidBytes, self};
use store;
use types::Type;

/// A placeholder variable in a query.
///
/// Variables can be named, but the names are specified separately; internally
/// variables are referenced by index.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Var(pub u16);

/// An attribute in a query can be named, or a fixed attribute id.
///
/// Named attributes are converted into fixed attribute ids at the start
/// of evaluation. Names provide a level of indirection: attribute names
/// may change, but their ids will not. In order to use named attributes,
/// we must put the names on a temporary heap during query parsing.
#[derive(Clone, Debug)]
pub enum QueryAttribute {
    Named(CidBytes),
    Fixed(Aid),
}

/// The value in a query can be a constant or a variable.
#[derive(Clone, Debug)]
pub enum QueryValue {
    Const(Value),
    Var(Var),
}

/// A statement that relates an entity, attribute, and a value.
#[derive(Clone, Debug)]
pub struct Statement {
    pub entity: Var,
    pub attribute: QueryAttribute,
    pub value: QueryValue,
}

impl Statement {
    pub fn named_var(entity: Var, attribute_name: CidBytes, value: Var) -> Statement {
        Statement {
            entity: entity,
            attribute: QueryAttribute::Named(attribute_name),
            value: QueryValue::Var(value),
        }
    }

    pub fn named_const(entity: Var, attribute_name: CidBytes, value: Value) -> Statement {
        Statement {
            entity: entity,
            attribute: QueryAttribute::Named(attribute_name),
            value: QueryValue::Const(value),
        }
    }
}

/// Convert named attributes into fixed (by-id) attributes.
pub fn fix_attributes_in_statements<
    Store: store::Store,
    Heap: heap::Heap,
> (
    view: &View<Store, Heap>,
    statements: &mut [Statement],
) {
    for stmt in statements.iter_mut() {
        let aid = match stmt.attribute {
            QueryAttribute::Fixed(aid) => aid,
            QueryAttribute::Named(cid) => match view.lookup_attribute_id(cid) {
                Ok(aid) => aid,
                Err(name) => {
                    // TODO: Have proper error handling for missing attributes.
                    panic!("No such attribute: {:?}", name);
                }
            }
        };

        stmt.attribute = QueryAttribute::Fixed(aid);
    }
}

/// A query (read-only).
pub struct Query {
    /// A human-meaningful name for every variable.
    pub variable_names: Vec<String>,

    /// Relations that must be true about the results.
    pub where_statements: Vec<Statement>,

    /// The variables to return results for, and their order.
    pub select: Vec<Var>,
}

impl Query {
    pub fn fix_attributes<
        Store: store::Store,
        Heap: heap::Heap,
    > (
        &mut self,
        view: &View<Store, Heap>,
    ) {
        fix_attributes_in_statements(view, &mut self.where_statements[..]);
    }

    /// Infer the type of every variable.
    pub fn infer_types<
        Store: store::Store,
        Heap: heap::Heap,
    > (
        &self,
        view: &View<Store, Heap>,
    ) -> Result<Vec<Type>, String>
    {
        let mut types: Vec<Option<Type>> = self.variable_names.iter().map(|_| None).collect();

        for statement in &self.where_statements {
            let entity_type = types[statement.entity.0 as usize];
            let entity_name = &self.variable_names[statement.entity.0 as usize];
            match entity_type {
                None => types[statement.entity.0 as usize] = Some(Type::Ref),
                Some(Type::Ref) => { /* Ok, the types unify. */ }
                Some(t) => return Err(format!(
                    "Type mismatch for variable '{}': used as ref and {:?}.", entity_name, t
                )),
            };

            let aid = match statement.attribute {
                QueryAttribute::Fixed(id) => id,
                QueryAttribute::Named(..) =>
                    panic!("Should have fixed attributes before type inference."),
            };
            // TODO: What if the attribute does not exist?
            let attr_type = view.lookup_attribute_type(aid);

            let value = match statement.value {
                QueryValue::Const(..) => continue,
                QueryValue::Var(v) => v,
            };
            let value_type = types[value.0 as usize];
            let value_name = &self.variable_names[value.0 as usize];
            match value_type {
                None => types[value.0 as usize] = Some(attr_type),
                Some(t) if t == attr_type => { /* Ok, the types unify. */ },
                Some(t) => return Err(format!(
                    "Type mismatch for variable '{}': used as {:?} and {:?}.", value_name, t, attr_type
                )),
            }
        }

        let result = types
            .iter()
            .map(|opt_t| match opt_t {
                Some(t) => t,
                None => panic!("A variable did not occur in any statement."),
            })
            .cloned()
            .collect();

        Ok(result)
    }
}
