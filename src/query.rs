// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines queries.
//!
//! Queries need to be translated into a query plan before they can be
//! evaluated.

use binary::{Cursor, CursorError};
use database::QueryEngine;
use datom::{Aid, Value};
use pool;
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
/// may change, but their ids will not.
#[derive(Clone, Debug)]
pub enum QueryAttribute {
    Named(String),
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
    pub fn named_var(entity: Var, attribute: &str, value: Var) -> Statement {
        Statement {
            entity: entity,
            attribute: QueryAttribute::Named(attribute.to_string()),
            value: QueryValue::Var(value),
        }
    }

    pub fn named_const(entity: Var, attribute: &str, value: Value) -> Statement {
        Statement {
            entity: entity,
            attribute: QueryAttribute::Named(attribute.to_string()),
            value: QueryValue::Const(value),
        }
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
    // TODO: Move binary format parsing into its own module.

    /// Deserialize variable names from binary format.
    fn parse_strings(cursor: &mut Cursor) -> Result<Vec<String>, CursorError> {
        let num_variables = cursor.take_u16_le()?;
        let mut variable_names = Vec::with_capacity(num_variables as usize);
        for _ in 0..num_variables {
            let len = cursor.take_u16_le()?;
            let name = cursor.take_utf8(len as usize)?;
            variable_names.push(name.to_string());
        }

        Ok(variable_names)
    }

    /// Deserialize statements from binary format.
    fn parse_statements(cursor: &mut Cursor) -> Result<Vec<Statement>, CursorError> {
        let num_statements = cursor.take_u16_le()?;
        let mut statements = Vec::with_capacity(num_statements as usize);
        for _ in 0..num_statements {
            let entity_var = cursor.take_u16_le()?;
            let attribute_len = cursor.take_u16_le()?;
            let attribute = cursor.take_utf8(attribute_len as usize)?;
            let value_type = cursor.take_u8()?;
            let value = match value_type {
                0 => {
                    let value_var = cursor.take_u16_le()?;
                    QueryValue::Var(Var(value_var))
                }
                1 => {
                    // TODO: Deal with pooling large values.
                    let value_u64 = cursor.take_u64_le()?;
                    QueryValue::Const(Value::from_u64(value_u64))
                }
                2 => {
                    // TODO: Deal with pooling large values.
                    let value_len = cursor.take_u16_le()?;
                    let value_str = cursor.take_utf8(value_len as usize)?;
                    QueryValue::Const(Value::from_str(value_str))
                }
                _ => unimplemented!("TODO: Proper error handling."),
            };

            let statement = Statement {
                entity: Var(entity_var),
                attribute: QueryAttribute::Named(attribute.to_string()),
                value: value,
            };
            statements.push(statement);
        }

        Ok(statements)
    }

    fn parse_variables(cursor: &mut Cursor) -> Result<Vec<Var>, CursorError> {
        let num_variables = cursor.take_u16_le()?;
        let mut variables = Vec::with_capacity(num_variables as usize);
        for _ in 0..num_variables {
            let var = cursor.take_u16_le()?;
            variables.push(Var(var));
        }

        Ok(variables)
    }

    /// Deserialize a query in binary format.
    pub fn parse(cursor: &mut Cursor) -> Result<Query, CursorError> {
        let variable_names = Query::parse_strings(cursor)?;
        let where_statements = Query::parse_statements(cursor)?;
        let selects = Query::parse_variables(cursor)?;

        let query = Query {
            variable_names: variable_names,
            where_statements: where_statements,
            select: selects,
        };

        Ok(query)
    }

    pub fn fix_attributes<
        Store: store::Store,
        Pool: pool::Pool,
    > (
        &mut self,
        engine: &mut QueryEngine<Store, Pool>,
    ) {
        for stmt in self.where_statements.iter_mut() {
            let aid = match stmt.attribute {
                QueryAttribute::Fixed(aid) => aid,
                QueryAttribute::Named(ref name) => match engine.lookup_attribute_id(&name[..]) {
                    Some(aid) => aid,
                    None => {
                        // TODO: Have proper error handling for missing attributes.
                        panic!("No such attribute: {:?}", name);
                    }
                }
            };

            stmt.attribute = QueryAttribute::Fixed(aid);
        }
    }

    /// Infer the type of every variable.
    pub fn infer_types<
        Store: store::Store,
        Pool: pool::Pool,
    > (
        &self,
        engine: &QueryEngine<Store, Pool>,
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
            let attr_type = engine.lookup_attribute_type(aid);

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

/// A request to transact new datoms.
pub struct QueryMut {
    /// A human-meaningful name for every variable.
    pub variable_names: Vec<String>,

    /// Relations that bind variables, in order to refer to existing entities.
    pub where_statements: Vec<Statement>,

    /// New datoms to insert in this transaction.
    pub assertions: Vec<Statement>,

    /// Variables bound by the "where" part of the query.
    pub bound_variables: Vec<Var>,

    /// Free variables, for which we need to create new entities.
    pub free_variables: Vec<Var>,

    /// The variables to return results for, and their order.
    pub select: Vec<Var>,
}

impl QueryMut {
    /// Deserialize a transaction request in binary format.
    pub fn parse(cursor: &mut Cursor) -> Result<QueryMut, CursorError> {
        use std::iter;

        // The first part is the same as for a regular query.
        let variable_names = Query::parse_strings(cursor)?;
        let where_statements = Query::parse_statements(cursor)?;
        let selects = Query::parse_variables(cursor)?;

        // Then we get the assertions.
        let assertions = Query::parse_statements(cursor)?;

        // Determine which variables are bound, by marking all variables that
        // occur in the "where" part as bound.
        let mut is_bound: Vec<bool> = iter::repeat(false)
            .take(variable_names.len())
            .collect();

        for statement in where_statements.iter() {
            // TODO: Guard against malicious inputs, report error on index out
            // of bounds.
            is_bound[statement.entity.0 as usize] = true;

            if let QueryValue::Var(v) = statement.value {
                is_bound[v.0 as usize] = true;
            }
        }

        // From the bound/free bitmap, produce two collections with variables.
        // These are mainly used for convenience.
        let mut bound_variables = Vec::with_capacity(variable_names.len());
        let mut free_variables = Vec::with_capacity(variable_names.len());
        let mut was_bound = true;
        for (i, &bound) in is_bound.iter().enumerate() {
            if bound {
                bound_variables.push(Var(i as u16));
                assert!(was_bound, "Bound variables must precede free variables.");
            } else {
                free_variables.push(Var(i as u16));
                was_bound = false;
            }
        }

        let query_mut = QueryMut {
            variable_names: variable_names,
            where_statements: where_statements,
            assertions: assertions,
            bound_variables: bound_variables,
            free_variables: free_variables,
            select: selects,
        };

        Ok(query_mut)
    }

    /// Return the read-only part of the query.
    ///
    /// For every tuple in the result of this read-only query, the assertions
    /// will produce new datoms.
    pub fn read_only_part(&self) -> Query {
        Query {
            // TODO: Avoid the clones.
            variable_names: self.variable_names.clone(),
            where_statements: self.where_statements.clone(),
            select: self.bound_variables.clone(),
        }
    }
}
