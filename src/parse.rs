// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Parsers for the wire protocol.

use std::io;

use heap;
use datom::{Value};
use binary::Cursor;
use query::{Query, QueryAttribute, QueryValue, Statement, Var};
use mutation::Mutation;
use temp_heap::TempHeap;

/// Deserialize variable names from binary format.
///
/// Variable names are a 16-bit length-prefixed list of 16-bit length prefixed
/// strings.
/// TODO: Document the protocol properly somewhere.
fn parse_strings(cursor: &mut Cursor) -> io::Result<Vec<String>> {
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
///
/// TODO: Document the protocol properly somewhere.
fn parse_statements<Heap: heap::Heap>(
    cursor: &mut Cursor,
    heap: &mut TempHeap<Heap>
) -> io::Result<Vec<Statement>> {
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
                let value_u64 = cursor.take_u64_le()?;
                let value = match Value::from_u64(value_u64) {
                    Some(v) => v,
                    None => Value::from_const_u64(heap.push_u64(value_u64)),
                };
                QueryValue::Const(value)
            }
            2 => {
                let value_len = cursor.take_u16_le()?;
                let value_str = cursor.take_utf8(value_len as usize)?;
                let value = match Value::from_str(&value_str[..]) {
                    Some(v) => v,
                    None => {
                        let bytes = value_str
                            .into_boxed_str()
                            .into_boxed_bytes();
                        Value::from_const_bytes(heap.push_bytes(bytes))
                    }
                };
                QueryValue::Const(value)
            }
            _ => unimplemented!("Unsupported value. TODO: Proper error handling."),
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

/// Parse a variable list.
///
/// A variable list is a 16-bit length-prefixed list of 16-bit variable ids.
/// TODO: Document the protocol properly.
fn parse_variables(cursor: &mut Cursor) -> io::Result<Vec<Var>> {
    let num_variables = cursor.take_u16_le()?;
    let mut variables = Vec::with_capacity(num_variables as usize);
    for _ in 0..num_variables {
        let var = cursor.take_u16_le()?;
        variables.push(Var(var));
    }

    Ok(variables)
}

/// Deserialize a query in binary format.
pub fn parse_query<Heap: heap::Heap>(
    cursor: &mut Cursor,
    heap: &mut TempHeap<Heap>
) -> io::Result<Query> {
    let variable_names = parse_strings(cursor)?;
    let where_statements = parse_statements(cursor, heap)?;
    let selects = parse_variables(cursor)?;

    let query = Query {
        variable_names: variable_names,
        where_statements: where_statements,
        select: selects,
    };

    Ok(query)
}

/// Deserialize a mutation request in binary format.
pub fn parse_mutation<Heap: heap::Heap>(
    cursor: &mut Cursor,
    heap: &mut TempHeap<Heap>
) -> io::Result<Mutation> {
    use std::iter;

    // The first part is the same as for a regular query.
    let variable_names = parse_strings(cursor)?;
    let where_statements = parse_statements(cursor, heap)?;
    let selects = parse_variables(cursor)?;

    // Then we get the assertions.
    let assertions = parse_statements(cursor, heap)?;

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

    let mutation = Mutation {
        variable_names: variable_names,
        where_statements: where_statements,
        assertions: assertions,
        bound_variables: bound_variables,
        free_variables: free_variables,
        select: selects,
    };

    Ok(mutation)
}

