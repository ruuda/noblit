// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines mutations that append datoms to the database.
//!
//! Mutations consist of a query part that provides context for the mutation,
//! and an assertion part that specifies which new datoms to insert.

use database::View;
use heap;
use query::{self, Query, Statement, Var};
use store;

/// A request to transact new datoms.
pub struct Mutation {
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

impl Mutation {
    pub fn fix_attributes<
        Store: store::Store,
        Heap: heap::Heap,
    > (
        &mut self,
        view: &mut View<Store, Heap>,
    ) {
        query::fix_attributes_in_statements(view, &mut self.where_statements[..]);
        query::fix_attributes_in_statements(view, &mut self.assertions[..]);
    }

    /// Return the read-only part of the query.
    ///
    /// For every tuple in the result of this read-only query, the assertions
    /// will produce new datoms.
    pub fn read_only_part(&self) -> Query {
        Query {
            // TODO: Avoid the clones.
            variable_names: self.variable_names[..self.bound_variables.len()].to_vec(),
            where_statements: self.where_statements.clone(),
            select: self.bound_variables.clone(),
        }
    }
}
