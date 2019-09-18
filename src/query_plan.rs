// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines query plans and their evaluation.

// TODO: Remove this once I have decent query plans.
#![allow(dead_code)]

use database::{Builtins, QueryEngine};
use datom::{Eid, Aid, Value, Tid, Operation, Datom};
use pool;
use query::{Query, self};
use store;
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

/// A variable definition.
///
/// Specifies how to obtain the allowed values for a variable. A variable is
/// defined in two parts:
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
    pub definitions: Vec<Definition>,

    /// The type of every numbered variable.
    pub variable_types: Vec<Type>,

    /// The name of every numbered variable.
    ///
    /// Variables are identified by number, not by name; the name is only there
    /// to have human-meaningful names which can be used as column headers when
    /// debug-printing tables, etc.
    pub variable_names: Vec<String>,

    /// The variables to return in the result, and in which order.
    pub select: Vec<Var>,

    /// The types of the selectde variables only.
    pub select_types: Vec<Type>,
}

/// Maps numbered variables in the query to variables in the plan.
struct Mapping {
    mapping: Vec<Option<Var>>,
    unmapping: Vec<query::Var>,
    fresh: u32,
}

impl Mapping {
    pub fn new(n: usize) -> Mapping {
        use std::iter;
        Mapping {
            mapping: iter::repeat(None).take(n).collect(),
            unmapping: iter::repeat(query::Var(0)).take(n).collect(),
            fresh: 0,
        }
    }

    pub fn get(&mut self, var: query::Var) -> Var {
        let current = self.mapping[var.0 as usize];
        match current {
            Some(x) => x,
            None => {
                let mapped = Var(self.fresh);
                self.mapping[var.0 as usize] = Some(mapped);
                self.unmapping[self.fresh as usize] = query::Var(var.0);
                self.fresh += 1;
                mapped
            }
        }
    }

    pub fn unget(&self, var: Var) -> query::Var {
        self.unmapping[var.0 as usize]
    }
}

impl QueryPlan {
    /// Plan a query.
    ///
    /// For now, this uses an extremely naive query planner, which loops over
    /// all variables in the order that they appear.
    pub fn new<Store>(query: Query, engine: &QueryEngine<Store>) -> QueryPlan
    where Store: store::Store + pool::Pool
    {
        // Map variables in the query to variables in the plan. They may have
        // different indices.
        let mut mapping = Mapping::new(query.variable_names.len());
        let mut definitions = Vec::new();

        for statement in &query.where_statements {
            let aid = match statement.attribute {
                query::QueryAttribute::Named(..) => panic!("Attributes should be fixed before planning."),
                query::QueryAttribute::Fixed(id) => id,
            };

            let evar = mapping.get(statement.entity);
            if definitions.len() <= evar.0 as usize {
                // The variable is not yet defined. We need to introduce a
                // definition.

                match statement.value {
                    query::QueryValue::Const(v) => {
                        let def_entity = Definition {
                            retrieval: Retrieval::ScanAvetConst {
                                attribute: aid,
                                value: v,
                            },
                            filters: Vec::new(),
                        };
                        definitions.push(def_entity);
                    }
                    query::QueryValue::Var(v) => {
                        let vvar = mapping.get(v);
                        if definitions.len() <= vvar.0 as usize {
                            // The second variable is also not defined yet. We
                            // need to define it too.
                            let def_entity = Definition {
                                retrieval: Retrieval::ScanAvetAny {
                                    attribute: aid,
                                },
                                filters: Vec::new(),
                            };
                            let def_value = Definition {
                                // NOTE: Aevt would be more appropriate here,
                                // because the outer loop is over a single
                                // attribute, so aevt would have more locality.
                                retrieval: Retrieval::LookupEavt {
                                    entity: evar,
                                    attribute: aid,
                                },
                                filters: Vec::new(),
                            };
                            definitions.push(def_entity);
                            definitions.push(def_value);
                        } else {
                            // The second variable does exit already, so we can
                            // refer to it.
                            let def_entity = Definition {
                                retrieval: Retrieval::ScanAvetVar {
                                    attribute: aid,
                                    value: vvar,
                                },
                                filters: Vec::new(),
                            };
                            definitions.push(def_entity);
                        }
                    }
                }
            } else {
                // The variable is defined already. We either need to define the
                // second variable, or we need to add a restriction somewhere.

                match statement.value {
                    query::QueryValue::Const(_v) => {
                        unimplemented!("TODO: Add filter to check for const.")
                    }
                    query::QueryValue::Var(v) => {
                        let vvar = mapping.get(v);
                        if definitions.len() <= vvar.0 as usize {
                            // The second variable is not defined yet. We need
                            // to define it.
                            let def_value = Definition {
                                retrieval: Retrieval::LookupEavt {
                                    entity: evar,
                                    attribute: aid,
                                },
                                filters: Vec::new(),
                            };
                            definitions.push(def_value);
                        } else {
                            // The second variable is already defined.
                            unimplemented!("TODO: Add filter to check for var {:?} (mapped {:?}).", v, vvar)
                        }
                    }
                }
            }
        }

        let perm_types = query.infer_types(engine).expect("TODO: Handle type errors.");
        let perm_names = query.variable_names;

        // TODO: Do a true permutation, avoid the clone.
        let names: Vec<String> = (0..definitions.len()).map(
            |i| perm_names[mapping.unget(Var(i as u32)).0 as usize].clone()
        ).collect();
        let types: Vec<Type> = (0..definitions.len()).map(
            |i| perm_types[mapping.unget(Var(i as u32)).0 as usize]
        ).collect();

        let select: Vec<Var> = query.select.iter().map(|&v| mapping.get(v)).collect();
        let select_types: Vec<Type> = select.iter().map(|&v| types[v.0 as usize]).collect();


        let plan = QueryPlan {
            definitions: definitions,
            variable_types: types,
            variable_names: names,
            select: select,
            select_types: select_types,
        };

        plan
    }

    /// Return the type of a variable in this query plan.
    fn get_type(&self, var: Var) -> Type {
        self.variable_types[var.0 as usize]
    }

    /// Assert that all invariants are respected.
    pub fn assert_valid<Store>(&self, engine: &QueryEngine<Store>)
    where Store: store::Store + pool::Pool
    {
        for (i, ref def) in self.definitions.iter().enumerate() {
            let v = Var(i as u32);

            match def.retrieval {
                Retrieval::ScanAvetAny { .. } => { }
                Retrieval::ScanAvetConst { .. } => { }
                Retrieval::ScanAvetVar { attribute, value } => {
                    let attr_value_type = engine.lookup_attribute_type(attribute);
                    let var_type = self.get_type(value);
                    assert_eq!(var_type, attr_value_type,
                               "Type mismatche in avet scan for variable {:?}. \
                                Attribute has type {:?} but value variable has type {:?}.",
                               v, attr_value_type, var_type);
                    assert!(value < v, "Variable {:?} refers to later variable {:?}.", v, value);
                },
                Retrieval::LookupEavt { entity, attribute } => {
                    let attr_value_type = engine.lookup_attribute_type(attribute);
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

    /// Return a query plan to query all attributes.
    ///
    /// This is intended for debugging, once there is a query planner, there
    /// would be no more need to write a plan manually. The query corresponds to
    ///
    /// ```noblit
    /// where
    ///   a db.attribute.name name
    ///   a db.attribute.type t
    ///   a db.attribute.unique unique
    ///   a db.attribute.many many
    ///   t db.type.name type
    /// select
    ///   a, name, t, type, unique, many
    /// ```
    pub fn example_all_attributes(builtins: &Builtins) -> QueryPlan {
        let db_attr_name = builtins.attribute_db_attribute_name;
        let db_attr_type = builtins.attribute_db_attribute_type;
        let db_attr_unique = builtins.attribute_db_attribute_unique;
        let db_attr_many = builtins.attribute_db_attribute_many;
        let db_type_name = builtins.attribute_db_type_name;
        QueryPlan {
            // Variables:
            // 0: Entity id (of the attribute): ref
            // 1: db.attribute.name: string
            // 2: db.attribute.type: ref
            // 3: db.type.name: string
            // 4: db.attribute.unique: bool
            // 5: db.attribute.many: bool
            variable_types: vec![
                Type::Ref,
                Type::String,
                Type::Ref,
                Type::String,
                Type::Bool,
                Type::Bool,
            ],
            variable_names: vec![
                "id".to_string(),
                "name".to_string(),
                "type".to_string(),
                "type_name".to_string(),
                "unique".to_string(),
                "many".to_string(),
            ],
            definitions: vec![
                Definition {
                    retrieval: Retrieval::ScanAvetAny { attribute: db_attr_name },
                    filters: Vec::new(),
                },
                // TODO: This attribute should be retrievable in the initial scan.
                Definition {
                    retrieval: Retrieval::LookupEavt { entity: Var(0), attribute: db_attr_name },
                    filters: Vec::new(),
                },
                Definition {
                    retrieval: Retrieval::LookupEavt { entity: Var(0), attribute: db_attr_type },
                    filters: Vec::new(),
                },
                Definition {
                    retrieval: Retrieval::LookupEavt { entity: Var(2), attribute: db_type_name },
                    filters: Vec::new(),
                },
                Definition {
                    retrieval: Retrieval::LookupEavt { entity: Var(0), attribute: db_attr_unique },
                    filters: Vec::new(),
                },
                Definition {
                    retrieval: Retrieval::LookupEavt { entity: Var(0), attribute: db_attr_many },
                    filters: Vec::new(),
                }
            ],
            select: vec![Var(0), Var(1), Var(2), Var(3), Var(4), Var(5)],
            select_types: vec![
                Type::Ref,
                Type::String,
                Type::Ref,
                Type::String,
                Type::Bool,
                Type::Bool
            ],
        }
    }

    /// Return a query plan to query all types.
    ///
    /// This is intended for debugging, once there is a query planner, there
    /// would be no more need to write a plan manually. The query corresponds to
    ///
    /// ```noblit
    /// where
    ///   t db.type.name name
    /// select
    ///   t, name
    /// ```
    pub fn example_all_types(builtins: &Builtins) -> QueryPlan {
        let db_type_name = builtins.attribute_db_type_name;
        QueryPlan {
            // Variables:
            // 0: Entity id (of the attribute): ref
            // 1: db.type.name: string
            variable_types: vec![
                Type::Ref,
                Type::String,
            ],
            variable_names: vec![
                "id".to_string(),
                "name".to_string(),
            ],
            definitions: vec![
                Definition {
                    retrieval: Retrieval::ScanAvetAny { attribute: db_type_name },
                    filters: Vec::new(),
                },
                // TODO: This attribute should be retrievable in the initial scan.
                Definition {
                    retrieval: Retrieval::LookupEavt { entity: Var(0), attribute: db_type_name },
                    filters: Vec::new(),
                },
            ],
            select: vec![Var(0), Var(1)],
            select_types: vec![Type::Ref, Type::String],
        }
    }
}

type ValueIter<'a> = Box<dyn Iterator<Item = Value> + 'a>;

/// Iterator that yields results from a given query plan.
pub struct Evaluator<'a, Store: 'a + store::Store + pool::Pool> {
    /// The database to query.
    engine: &'a QueryEngine<'a, Store>,

    /// The query plan.
    plan: &'a QueryPlan,

    /// One iterator for every variable.
    iters: Vec<ValueIter<'a>>,

    /// The current value for every variable.
    values: Vec<Value>,
}

impl<'a, Store: store::Store + pool::Pool> Evaluator<'a, Store> {
    pub fn new(plan: &'a QueryPlan, engine: &'a QueryEngine<'a, Store>) -> Evaluator<'a, Store> {
        use std::iter;

        plan.assert_valid(engine);
        let num_variables = plan.definitions.len();

        let iters = iter::repeat_with(|| {
            let empty_iter: ValueIter = Box::new(iter::empty());
            empty_iter
        }).take(num_variables).collect();

        let mut eval = Evaluator {
            engine: engine,
            plan: plan,
            iters: iters,
            values: iter::repeat(Value::min()).take(num_variables).collect(),
        };

        // Set only the first iterator: this is the one that we won't reset when
        // it is exhausted. The others we'll reset during iteration.
        eval.iters[0] = eval.make_iter(&eval.plan.definitions[0]);

        eval
    }

    fn get_value(&self, var: Var) -> Value {
        self.values[var.0 as usize]
    }

    fn make_iter(&self, def: &Definition) -> ValueIter<'a> {
        match def.retrieval {
            Retrieval::ScanAvetAny { attribute } => {
                let min = Datom::new(Eid::min(), attribute, Value::min(), Tid::min(), Operation::Retract);
                let max = Datom::new(Eid::max(), attribute, Value::max(), Tid::max(), Operation::Assert);
                let iter = self
                    .engine
                    .avet()
                    .into_iter(&min, &max)
                    .map(|&datom| Value::from_eid(datom.entity));
                Box::new(iter)
            }
            Retrieval::ScanAvetConst { attribute, value } => {
                let min = Datom::new(Eid::min(), attribute, value, Tid::min(), Operation::Retract);
                let max = Datom::new(Eid::max(), attribute, value, Tid::max(), Operation::Assert);
                let iter = self
                    .engine
                    .avet()
                    .into_iter(&min, &max)
                    .map(|&datom| Value::from_eid(datom.entity));
                Box::new(iter)
            }
            // TODO: Use var_ prefix on variables.
            Retrieval::ScanAvetVar { attribute, value } => {
                let v = self.get_value(value);
                let min = Datom::new(Eid::min(), attribute, v, Tid::min(), Operation::Retract);
                let max = Datom::new(Eid::max(), attribute, v, Tid::max(), Operation::Assert);
                let iter = self
                    .engine
                    .avet()
                    .into_iter(&min, &max)
                    .map(|&datom| Value::from_eid(datom.entity));
                Box::new(iter)
            }
            Retrieval::LookupEavt { entity, attribute } => {
                let e = self.get_value(entity).as_eid(&self.engine.pool());
                let min = Datom::new(e, attribute, Value::min(), Tid::min(), Operation::Retract);
                let max = Datom::new(e, attribute, Value::max(), Tid::max(), Operation::Assert);
                let iter = self
                    .engine
                    .eavt()
                    .into_iter(&min, &max)
                    .map(|&datom| datom.value);
                Box::new(iter)
            }
        }
    }

    /// Increment the i-th iterator.
    ///
    /// If it is exhausted, increment the (i-1)-th iterator and reset the ith
    /// one, etc., until the 0th iterator is exhausted.
    ///
    /// Returns whether a new value was successfully stored.
    fn increment(&mut self, i: usize) -> bool {
        loop {
            match self.iters[i].next() {
                None if i == 0 => {
                    return false
                }
                Some(v) => {
                    self.values[i] = v;
                    return true
                }
                None => {
                    if !self.increment(i - 1) {
                        return false
                    }
                    self.iters[i] = self.make_iter(&self.plan.definitions[i]);
                }
            }
        }
    }
}

impl<'a, Store: store::Store + pool::Pool> Iterator for Evaluator<'a, Store> {
    type Item = Box<[Value]>;

    fn next(&mut self) -> Option<Box<[Value]>> {
        let i = self.plan.definitions.len() - 1;
        match self.increment(i) {
            false => None,
            true => {
                let results: Vec<Value> = self
                    .plan
                    .select
                    .iter()
                    .map(|&v| self.get_value(v))
                    .collect();

                Some(results.into_boxed_slice())
            }
        }
    }
}
