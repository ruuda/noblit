// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! This module defines the database itself, and its data types.

use std::collections::BTreeSet;
use std::collections::HashSet;

use datom::{Eid, Aid, Value, Tid, Operation, TidOp, Datom};
use index::{Aevt, Avet, Eavt, Vaet};

/// The supported value types for entity values.
enum Type {
    Bool,
    Ref,
    Uint64,
    Bytes,
    String,
}

/// The genisis transaction adds all built-in attributes.
pub struct Builtins {
    /// Transaction id of the genisis transaction.
    pub genisis_transaction: Tid,

    /// Built-in attribute `db.attribute.name`.
    pub attribute_db_attribute_name: Aid,
    /// Built-in attribute `db.attribute.type`.
    pub attribute_db_attribute_type: Aid,
    /// Built-in attribute `db.attribute.unique`.
    pub attribute_db_attribute_unique: Aid,
    /// Built-in attribute `db.attribute.many`.
    pub attribute_db_attribute_many: Aid,
    /// Built-in attribute `db.type.name`.
    pub attribute_db_type_name: Aid,
    /// Built-in attribute `db.transaction.time`.
    pub attribute_db_transaction_time: Aid,

    /// Built-in type `db.type.bool`.
    pub entity_db_type_bool: Eid,
    /// Built-in type `db.type.ref`.
    pub entity_db_type_ref: Eid,
    /// Built-in type `db.type.uint64`.
    pub entity_db_type_uint64: Eid,
    /// Built-in type `db.type.bytes`.
    pub entity_db_type_bytes: Eid,
    /// Built-in type `db.type.string`.
    pub entity_db_type_string: Eid,
}

impl Builtins {
    pub fn new() -> (Builtins, Vec<Datom>) {
        let id_transaction = 0;
        let id_db_attr_name = 1;
        let id_db_attr_type = 2;
        let id_db_attr_unique = 3;
        let id_db_attr_many = 4;
        let id_db_type_name = 5;
        let id_db_transaction_time = 6;
        let id_db_type_bool = 7;
        let id_db_type_ref = 8;
        let id_db_type_uint64 = 9;
        let id_db_type_bytes = 10;
        let id_db_type_string = 11;

        let builtins = Builtins {
            genisis_transaction: Tid(id_transaction),
            attribute_db_attribute_name: Aid(id_db_attr_name),
            attribute_db_attribute_type: Aid(id_db_attr_type),
            attribute_db_attribute_unique: Aid(id_db_attr_unique),
            attribute_db_attribute_many: Aid(id_db_attr_many),
            attribute_db_type_name: Aid(id_db_type_name),
            attribute_db_transaction_time: Aid(id_db_transaction_time),
            entity_db_type_bool: Eid(id_db_type_bool),
            entity_db_type_ref: Eid(id_db_type_ref),
            entity_db_type_uint64: Eid(id_db_type_uint64),
            entity_db_type_bytes: Eid(id_db_type_bytes),
            entity_db_type_string: Eid(id_db_type_string),
        };

        let tuples = vec![
            Datom::new(
                Eid(id_db_attr_name),
                Aid(id_db_attr_name), Value::from_str("name"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_attr_name),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_string)),
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_attr_name),
                Aid(id_db_attr_unique), Value::from_bool(true),
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_attr_type),
                Aid(id_db_attr_name), Value::from_str("type"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_attr_type),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_ref)),
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_attr_unique),
                Aid(id_db_attr_name), Value::from_str("unique"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_attr_unique),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_bool)),
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_attr_many),
                Aid(id_db_attr_name), Value::from_str("many"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_attr_many),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_bool)),
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_type_name),
                Aid(id_db_attr_name), Value::from_str("name"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_type_name),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_string)),
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_type_name),
                Aid(id_db_attr_unique), Value::from_bool(true),
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_transaction_time),
                Aid(id_db_attr_name), Value::from_str("time"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_transaction_time),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_uint64)), // TODO: Time type.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_type_bool),
                Aid(id_db_type_name), Value::from_str("bool"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_type_ref),
                Aid(id_db_type_name), Value::from_str("ref"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_type_uint64),
                Aid(id_db_type_name), Value::from_str("uint64"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_type_bytes),
                Aid(id_db_type_name), Value::from_str("bytes"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_db_type_string),
                Aid(id_db_type_name), Value::from_str("string"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Datom::new(
                Eid(id_transaction),
                Aid(id_db_transaction_time), Value::from_u64(0),
                Tid(id_transaction), Operation::Assert,
            ),
        ];

        (builtins, tuples)
    }
}

pub struct Database {
    pub builtins: Builtins,
    eavt: BTreeSet<Eavt>,
    aevt: BTreeSet<Aevt>,
    avet: BTreeSet<Avet>,
    vaet: BTreeSet<Vaet>,
    next_id: u64,
    next_transaction_id: u64,
}

impl Database {
    pub fn new() -> Database {
        let (builtins, genisis_tuples) = Builtins::new();
        let mut db = Database {
            eavt: BTreeSet::new(),
            aevt: BTreeSet::new(),
            avet: BTreeSet::new(),
            vaet: BTreeSet::new(),
            builtins: builtins,
            // Transaction ids must be even. For now we do that by just tracking
            // separate counters and incrementing both by 2. Perhaps the
            // property that transaction ids are even could be exploited later,
            // or perhaps it is a very bad idea and we want to have the entities
            // created in a transaction and the transaction itself be adjacent
            // in the indices by giving them adjacent ids. We start these
            // counters at 100 to reserve some room to extend the geneisis
            // transaction.
            next_id: 101,
            next_transaction_id: 100,
        };

        for tuple in &genisis_tuples[..] {
            db.insert(tuple);
        }

        db
    }

    pub fn insert(&mut self, tuple: &Datom) {
        self.eavt.insert(Eavt(*tuple));
        self.aevt.insert(Aevt(*tuple));
        self.avet.insert(Avet(*tuple));
        self.vaet.insert(Vaet(*tuple));
    }

    pub fn create_transaction(&mut self) -> Tid {
        let tid = Tid(self.next_transaction_id);
        self.next_transaction_id += 2;

        let timestamp_aid = Aid(1);
        let timestamp_value = Value(0); // TODO
        let attr_timestamp = self.create_entity(timestamp_aid, timestamp_value, tid, Operation::Assert);

        tid
    }

    pub fn create_entity(&mut self, attribute: Aid, value: Value, transaction: Tid, operation: Operation) -> Eid {
        let eid = Eid(self.next_id);
        self.next_transaction_id += 2;

        let tuple = Datom {
            entity: eid,
            attribute: attribute,
            value: value,
            transaction_operation: TidOp::new(transaction, operation),
        };

        self.insert(&tuple);

        eid
    }

    pub fn lookup_value(&self, entity: Eid, attribute: Aid) -> Option<Value> {
        // TODO: This function should return an iterator of values, from newer
        // to older, with retracted tuples deleted.
        let min = Datom::new(entity, attribute, Value::min(), Tid(0), Operation::Retract);
        let max = Datom::new(entity, attribute, Value::max(), Tid(0), Operation::Retract);
        // TODO: Could use Eavt or Aevt for this, which indicates it is probably
        // inefficient to do one by one. I could look up multiple attributes, or
        // multiple entities, at once.
        let mut range = self.eavt.range(Eavt(min)..Eavt(max));
        range.map(|&Eavt(tuple)| tuple.value).next_back()
    }

    pub fn lookup_attribute_name(&self, attribute: Aid) -> Value {
        let entity = Eid(attribute.0);
        let value = self
            .lookup_value(entity, self.builtins.attribute_db_attribute_name)
            .expect("All attributes must have a name.");

        value
    }

    pub fn lookup_attribute_type(&self, attribute: Aid) -> Type {
        // TODO: Attribute name and type are adjacent in Eavt,
        // a function to look both of them up at once would be more efficient.
        let entity = Eid(attribute.0);
        let value = self
            .lookup_value(entity, self.builtins.attribute_db_attribute_type)
            .expect("All attributes must have a value type.");

        // TODO: I can make these numbers constants rather than variables.
        match value.as_u64() {
            k if k == self.builtins.entity_db_type_bool.0 => Type::Bool,
            k if k == self.builtins.entity_db_type_ref.0 => Type::Ref,
            k if k == self.builtins.entity_db_type_uint64.0 => Type::Uint64,
            k if k == self.builtins.entity_db_type_bytes.0 => Type::Bytes,
            k if k == self.builtins.entity_db_type_string.0 => Type::String,
            _ => panic!("Attribute has unsupported value type."),
        }
    }

    /// Return the entities for which the given attribute is set.
    /// TODO: Should return iterator.
    pub fn select_where_has_attribute(&self, attribute: Aid) -> Vec<Eid> {
        let min = Datom::new(Eid::min(), attribute, Value::min(), Tid(0), Operation::Retract);
        let max = Datom::new(Eid::max(), attribute, Value::max(), Tid(0), Operation::Retract);
        self.aevt
            .range(Aevt(min)..Aevt(max))
            .map(|&Aevt(tuple)| tuple.entity)
            .collect()
    }

    /// Return the attributes that are set for at least one of the entities.
    pub fn get_entity_attributes<'a, I>(&'a self, entities: I) -> HashSet<Aid>
    where I: IntoIterator<Item = &'a Eid>
    {
        let mut attributes = HashSet::new();
        for &eid in entities.into_iter() {
            let min = Datom::new(eid, Aid::min(), Value::min(), Tid(0), Operation::Retract);
            let max = Datom::new(eid, Aid::max(), Value::max(), Tid(0), Operation::Retract);

            // TODO: Cancel tuples against retractions, if there is a
            // retraction.
            for &Eavt(tuple) in self.eavt.range(Eavt(min)..Eavt(max)) {
                attributes.insert(tuple.attribute);
            }
        }

        attributes
    }

    pub fn debug_print(&self) {
        // 6 9 7 11 9
        println!("entity  attribute     value       type    transaction  operation");
        println!("------  ------------  ----------  ------  -----------  ---------");
        for &Eavt(tuple) in self.eavt.iter() {
            let attribute_name = self.lookup_attribute_name(tuple.attribute);
            let attribute_type = self.lookup_attribute_type(tuple.attribute);

            let attribute = format!("{} ({})", attribute_name.as_str(), tuple.attribute.0);
            print!("{:6}  {:12}  ", tuple.entity.0, attribute);


            match attribute_type {
                Type::Bool if tuple.value.as_bool() => print!("true        bool  "),
                Type::Bool   => print!("false       bool  "),
                Type::Ref    => print!("{:>10}  ref   ", tuple.value.as_u64()),
                Type::Uint64 => print!("{:>10}  uint64", tuple.value.as_u64()),
                Type::Bytes  => unimplemented!("TODO"),
                Type::String => print!("{:<10}  string", tuple.value.as_str()),
            }

            println!("  {:11}  {:>9}",
                tuple.transaction_operation.transaction().0,
                match tuple.transaction_operation.operation() {
                    Operation::Retract => "retract",
                    Operation::Assert => "assert",
                }
            );
        }
    }

    pub fn debug_print_table(&self, entities: &[Eid]) {
        let mut attributes: Vec<Aid> = self
            .get_entity_attributes(entities)
            .iter()
            .cloned()
            .collect();

        if attributes.len() == 0 {
            print!("No data.");
            return
        }

        // The tuples collection is ordered by entity id and then attribute id.
        // If we sort all of the attributes to print, then we can do one single
        // pass over the tuples and print the table.
        attributes.sort();

        let mut attribute_types = Vec::new();

        print!("id    ");
        for &attribute in &attributes {
            let attribute_name = self.lookup_attribute_name(attribute);
            let attribute_type = self.lookup_attribute_type(attribute);
            print!("{:12}  ", attribute_name.as_str());
            attribute_types.push(attribute_type);
        }
        print!("\n----  ");
        for _ in &attributes {
            print!("------------  ");
        }


        for &eid in entities {
            let min = Datom::new(eid, Aid::min(), Value::min(), Tid(0), Operation::Retract);
            let max = Datom::new(eid, Aid::max(), Value::max(), Tid(0), Operation::Retract);

            let mut current_attribute = 0;

            for &Eavt(tuple) in self.eavt.range(Eavt(min)..Eavt(max)) {
                if current_attribute == 0 {
                    print!("\n{:4}  ", tuple.entity.0);
                }

                // Skip over all attributes that this entity does not have.
                while attributes[current_attribute] != tuple.attribute {
                    print!("<null>        ");
                    current_attribute += 1;

                    if current_attribute == attributes.len() {
                        break
                    }
                }

                if current_attribute < attributes.len() {
                    // TODO: Deduplicate printing per type.
                    match attribute_types[current_attribute] {
                        Type::Bool if tuple.value.as_bool() => print!("true          "),
                        Type::Bool   => print!("false         "),
                        Type::Ref    => print!("{:>12}  ", tuple.value.as_u64()),
                        Type::Uint64 => print!("{:>12}  ", tuple.value.as_u64()),
                        Type::Bytes  => unimplemented!("TODO"),
                        Type::String => print!("{:<12}  ", tuple.value.as_str()),
                    }
                    current_attribute += 1;
                }
            }
        }

        println!("");
    }
}
