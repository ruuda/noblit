// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines the database itself.

use std::collections::HashSet;
use std::io;

use datom::{Eid, Aid, Value, Tid, Operation, TidOp, Datom};
use htree::HTree;
use index::{DatomOrd, Aevt, Avet, Eavt};
use store::{PageId, self};
use pool::{ConstId, self};
use types::Type;

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
    pub fn new() -> (Builtins, Vec<Datom>, Vec<&'static str>) {
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

        let mut const_off = 0;
        let mut define_const = |value: &'static str| {
            let cid = ConstId(const_off);
            // Increment 8 bytes for the size, and then by the value of the
            // constant, rounded up to 8 bytes.
            const_off += 8;
            const_off += (value.len() as u64 + 7) / 8 * 8;
            cid
        };

        let mut consts = Vec::new();
        let mut tuples = Vec::new();

        macro_rules! define_attribute {
            {
                aid: $attribute:expr,
                name: $name:expr,
                type: $type:expr,
                unique: $unique:expr,
                many: $many:expr,
            } => {
                let cid = define_const($name);
                consts.push($name);
                tuples.push(Datom::new(
                    Eid($attribute),
                    Aid(id_db_attr_name), Value::from_const_bytes(cid),
                    Tid(id_transaction), Operation::Assert,
                ));
                tuples.push(Datom::new(
                    Eid($attribute),
                    Aid(id_db_attr_type), Value::from_eid(Eid($type)),
                    Tid(id_transaction), Operation::Assert,
                ));
                tuples.push(Datom::new(
                    Eid($attribute),
                    Aid(id_db_attr_unique), Value::from_bool($unique),
                    Tid(id_transaction), Operation::Assert,
                ));
                tuples.push(Datom::new(
                    Eid($attribute),
                    Aid(id_db_attr_many), Value::from_bool($many),
                    Tid(id_transaction), Operation::Assert,
                ));
            }
        };

        macro_rules! define_type {
            { eid: $entity:expr, name: $name:expr, } => {
                let cid = define_const($name);
                consts.push($name);
                tuples.push(Datom::new(
                    Eid($entity),
                    Aid(id_db_type_name), Value::from_const_bytes(cid),
                    Tid(id_transaction), Operation::Assert,
                ));
            }
        };

        define_attribute! {
            aid: id_db_attr_name,
            name: "db.attribute.name",
            type: id_db_type_string,
            unique: true,
            many: false,
        };
        define_attribute! {
            aid: id_db_attr_type,
            name: "db.attribute.type",
            type: id_db_type_ref,
            unique: false,
            many: false,
        };
        define_attribute! {
            aid: id_db_attr_unique,
            name: "db.attribute.unique",
            type: id_db_type_bool,
            unique: false,
            many: false,
        };
        define_attribute! {
            aid: id_db_attr_many,
            name: "db.attribute.many",
            type: id_db_type_bool,
            unique: false,
            many: false,
        };
        // TODO: Add attribute attribute that indicates that the presence of
        // this attribute implies (or requires) the presence of another
        // attribute. This is how we can do schema, and "tables".
        define_attribute! {
            aid: id_db_type_name,
            name: "db.type.name",
            type: id_db_type_string,
            unique: true,
            many: false,
        };
        define_attribute! {
            aid: id_db_transaction_time,
            name: "db.transaction.time",
            type: id_db_type_uint64, // TODO: Time type.
            unique: false, // TODO: Timestamp uniqueness is debatable.
            many: false,
        };

        define_type! {
            eid: id_db_type_bool,
            name: "db.type.bool",
        };
        define_type! {
            eid: id_db_type_ref,
            name: "db.type.ref",
        };
        define_type! {
            eid: id_db_type_uint64,
            name: "db.type.uint64",
        };
        define_type! {
            eid: id_db_type_bytes,
            name: "db.type.bytes",
        };
        define_type! {
            eid: id_db_type_string,
            name: "db.type.string",
        };

        tuples.push(Datom::new(
            Eid(id_transaction),
            Aid(id_db_transaction_time), Value::from_u64(0),
            Tid(id_transaction), Operation::Assert,
        ));

        (builtins, tuples, consts)
    }
}

pub struct Database<Store> {
    pub builtins: Builtins,
    store: Store,
    next_id: u64,
    next_transaction_id: u64,
    eavt_root: PageId,
    aevt_root: PageId,
    avet_root: PageId,
}

impl<Store: store::Store> Database<Store> {
    pub fn new(mut store: Store) -> io::Result<Database<Store>>
    where Store: store::StoreMut + pool::PoolMut
    {
        let (builtins, genisis_datoms, genisis_consts) = Builtins::new();

        let eavt_root = HTree::initialize(Eavt, &mut store, &genisis_datoms)?.root_page;
        let aevt_root = HTree::initialize(Aevt, &mut store, &genisis_datoms)?.root_page;
        let avet_root = HTree::initialize(Avet, &mut store, &genisis_datoms)?.root_page;

        for const_str in genisis_consts {
            store.append_bytes(const_str.as_bytes())?;
        }

        let db = Database {
            builtins: builtins,
            store: store,
            // Transaction ids must be even. For now we do that by just tracking
            // separate counters and incrementing both by 2. Perhaps this is a
            // very bad idea and we want to have the entities created in a
            // transaction and the transaction itself be adjacent in the indices
            // by giving them adjacent ids. We start these counters at 100 to
            // reserve some room to extend the geneisis transaction.
            next_id: 101,
            next_transaction_id: 100,
            eavt_root: eavt_root,
            aevt_root: aevt_root,
            avet_root: avet_root,
        };

        Ok(db)
    }

    /// Return the (entity, attribute, value, transaction) index.
    pub fn eavt(&self) -> HTree<Eavt, &Store> {
        HTree::new(self.eavt_root, Eavt, &self.store)
    }

    /// Return the (entity, attribute, value, transaction) index, writable.
    pub fn eavt_mut(&mut self) -> HTree<Eavt, &mut Store> where Store: store::StoreMut {
        HTree::new(self.eavt_root, Eavt, &mut self.store)
    }

    /// Return the (entity, attribute, value, transaction) index.
    pub fn aevt(&self) -> HTree<Aevt, &Store> {
        HTree::new(self.aevt_root, Aevt, &self.store)
    }

    /// Return the (entity, attribute, value, transaction) index, writable.
    pub fn aevt_mut(&mut self) -> HTree<Aevt, &mut Store> where Store: store::StoreMut {
        HTree::new(self.aevt_root, Aevt, &mut self.store)
    }

    /// Return the (attribute, value, entity, transaction) index.
    pub fn avet(&self) -> HTree<Avet, &Store> {
        HTree::new(self.avet_root, Avet, &self.store)
    }

    /// Return the (attribute, value, entity, transaction) index, writable.
    pub fn avet_mut(&mut self) -> HTree<Avet, &mut Store> where Store: store::StoreMut {
        HTree::new(self.avet_root, Avet, &mut self.store)
    }

    /// Insert datoms into the database.
    ///
    /// Takes the datoms by value in order to sort them in-place during
    /// insertion without allocating an unnecessary copy, in case the datoms
    /// are not going to be used afterwards anyway.
    pub fn insert(&mut self, mut datoms: Vec<Datom>) -> io::Result<()>
    where Store: store::StoreMut {
        self.eavt_root = {
            let mut index = self.eavt_mut();
            datoms.sort_by(|x, y| index.comparator.cmp(x, y));
            index.insert(&datoms[..])?
        };
        self.aevt_root = {
            let mut index = self.aevt_mut();
            datoms.sort_by(|x, y| index.comparator.cmp(x, y));
            index.insert(&datoms[..])?
        };
        self.avet_root = {
            let mut index = self.avet_mut();
            datoms.sort_by(|x, y| index.comparator.cmp(x, y));
            index.insert(&datoms[..])?
        };
        Ok(())
    }

    pub fn create_transaction(&mut self, datoms: &mut Vec<Datom>) -> Tid {
        let tid = Tid(self.next_transaction_id);
        self.next_transaction_id += 2;

        let timestamp_aid = Aid(1);
        let timestamp_value = Value(0); // TODO
        let _attr_timestamp = self.create_entity(datoms, timestamp_aid, timestamp_value, tid);

        tid
    }

    pub fn create_entity(
        &mut self,
        datoms: &mut Vec<Datom>,
        attribute: Aid,
        value: Value,
        transaction: Tid
    ) -> Eid {
        let eid = Eid(self.next_id);
        self.next_id += 2;

        let datom = Datom {
            entity: eid,
            attribute: attribute,
            value: value,
            transaction_operation: TidOp::new(transaction, Operation::Assert),
        };

        datoms.push(datom);

        eid
    }

    pub fn assert(
        &mut self,
        datoms: &mut Vec<Datom>,
        entity: Eid,
        attribute: Aid,
        value: Value,
        transaction: Tid
    ) {
        let datom = Datom {
            entity: entity,
            attribute: attribute,
            value: value,
            transaction_operation: TidOp::new(transaction, Operation::Assert),
        };

        datoms.push(datom);
    }

    pub fn lookup_value(&self, entity: Eid, attribute: Aid) -> Option<Value> {
        // TODO: This function should return an iterator of values, from newer
        // to older, with retracted tuples deleted. Currently it just returns
        // the first one.
        let min = Datom::new(entity, attribute, Value::min(), Tid(0), Operation::Retract);
        let max = Datom::new(entity, attribute, Value::max(), Tid(0), Operation::Retract);
        // TODO: Could use Eavt or Aevt for this, which indicates it is probably
        // inefficient to do one by one. I could look up multiple attributes, or
        // multiple entities, at once.
        self.eavt().into_iter(&min, &max).map(|&datom| datom.value).next()
    }

    pub fn lookup_entity(&self, attribute: Aid, value: Value) -> Option<Eid> {
        // TODO: This function should return an iterator of values, from newer
        // to older, with retracted tuples deleted. Currently it just returns
        // the first one.
        let min = Datom::new(Eid::min(), attribute, value, Tid::min(), Operation::Retract);
        let max = Datom::new(Eid::max(), attribute, value, Tid::max(), Operation::Retract);
        self.avet().into_iter(&min, &max).map(|&datom| datom.entity).next()
    }

    pub fn lookup_attribute_id(&self, name: &str) -> Option<Aid> {
        // TODO: How am I going to deal with temporary on-heap values?
        self.lookup_entity(self.builtins.attribute_db_attribute_name, Value::from_str(name))
            .map(|Eid(id)| Aid(id))
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
            for &datom in self.eavt().into_iter(&min, &max) {
                attributes.insert(datom.attribute);
            }
        }

        attributes
    }

    pub fn debug_print(&self) {
        // 6 9 7 11 9
        println!("entity  attribute     value       type    transaction  operation");
        println!("------  ------------  ----------  ------  -----------  ---------");
        // TODO: Add support for open-ended ranges.
        let min = Datom::new(Eid::min(), Aid::min(), Value::min(), Tid(0), Operation::Assert);
        let max = Datom::new(Eid::max(), Aid::max(), Value::max(), Tid(0), Operation::Retract);
        for &tuple in self.eavt().into_iter(&min, &max) {
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
}
