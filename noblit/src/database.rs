// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines the database itself.

use std::collections::HashSet;
use std::io;

use datom::{Eid, Aid, Value, Tid, Operation, Datom};
use head::{Head, IdGen, IndexRoots};
use heap::{CidBytes, self};
use htree::HTree;
use index::{DatomOrd, Aevt, Avet, Eavt};
use store::{self};
use temp_heap::{TempHeap, Temporaries};
use types::Type;

/// The genesis transaction adds all built-in attributes.
pub struct Builtins {
    /// Transaction id of the genesis transaction.
    pub genesis_transaction: Tid,

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
        let id_db_type_bool = 6;
        let id_db_type_ref = 7;
        let id_db_type_uint64 = 8;
        let id_db_type_bytes = 9;
        let id_db_type_string = 10;

        let builtins = Builtins {
            genesis_transaction: Tid(id_transaction),
            attribute_db_attribute_name: Aid(id_db_attr_name),
            attribute_db_attribute_type: Aid(id_db_attr_type),
            attribute_db_attribute_unique: Aid(id_db_attr_unique),
            attribute_db_attribute_many: Aid(id_db_attr_many),
            attribute_db_type_name: Aid(id_db_type_name),
            entity_db_type_bool: Eid(id_db_type_bool),
            entity_db_type_ref: Eid(id_db_type_ref),
            entity_db_type_uint64: Eid(id_db_type_uint64),
            entity_db_type_bytes: Eid(id_db_type_bytes),
            entity_db_type_string: Eid(id_db_type_string),
        };

        let mut const_off = 0;
        let mut define_const = |value: &'static str| {
            let cid = CidBytes(const_off);
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

        (builtins, tuples, consts)
    }
}

/// A collection of datoms to be added, with means to create those.
///
/// A transaction can be started with `database::begin()`. The transaction
/// stores new datoms to be added, and it allocates ids for new entities. Datoms
/// added to the transaction are stored in the transaction itself, they are not
/// yet written to the database. Only `database::commit()` modifies the database
/// to write the datoms. This means that there is no explicit rollback. Simply
/// drop the transaction without committing it instead.
pub struct Transaction {
    /// Database head at the start of the transaction.
    base: Head,

    /// Mutable id counters to supply entity ids for this transaction.
    ///
    /// The id generator in the base remains unchanged; it is used to ensure
    /// that the database has not changed by the time we are ready to commit.
    /// The id generator here is the one that we use to generate ids for new
    /// entities. At commit, the database head is forwarded to it.
    id_gen: IdGen,

    /// The id of this transaction.
    id: Tid,

    /// The datoms that committing this transaction will add.
    datoms: Vec<Datom>,
}

impl Transaction {
    fn new(base: Head) -> Transaction {
        let mut id_gen = base.id_gen.clone();
        let transaction_id = id_gen.take_transaction_id();

        Transaction {
            base: base,
            id_gen: id_gen,
            id: transaction_id,
            datoms: Vec::new(),
        }
    }

    /// Generate a fresh entity id.
    pub fn create_entity(&mut self) -> Eid {
        self.id_gen.take_entity_id()
    }

    /// Add a new datom to this transaction.
    ///
    /// The transaction field of the datom will be filled automatically with the
    /// id of this transaction.
    pub fn insert(&mut self, entity: Eid, attribute: Aid, value: Value, operation: Operation) {
        let datom = Datom::new(entity, attribute, value, self.id, operation);
        self.datoms.push(datom);
    }

    /// Shorthand for inserting an assertion datom.
    pub fn assert(&mut self, entity: Eid, attribute: Aid, value: Value) {
        self.insert(entity, attribute, value, Operation::Assert);
    }

    /// Shorthand for inserting a retraction datom.
    pub fn retract(&mut self, entity: Eid, attribute: Aid, value: Value) {
        self.insert(entity, attribute, value, Operation::Retract);
    }
}

/// A database.
///
/// The database stores the current tree roots of the indexes, and counters to
/// allocate new ids for transactions and other entities.
pub struct Database<Store, Heap> {
    pub builtins: Builtins,
    store: Store,
    heap: Heap,
    head: Head,
}

/// An immutable view of the database with temporaries.
///
/// The view combines data from two places to be able to evaluate queries:
///
///  * The database itself, read-only.
///  * A temporary heap, with large values that occur in the query, but which
///    may not be in the database itself.
///
pub struct View<'a, Store: 'a + store::Store, Heap: 'a + heap::Heap> {
    /// The database, which is immutable while we query it.
    database: &'a Database<Store, Heap>,

    /// The temporary constants that are needed for a query.
    ///
    /// The temporary values live on top of the base heap from the database.
    temp_heap: TempHeap<&'a Heap>,
}

impl<Store: store::Store, Heap: heap::Heap> Database<Store, Heap> {
    pub fn new(mut store: Store, mut heap: Heap) -> io::Result<Database<Store, Heap>>
    where Store: store::StoreMut, Heap: heap::HeapMut
    {
        let (builtins, genesis_datoms, genesis_consts) = Builtins::new();

        for const_str in genesis_consts {
            heap.append_bytes(const_str.as_bytes())?;
        }

        let roots = IndexRoots {
            eavt_root: HTree::initialize(Eavt, &mut store, &heap, &genesis_datoms)?.root_page,
            aevt_root: HTree::initialize(Aevt, &mut store, &heap, &genesis_datoms)?.root_page,
            avet_root: HTree::initialize(Avet, &mut store, &heap, &genesis_datoms)?.root_page,
        };

        let head = Head {
            // We start allocating ids for user data at 100 to reserve some room
            // to extend the genesis transaction later.
            id_gen: IdGen::new(100),
            roots: roots,
        };

        let db = Database {
            builtins: builtins,
            store: store,
            heap: heap,
            head: head,
        };

        Ok(db)
    }

    /// Erase type arguments at the cost of virtual dispatch.
    pub fn into_dyn(self) -> Database<
        Box<dyn store::Store<Size = Store::Size>>,
        Box<dyn heap::Heap>,
    > where
        Store: 'static,
        Heap: 'static,
    {
        Database::open(Box::new(self.store), Box::new(self.heap), self.head)
    }

    /// Erase type arguments at the cost of virtual dispatch.
    pub fn into_dyn_mut(self) -> Database<
        Box<dyn store::StoreMut<Size = Store::Size>>,
        Box<dyn heap::HeapMut>,
    > where
        Store: store::StoreMut + 'static,
        Heap: heap::HeapMut + 'static,
    {
        Database::open(Box::new(self.store), Box::new(self.heap), self.head)
    }

    /// Open an existing database.
    pub fn open(store: Store, heap: Heap, head: Head) -> Database<Store, Heap> {
        // TODO: Check that the builtins are consistent with the database that
        // we are opening. All of them should be present. Having more of them is
        // fine. Also, we should probably do a few basic sanity checks to ensure
        // that the head and the store and heap are consistent.
        let (builtins, _genesis_datoms, _genesis_consts) = Builtins::new();
        Database {
            builtins: builtins,
            store: store,
            heap: heap,
            head: head,
        }
    }

    /// Return the inner store.
    pub fn get_store(&self) -> &Store {
        &self.store
    }

    /// Return the inner heap.
    pub fn get_heap(&self) -> &Heap {
        &self.heap
    }

    /// Return the current head.
    pub fn get_head(&self) -> &Head {
        &self.head
    }

    /// View the current database and given temporaries together.
    pub fn view(&self, temporaries: Temporaries) -> View<Store, Heap> {
        View {
            database: self,
            temp_heap: TempHeap::new(&self.heap, temporaries),
        }
    }

    /// Return the (entity, attribute, value, transaction) index, immutable.
    fn eavt(&self) -> HTree<Eavt, &Store, &Heap> {
        HTree::new(self.head.roots.eavt_root, Eavt, &self.store, &self.heap)
    }

    /// Return the (entity, attribute, value, transaction) index, immutable.
    fn aevt(&self) -> HTree<Aevt, &Store, &Heap> {
        HTree::new(self.head.roots.aevt_root, Aevt, &self.store, &self.heap)
    }

    /// Return the (attribute, value, entity, transaction) index, immutable.
    fn avet(&self) -> HTree<Avet, &Store, &Heap> {
        HTree::new(self.head.roots.avet_root, Avet, &self.store, &self.heap)
    }

    /// Return the (entity, attribute, value, transaction) index, writable.
    fn eavt_mut(&mut self, roots: &IndexRoots) -> HTree<Eavt, &mut Store, &Heap>
    where Store: store::StoreMut {
        HTree::new(roots.eavt_root, Eavt, &mut self.store, &self.heap)
    }

    /// Return the (entity, attribute, value, transaction) index, writable.
    fn aevt_mut(&mut self, roots: &IndexRoots) -> HTree<Aevt, &mut Store, &Heap>
    where Store: store::StoreMut {
        HTree::new(roots.aevt_root, Aevt, &mut self.store, &self.heap)
    }

    /// Return the (attribute, value, entity, transaction) index, writable.
    fn avet_mut(&mut self, roots: &IndexRoots) -> HTree<Avet, &mut Store, &Heap>
    where Store: store::StoreMut {
        HTree::new(roots.avet_root, Avet, &mut self.store, &self.heap)
    }

    /// Assert that the database is well-formed.
    ///
    /// This is used in tests and during fuzzing. This is a high-level check
    /// that invokes `check_invariants` on the parts that make up the database.
    ///
    /// Panics on a violated invariant, returns unit on success.
    pub fn check_invariants(&self) -> io::Result<()> where Heap: heap::SizedHeap {
        heap::check_invariants(&self.heap);
        self.eavt().check_invariants()?;
        self.aevt().check_invariants()?;
        self.avet().check_invariants()?;
        Ok(())
    }

    /// Persist temporary values that the datoms may reference the heap.
    ///
    /// Values that come from the input query (whether read-only or write) are
    /// stored as temporaries. When an assertion produces datoms that have
    /// tempoaries as values, these datoms cannot be inserted directly. The
    /// temporaries must first be persisted on the heap, and the values in the
    /// datoms must be updated to reference the new stable offsets.
    fn persist_temporaries(
        &mut self,
        temporaries: &Temporaries,
        datoms: &mut [Datom],
    ) -> io::Result<()>
    where Heap: heap::HeapMut {
        for datom in datoms.iter_mut() {
            let value = datom.value;
            if value.is_temporary() {
                let value_fin = match () {
                    _ if value.is_u64() => {
                        let cid_tmp = value.as_const_u64();
                        let data = temporaries.get_u64(cid_tmp);
                        let cid_fin = self.heap.append_u64(data)?;
                        Value::from_const_u64(cid_fin)
                    }
                    _ if value.is_bytes() => {
                        let cid_tmp = value.as_const_bytes();
                        let data = temporaries.get_bytes(cid_tmp);
                        let cid_fin = self.heap.append_bytes(data)?;
                        Value::from_const_bytes(cid_fin)
                    }
                    _ => unreachable!("Value is either u64 or bytes."),
                };
                datom.value = value_fin;
            }
        }
        Ok(())
    }

    /// Insert datoms into the database, return new index roots.
    ///
    /// Takes the datoms by value in order to sort them in-place during
    /// insertion without allocating an unnecessary copy, as the datoms
    /// are not going to be used afterwards anyway.
    ///
    /// Returns the new tree roots. When the method returns, tree nodes have
    /// been persisted, but they will not be reachable unless the new roots are
    /// persisted as well.
    fn insert(&mut self, roots: &IndexRoots, mut datoms: Vec<Datom>) -> io::Result<IndexRoots>
    where Store: store::StoreMut {
        // Perform some sanity checks over the datoms to be inserted.
        for &datom in &datoms {
            // The max value can be used as an upper bound for range queries;
            // it should not be persisted directly.
            assert!(
                !datom.value.is_max(),
                "Value::max() should not be persisted.",
            );

            // Temporaries need to be persisted before inserting the datoms.
            assert!(
                !datom.value.is_temporary(),
                "Datoms with temporary values should not be persisted.",
            );
        }

        // We do aevt after avet so the data is still partiall sorted for the
        // second sort.
        let avet_root = {
            let mut index = self.avet_mut(roots);
            datoms.sort_by(|x, y| index.comparator.cmp(x, y, index.heap));
            index.insert(&datoms[..])?
        };
        let aevt_root = {
            let mut index = self.aevt_mut(roots);
            datoms.sort_by(|x, y| index.comparator.cmp(x, y, index.heap));
            index.insert(&datoms[..])?
        };
        let eavt_root = {
            let mut index = self.eavt_mut(roots);
            datoms.sort_by(|x, y| index.comparator.cmp(x, y, index.heap));
            index.insert(&datoms[..])?
        };

        let roots = IndexRoots {
            avet_root: avet_root,
            aevt_root: aevt_root,
            eavt_root: eavt_root,
        };

        Ok(roots)
    }

    /// Begin a new transaction.
    ///
    /// A transaction holds datoms to be added, and it can allocate ids for new
    /// entities. The transaction itself does not mutate the database, only
    /// committing it does.
    ///
    /// A transaction tracks its *base*, the database tip that the transaction
    /// was created from. The transaction can only be committed if the tip has
    /// remained unchanged since the transaction was created.
    ///
    /// TODO: Could we use the type system, to ensure that we begin at most one
    /// transaction at a time, while still allowing query while the transaction
    /// exists, so new datoms can be partially streamed?
    pub fn begin(&self) -> Transaction {
        Transaction::new(self.head.clone())
    }

    /// Commit a transaction to storage, and advance the tip of the database.
    ///
    /// Given the transaction, which contains the datoms to be committed, and
    /// updated counters for entity id allocation, commit proceeds in three
    /// stages:
    ///
    /// * Persist large values that occur in new datoms, to the large value heap.
    /// * Persist the new datoms themselves, to the index trees.
    /// * Write the new head (tree roots and fresh id counters).
    ///
    /// In case of an IO error, some data may have been persisted, but the
    /// transaction will not be visible.
    pub fn commit(&mut self, temporaries: &Temporaries, transaction: Transaction) -> io::Result<()>
    where Store: store::StoreMut, Heap: heap::HeapMut {
        // TODO: Return error instead of panicking.
        assert_eq!(self.head, transaction.base, "Transaction was not based on current tip.");

        // First persist temporaries in the new datoms to the heap. This updates
        // the datoms in-place.
        let mut datoms = transaction.datoms;
        self.persist_temporaries(temporaries, &mut datoms)?;

        // Then insert the new datoms into the index trees. We get the new
        // roots to the trees that include the datoms we just inserted.
        let new_roots = self.insert(&transaction.base.roots, datoms)?;

        // Sanity check: the transaction base was the current head, so the
        // current roots must precede the new roots.
        assert!(self.head.roots.precedes(&new_roots), "Old index roots must precede new roots.");

        // TODO also check the id generator.
        // Also, possibly include a generation number to double check.

        self.head = Head {
            roots: new_roots,
            id_gen: transaction.id_gen,
        };

        // TODO: Have some abstraction for storing the head.
        Ok(())
    }
}

impl<'a, Store: 'a + store::Store, Heap: 'a + heap::Heap> View<'a, Store, Heap> {
    /// Return the heap that should be used to resolve values.
    ///
    /// TODO: is there a better way to do this, can we avoid making it public?
    pub fn heap(&self) -> &TempHeap<&'a Heap> {
        &self.temp_heap
    }

    /// Assume that the value represents an entity id, and look it up.
    ///
    /// Usually this will be a no-op, but for large entity ids, the value
    /// representation may be as an external value on the large value heap,
    /// and in that case we need to look it up.
    pub fn value_as_eid(&self, value: Value) -> Eid {
        value.as_eid(&self.temp_heap)
    }

    /// Destroy the view, release underlying heap, and return temporaries.
    ///
    /// This allows the temporaries that may have been created for a query, to
    /// be persisted to the underlying heap (which will be available for writes
    /// again after the view no longer borrows it read-only).
    pub fn into_temporaries(self) -> Temporaries {
        let (_db_heap, temporaries) = self.temp_heap.into_inner();
        temporaries
    }

    /// Return the (entity, attribute, value, transaction) index.
    pub fn eavt(&self) -> HTree<Eavt, &Store, &TempHeap<&'a Heap>> {
        HTree::new(self.database.head.roots.eavt_root, Eavt, &self.database.store, &self.temp_heap)
    }

    /// Return the (entity, attribute, value, transaction) index.
    pub fn aevt(&self) -> HTree<Aevt, &Store, &TempHeap<&'a Heap>> {
        HTree::new(self.database.head.roots.aevt_root, Aevt, &self.database.store, &self.temp_heap)
    }

    /// Return the (attribute, value, entity, transaction) index.
    pub fn avet(&self) -> HTree<Avet, &Store, &TempHeap<&'a Heap>> {
        HTree::new(self.database.head.roots.avet_root, Avet, &self.database.store, &self.temp_heap)
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

    /// Resolve a named attribute to its attribute id.
    ///
    /// If no such attribute exists, the resolved name is returned, which is
    /// helpful for making error messages more actionable.
    pub fn lookup_attribute_id(&self, name_cid: CidBytes) -> Result<Aid, String> {
        let value = Value::from_const_bytes(name_cid);
        match self.lookup_entity(self.database.builtins.attribute_db_attribute_name, value) {
            Some(Eid(id)) => Ok(Aid(id)),
            None => {
                let name_bytes = value.as_bytes(&self.temp_heap).to_vec();
                match String::from_utf8(name_bytes) {
                    Ok(attr_name) => Err(attr_name),
                    Err(..) => Err("<attribute name is invalid UTF-8>".into()),
                }
            }
        }
    }

    pub fn lookup_attribute_name(&self, attribute: Aid) -> Value {
        let entity = Eid(attribute.0);
        self
            .lookup_value(entity, self.database.builtins.attribute_db_attribute_name)
            .expect("All attributes must have a name.")
    }

    pub fn lookup_attribute_type(&self, attribute: Aid) -> Type {
        // TODO: Attribute name and type are adjacent in Eavt,
        // a function to look both of them up at once would be more efficient.
        let entity = Eid(attribute.0);
        let value = self
            .lookup_value(entity, self.database.builtins.attribute_db_attribute_type)
            .expect("All attributes must have a value type.");

        // TODO: I can make these numbers constants rather than variables.
        match value.as_u64(&self.temp_heap) {
            k if k == self.database.builtins.entity_db_type_bool.0 => Type::Bool,
            k if k == self.database.builtins.entity_db_type_ref.0 => Type::Ref,
            k if k == self.database.builtins.entity_db_type_uint64.0 => Type::Uint64,
            k if k == self.database.builtins.entity_db_type_bytes.0 => Type::Bytes,
            k if k == self.database.builtins.entity_db_type_string.0 => Type::String,
            _ => panic!("Attribute has unsupported value type."),
        }
    }

    /// Return the attributes that are set for at least one of the entities.
    pub fn get_entity_attributes<'b, I>(&'b self, entities: I) -> HashSet<Aid>
    where I: IntoIterator<Item = &'b Eid>
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
        let heap = &self.temp_heap;
        for &tuple in self.eavt().into_iter(&min, &max) {
            let attribute_name = self.lookup_attribute_name(tuple.attribute);
            let attribute_type = self.lookup_attribute_type(tuple.attribute);

            let attribute = format!("{} ({})", attribute_name.as_str(heap), tuple.attribute.0);
            print!("{:6}  {:12}  ", tuple.entity.0, attribute);


            match attribute_type {
                Type::Bool if tuple.value.as_bool() => print!("true        bool  "),
                Type::Bool   => print!("false       bool  "),
                Type::Ref    => print!("{:>10}  ref   ", tuple.value.as_u64(heap)),
                Type::Uint64 => print!("{:>10}  uint64", tuple.value.as_u64(heap)),
                Type::Bytes  => unimplemented!("TODO"),
                Type::String => print!("{:<10}  string", tuple.value.as_str(heap)),
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
