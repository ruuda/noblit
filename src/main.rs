use std::collections::BTreeSet;
use std::cmp::{PartialOrd, Ord, Ordering};

/// Entity id.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Eid(u64);

/// Attribute id.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Aid(u64);

/// Transaction number.
/// TODO: Don't expose internals, add a constructor that validates the tid is
/// even.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Tid(u64);

/// Transaction number and operation.
///
/// A packed representation of an even transaction number and an operation. The
/// transaction number can be obtained by zeroing the least significant bit, and
/// the least significant bit indicates the operation:
///
/// * 0 indicates a retraction.
/// * 1 indicates an assertion.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct TidOp(u64);

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Operation {
    Retract,
    Assert,
}

impl TidOp {
    pub fn new(transaction: Tid, operation: Operation) -> TidOp {
        debug_assert_eq!(transaction.0 & 1, 0, "Transaction id must be even.");
        let low_bit = match operation {
            Operation::Retract => 0,
            Operation::Assert => 1,
        };
        TidOp(transaction.0 | low_bit)
    }

    pub fn transaction(self) -> Tid {
        Tid(self.0 & 0xffff_ffff_ffff_fffe)
    }

    pub fn operation(self) -> Operation {
        if self.0 & 1 == 0 {
            Operation::Retract
        } else {
            Operation::Assert
        }
    }
}

/// The supported value types for entity values.
enum Type {
    Bool,
    Ref,
    Uint64,
    Bytes,
    String,
}

/// A value.
///
/// A value is either a byte string, or an unsigned 62-bit integer. It is up to
/// the attribute schema to give meaning to a value. For example, a byte string
/// might store utf-8 text, and an integer might hold an entity id.
///
/// The most significant two bits contain the type and representation tag:
///
/// * `0b00`: An unsigned 62-bit integer stored inline. This means that all
///           unsigned integers less than 2^62 are represented as themselves.
/// * `0b01`: An unsigned 64-bit integer stored externally. The remaining 62
///           bits indicate its storage address.
/// * `0b10`: A small string. The next 6 most significant bits indicate the
///           length, although the maximum valid length is 7. The next 7 bytes
///           contain the string, padded with zeros. This means that the empty
///           string is represented as `0x8000_0000_0000_0000`.
/// * `0b11`: A string stored externally. The remaining 62 bits indicate its
///           storage address.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Value(u64);

impl Value {
    pub fn from_u64(value: u64) -> Value {
        assert_eq!(value & 0xc000_0000_0000_0000, 0, "TODO: Implement spilling.");
        Value(value)
    }

    pub fn from_bool(value: bool) -> Value {
        match value {
            false => Value(0),
            true => Value(1),
        }
    }

    pub fn from_eid(value: Eid) -> Value {
        Value::from_u64(value.0)
    }

    pub fn from_str(value: &str) -> Value {
        assert!(value.len() < 8);
        let mut bytes = [0_u8; 7];
        bytes[..value.len()].copy_from_slice(value.as_bytes());
        let bits = 0_u64
            | 0b10_u64 << 62
            | (value.len() as u64) << 56
            | (bytes[6] as u64) << 48
            | (bytes[5] as u64) << 40
            | (bytes[4] as u64) << 32
            | (bytes[3] as u64) << 24
            | (bytes[2] as u64) << 16
            | (bytes[1] as u64) << 8
            | (bytes[0] as u64)
            ;
        Value(bits)
    }

    pub fn as_str(&self) -> &str {
        use std::mem;
        use std::str;
        debug_assert_ne!(self.0 & 0xc000_0000_0000_0000, 0, "Value must be string for as_str.");
        let bytes: &[u8; 8] = unsafe { mem::transmute(&self.0) };
        let len = (bytes[7] & 0x3f) as usize;
        debug_assert!(len <= 7);
        str::from_utf8(&bytes[..len]).expect("Should only store UTF-8 strings.")
    }

    pub fn as_u64(&self) -> u64 {
        // TODO: Check only the top bit, deal with out of band integers.
        debug_assert_eq!(self.0 & 0xc000_0000_0000_0000, 0, "Value must be int for as_u64.");
        self.0 & 0x3fff_ffff_ffff_ffff
    }

    pub fn as_bool(&self) -> bool {
        debug_assert_eq!(self.0 & 0xc000_0000_0000_0000, 0, "Value must be int for as_bool.");
        match self.0 {
            0 => false,
            1 => true,
            _ => unreachable!("Bool values should be either 0 or 1."),
        }
    }

    pub fn min() -> Value {
        Value(0)
    }

    pub fn max() -> Value {
        Value(0xffff_ffff_ffff_ffff)
    }
}

/// An (entity, attribute, value, transaction, operation) tuple.
#[derive(Copy, Clone)]
struct Tuple {
    entity: Eid,
    attribute: Aid,
    value: Value,
    transaction_operation: TidOp,
}

impl Tuple {
    pub fn new(entity: Eid, attribute: Aid, value: Value, transaction: Tid, operation: Operation) -> Tuple {
        Tuple {
            entity: entity,
            attribute: attribute,
            value: value,
            transaction_operation: TidOp::new(transaction, operation),
        }
    }

    /// The (attribute, entity, value, transaction) tuple.
    pub fn aevt(&self) -> (u64, u64, u64, u64) {
        // TODO: Deref the value.
        (self.attribute.0, self.entity.0, self.value.0, self.transaction_operation.0)
    }

    /// The (attribute, value, entity, transaction) tuple.
    pub fn avet(&self) -> (u64, u64, u64, u64) {
        // TODO: Deref the value.
        (self.attribute.0, self.value.0, self.entity.0, self.transaction_operation.0)
    }

    /// The (entity, attribute, value, transaction) tuple.
    pub fn eavt(&self) -> (u64, u64, u64, u64) {
        // TODO: Deref the value.
        (self.entity.0, self.attribute.0, self.value.0, self.transaction_operation.0)
    }

    /// The (value, attribute, entity, transaction) tuple.
    pub fn vaet(&self) -> (u64, u64, u64, u64) {
        // TODO: Deref the value.
        (self.value.0, self.attribute.0, self.value.0, self.transaction_operation.0)
    }

    /// Return the tuple, with operation set to `Operation::Assert`.
    pub fn assert(&self) -> Tuple {
        let transaction = self.transaction_operation.transaction();
        Tuple {
            entity: self.entity,
            attribute: self.attribute,
            value: self.value,
            transaction_operation: TidOp::new(transaction, Operation::Assert),
        }
    }
}

struct Aevt(pub Tuple);
struct Avet(pub Tuple);
struct Eavt(pub Tuple);
struct Vaet(pub Tuple);

impl Eq for Aevt {}
impl Eq for Avet {}
impl Eq for Eavt {}
impl Eq for Vaet {}

impl Ord for Aevt {
    fn cmp(&self, other: &Aevt) -> Ordering {
        self.0.aevt().cmp(&other.0.aevt())
    }
}

impl Ord for Avet {
    fn cmp(&self, other: &Avet) -> Ordering {
        self.0.avet().cmp(&other.0.avet())
    }
}

impl Ord for Eavt {
    fn cmp(&self, other: &Eavt) -> Ordering {
        self.0.eavt().cmp(&other.0.eavt())
    }
}

impl Ord for Vaet {
    fn cmp(&self, other: &Vaet) -> Ordering {
        self.0.vaet().cmp(&other.0.vaet())
    }
}

impl PartialOrd for Aevt {
    fn partial_cmp(&self, other: &Aevt) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd for Avet {
    fn partial_cmp(&self, other: &Avet) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd for Eavt {
    fn partial_cmp(&self, other: &Eavt) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd for Vaet {
    fn partial_cmp(&self, other: &Vaet) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Aevt {
    fn eq(&self, other: &Aevt) -> bool {
        self.0.aevt().eq(&other.0.aevt())
    }
}

impl PartialEq for Avet {
    fn eq(&self, other: &Avet) -> bool {
        self.0.avet().eq(&other.0.avet())
    }
}

impl PartialEq for Eavt {
    fn eq(&self, other: &Eavt) -> bool {
        self.0.eavt().eq(&other.0.eavt())
    }
}

impl PartialEq for Vaet {
    fn eq(&self, other: &Vaet) -> bool {
        self.0.vaet().eq(&other.0.vaet())
    }
}

/// The genisis transaction adds all built-in attributes.
struct Builtins {
    /// Transaction id of the genisis transaction.
    genisis_transaction: Tid,

    /// Built-in attribute `db.attribute.name`.
    attribute_db_attribute_name: Aid,
    /// Built-in attribute `db.attribute.type`.
    attribute_db_attribute_type: Aid,
    /// Built-in attribute `db.attribute.unique`.
    attribute_db_attribute_unique: Aid,
    /// Built-in attribute `db.attribute.many`.
    attribute_db_attribute_many: Aid,
    /// Built-in attribute `db.type.name`.
    attribute_db_type_name: Aid,
    /// Built-in attribute `db.transaction.time`.
    attribute_db_transaction_time: Aid,

    /// Built-in type `db.type.bool`.
    entity_db_type_bool: Eid,
    /// Built-in type `db.type.ref`.
    entity_db_type_ref: Eid,
    /// Built-in type `db.type.uint64`.
    entity_db_type_uint64: Eid,
    /// Built-in type `db.type.bytes`.
    entity_db_type_bytes: Eid,
    /// Built-in type `db.type.string`.
    entity_db_type_string: Eid,
}

impl Builtins {
    pub fn new() -> (Builtins, Vec<Tuple>) {
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
            Tuple::new(
                Eid(id_db_attr_name),
                Aid(id_db_attr_name), Value::from_str("name"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_attr_name),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_string)),
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_attr_name),
                Aid(id_db_attr_unique), Value::from_bool(true),
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_attr_type),
                Aid(id_db_attr_name), Value::from_str("type"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_attr_type),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_ref)),
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_attr_unique),
                Aid(id_db_attr_name), Value::from_str("unique"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_attr_unique),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_bool)),
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_attr_many),
                Aid(id_db_attr_name), Value::from_str("many"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_attr_many),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_bool)),
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_type_name),
                Aid(id_db_attr_name), Value::from_str("name"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_type_name),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_string)),
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_type_name),
                Aid(id_db_attr_unique), Value::from_bool(true),
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_transaction_time),
                Aid(id_db_attr_name), Value::from_str("time"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_transaction_time),
                Aid(id_db_attr_type), Value::from_eid(Eid(id_db_type_uint64)), // TODO: Time type.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_type_bool),
                Aid(id_db_type_name), Value::from_str("bool"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_type_ref),
                Aid(id_db_type_name), Value::from_str("ref"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_type_uint64),
                Aid(id_db_type_name), Value::from_str("uint64"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_type_bytes),
                Aid(id_db_type_name), Value::from_str("bytes"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_db_type_string),
                Aid(id_db_type_name), Value::from_str("string"), // TODO: Long name.
                Tid(id_transaction), Operation::Assert,
            ),
            Tuple::new(
                Eid(id_transaction),
                Aid(id_db_transaction_time), Value::from_u64(0),
                Tid(id_transaction), Operation::Assert,
            ),
        ];

        (builtins, tuples)
    }
}

struct Database {
    eavt: BTreeSet<Eavt>,
    aevt: BTreeSet<Aevt>,
    avet: BTreeSet<Avet>,
    vaet: BTreeSet<Vaet>,
    builtins: Builtins,
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

    pub fn insert(&mut self, tuple: &Tuple) {
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

        let tuple = Tuple {
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
        let min = Tuple::new(entity, attribute, Value::min(), Tid(0), Operation::Retract);
        let max = Tuple::new(entity, attribute, Value::max(), Tid(0), Operation::Retract);
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
}

fn main() {
    let db = Database::new();
    db.debug_print();
}
