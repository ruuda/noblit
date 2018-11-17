use std::cmp::{PartialOrd, Ord, Ordering};

/// Entity id.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Eid(u64);

/// Attribute id.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Aid(u64);

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

/// Transaction number.
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

/// An (entity, attribute, value, transaction, operation) tuple.
struct Tuple {
    entity: Eid,
    attribute: Aid,
    value: Value,
    transaction_operation: TidOp,
}

impl Tuple {
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

fn main() {
    println!("Hello, world!");
}
