// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines the datom, the basic unit of information.

use std;
use std::cmp::Ordering;

use pool::{CidInt, CidBytes, Pool};

/// Entity id.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Eid(pub u64); // TODO: non-pub field?

impl Eid {
    pub fn min() -> Eid {
        Eid(0)
    }

    pub fn max() -> Eid {
        Eid(std::u64::MAX)
    }
}

/// Attribute id.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Aid(pub u64); // TODO: Non-pub field?

impl Aid {
    pub fn min() -> Aid {
        Aid(0)
    }

    pub fn max() -> Aid {
        Aid(std::u64::MAX)
    }
}

/// Transaction number.
// TODO: Don't expose internals, add a constructor that validates the tid is
// even.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Tid(pub u64); // TODO: Non-pub field?

impl Tid {
    pub fn min() -> Tid {
        Tid(0)
    }

    pub fn max() -> Tid {
        // Transaction ids must be even.
        Tid(std::u64::MAX - 1)
    }
}

/// Transaction number and operation.
///
/// A packed representation of an even transaction number and an operation. The
/// transaction number can be obtained by zeroing the least significant bit, and
/// the least significant bit indicates the operation:
///
/// * 0 indicates a retraction.
/// * 1 indicates an assertion.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TidOp(u64);

/// An assertion or a retraction.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Operation {
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

/// A value: byte string or integer.
///
/// A value is either a byte string, or an unsigned 64-bit integer. It is up to
/// the attribute schema to give meaning to a value. For example, a byte string
/// might store utf-8 text, and an integer might hold an entity id.
///
/// The most significant two bits contain the type and representation tag:
///
/// * 00: An unsigned 62-bit integer stored inline. This means that all unsigned
///       integers less than 2<sup>62</sup> are represented as themselves.
/// * 01: An unsigned 64-bit integer stored externally. The remaining 62 bits
///       indicate its storage address.
/// * 10: A small string. The next 6 most significant bits indicate the length,
///       although the maximum valid length is 7. The next 7 bytes contain the
///       string, padded with zeros. This means that the empty string is
///       represented as `0x8000_0000_0000_0000`.
/// * 11: A string stored externally. The remaining 62 bits indicate its storage
///       address. The special addres `0xfff_ffff_ffff_ffff` indicates the
///       maximal value.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct Value(pub u64); // TODO: Non-public field?

impl Value {
    /// Only "external" bit set.
    const TAG_EXTERNAL: u64 = 0x4000_0000_0000_0000;
    /// Only "bytestring" bit set (unset indicates "64-bit int");
    const TAG_BYTES: u64 = 0x8000_0000_0000_0000;
    /// Both tag bits set.
    const TAG_MASK: u64 = Value::TAG_EXTERNAL | Value::TAG_BYTES;

    /// Return whether this value is the sentinel max value.
    pub fn is_max(&self) -> bool {
        self.0 == 0xffff_ffff_ffff_ffff
    }

    /// Return whether the value type is u64, as opposed to a byte string.
    pub fn is_u64(&self) -> bool {
        self.0 & Value::TAG_BYTES == 0
    }

    /// Return whether the value type is a byte string, as opposed to u64.
    pub fn is_bytes(&self) -> bool {
        debug_assert!(!self.is_max(), "Should test for max before testing type.");
        self.0 & Value::TAG_BYTES != 0
    }

    /// Return whether the value is the offset of an externally stored value.
    pub fn is_external(&self) -> bool {
        debug_assert!(!self.is_max(), "Should test for max before testing external.");
        self.0 & Value::TAG_EXTERNAL != 0
    }

    /// Return whether the value is the index of an external temporary.
    ///
    /// Temporaries are used to refer to large values that occur in queries, but
    /// which are not necessarily in the database already. They are stored on
    /// the `StackPool`.
    pub fn is_temporary(&self) -> bool {
        // External values are aligned to 8 bytes. Non-aligned values indicate
        // temporaries.
        self.is_external() && (self.0 & 7 != 0)
    }

    /// Construct an integer value from a 62-bit unsigned integer.
    ///
    /// Panics if the value is too large. Use `from_u64` to handle large values.
    pub fn from_u64_inline(value: u64) -> Value {
        assert_eq!(value & Value::TAG_MASK, 0, "Value is too large to be inlined.");
        Value(value)
    }

    /// Construct an integer value from an unsigned integer.
    ///
    /// Returns `None` if the value is too large to be stored inline. In that
    /// case, store the value in a pool and use `from_const_u64`.
    pub fn from_u64(value: u64) -> Option<Value> {
        if value < Value::TAG_MASK {
            Some(Value::from_u64_inline(value))
        } else {
            None
        }
    }

    pub fn from_bool(value: bool) -> Value {
        match value {
            false => Value(0),
            true => Value(1),
        }
    }

    pub fn from_eid(value: Eid) -> Value {
        // TODO: Do not assume that entity ids fit in 62 bits, handle spilling.
        Value::from_u64_inline(value.0)
    }

    /// Construct a byte string value from a 7-byte slice or shorter.
    ///
    /// Panics if the value is too large. Use `from_bytes` to handle large values.
    pub fn from_bytes_inline(value: &[u8]) -> Value {
        assert!(value.len() < 8, "Byte string is too long to be inlined.");
        let mut bytes = [0_u8; 7];
        bytes[..value.len()].copy_from_slice(value);
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

    /// Construct a byte string value from a byte slice.
    ///
    /// Returns `None` if the value is too large to be stored inline. In that
    /// case, store the value in a pool and use `from_const_bytes`.
    pub fn from_bytes(value: &[u8]) -> Option<Value> {
        if value.len() < 8 {
            Some(Value::from_bytes_inline(value))
        } else {
            None
        }
    }

    /// Construct a byte string value from a 7-byte string slice or shorter.
    pub fn from_str_inline(value: &str) -> Value {
        Value::from_bytes_inline(value.as_bytes())
    }

    /// Construct a byte string value from string slice.
    pub fn from_str(value: &str) -> Option<Value> {
        Value::from_bytes(value.as_bytes())
    }

    pub fn from_const_u64(cid: CidInt) -> Value {
        let offset = cid.0;
        assert_eq!(offset & Value::TAG_MASK, 0, "Const id must fit in 62 bits.");
        Value(offset | Value::TAG_EXTERNAL)
    }

    pub fn from_const_bytes(cid: CidBytes) -> Value {
        let offset = cid.0;
        assert_eq!(offset & Value::TAG_MASK, 0, "Const id must fit in 62 bits.");
        Value(offset | Value::TAG_EXTERNAL | Value::TAG_BYTES)
    }

    pub fn as_bytes<'a, P: Pool + ?Sized>(&'a self, pool: &'a P) -> &'a [u8] {
        use std::mem;
        debug_assert!(self.is_bytes(), "Value must be byte string for as_bytes.");
        if self.is_external() {
            pool.get_bytes(self.as_const_bytes())
        } else {
            let bytes: &[u8; 8] = unsafe { mem::transmute(&self.0) };
            let len = (bytes[7] & 0x3f) as usize;
            debug_assert!(len <= 7);
            &bytes[..len]
        }
    }

    pub fn as_str<'a, P: Pool + ?Sized>(&'a self, pool: &'a P) -> &'a str {
        std::str::from_utf8(self.as_bytes(pool)).expect("Should only store UTF-8 strings.")
    }

    pub fn as_u64<P: Pool + ?Sized>(&self, pool: &P) -> u64 {
        debug_assert!(self.is_u64(), "Value must be int for as_u64.");
        if self.is_external() {
            pool.get_u64(self.as_const_u64())
        } else {
            self.0
        }
    }

    pub fn as_eid<P: Pool + ?Sized>(&self, pool: &P) -> Eid {
        Eid(self.as_u64(pool))
    }

    pub fn as_bool(&self) -> bool {
        debug_assert!(self.is_u64(), "Value must be int for as_bool.");
        debug_assert!(!self.is_external(), "Value must be inline for as_bool.");
        match self.0 {
            0 => false,
            1 => true,
            _ => unreachable!("Bool values should be either 0 or 1."),
        }
    }

    pub fn as_const_u64(&self) -> CidInt {
        debug_assert!(self.is_u64(), "Value must be int for as_const_u64.");
        debug_assert!(self.is_external(), "Value must be external for as_const_u64.");
        CidInt(self.0 & !Value::TAG_MASK)
    }

    pub fn as_const_bytes(&self) -> CidBytes {
        debug_assert!(self.is_u64(), "Value must be int for as_const_bytes.");
        debug_assert!(self.is_external(), "Value must be external for as_const_bytes.");
        CidBytes(self.0 & !Value::TAG_MASK)
    }

    pub fn min() -> Value {
        Value(0)
    }

    pub fn max() -> Value {
        Value(0xffff_ffff_ffff_ffff)
    }

    pub fn cmp<P: Pool + ?Sized>(&self, other: &Value, pool: &P) -> Ordering {
        // Integers sort before byte strings, so if the types differ, we are
        // done. If the types match, then do a type-based comparison.
        match (self.is_u64(), other.is_u64()) {
            (true, true) => self.as_u64(pool).cmp(&other.as_u64(pool)),
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
            // If the value is not an int, it could be bytes (internal or
            // external), or it could be the special marker for max.
            (false, false) => match (self.is_max(), other.is_max()) {
                (false, true) => Ordering::Less,
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Greater,
                (false, false) => self.as_bytes(pool).cmp(other.as_bytes(pool)),
            }
        }
    }
}

/// An (entity, attribute, value, transaction, operation) tuple.
// TODO: Make copy explicit?
// TODO: Proper debug impl, and something to compare in tests that is not Eq.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Datom {
    pub entity: Eid,
    pub attribute: Aid,
    pub value: Value,
    pub transaction_operation: TidOp,
}

impl Datom {
    pub fn new(entity: Eid, attribute: Aid, value: Value, transaction: Tid, operation: Operation) -> Datom {
        Datom {
            entity: entity,
            attribute: attribute,
            value: value,
            transaction_operation: TidOp::new(transaction, operation),
        }
    }

    /// Shorthand for `new` with operation `Assert`.
    pub fn assert(entity: Eid, attribute: Aid, value: Value, transaction: Tid) -> Datom {
        Datom::new(entity, attribute, value, transaction, Operation::Assert)
    }

    /// Shorthand for `new` with operation `Retract`.
    pub fn retract(entity: Eid, attribute: Aid, value: Value, transaction: Tid) -> Datom {
        Datom::new(entity, attribute, value, transaction, Operation::Retract)
    }
}

impl std::fmt::Debug for Datom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let operation = match self.transaction_operation.operation() {
            Operation::Assert => "assert",
            Operation::Retract => "retract",
        };
        write!(
            f,
            "{} ({}, {}, {:?}) at {}",
            operation,
            self.entity.0,
            self.attribute.0,
            self.value,
            self.transaction_operation.transaction().0,
        )
    }
}

#[cfg(test)]
mod test {
    use std::cmp::Ordering;
    use std::u64;

    use memory_store::MemoryPool;
    use pool::PoolMut;
    use super::Value;

    #[test]
    fn cmp_works_on_small_uint64s() {
        let numbers = [0, 1, 2, 3, 5, 7, 128, 4096, u64::MAX >> 2];
        let pool = MemoryPool::new();
        for &i in &numbers {
            let v_i = Value::from_u64(i);
            for &j in &numbers {
                let v_j = Value::from_u64(j);
                assert_eq!(v_i.cmp(&v_j, &pool), i.cmp(&j), "{} cmp {}", i, j);
            }

            // Also abuse this test to enforce that no value is greater than max.
            assert_eq!(v_i.cmp(&Value::max(), &pool), Ordering::Less);
        }
    }

    #[test]
    fn cmp_works_on_large_uint64s() {
        let numbers = [u64::MAX, u64::MAX - 2, u64::MAX / 2, u64::MAX - 7];
        let mut pool = MemoryPool::new();
        for &i in &numbers {
            let cid_i = pool.append_u64(i).unwrap();
            let v_i = Value::from_const_u64(cid_i);
            for &j in &numbers {
                let cid_j = pool.append_u64(j).unwrap();
                let v_j = Value::from_const_u64(cid_j);
                assert_eq!(v_i.cmp(&v_j, &pool), i.cmp(&j), "{} cmp {}", i, j);
            }

            // Also abuse this test to enforce that no value is greater than max.
            assert_eq!(v_i.cmp(&Value::max(), &pool), Ordering::Less);
        }
    }

    #[test]
    fn cmp_works_on_mixed_uint64s() {
        let numbers = [0, u64::MAX >> 2, 5, u64::MAX >> 1, 7, u64::MAX];
        let mut pool = MemoryPool::new();
        for &i in &numbers {
            let v_i = match i {
                0...0x3fff_ffff_ffff_ffff => Value::from_u64(i),
                _ => Value::from_const_u64(pool.append_u64(i).unwrap()),
            };
            for &j in &numbers {
                let v_j = match j {
                    0...0x3fff_ffff_ffff_ffff => Value::from_u64(j),
                    _ => Value::from_const_u64(pool.append_u64(j).unwrap()),
                };
                assert_eq!(v_i.cmp(&v_j, &pool), i.cmp(&j), "{} cmp {}", i, j);
            }
        }
    }

    #[test]
    fn cmp_works_on_small_bytes_strings() {
        let values = [
            "",
            "aaaaaaa",
            "aaaaa",
            "abcdef",
            "zzz",
        ];
        let pool = MemoryPool::new();
        for &i in &values {
            let v_i = Value::from_str(i);
            for &j in &values {
                let v_j = Value::from_str(j);
                assert_eq!(v_i.cmp(&v_j, &pool), i.cmp(&j), "{} cmp {}", i, j);
            }

            // Also abuse this test to enforce that no value is greater than max.
            assert_eq!(v_i.cmp(&Value::max(), &pool), Ordering::Less);
        }
    }

    #[test]
    fn cmp_works_on_large_byte_strings() {
        let values = [
            "aaaaaaaaaaa",
            "01234567",
            "zzzzzzzz",
        ];
        let mut pool = MemoryPool::new();
        for &i in &values {
            let cid_i = pool.append_bytes(i.as_bytes()).unwrap();
            let v_i = Value::from_const_bytes(cid_i);
            for &j in &values {
                let cid_j = pool.append_bytes(j.as_bytes()).unwrap();
                let v_j = Value::from_const_bytes(cid_j);
                assert_eq!(v_i.cmp(&v_j, &pool), i.cmp(&j), "{} cmp {}", i, j);
            }

            // Also abuse this test to enforce that no value is greater than max.
            assert_eq!(v_i.cmp(&Value::max(), &pool), Ordering::Less);
        }
    }

    #[test]
    fn cmp_works_on_mixed_byte_strings() {
        let values = [
            "aaaaaaaaaaa",
            "",
            "01234567",
            "pqr",
            "zzzzzzzz",
        ];
        let mut pool = MemoryPool::new();
        for &i in &values {
            let v_i = match i.len() {
                0...7 => Value::from_str(i),
                _ => Value::from_const_bytes(pool.append_bytes(i.as_bytes()).unwrap()),
            };
            for &j in &values {
                let v_j = match j.len() {
                    0...7 => Value::from_str(j),
                    _ => Value::from_const_bytes(pool.append_bytes(j.as_bytes()).unwrap()),
                };
                assert_eq!(v_i.cmp(&v_j, &pool), i.cmp(&j), "{} cmp {}", i, j);
            }
        }
    }
}
