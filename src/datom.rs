// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! This module defines the datom, the basic unit of information.

use std;

/// Entity id.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
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
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
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
/// TODO: Don't expose internals, add a constructor that validates the tid is
/// even.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Tid(pub u64); // TODO: Non-pub field?

/// Transaction number and operation.
///
/// A packed representation of an even transaction number and an operation. The
/// transaction number can be obtained by zeroing the least significant bit, and
/// the least significant bit indicates the operation:
///
/// * 0 indicates a retraction.
/// * 1 indicates an assertion.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct TidOp(u64);

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
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
pub struct Value(pub u64); // TODO: Non-public field?

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
pub struct Tuple {
    pub entity: Eid,
    pub attribute: Aid,
    pub value: Value,
    pub transaction_operation: TidOp,
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

