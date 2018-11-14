/// Entity id.
struct Eid(u64);

/// Attribute id.
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
struct Value(u64);

/// Transaction number.
struct Tid(u64);

/// An (entity, attribute, value, transaction, operation) tuple.
struct Tuple {
    entity: Eid,
    attribute: Aid,
    value: Value,
    transaction: Tid,
    // TODO: Pack the bit somewhere, so `Tuple` is 32 bytes and two of them fit
    // in a cache line. On the other hand, it might not matter in a tree
    // structure, perhaps we can make it pack anyway.
    operation: bool,
}

fn main() {
    println!("Hello, world!");
}
