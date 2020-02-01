// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Readers and writers for the disk format.
//!
//! While the memory and disk layout of datoms, pages, and values on the value
//! heap, is fixed, the way in which pages and values are provided is abstracted
//! over by `Store` and `Heap`. Pages could come from memory, a file on disk, or
//! from some other backend, possibly over the network.
//!
//! This module handles storage of pages, values, and database metadata on disk.

use std::io;

use binary::{slice_2, slice_8};
use binary::{u16_from_le_bytes, u16_to_le_bytes};
use binary::{u64_from_le_bytes, u64_to_le_bytes};
use database::Database;
use heap;
use head::Head;
use memory_store::{MemoryStore, MemoryHeap};
use store;

/// The magic bytes that indicate a file is a Noblit database.
///
/// The header is inspired by the header of the PNG format:
///
/// * We use a byte with the high bit set to reduce the probability that the
///   file is classified as ASCII. (Not the same byte that PNG uses.)
/// * Then the name of the format (6 characters, instead of 3 for PNG).
/// * Then a carriage return and newline, to catch transmission errors that turn
///   CRLF into LF.
/// * A control-z to stop display on DOS. Probably not relevant nowadays, but it
///   may help to avoid classifying the file as text.
/// * Another newline, to catch transmission errors that turn LF into CRLF.
/// * A null byte to pad to a multiple of 4 (not present in the PNG header).
pub const SIGNATURE: [u8; 12] = *b"\x91Noblit\r\n\x1a\n\0";

/// Write the database as a single stream, in packed format.
///
/// The packed format has the advantage that the entire database is a single
/// file, which makes it easy to share and relocate. The downside of the packed
/// format is that it is read-only. A packed database cannot record new
/// transactions after it has been written.
///
/// The packed format consists of a header, followed by the store file and heap
/// file concatenated.
pub fn write_packed<Store, Heap>(
    db: &Database<Store, Heap>,
    out: &mut dyn io::Write,
    ) -> io::Result<()>
where
    Store: store::Store,
    Heap: heap::SizedHeap,
{
    // Byte [0..12): The signature magic bytes.
    out.write_all(&SIGNATURE[..])?;
    // Byte [12..14): The 16-bit format version.
    out.write_all(&u16_to_le_bytes(1))?;
    // Byte [14..16): The 8-bit format type. 0 indicates the packed format.
    // Then an unused padding byte.
    out.write_all(&[0, 0])?;

    let head = db.get_head();

    // Byte [16..56): The 40-byte head.
    out.write_all(&head.to_bytes()[..])?;

    let max_page = head.max_page();
    let num_pages = 1 + max_page.0;
    let page_size_in_bytes = <Store::Size as store::PageSize>::SIZE as u64;
    let store_len_in_bytes = num_pages * page_size_in_bytes;
    let heap_len_in_bytes = db.get_heap().len();

    // Byte [56..64): The page size, in bytes.
    out.write_all(&u64_to_le_bytes(page_size_in_bytes))?;
    // Byte [64..72): The size of the store, in bytes.
    out.write_all(&u64_to_le_bytes(store_len_in_bytes))?;
    // Byte [72..80): The size of the heap, in bytes.
    out.write_all(&u64_to_le_bytes(heap_len_in_bytes))?;

    // Pad with zeros until we are aligned to the page size. We should be able
    // to use a fixed-size slice here instead of a vec, but unfortunately that
    // results in a compile error about an associated type not being found.
    out.write_all(&vec![0; <Store::Size as store::PageSize>::SIZE - 80])?;

    db.get_store().dump(out)?;
    db.get_heap().dump(out)?;

    Ok(())
}

fn read_exact<R: io::Read>(mut input: R, len: usize) -> io::Result<Vec<u8>> {
    // Note that this makes wasteful copies and does a pointless zero-
    // initialization, but at this point performance is not a concern. We can
    // optimize later if it turns out to be a bottleneck.
    let mut result = Vec::with_capacity(len);
    let mut buffer = vec![0_u8; 4096];
    while result.len() < len {
        let n_left = len - result.len();
        let n_take = buffer.len().min(n_left);
        let n_read = input.read(&mut buffer[..n_take])?;
        result.extend_from_slice(&buffer[..n_read]);

        if n_read == 0 {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "Expected more bytes in read_exact.",
            ));
        }
    }

    Ok(result)
}

/// Read a database from a single stream, in packed format.
///
/// This reads the entire database into memory. It is mostly useful for toy use
/// cases.
///
/// The compile-time page size must match the page size of the file. In practice
/// only page size 4096 should be used; the other page sizes exist only for use
/// in tests and fuzzing, to test deeper trees, and uncover edge cases.
/// Nonetheless, the page size is stored in the file, so we can confirm that it
/// matches.
pub fn read_packed<Size: store::PageSize>(
    mut input: &mut dyn io::Read
) -> io::Result<Database<MemoryStore<Size>, MemoryHeap>> {
    let mut buffer = [0_u8; 16];
    input.read_exact(&mut buffer[..])?;

    // TODO: Add a custom error type, and report an error, instead of panicing.

    // Byte [0..12): The signature magic bytes.
    assert_eq!(&buffer[0..12], &SIGNATURE[..]);
    // Byte [12..14): The 16-bit format version.
    assert_eq!(u16_from_le_bytes(slice_2(&buffer[12..14])), 1);
    // Byte [14..16): The 8-bit format type. 0 indicates the packed format.
    // Then an unused padding byte.
    assert_eq!(buffer[14], 0); // Format type.
    assert_eq!(buffer[15], 0); // Padding.

    // Byte [16..56): The 40-byte head.
    let mut buffer = [0_u8; 40];
    input.read_exact(&mut buffer[..])?;
    let head = Head::from_bytes(&buffer);

    let mut buffer = [0_u8; 24];
    input.read_exact(&mut buffer[..])?;
    // Byte [56..64): The page size, in bytes.
    let page_size_in_bytes = u64_from_le_bytes(slice_8(&buffer[0..8]));
    // Byte [64..72): The size of the store, in bytes.
    let store_size_in_bytes = u64_from_le_bytes(slice_8(&buffer[8..16]));
    // Byte [72..80): The size of the heap, in bytes.
    let heap_size_in_bytes = u64_from_le_bytes(slice_8(&buffer[16..24]));

    assert_eq!(page_size_in_bytes as usize, Size::SIZE);

    // Then we expect to find zero padding, up to the next multiple of the page
    // size.
    let mut buffer = vec![0_u8; Size::SIZE - 80];
    input.read_exact(&mut buffer[..])?;
    assert!(buffer.iter().all(|byte| *byte == 0));

    let store_buffer = read_exact(&mut input, store_size_in_bytes as usize)?;
    let heap_buffer = read_exact(&mut input, heap_size_in_bytes as usize)?;

    let store: MemoryStore<Size> = MemoryStore::from_vec(store_buffer);
    let heap = MemoryHeap::from_vec(heap_buffer);
    let db = Database::open(store, heap, head);
    Ok(db)
}

#[cfg(test)]
mod test {
    use std::io;

    use database::Database;
    use datom::{Datom, Eid, Aid, Tid, Value};
    use disk::{read_packed, write_packed};
    use memory_store::{MemoryStore, MemoryHeap};
    use store::{PageSize, PageSize256, PageSize568, PageSize4096};
    use temp_heap::Temporaries;

    fn empty_database_write_read_packed_roundtrips<Size: PageSize>() {
        // Set up an empty database.
        let store: MemoryStore<Size> = MemoryStore::new();
        let heap = MemoryHeap::new();
        let db0 = Database::new(store, heap).unwrap();

        // Write it to a buffer.
        let mut buffer = Vec::new();
        write_packed(&db0, &mut buffer).unwrap();

        // Read a new in-memory database out of that buffer.
        let db1 = read_packed::<Size>(&mut io::Cursor::new(buffer)).unwrap();

        assert_eq!(db0.get_head(), db1.get_head());

        // Assert that the contents of the EAVT indexes are idential.
        let min = Datom::assert(Eid::min(), Aid::min(), Value::min(), Tid::min());
        let max = Datom::assert(Eid::max(), Aid::max(), Value::max(), Tid::max());

        let view0 = db0.view(Temporaries::new());
        let eavt0 = view0.eavt().into_iter(&min, &max);
        let view1 = db1.view(Temporaries::new());
        let eavt1 = view1.eavt().into_iter(&min, &max);

        for (d0, d1) in eavt0.zip(eavt1) {
            // We compare datoms for bitwise equality. Often this is not what we
            // want, because large values may need to be resolved. But in this
            // case, we saved the database and loaded it back, so we expect a
            // bitwise match.
            assert_eq!(d0, d1);
        }
    }

    #[test]
    fn empty_database_write_read_packed_roundtrips_page_size_256() {
        empty_database_write_read_packed_roundtrips::<PageSize256>();
    }

    #[test]
    fn empty_database_write_read_packed_roundtrips_page_size_568() {
        empty_database_write_read_packed_roundtrips::<PageSize568>();
    }

    #[test]
    fn empty_database_write_read_packed_roundtrips_page_size_4096() {
        empty_database_write_read_packed_roundtrips::<PageSize4096>();
    }
}
