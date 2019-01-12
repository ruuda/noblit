# Disk Format

Noblit stores thee kinds of data on disk:

 * [The *journal*](#the-journal) (sometimes called *write-ahead log*) of new datoms.
 * [The *indexes*](#the-indexes) that store datoms in sorted order.
 * [The *heap*](#the-heap) of large values (integers larger than 62 bits, and
   byte strings longer than 7 bytes).

## The Journal

Because Noblit is an append-only database, transactions only add new datoms.
These new datoms are appended to the journal. Committing a transaction is a
three-stage process:

 1. Append any new large values to the heap file, and sync the heap.
 2. Append new datoms at the end of the journal, and sync the journal.
 3. Update the journal size in the journal header, and sync the header.

The final header update ensures commits are atomic. When replaying the journal,
only as many datoms as the header indicates are restored. If the header was not
updated, the journal will have trailing data that is discarded. The heap may
have trailing data as well. If the header was updated, then all referenced
datoms will have been stored.

## The Indexes

The indexes in Noblit store datoms in sorted order. There are four indexes:

 * **Eavt**: sorted by entity, attribute, value, and transaction.
 * **Aevt**: sorted by attribute, entity, value, and transaction.
 * **Avet**: sorted by attribute, value, entity, and transaction.
 * **Vaet**: sorted by value, attribute, entity, and transaction.

See also [Datomic's index documentation][datomic-indexes], which formed the
inspiration for Noblit.

Each index stores all datoms in full. A datom is 32 bytes. Small values are
stored inline in the datom, large values (integers larger than 62 bits and byte
strings longer than 7 bytes) are stored on the value heap, with the datom
containing the index into the value heap.

[datomic-indexes]: https://docs.datomic.com/cloud/query/raw-index-access.html

Noblit stores indexes as [hitchhiker trees][htree], a variation on immutable
B-trees which reduces write amplification. Noblit accumulates new datoms in
memory first. When there is enough new data, it flushes those at once to the
disk as new tree nodes, which may share child nodes with the previous tree. To
prevent unreachable nodes from accumulating, trees need to be compacted
occasionally though a copying garbage collection process. Unreachable nodes
are not recycled to ensure immutability of written files: new data is only ever
appended at the end. This simplifies caching.

[htree]: htree.md

## The Heap

The heap is a file that contains *large values*: integers larger than 62 bytes
and byte strings longer than 7 bytes. Because Noblit is an append-only database
that never removes data, storing a value on the heap is a simple bump-pointer
allocation; the heap only grows.

The heap stores two kinds of values:

 * 64-bit integers which don't fit into 62 bytes. They are stored as-is, in 8
   bytes.
   TODO: Elaborate on endinanness.
 * Byte strings longer than 7 bytes. Note that e.g. strings are also stored as
   byte strings; it is the schema that specifies that those bytes should be
   interpreted as a UTF-8-encoded string. Byte strings are length-prefixed with
   a 32-bit length. (If you want to store values larger than 4 GiB, Noblit is
   probably not the best tool for the job. A key-value store, blob store, or
   file system might be more suitable.) The address of a byte string is the
   offset of the data, so its length can be found at the offset 4 bytes lower.

Values on the value heap are aligned to 8 bytes.

TODO: Do I want some kind of chunking with checksums? Validate integrity
somewhere?

The value heap might store duplicates. Because values are immutable once stored,
deduplication is safe. If a Datom contains a value that already exists on the
heap, it is safe to reference the existing value, rather than storing it again
on the heap. Noblit may do this, but identifying duplicates is not free, hence
Noblit may store the same value twice.

TODO: Persist a hash table of values too, to identify duplicates?
