# Files

*Vaporware warning: much of the content below is hypothetical. Currently Noblit
does not persist anything to disk at all.*

Noblit stores thee kinds of data on disk:

 * [The *heap*](#the-heap) of large values (integers larger than 62 bits, and
   byte strings longer than 7 bytes).
 * [The *indexes*](#the-indexes) that store datoms in sorted order.
 * [The *head*](#the-head) with the most recent tree roots, and id counters.

## Accretion

Because Noblit is an append-only database, transactions only add new datoms.
Datoms are only stored in indexes (the indexes are *covering*), but they may
reference large values on the heap. The indexes are persistent data structures,
in the sense that data is immutable once written, but we can construct a new
index that shares most of its nodes with a previous version. Indexes are trees
that consist of nodes. The index file is an append-only collection of nodes. The
head points to the latest roots of the index trees, and it stores the counters
for allocating entity ids. The head is the only part of Noblit that is updated
in place, the other files are append-only.

Committing a transaction is a three-stage process:

 1. Append any new large values to the heap file, and sync the heap.
 2. Add datoms to the indexes. This produces one or more new tree nodes per
    index. Append the new nodes to the index file, and sync it.
 3. Write the new head, and sync it.

By making the head update atomic, the entire transaction becomes atomic. If the
commit fails at some point before the new head is written, the old head is still
valid, and it points to valid data. New data may have been appended, but that
data is not yet referenced.

## The Indexes

The indexes in Noblit store datoms in sorted order. There are three indexes:

 * **Eavt**: sorted by entity, attribute, value, and transaction.
 * **Aevt**: sorted by attribute, entity, value, and transaction.
 * **Avet**: sorted by attribute, value, entity, and transaction.

See also [Datomic's index documentation][datomic-indexes], which formed the
inspiration for Noblit. Note that unlike Datomic, Noblit does not have a Vaet
(value, attribute, entity, transaction) index. This is because attributes in
Noblit are strongly typed. A query such as “list all attributes that this entity
has” is impossible to express in Noblit, because the values associated with the
attribute may not have the same type.

Each index stores all datoms in full. A datom is 32 bytes. Small values are
stored inline in the datom, large values (integers larger than 62 bits and byte
strings longer than 7 bytes) are stored on the value heap, with the datom
containing the index into the value heap.

[datomic-indexes]: https://docs.datomic.com/cloud/query/raw-index-access.html

Noblit stores indexes as [hitchhiker trees][htree], a variation on immutable
B-trees which reduces write amplification. Noblit accumulates new datoms in
memory first. At the end of a transaction it flushes those at once to the disk
as new tree nodes, which may share child nodes with the previous tree. To
prevent unreachable nodes from accumulating, trees need to be compacted
occasionally though a copying garbage collection process. Unreachable nodes
are not recycled to ensure immutability of written files: new data is only ever
appended at the end. This simplifies caching.

[htree]: htree.md

## The Head

For every index, the head stores the page id of the root of the tree for that
index. Furthermore, the head stores the counters for id allocation.

TODO: The head should store the size of the index file and heap file, so that it
can truncate them after a failed transaction.

## The Heap

The heap is a file that contains *large values*: integers larger than 62 bytes
and byte strings longer than 7 bytes. Because Noblit is an append-only database
that never removes data, storing a value on the heap is a simple bump-pointer
allocation; the heap only grows.

The heap stores two kinds of values:

 * 64-bit integers which don't fit into 62 bytes. They are stored as-is, in 8
   bytes, big endian. (TODO: It should be little endian.)
 * Byte strings longer than 7 bytes. Note that e.g. strings are also stored as
   byte strings; it is the schema that specifies that those bytes should be
   interpreted as a <abbr>UTF-8</abbr> encoded string. Byte strings are
   length-prefixed with a 64-bit length. The address of a byte string is the
   offset of its length prefix, so its data can be found at the offset 8 bytes
   higher.

Values on the value heap are aligned to 8 bytes.

The heap is not checksummed. (Nor the indexes for that matter.) If you do not
trust your storage medium, use a file system or virtual block device that can
detect and report integrity problems.

The value heap might store duplicates. Because values are immutable once stored,
deduplication is safe. If a Datom contains a value that already exists on the
heap, it is safe to reference the existing value, rather than storing it again
on the heap. Noblit may do this, but identifying duplicates is not free, hence
Noblit may store the same value twice.

TODO: Persist a hash table of values too, to identify duplicates? Not really,
can read heap at startup, although that does not scale, persistent hash table
may be needed.
