# Execution

This page gives a high-level overview of how queries are executed. Rust’s
borrowing rules enforce a rigid structure on the ordering of operations. The
components involved are:

 * **Indexes**: The index trees and their backing storage. Because indexes are
   persistent data structures, it would be possible to have multiple readers and
   a single writer simultaneously. However, the current implementation requires
   exclusive access for writes, as this is easier to express in Rust.
 * **The heap**: Large values live on the large value heap. Like indexes it is
   append-only, so it would admit multiple readers and a single writer
   simultaneously, but the current implementation requires exclusive access.
 * **The temporary heap**: Query evaluation requires all large values to live on
   a heap. Large values that occur in queries are placed on a temporary heap.
   The temporary heap is a layer on top of the persistent large value heap:
   lookups for non-temporaries fall through to the underlying heap.

## Queries

 1. Create a temporary heap.
 2. Parse the query. Place values on the temporary heap.
 3. Acquire persistent indexes and heap (read-only).
 4. Evaluate the query.
 5. Release persistent indexes and heap (read-only).
 6. Drop the temporaries.

## Mutations

  1. Create a temporary heap. (TODO: Create two, one for reads, one for writes.)
  2. Parse the query.
     Place values that occur in the query part on the reads temporary heap.
     Place values that occur in assertions on the writes temporary heap.
  3. Acquire persistent indexes and heap (read-only).
  4. Evaluate the query part (if any, otherwise use unit).
     For every result, collect new datoms.
  5. Release persistent indexes and heap (read-only).
  6. Persist any temporaries referenced by new datoms.
     This requires write access to the persistent heap.
  7. Acquire the persistent heap, now containing necessary values (read-only).
  8. Persist new datoms.
     This requires write access to the persistent indexes.
  9. Release the persistent heap (read-only).
