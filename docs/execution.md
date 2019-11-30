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

 1. Acquire indexes (read-only).
 2. Acquire the heap (read-only).
 3. Create a temporary heap on top of the persistent one.
 4. Parse the query. Place values on the temporary heap.
 5. Evaluate the query.
 6. Drop the temporary heap.
 7. Release the heap (read-only).
 8. Release indexes (read-only).

## Mutations

  1. Acquire indexes (read-only).
  2. Acquire the heap (read-only).
  3. Create two temporary heaps: one for reads, one for writes.
  4. Parse the query.
     Place values that occur in the query part on the reads temporary heap.
     Place values that occur in assertions on the writes temporary heap.
  5. Evaluate the query part (if any, otherwise use unit).
     For every result, collect new datoms.
  6. Drop the reads temporary heap.
  7. Drop the writes temporary heap, but keep the values.
  8. Release the persistent heap (read-only).
  9. Acquire the persistent heap (read-write).
 10. Persist any temporaries referenced by new datoms.
 11. Release the persistent heap (read-write).
 12. Release indexes (read-only).
 13. Acquire indexes (read-write).
 14. Acquire the persistent heap, now containing necessary values (read-only).
 15. Persist new datoms.
 16. Release the persistent heap (read-only).
 17. Release indexes (read-write).
