# Architecture

*Vaporware warning: this document is as much of a roadmap, as a description of
what is currently implemented.*

 * A Noblit database is conceptually a set of [datoms](data-model.md).
 * Indexes facilitate queries, by storing the datoms in a particular order.
 * Indexes are [trees](htree.md). Tree nodes are stored in a *store*, and
   identified by a page id.
 * The value of a datom is stored externally, on the *heap*. The heap is a
   bump-pointer backing store for values, and values are identified by their
   offset in the heap.
 * As an optimization, small values can be stored inline in the datom.

## Immutability

Noblit is immutable on two levels:

 * Changing or deleting data is possible on a logical level, but these
   operations produce *new* datoms; datoms are immutable and they are never
   removed. This is similar to Git, where you can delete and change files, but
   changes are recorded as new commits.
 * Indexes are implemented as persistent trees (referring to *persistent data
   structure*, not to *persistent storage*). A tree modification produces a new
   root node, that may reference nodes from the old tree, but nodes are immutable
   once created, and never updated in place.

Backing indexes by persistent trees, enables consistent reads to happen in
parallel with writes. Once you have the root node of an index, that tree will
never change. Reads based on that root will always see the same tree, providing
consistency. Because the tree is immutable, parallel reads are safe, without the
need for coordination. Therefore, Noblit is very suitable for read-heavy
workloads.

The flip side of Noblit’s approach to indexing is that writes need to be
serialized. A write first writes the new tree nodes, up to the new root. A
commit is an atomic swap of the current root page ids. Serializing writes
provides the serializable isolation level by construction, but it eliminates
the possibility of trading isolation for write throughput. Therefore, Noblit is
unsuitable for write-heavy workloads.

## Append-only

Noblit is append-only on two levels:

 * The database is conceptually a set of datoms, and the only supported
   operation is *insert*.
 * The nodes of the index trees are stored in a page store, and new nodes get a
   new page id. Noblit does not reuse ids of unreachable pages.

The current in-memory page store implementation is an append-only vector of
pages, where the page id is simply the index. The plan for the disk-backed page
store is similar: an append-only file of pages, that never reuses pages in the
middle.

Not reusing old pages for new tree nodes has one major advantage: caching
becomes trivial. If you have retrieved the page with a given id previously, then
that data will still be valid. This enables caching at all levels: caching disk
data in memory, but also caching remote data locally. No coordination is
required for caching either: everything is always safe to cache.

An append-only page store has the additional benefit that it enables a
reproducible disk format, making Noblit databases suitable for use in
[reproducible builds](https://reproducible-builds.org/). How data ends up on
disk depends only on writes. If page ids were recycled, there would need to be
a coordination mechanism to determine which pages can be recycled safely, which
might make the output dependent on both reads and writes.

The downsides of an append-only page store are fragmentation and space use.
Eventually, tree nodes will become unreachable, but they still exist in the
index file, where they consume space, and form gaps between reachable nodes.

## Garbage collection

*Vaporware warning: garbage collection has not been implemented.*

The downsides of an append-only page store can be mitigated with a compacting
garbage collection. Traverse the trees, and write all reachable nodes to a *new*
index file. In addition to dropping unreachable nodes, the tree nodes in the new
index file can be ordered such that a tree traversal becomes a sequential scan
through the file, improving disk access patterns.

A garbage collection can happen in parallel with normal operation. Apart from
the required <abbr>IO</abbr> bandwith, writing a new index file does not
interfere with the existing index file, which can continue to be used. If, once
the collection is complete, the compacted trees are no longer the latest trees,
then the missing datoms can be inserted into the new index file too (as a
regular tree insertion), until the two files are in sync, at which point we can
perform new writes against the new index file, and forget about the old file.

TODO: To be able to efficiently get all datoms added since a given transaction
though, we would need a transaction-ordered index, which Noblit currently does
not have. Is it worth adding one for this purpose? Or do we do something like
buffer writes while the collection is in progress?

While garbage collection can mitigate the downsides of an append-only page
store, it comes at a cost:

 * During the collection, extra disk space and <abbr>IO</abbr> bandwidth are
   required.
 * The collection causes write amplification: every datom in the database will
   be written again. While the hitchhiker tree itself ensures a logarithmic
   (in the total number of datoms) number of writes per datom, the rate at which
   nodes become unreachable depends on the transaction rate in addition to the
   total number of datoms, so especially in the case of many small transactions,
   garbage collection may be required often.
 * Coordination is required to know when the old index file can safely be
   removed, after collection is complete.
 * The page id no longer uniquely identifies a tree node. To retain the benefits
   of not reusing old names for new pages, we would need to identify tree nodes
   by the version of the index file, in addition to their page id.

As an alternative to garbage collection, it may be possible to keep most of the
benefits of an append-only page store, while avoiding the wasted space of backing
it by an append-only file, by keeping track of a mutable mapping from (virtual)
page ids to (physical) file offsets. The downside is that coordination is
required to update this mapping, even for reads. Also, while this avoids wasted
space, it does not by itself mitigate fragmentation.
