# Trees

Indexes in Noblit are [hitchhiker trees][hitchhiker]. The trees have the same
on-disk format as memory format, which allows them to be memory mapped. A
hitchhiker tree is similar to an immutable B-tree, but updates usually only
allocate a single new node, rather than log<sub>B</sub>(n) nodes. This gives us
the advantages of an immutable data structure, without the write amplification.

[hitchhiker]: https://www.youtube.com/watch?v=jdn617M3-P4

Tree nodes in Noblit are immutable. Index updates produce new nodes that succeed
older nodes. Nodes can become unreachable, but they are never removed.

TODO: Do I need a garbage collector? Breaks the can-always-cache-everything
property, although not too badly (just get a new index). Could maybe use sparse
files to cut out parts of a file?

## Index Trees 

Indexes in Noblit are sorted sets of datoms. Each dataom is 32 bytes. (For large
values, the datom contains a reference to a value on the heap.) The indexes
store the datoms themselves: they are sorted sets, not key-values maps. In other
words, indexes are *covering indexes*.

Trees in Noblit are based on B-trees, which means that datoms in the interior
nodes are *not* repeated in the leaf nodes. (Unlike a B+ tree, which would store
all datoms in leaf nodes, and repeat some in the interior nodes.)

In addition to the midpoint datoms, tree nodes store *pending* datoms: datoms
that need to be flushed into the leaves, but which we avoid as long as possible.
This modification is what makes the tree a hitchhiker tree.

## Disk Format

Tree nodes are 4096 bytes.

 * Byte 0 contains the depth of the node (0 for a leaf, 1 for its parent, etc.).
 * Byte 1 contains the number of child nodes, say <var>k</var>.
 * Byte 2 contains the number of pending values, say <var>p</var>.
 * Bytes 3 through 31 are currently not used.
   TODO: I could store a checksum there.
 * At byte 32, the datom array starts. It contains <var>k</var> - 1 +
   <var>p</var> datoms: <var>k</var> midpoints that separate the child nodes,
   and <var>p</var> pending values.
 * At byte 4096 - 8<var>k</var>, the child array starts. It contains 64-bit
   pointers to the child nodes. TODO: Page index, or page byte offset?

The child datoms and pending datoms are both sorted. The child midpoints and
pending values compete for space in the node. This is not a problem, because
the kind of update determines where to add new datoms:

 * For inserts, we insert datoms as pending datoms if possible. If this would
   overflow the node, then the datoms to be inserted, as well as the pending
   datoms in the node, are all flushed.
 * For splits, we add a midpoint to the parent.

TODO: But then there is no upper bound on midpoints, and we might be left with
no space for pending datoms.

A leaf stores all of its datoms as pending datoms, and it fits 127 datoms.
A fully-loaded interior node has 102 children and stores 101 midpoint datoms.
