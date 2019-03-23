# Trees

Indexes in Noblit are [hitchhiker trees][hitchhiker]. The trees have the same
on-disk format as memory format, which allows them to be memory mapped. A
hitchhiker tree is similar to an immutable B-tree, but updates usually only
allocate a single new node, rather than log<sub>B</sub>(n) nodes. This gives us
the advantages of an immutable data structure, without the write amplification.

[hitchhiker]: https://www.youtube.com/watch?v=jdn617M3-P4

Tree nodes in Noblit are immutable. Index updates produce new nodes that succeed
older nodes. Nodes can become unreachable, but they are never removed.
Eventually, many nodes in a file may not be reachable. In that case the tree can
be copied to a _new_ file, omitting the unreachable blocks. This is a copying
garbage collection.

## Index Trees 

Indexes in Noblit are sorted sets of datoms. Each datom is 32 bytes. (For large
values, the datom contains a reference to a value on the heap.) The indexes
store the datoms themselves: they are sorted sets, not key-values maps. In other
words, indexes are *covering indexes*.

Trees in Noblit are based on B-trees, which means that datoms in the interior
nodes are not repeated in the leaf nodes. (Unlike a B+ tree, which would store
all datoms in leaf nodes, and repeat some in the interior nodes.)

In addition to the midpoint datoms, tree nodes store *pending* datoms: datoms
that need to be flushed into the leaves, but which we avoid as long as possible.
This modification is what makes the tree a hitchhiker tree.

## Disk Format

Tree nodes are 4096 bytes.

 * Byte 4088 through 4095 contains the node header, which is built up of the
   following 8 bytes:
   * Byte 0 contains the depth of the node (0 for a leaf, 1 for its parent, etc.).
   * Byte 1 contains the number of datoms in the node internally, say <var>k</var>.
     <var>k</var> is at most 102.
     TODO: Leaf nodes could store 127 datoms and no child page ids,
     as opposed to 102 midpoints.
   * The remaining 6 bytes are currently not used.
     TODO: I could store a checksum there.
 * At byte 0, the datom array starts. It contains <var>k</var> datoms in
   increasing order.
 * At byte 3264, the child array starts. It contains 64-bit page ids of the
   child nodes. The child array has <var>k</var> + 1 elements, such that all
   datoms in node `children[i]` order before `datoms[i]`. All datoms in node
   `children[i + 1]` order after `datoms[i]`. The special value
   2<sup>64</sup> – 1 indicates that there is no child node in this slot.
   If this is the case for slot <var>i</var>, then datom <var>i</var> is a
   *pending* datom rather than a midpoint, and that datom should be flushed
   to a child node if space runs out in the node.
