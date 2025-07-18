# Noblit KV

(Evolution in IDEAS.md)

The key-value store is implemented as a persistent hitchhiker tree. This
means means that we never update things in place, so we can make all writes
append-only. This does mean that we produce garbage. Especially in a hitchhiker
tree, the generational hypothesis holds strongly: updates usually only produce
a new root node, until it overflows, so most nodes get superseded quickly.

We can leverage this generational property and append-only nature of the writes,
by writing to generation 0 of a copying, generational GC. As soon as the
generation reaches some size threshold, we append the live nodes to generation
1, and we restart generation 0. We can do this for multiple generations,
exponentially sized, so limit write amplification.

The tree nodes of the hitchhiker tree have a fixed size. We store the nodes in
a _nodes file_, of which there is one per generation. Keys and values are
bytestrings. Those are stored in a _data file_, of which there is also one per
generation. Nodes in generation _i_ can reference bytestrings in generation _i_
simply by offset. When we do a copying collection, we copy both the live tree
nodes and the values at the same time, into the next generation.

We have the following invariants:

 * Tree node _k_ can only reference nodes < _k_.
 * Nodes in generation _i_ can only reference nodes in generation >= _i_.
 * Nodes inside a generation are stored by ascending node id.

This means that it’s sufficient to track a max id per generation. This divides
the id range into spans. Then we can tell what generation a node is in, by
walking this list of spans. (We could even binary search, but we only have
_O_(log _n_) generations anyway, where _n_ is the number of nodes.)

Aside from the nodes file and data file, we should store an index file per
generation, that stores just the node ids. We can probably keep these indexes
entirely in memory, but it means that we don't need to scan the full database
to reconstruct them at startup, and it means we don't need to store node ids
inline in the node, though maybe we want to do that anyway?
