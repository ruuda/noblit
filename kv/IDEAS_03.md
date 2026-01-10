# Noblit KV

(Iteration of IDEAS_02.md)

They key-value store is implemented as a persistent hitchhiker tree. In a
hitchhiker tree, the generational hypothesis presumably holds strongly: most
mutations only cause the root node to be replaced, so these older root versions
die quickly. Therefore we store tree nodes in a storage system managed by a
generational GC.

We want to support efficient sync between instances of the database, by sending
over a range of tree nodes, so data transfer at the page level rather than
key-value level.

## Tree nodes

Tree nodes have a node id, allocated from an ever-increasing counter. Keys and
values are either stored inline when they are small, or referenced by blob id
when they are large (see the Blobs section below).

We store tree nodes in the tree store. Per generation, this is an append-only
file. When the file grows large, we might GC that generation. All of the nodes
that are still live get appended to the next level, and we clear the current
level. In principle we can have an unbounded number of generations, and their
target sizes should be growing exponentially.

Because of the way nodes get promoted through generations, generations partition
the id space. At level `n`, we have id `0..k_n`, at level `n-1`, we have id
`k_n..k_{n-1}`, ..., at level 0, we have id `k_1..k_0`, where `k_0` is the
highest unallocated node id across the system. We expect to have few levels due
to them being exponentially sized. (And it should be with a relatively big
growth factor, maybe 10 rather than 2.0.) So given a node id, we can efficiently
identify the level it's in.

But given the level, how do we know where in the file the node is? That's where
the _index file_ comes in. Aside from an append-only file that stores tree
nodes, we have an append-only file that stores `(node id, offset)`. On this file
we can do binary search (or other search strategies) to find the node offset.

## Blobs

Values too large to be stored inline in tree nodes (which can also be keys, not
just the ‘V’ in ‘KV’), are called “blobs”, and they are stored in the blob
store. Blobs have a blob id, allocated from an ever-increasing counter.

Blob storage works exactly the same as tree node storage, with an append-only
file that stores the blobs, and an append-only index file that stores
`(blob id, offset)`.

## Root log

Because everything is append-only, we can first append any blobs we need to
save, and then any tree nodes that reference those blobs. If anything fails,
existing tree nodes will not be affected. Same for a GC, we can copy over tree
nodes or blobs before we clear out the old generation. But after all of that is
in place, we do need to store *somewhere* what the current id of the root node
is, and maybe also the level boundaries, although those could be inferred. For
this, we have one final append-only file: the _root log_. After committing the
blobs and tree nodes to disk (do we need to fsync first?), we finally append the
new root to the root log, and when that's fsynced, the mutation is committed.

There needs to be some mechanism to prevent the tree log from growing
indefinitely. I haven't figured out the details, maybe we could restart it
after it grows to a certain size or something like that.

The tree log is not the same as the GC roots. It's up to the application to
specify the tree node ids of the nodes that should be GC roots.

## Garbage Collection

Garbage collection can be triggered for the tree store and the blob store
separately, in principle they are independent.

Marking of the lower levels is efficient, because we know that we are only
marking that generation, and we know the bounds of the node ids in that
generation, so when a node references a node outside of the current generation,
we can just stop marking there. For marking the higher levels though, we
potentially need to traverse the entire store. For marking, we could keep a bit
vector with a bit per entry in the index file. We can walk all index files
backwards, from high id to low id (and the levels from low level, which stores
high ids, to higher levels, which store low ids), in a single linear scan, so
the access patterns are predictable.

Possible optimization: after marking, we can walk the bit vector to indentify
spans of live nodes, and then we can use `ficlonerange` to move the data between
generations without causing an excessive amount of writes on the underlying
disk, if it's deemed worth the effort. This comes with alignment requirements
though, which for tree nodes should be fine, but for blobs would be harder
unless we explicitly add padding.

If we encrypt the data, this `ficlonerange` only works if the encryption is tied
to the node id, and not to where the node is stored. (And same for blobs.) This
should be fine with e.g. AES counter mode?

## Syncing

If we have a store that contains data up to blob id `x` and tree node id `y`,
and a different store that is further ahead, we can send over all blobs and
nodes with id greater than `y`. This is just linearly walking the backing files,
we don't need to descend into the trees to figure stuff out, all of this can
happen on the page level. If we know ahead of time that we are going to send a
large amount, we could even trigger a GC on the receiving end before the
transfer, and then directly write the blobs and nodes to the level where we
expect them to land. No point in filling up L0 and then copying everything into
L1, when we already know we will fill up L0; we can write straight into L1.
Collectiong during receive is generally unsafe because we don't yet have the
nodes that are going to refer to the nodes we are writing.

## Implementation

The implementation should be sans-io, with submission and completion queues.
Submission and completion queue entries are requests such as:

 * `Get(root_id, key) -> Entry`
 * `GetRange(root_id, start_key, end_key) -> &[Entry]`
 * `Set(tx_id, key, value)`
 * `Delete(tx_id, key)`
 * `Commit(tx_id) -> Result<RootId>`

To make things happen, we can feed in IO completions, and those may cause new
IO requests (commands). E.g.

 * `OpenFile(path) -> Result<Fp>`
 * `ReadRange(fp, start, end) -> Result<&[u8]>`
 * `WriteRange(fp, start, data) -> Result<()>`
 * `Fsync(fp) -> Result<()>`

This design puts the user in control of driving IO. It fits very naturally with
io_uring, we can truly serve multiple parallel queries in a single syscall.
Testing and fuzzing also becomes trivial, we can have a test impl with a virtual
file system, and we can inject faults etc. For users who do dare to use the
Rust async ecosystem, they can hook it up with their favorite async runtime. For
users on systems without io_uring, we can still use stdlib IO.

Maybe there should be two interfaces then. A low-level queue-based one, and then
on top of it, a more traditional interface implemented with blocking IO (and
maybe also one implemented with uring). After messing for an entire afternoon
with Rust async, this suddenly feels very clear!
