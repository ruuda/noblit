# Distribution

Noblit is a modular database library. It consists of several components:

 * The page store and heap, which store index tree nodes and large datom values.
 * A query engine, that requires read-only access to the store and heap.
 * A mutator responsible for processing transactions. The mutator requires write
   access to the store and heap.

These components are part of a single native dynamic library that you can link
into your application. The store and the heap are an interface (*trait* in Rust)
with several implementations. Noblit features an in-memory store and heap, and
a file-backed store and heap. The modular approach allows Noblit to be used in
a few different ways.

*Vaporware warning: the following paragraphs about distributed operation are an
idea for the distant future. While Noblit is designed with distributed operation
in mind, none of it has been implemented.*

## Local operation

With its memory-backed and file-backed store, Noblit is similar to [SQLite][sqlite]:
an embeddable native library for interacting with an in-memory database, or a
local file-backed database.

[sqlite]:        https://sqlite.org/index.html

## Deconstructing the database

On top of Noblit the library, we can build a daemon that exposes query and
mutations over the network. Unlike traditional relational databases, the server
would not need to be a monolith. Like [Datomic][datomic], Noblit could be
deployed as a [deconstructed database][deconstructed], where storage, query
processing, and mutation, do not need to reside in the same place.

Query processing can still happen at clients through the embedded library,
like it does in local operation. This is similar to Datomic’s peer library.
Alternatively, query processing can happen at a dedicated server, in the
traditional client-server model. Either way, decoupling query from storage
allows scaling reads horizontally.

[datomic]:       https://www.datomic.com/
[deconstructed]: https://www.infoq.com/presentations/Deconstructing-Database/

## Distributed reads

Query processing requires an implementation of a read-only store and heap.
Because these store immutable data, they can be scaled easily.

 * The store and heap may be backed by a remote service, rather than by a local
   file.
 * Because everything is immutable, all data can be cached safely at every
   level.
 * A multi-level cache is possible. Mix and match a local memory cache, a
   distributed memory cache (like Memcached), a disk cache backed by fast but
   volatile local <abbr>SSD</abbr>s, and a cache backed by slow but stable
   storage.
 * Check peers first, and only hit the main source of truth (the store and heap
   that the mutator writes to) if the page is available nowhere else. This
   unloads the mutator, so it can devote all its resources to transaction
   processing.

When clients execute a query, they need to specify the revision of the database
to execute it agaist. The current latest revision is the only piece of mutable
state in Noblit, and the mutator is responsible for managing it. Committing a
transaction causes that transaction to become the new latest revision. Often,
clients want to execute a query against the lastest revision of the database. To
get the latest revision, they would need to query the mutator, making the
mutator contended again, even for reads. Obtaining the latest revision is
inherently racy: by the time the client receives the response, it may already be
outdated. Fortunately, this is often not a problem; clients do not want the
absolute latest revision, just a sufficiently recent revision. This means that
replicas can also service the “get latest” query, as long as they do not lag too
far behind.

Together, this means that query capacity can be scaled virtually without limits.
The cost of adding a read replica — the bandwith of reading pages that have not
yet been cached locally from its peers — can be amortized over all existing peers,
so it goes down per added replica.
