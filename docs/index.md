# Noblit

*Vaporware warning: much of the content below is hypothetical.*

Noblit is an embeddable append-only database. The database is a set of immutable
(entity, attribute, value, time) tuples called *datoms*. Datoms can be asserted
and retracted. A retraction is recorded as a new fact; it is not a delete. Any
historical state of the database can be reproduced, simply by ignoring new
datoms. Furthermore, Noblit makes history first-class and queryable.

## Features

 * **Simple but flexible data model.**
   Noblit stores (entity, attribute, value) tuples. A traditional relational
   model can be expressed like this, but less rigid structures such as graphs
   can be expressed too.
 * **Machine-friendly relational queries.** Queries are data structures rather
   than strings. There is no need for string formatting or escaping, and the
   full power of the host language is available to safely generate and compose
   queries.
 * **Point in time queries.**
   Any historical state of the database can be reproduced and queried
   efficiently.
 * **Reified schema**.
   Noblit stores the schema as ordinary datoms in the database itself.
   The schema can be evolved through normal assertions and retractions,
   and the full migration history is available.
 * **Reified transactions**.
   Transactions in Noblit are entities that can have attributes like any other
   entity. Possibilities include the transaction timestamp and the user who
   initiated the transaction. Transactions can inspected in queries.

## Comparison

Noblit is heavily inspired by [Datomic][datomic], especially by its data model.
In comparison to other databases such as [SQLite][sqlite] and
[Postgres][postgres], Noblit positions itself as follows:

|               | Client-server | Embedded |
|---------------|---------------|----------|
| **Mutable**   | Postgres      | SQLite   |
| **Immutable** | Datomic       | *Noblit* |

Noblit combines the low operational overhead of an embedded database with the
simplicity of an append-only database with value semantics.

[datomic]:  https://www.datomic.com/
[sqlite]:   https://sqlite.org/index.html
[postgres]: https://www.postgresql.org/about/

## Goals

 * An embeddable library for queries, and transactions.
 * Eventually supporting a client-server database on top of the library, with a
   single transactor and scale-out reads.
 * Supporting databases that do not fit in working memory.
 * Handling moderate write workloads and moderately sized data.
 * Relational queries in a machine-first query format.
 * Simplicity. First make it work, then make it fast. I am writing this down to
   remind myself to not micro-optimize. Complications should be justified by
   profiling measurements.
 * Easy to build, few or zero dependencies.

## Non-goals

 * Deleting data or updating data in place.
 * Writing large values (say, larger than 64 KiB). A key-value store or document
   store might be better suited for this.
 * High write throughput. If you produce large volumes of data quickly, probably
   not all of it remains valuable for a long time, and storage space might
   become a concern. A time series database or durable message queue might be
   a better alternative.
 * Portability. Little-endian architectures that run Linux are the only target.
