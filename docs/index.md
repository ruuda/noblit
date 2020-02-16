# Noblit

*Vaporware warning: much of the content below is hypothetical.*

Noblit is an embeddable append-only database. The database records a history
of immutable (entity, attribute, value) tuples. Tuples can be asserted and
retracted. A retraction is recorded as a new fact; it is not a delete. Any
historical state of the database can be reproduced, and the history is
first-class and queryable.

## Features

 * **Embeddable.** Noblit comes as a library that you link into your
   application. Global installation or daemons are not necessary.
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
   Noblit stores the schema as entities in the database itself. The schema can
   be evolved through normal assertions and retractions, and the full migration
   history is available.
 * **Reified transactions**.
   Transactions in Noblit are entities that can have attributes like any other
   entity. Possibilities include the transaction timestamp and the user who
   initiated the transaction. Transactions can be inspected in queries.
 * **Profile-guided query optimization**.
   While queries are declarative, statement order affects the query plan. An
   explicit query optimizer can optimize a given query by measuring how
   alternatives perform, instead of having to estimate from statistics.

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

In comparison to Datomic, Noblit puts more focus on static type safety. This
makes schemas more rigid. On the one hand this impedes iterating quickly, but on
the other hand it helps to provide data consistency in the long term. Datomic
is a distributed system running on the <abbr>JVM</abbr>, while Noblit is a small
native library.

Another attempt at an embedded database inspired by Datomic was [Mentat][mentat].
Both Noblit and Mentat happen to be written in Rust. Mentat is a layer on top of
SQLite, whereas Noblit has its own storage backend. Mentat exists but is
unmaintained, Noblit is vaporware.

[datomic]:  https://www.datomic.com/
[sqlite]:   https://sqlite.org/index.html
[postgres]: https://www.postgresql.org/about/
[mentat]:   https://github.com/mozilla/mentat

## Goals

 * An embeddable library for queries and transactions.
 * Relational queries in a machine-first query format.
 * Durable storage on disk, with atomic transactions.
 * Easy to build, few or zero dependencies.
 * Handling moderate write workloads and moderately sized data.
 * Storing datasets that do not fit in working memory. The result of a query and
   data to insert must fit though.
 * Enabling — but not requiring — a client-server database on top of the library,
   with a single transactor and scale-out reads.
 * Simplicity. First make it work, then make it fast. I am writing this down to
   remind myself to not micro-optimize. Complications should be justified by
   profiling measurements.

## Non-goals

 * Deleting data or updating data in place.
 * High write throughput. If you produce large volumes of data quickly, probably
   not all of it remains valuable for a long time, and storage space might
   become a concern. A time series database or durable message queue might be
   a better alternative.
 * Having the fastest lookups. A key-value store may be better suited for this.
 * Portability. Little-endian architectures that run Linux are the only target.
