# Noblit

**Vaporware warning: much of the content below is hypothetical.**

Noblit is an embeddable append-only database. The database is a set of immutable
(entity, attribute, value, time) tuples called *datoms*. Datoms can be asserted
and retracted. A retraction is recorded as a new fact; it is not a delete. Any
historical state of the database can be reproduced, simply by ignoring new
datoms. Furthermore, Noblit makes history first-class and queryable.

Features:

 * **Simple but flexible data model.**
   Noblit stores (entity, attribute, value) tuples. A traditional relational
   model can be expressed like this, but less rigid structures such as graphs
   can be expressed too.
 * **Point in time queries.**
   Any historical state of the database can be reproduced and queried
   efficiently.
 * **Reified schema**.
   Noblit stores the schema as ordinary datoms in the database itself.
   The schema can be evolved through normal assertions and retractions,
   and the full history is available.
