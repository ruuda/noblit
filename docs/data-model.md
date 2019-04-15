# Data model

Noblit stores *datoms*. Datoms assert or retract attributes of entities. For
example, entity 12 might have an attribute `user.email` with value
`rachael@tyrell.com`. Conceptually, the database is an append-only log of
(entity, attribute, value) tuples, together with the time at which they were
asserted or retracted in the form of a transaction id. A view of the database at
a given point in time is the set of all (entity, attribute, value) tuples that
have been asserted and not retracted before or at that time.

A datom is a tuple of the following five values:

 * **Entity**: An integer that uniquely identifies an entity in the database.
 * **Attribute**: Analogous to a column in a relational database.
 * **Value**: The value for the attribute.
 * **Transaction**: The transaction id of the transaction that added the datom.
 * **Operation**: Either _assert_ or _retract_.

To get a view of the database at a given transaction _t_, we exclude all datoms
with a transaction greater than _t_. Then we cancel assertions against
subsequent retractions. What is left is the set of (entity, attribute, value)
tuples that were true after transaction _t_.
