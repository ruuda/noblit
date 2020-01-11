# Query planning

Noblit features a straightforward and transparent query planner. On the one
hand, this means that query performance can vary a lot depending on how the
query is structured. On the other hand, this gives you a lot of control over
the query plan, and it ensures predictible performance.

## Plans

Noblit evaluates every query as a set of nested loops that scan over its
indexes. Every where-statement in the query translates to one loop. The query
plan determines the order of those loops, and which index to scan for each loop.

There are three kinds of scan in Noblit:

 * **A full index scan.** Such a scan can be used for statements where both the
   entity and the value are unknown. A full index scan provides two variables
   in one loop, but it may have to scan the entire index. For scans over
   attribute-leading indexes, the range is still constrained by attribute.
 * **A partial index scan.** Such a scan can be used for statements where either
   the entity or the value is known, either because it was provided by an outer
   loop, or because it is a constant in the query. A partial index scan can be
   used to find a single datom (for example, to look up the value of an
   attribute for a known entity in the <abbr>EAVT</abbr> index), or to look up a
   range of datoms (for example, to get all entities where an attribute has a
   known value in the <abbr>AVET</abbr> index).
 * **An existence test.** Such a scan can be used to test if a datom exists in
   the database when both the entity and value are known. If it does not exists,
   the loop associated with this can will have zero iterations, and the result
   of the entire query becomes empty.

The variables in a statement determine which scan is used:

 * If both the entity and value are variable, and neither of these variables was
   referenced in an earlier statement, a full index scan will be used.
 * If the statement contains one variable that was not referenced in an earlier
   statement, a partial index scan will be used.
 * If all variables in the statement were referenced in earlier statements, an
   existence test will be used.

Consequently, the order of statements in the query completely specifies what
kind of scans are used, and which variables are provided by every loop. There
is, however, some freedom left to the planner. When a scan can be serviced by
multiple indexes, the planner decides which one to use. For example, to look up
the `user.name` attribute of a known entity, we could use either
<abbr>EAVT</abbr>, or <abbr>AEVT</abbr>. The former facilitates row access like
a traditional relational database, whereas the latter facilitates column access
like in a column database. It depends on the full query which one is preferred.
If we are *only* selecting `user.name`, then column access would be preferred.
But if we are also selecting a few dozen other attributes of the user, then row
access would provide better locality. The planner uses heuristics to select the
index to use for each scan.

TODO: The planner should take schema into account. For example, for a
single-nonunique attribute, we expect a partial <abbr>EAVT</abbr> scan to be
cheaper than a partial <abbr>AVET</abbr> scan. But for a many-unique attribute,
the converse is true.

## Ordering

TODO: Write about interaction between ordering constraints and the query plan.

## Advice

TODO: Write down performance advice.

## Query optimization

TODO: Write down optimization idea.
