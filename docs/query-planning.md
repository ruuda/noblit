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
   the loop associated with this scan will have zero iterations (for a given
   iteration of the outer loops).

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

*Warning: The advice here is just a guess, and not based on measurements.*

Given the way that evaluation and the query planner work, it makes sense to
choose a statement order that constrains the ranges scanned as much as possible,
to minimize the number of loop iterations.

## Query optimization

Noblit does provide a query optimizer. Unlike traditional relational databases,
optimization is an explicit operation, it is not implicitly part of every query.
With the burden of runtime query plan optimization out of the way, Noblit can
focus on an off-line optimizer. This has several advantages over runtime query
optimization:

 * Because the runtime query planner is very simple, it is very fast.
 * There is less of a trade-off between spending time on planning and spending
   time on evaluation. Off-line (in the sense that it runs when instructed to,
   not implicitly as part of every query) we can afford to spend more time
   optimizing.
 * Now that we can afford to spend more time, the optimizer can _measure_, it
   does not have to guess. Instead of using statistics to try and estimate the
   cost of a given plan, Noblit can profile the query against your actual
   database.
 * Now that the optimizer measures, rather than estimates, there is no need to
   keep statistics about the data to base the estimates on. This eliminates a
   lot of complexity.

This optimization scheme is made possible by the control that a straightforward
query planner provides. If the planner would perform more advanced runtime
optimization, then there would be less opportunity to tell it exactly what kind
of plan we want.

Of course this optimization scheme has downsides too. In particular, off-line
optimization is not always possible or desirable.

 * For ad-hoc one-off queries, you have to keep performance in mind.
 * When representative data is unavailable (for example, when you only have a
   test database, not production data), results of off-line optimization may not
   be representative of real-world performance. Rules of thumb can help to
   ensure reasonable plans, but are no substitute for profiling.
 * There is no way to optimize generated or partially generated queries. What
   you *can* do though, is optimize a few generated queries, and use the lessons
   learned to tweak the generator.

The internals of the query optimizer are outlined in the [Query
optimization](query-optimization.md) chapter.
