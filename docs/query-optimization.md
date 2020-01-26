# Query optimization

The order of statements in a query affect the query plan that Noblit chooses. As
outlined in the [Query planning](query-planning.md) chapter, every statement
translates to a loop that scans over an index. The planner preserves statement
order: the first statement becomes the outer loop, the last statement becomes
the inner loop. Therefore, the statement order of the query can have a big
effect on query performance.

To help optimize the order of statements, Noblit features a query optimizer. The
optimizer is not part of the query planner. It is a standalone function that
takes a query and a database, and returns the optimized query.

## Optimizing statement order

Also called *macro-optimization*. TODO: Write. We explore the tree.

## Optimizing scans

Also called *micro-optimization*. TODO: Write.

## Explore-exploit

TODO: Justify min * (1 - 1/sqrt(n)) ordering.
