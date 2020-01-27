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

There are two aspects to optimization:

 * **Optimizing statement order**, also called *macro-optimization*, because
   statement order can make orders of magnitude difference in the evaluation
   time of a query. This is because statement order can make a difference in
   complexity, it can be the difference between a query plan that is quadratic
   in the size of an index, versus a query plan that is constant time. Statement
   order can be the difference between microseconds, and minutes.
 * **Choosing the indexes to use for a scan**, also called *micro-optimization*,
   because so far empirical evidence suggests that the performance difference
   between the various indexes that can service a scan is small. This is because
   the indexes have the same complexity, it is only locality effects that make
   one index more suitable than another. The difference between the best and
   worst plan may be a factor two in extreme cases, but not an order of
   magnitude.

Although the optimizer performs both macro and micro-optimization, currently the
indexes used can not be controlled by the query. Perhaps it would be best for
the optimizer to only consider plans that the planner generates.

## Optimizing statement order

Also called *macro-optimization*. TODO: Write. We explore the tree.

## Optimizing scans

Also called *micro-optimization*. TODO: Write.

## Explore-exploit

TODO: Justify min * (1 - 1/sqrt(n)) ordering.
