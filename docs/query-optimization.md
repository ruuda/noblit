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

There are two ways to optimize a query plan:

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

For a query with _n_ statements, there are _n!_ possible statement orders.
Trying every permutation quickly becomes prohibitive, especially because many
permutations lead to terrible query plans, so even trying each plan once can
take a long time. Fortunately, with some reasonable assumptions, it is possible
to find a good plan quickly.

**Assumption**: Adding an extra statement at the end of a query,
can not make it faster.

This assumption is justified, because every statement translates to a scan over
an index, so adding a statement results in strictly more work.

With the above assumption, we can express statement order optimization as a tree
search problem. The root of the tree is an empty query, and along the branches
we include one more statement of the original query. Interior nodes of the tree
are *partial queries*, while the leaves form all permutations of the statements.
To find the optimal query plan is to pick the best leaf node, and the assumption
we made implies that the query time of a child node is larger than the query
time of the parent node.

Given the tree of partial queries, we can explore the tree to find the leaf with
the minimal query time. Because the query time of a child is greater than that
of the parent, we can call the additional time the “cost” of an edge, and
finding the leaf with the minimal query time means finding a path through the
tree with minimal cost. Dijkstra’s algorithm solves this neatly:

 * Track an *open set* of candidate nodes.
 * Remove the candidate with the minimal query time from the open set, and add
   all of its children.
 * If the best candidate in the open set is a leaf node, then that is the
   optimal query plan.

While this algorithm will find the optimal query plan, it is not obvious that it
will find it quickly. The algorithm might explore breadth-first, and explore
most of the tree before it reaches a leaf node. But in practice, this is not
what happens. Early on, there tend to be a few partial queries that are so bad
that they are worse than many leaf nodes. This cuts off entire branches of the
tree, and the algorithm reaches a leaf node quickly.

## Optimizing scans

Also called *micro-optimization*. TODO: Write.

## Explore-exploit

TODO: Justify min * (1 - 1/sqrt(n)) ordering.
