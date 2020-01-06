# Query

Queries in Noblit are declarative, based on logic programming, inspired by
Datalog. Queries consist of a number of statements (logical claims, not steps in
a procedure) that relate variables. The result of a query are the possible
assignments of values to the variables.

## Structure

A query has three parts:

 * **Where**, a collection of statements that are true of the answers.
 * **Select**, the variables whose values will be returned.
 * **Order by**, to control ordering. (Not implemented yet.)

The where-part of a query consists of (entity, attribute, value) tuples. The
entity and value can be variables or constants. By convention, below we use
single-character variable names to refer to entities, and full names to refer
to other types of values (strings and integers).

## Example

As an example, select all tracks by Muse from a music database, ordered by
release date and then by track number:

    where
      a artist.name "Muse"
      b album.artist a
      b album.title album_title
      b album.release.year year
      b album.release.month month
      b album.release.day day
      t track.album b
      t track.number number
      t track.title track_title
    select
      number, track_title, album_title
    order by
      year, month, day, number

Select all tracks titled "One", their artist, and release year:

    where
      t track.title "One"
      t track.album b
      b album.release.year year
      b album.artist a
      a artist.name artist
    select
      artist, year

## Semantics

To answer a query, identify every variable in the query with the set of values
it can assume. The answer to the query is the cartesian product of these sets,
filtered such that for every tuple in the product, a datom exists in the
database for each of the where-statements. For example, consider the following
database of (entity, attribute, value) pairs:

    1 person.name "Henk"
    2 person.name "Klaas"
    3 person.name "Piet"
    1 person.age 32
    2 person.age 54
    3 person.age 32

Suppose we want to find all pairs of people with the same age. We would query:

    where
      p person.age a
      q person.age a
      p person.name p_name
      q person.name q_name
    select
      p_name, q_name

This query has five variables. Taking the Cartesian product, we get:

| p | p_name | q | q_name | a  |
|---|--------|---|--------|----|
| 1 | Henk   | 1 | Henk   | 32 |
| 2 | Henk   | 1 | Henk   | 32 |
|   | ...    |   | ...    |    |
| 3 | Piet   | 3 | Piet   | 54 |

Filtering first by the first and last two statements (`p person.age a`,
`p person.name p_name`, and `q person.name q_name`), we are left with only
the tuples where `a` is the age of person `p`, and where the names and the
person ids match:

| p | p_name | q | q_name | a  |
|---|--------|---|--------|----|
| 1 | Henk   | 1 | Henk   | 32 |
| 2 | Klaas  | 1 | Henk   | 52 |
|   | ...    |   | ...    |    |
| 3 | Piet   | 3 | Piet   | 32 |

Filtering by the remaining statement `q person.age a`, we find all tuples that
satisfy the query:

| p | p_name | q | q_name | a  |
|---|--------|---|--------|----|
| 1 | Henk   | 1 | Henk   | 32 |
| 3 | Piet   | 1 | Henk   | 32 |
| 2 | Klaas  | 2 | Klaas  | 52 |
| 3 | Henk   | 3 | Piet   | 32 |
| 3 | Piet   | 3 | Piet   | 32 |

Finally, keeping only the selected columns yields the answer:

| p_name | q_name |
|--------|--------|
| Henk   | Henk   |
| Piet   | Henk   |
| Klaas  | Klaas  |
| Henk   | Piet   |
| Piet   | Piet   |

## Aggregations

*Note: This is an idea, it has not been implemented yet.*

For aggregations such as _sum_, _count_, and _min_ and _max_, we put the
aggregate in the select part of the query. For example, in an order database,
we could select the total amount billed in 2020:

    where
      i invoice.year 2020
      i invoice.amount_eur amount_eur
    select
      sum(amount_eur)

When mixing aggregates and non-aggregates, aggregates are taken over the
group keyed on the non-aggregate variables. For example, we may compute the
total amount billed per year:

    where
      i invoice.year year
      i invoice.amount_eur amount_eur
    select
      year, sum(amount_eur)

The above query will return one row per distinct year, it will not return the
same year twice. This is in contrast to the non-aggregate query, which would
return a row for every invoice entity.

Aggregates can always be streamed, the grouping behavior does never cause a
collection to be materialized in memory. This is because a query is evaluated as
a nested loop. Non-aggregated variables are on the outer loops, and aggregated
variables on the inner loops. This means that we visit one group key at a time.
