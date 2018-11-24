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

The answer to a query, identify every variable in the query with the set values
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
