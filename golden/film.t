-- This file contains an example of a basic film database.

-- Before we can insert any data, we need to set up the schema. For this we
-- define four attributes. The schema below is analogous to the following SQL
-- schema, although there are some pretty fundamenal differences between SQL's
-- data model and Noblit's.
--
--   create table directors (
--     id       integer primary key autoincrement,
--     name     text
--   )
--   create table films (
--     id       integer primary key autoincrement,
--     title    text,
--     year     integer,
--     director integer,
--   )

where
  uint64_t db.type.name "db.type.uint64"
  string_t db.type.name "db.type.string"
  ref_t db.type.name "db.type.ref"

assert
  -- Define an attribute named "director.name" of type string.
  director_name db.attribute.name "director.name"
  director_name db.attribute.type string_t
  director_name db.attribute.unique true
  director_name db.attribute.many false

  -- Define an attribute named "film.title" of type string.
  film_title db.attribute.name "film.title"
  film_title db.attribute.type string_t
  film_title db.attribute.unique false
  film_title db.attribute.many false

  -- Define an attribute named "film.year" of type uint64.
  film_year db.attribute.name "film.year"
  film_year db.attribute.type uint64_t
  film_year db.attribute.unique false
  film_year db.attribute.many false

  -- Define an attribute named "film.director" of type ref.
  film_director db.attribute.name "film.director"
  film_director db.attribute.type ref_t
  film_director db.attribute.unique false
  film_director db.attribute.many false

select
  director_name, film_title, film_year, film_director

> ┌───────────────┬────────────┬───────────┬───────────────┐
> │ director_name │ film_title │ film_year │ film_director │
> ├───────────────┼────────────┼───────────┼───────────────┤
> │ # 101         │ # 102      │ # 103     │ # 104         │
> └───────────────┴────────────┴───────────┴───────────────┘

where
  -- For now, we need a dummy in the where clause that produces exactly one
  -- value, because the assertions are executed once for every output of the
  -- where clause.
  _ db.type.name "db.type.uint64"

assert
  -- Create three new entities, and set the director.name attribute for them.
  -- The variables "scott", "nolan", and "tarantino" will refer to these
  -- entities henceforth.
  scott director.name "Ridley Scott"
  nolan director.name "Christopher Nolan"
  tarantino director.name "Quentin Tarantino"

  -- Create a new entity with three attributes. The type of the film.title
  -- attribute is string, the type of film.year is uint64. The type of
  -- film.director is ref: it references an other entity. The variable "b"
  -- refers to the new entity.
  b film.title "Blade Runner"
  b film.director scott
  b film.year 1982

  p film.title "Pulp Fiction"
  p film.director tarantino
  p film.year 1994

  m film.title "Memento"
  m film.director nolan
  m film.year 2000

  d film.title "Django Unchained"
  d film.director tarantino
  d film.year 2012

  k film.title "The Dark Knight Rises"
  k film.director nolan
  k film.year 2012

select
  scott, nolan, tarantino, b, p, m, d, k

> ┌───────┬───────┬───────────┬───────┬───────┬───────┬───────┬───────┐
> │ scott │ nolan │ tarantino │ b     │ p     │ m     │ d     │ k     │
> ├───────┼───────┼───────────┼───────┼───────┼───────┼───────┼───────┤
> │ # 105 │ # 107 │ # 108     │ # 109 │ # 110 │ # 111 │ # 112 │ # 113 │
> └───────┴───────┴───────────┴───────┴───────┴───────┴───────┴───────┘

-- Now we can get all films released in 2012:

where
  -- Find all entities f, that have a film.year attribute with value 2012,
  -- and that have a film.title attribute. We don't restrict the value of
  -- the film.title attribute, but we do bind it to the variable "title".
  f film.year 2012
  f film.title title
select
  -- Return the entity id of the film, along with the title.
  f, title

> ┌───────┬─────────────────────────┐
> │ f     │ title                   │
> ├───────┼─────────────────────────┤
> │ # 112 │ "Django Unchained"      │
> │ # 113 │ "The Dark Knight Rises" │
> └───────┴─────────────────────────┘

-- All films directed by Christopher Nolan:

where
  nolan director.name "Christopher Nolan"
  f film.director nolan
  f film.title title
  f film.year year
select
  year, title

> ┌──────┬─────────────────────────┐
> │ year │ title                   │
> ├──────┼─────────────────────────┤
> │ 2000 │ "Memento"               │
> │ 2012 │ "The Dark Knight Rises" │
> └──────┴─────────────────────────┘

-- List all known directors:

where
  d director.name name
select
  name

> ┌─────────────────────┐
> │ name                │
> ├─────────────────────┤
> │ "Christopher Nolan" │
> │ "Quentin Tarantino" │
> │ "Ridley Scott"      │
> └─────────────────────┘
