-- Step 1: Define two attributes: "name" and "activation_year".
-- The query returns the ids of these new attributes.

where
  uint64_t db.type.name "db.type.uint64"
  string_t db.type.name "db.type.string"

assert
  -- Define an attribute named "name" of type uint64.
  name db.attribute.name "name"
  name db.attribute.type string_t
  name db.attribute.unique true
  name db.attribute.many false

  -- TODO: Rename back to "activation_year" once large value persistence works.
  -- Define an attribute named "year" of type uint64.
  level db.attribute.name "year"
  level db.attribute.type uint64_t
  level db.attribute.unique false
  level db.attribute.many false

select
  level, name

┌───────┬───────┐
│ level │ name  │
├───────┼───────┤
│ # 204 │ # 202 │
└───────┴───────┘

-- Step 2: Now that we have these new attributes, we can create a few entities
-- that use them. We return the entity ids of the new entities.

where
  -- For now, we need a dummy in the where clause that produces exactly one
  -- value, because the assertions are executed once for every output of the
  -- where clause.
  _ db.type.name "db.type.uint64"

assert
  leon name "Leon Kowalski"
  leon year 2017

  pris name "Pris Stratton"
  pris year 2016

  roy name "Roy Batty"
  roy year 2016

select
  leon, pris, roy

┌───────┬───────┬───────┐
│ leon  │ pris  │ roy   │
├───────┼───────┼───────┤
│ # 202 │ # 204 │ # 206 │
└───────┴───────┴───────┘

-- Step 3: Update the schema: add a "model" attribute. The query returns its id.

where
  string_t db.type.name "db.type.string"

assert
  model db.attribute.name "model"
  model db.attribute.type string_t
  model db.attribute.unique false
  model db.attribute.many false

select
  model

┌───────┐
│ model │
├───────┤
│ # 202 │
└───────┘

-- Step 4: Set the "model" attribute for every entity "r" that has a "name"
-- attribute. All of the current entities are of the same model. Return the ids
-- of all affected entities.

where
  r name _

assert
  r model "Nexus 6"

select
  r

┌─────┐
│ r   │
├─────┤
│ # 4 |
│ # 5 |
│ # 6 |
└─────┘

-- Step 5: Double check that we now have all the data.

where
  r name name
  r model model
  r year activation_year

select
  name, model, activation_year

┌─────────────────┬───────────┬─────────────────┐
│ name            │ model     │ activation_year │
├─────────────────┼───────────┼─────────────────┤
│ "Leon Kowalski" │ "Nexus 6" │            2017 │
│ "Roy Batty"     │ "Nexus 6" │            2016 │
│ "Pris Stratton" │ "Nexus 6" │            2016 │
└─────────────────┴───────────┴─────────────────┘
